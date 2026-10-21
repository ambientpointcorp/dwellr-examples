# Generated from chapters/16-stability-suppression-cooldown-and-hysteresis.qmd by tools/extract.py -- do not edit by hand.
# Chapter: Stability: Suppression, Cooldown, and Hysteresis
#
# Run from the repository root:
#   Rscript scripts/16-stability-suppression-cooldown-and-hysteresis.R

library(dwellr)
library(ggplot2)
library(ranger)

source("R/theme.R")


## ---- population --------------------------------------------------
sim    <- simulate_lifecycles(n = 5000, seed = 11)
orders <- sim$orders
dwell  <- stage_dwell(sim)

total <- tapply(dwell$dwell_hours, dwell$order_no, sum)
orders$lead <- as.numeric(total[orders$order_no])

t0 <- min(orders$entered_at)
orders$in_day  <- as.numeric(difftime(orders$entered_at, t0, units = "days"))
orders$out_day <- orders$in_day + orders$lead / 24
horizon <- floor(max(orders$in_day))

md <- data.frame(
  y = factor(orders$lead > 120),
  special = as.integer(orders$special_order),
  channel = factor(orders$order_source),
  branch  = factor(orders$branch),
  lines   = orders$lines,
  log_value = log(orders$value))

set.seed(5)
fit <- ranger(y ~ ., md, probability = TRUE, num.trees = 300, seed = 1)
base_score <- predict(fit, md)$predictions[, "TRUE"]

## ---- replay-elapsed ----------------------------------------------
replay_elapsed <- function(window_days, p = 0.90) {
  state <- rep(FALSE, nrow(orders)); flips <- rep(0L, nrow(orders))
  thr_hist <- numeric(0); queue <- integer(0)
  for (d in seq(window_days + 1, horizon)) {
    done <- orders$out_day <= d & orders$out_day > d - window_days
    if (sum(done) < 20) next
    thr <- quantile(orders$lead[done], p)
    thr_hist <- c(thr_hist, thr)
    live <- orders$in_day <= d & orders$out_day > d
    elapsed <- (d - orders$in_day) * 24
    new <- live & elapsed > thr
    flips <- flips + (new != state)
    state <- new
    queue <- c(queue, sum(new))
  }
  list(flips = flips, threshold_cv = sd(thr_hist) / mean(thr_hist),
       mean_queue = mean(queue))
}

do.call(rbind, lapply(c(7, 30), function(w) {
  r <- replay_elapsed(w)
  ever <- r$flips > 0
  data.frame(window_days = w,
             threshold_cv = round(r$threshold_cv, 4),
             ever_alerted = sum(ever),
             flipped_more_than_twice = sum(r$flips > 2),
             mean_queue = round(r$mean_queue, 1))
}))

## ---- replay-score ------------------------------------------------
replay_score <- function(K_in, K_out, noise, smooth = 0, seed = 9) {
  set.seed(seed)
  state <- rep(FALSE, nrow(orders)); flips <- rep(0L, nrow(orders))
  prev <- base_score; queue <- integer(0)
  for (d in seq(31, horizon)) {
    live <- orders$in_day <= d & orders$out_day > d
    s <- rep(-Inf, nrow(orders))
    raw <- base_score[live] + rnorm(sum(live), 0, noise)
    s[live] <- if (smooth > 0) smooth * prev[live] + (1 - smooth) * raw else raw
    prev[live] <- s[live]
    srt <- sort(s[live], decreasing = TRUE)
    cut_in  <- srt[min(K_in,  sum(live))]
    cut_out <- srt[min(K_out, sum(live))]
    new <- state
    new[live & !state & s >= cut_in]  <- TRUE
    new[live &  state & s <  cut_out] <- FALSE
    new[!live] <- FALSE
    flips <- flips + (new != state); state <- new
    queue <- c(queue, sum(new))
  }
  ever <- flips > 0
  c(admitted = sum(ever), flipped = sum(flips > 2),
    share = mean(flips[ever] > 2), mean_queue = mean(queue))
}

do.call(rbind, lapply(c(0.02, 0.10), function(n) {
  r <- replay_score(24, 24, n)
  data.frame(score_noise = n, admitted = r[["admitted"]],
             flipped_more_than_twice = r[["flipped"]],
             share = round(r[["share"]], 3))
}))

## ---- hysteresis --------------------------------------------------
do.call(rbind, lapply(c(24, 36, 48, 72), function(ko) {
  r <- replay_score(24, ko, noise = 0.05)
  data.frame(surface_rank = 24, clear_rank = ko,
             flipped_more_than_twice = r[["flipped"]],
             share = round(r[["share"]], 3),
             mean_queue = round(r[["mean_queue"]], 1))
}))

## ---- cooldown ----------------------------------------------------
rel <- stage_dwell(sim, "released")

c(median = round(dwell_quantile(rel, 0.5), 2),
  p75 = round(dwell_quantile(rel, 0.75), 2),
  p90 = round(dwell_quantile(rel, 0.9), 2))

do.call(rbind, lapply(c(1, 2, 4, 8, 24), function(cd)
  data.frame(cooldown_hours = cd,
             still_in_stage = round(mean(rel$dwell_hours > cd), 3))))

## ---- churn -------------------------------------------------------
churn_rate <- function(noise, K = 24, seed = 9) {
  set.seed(seed)
  prev <- NULL; out <- numeric(0)
  for (d in seq(31, horizon)) {
    live <- which(orders$in_day <= d & orders$out_day > d)
    if (length(live) < K) next
    s <- base_score[live] + rnorm(length(live), 0, noise)
    top <- orders$order_no[live[order(s, decreasing = TRUE)[1:K]]]
    if (!is.null(prev)) out <- c(out, length(setdiff(top, prev)) / K)
    prev <- top
  }
  mean(out)
}

do.call(rbind, lapply(c(0, 0.02, 0.05, 0.10), function(n)
  data.frame(score_noise = n,
             daily_churn = round(churn_rate(n), 3),
             items_changing = round(churn_rate(n) * 24, 1))))

## ---- smoothing ---------------------------------------------------
do.call(rbind, lapply(c(0, 0.5, 0.8), function(sm) {
  r <- replay_score(24, 24, noise = 0.05, smooth = sm)
  data.frame(smoothing_weight = sm, admitted = r[["admitted"]],
             flipped_more_than_twice = r[["flipped"]],
             share = round(r[["share"]], 3))
}))

## ---- digest ------------------------------------------------------
do.call(rbind, lapply(c(8, 24), function(thr) {
  over <- rel$dwell_hours > thr
  data.frame(threshold_hours = thr, visits_exceeding = sum(over),
             resolved_within_24h = round(mean(rel$dwell_hours[over] <=
                                              thr + 24), 3))
}))

## ---- ladder ------------------------------------------------------
lead <- data.frame(
  order_no = orders$order_no, stage = "lead", dwell_hours = orders$lead,
  censored = as.logical(tapply(dwell$censored, dwell$order_no,
                               any)[orders$order_no]),
  stringsAsFactors = FALSE)
span <- as.numeric(difftime(max(orders$entered_at), min(orders$entered_at),
                            units = "days"))

do.call(rbind, lapply(c(0.75, 0.90, 0.97, 0.99), function(q)
  data.frame(quantile = q,
             rung_hours = round(dwell_quantile(lead, q), 1),
             share_reached = round(1 - q, 4),
             per_day = round((1 - q) * nrow(orders) / span, 1))))

## ---- fig-tradeoff ------------------------------------------------
band <- do.call(rbind, lapply(c(24, 30, 36, 48, 72), function(ko) {
  r <- replay_score(24, ko, noise = 0.05)
  data.frame(width = ko - 24, share = r[["share"]],
             queue = r[["mean_queue"]])
}))

ggplot(band, aes(width, share)) +
  geom_line(colour = "grey60") +
  geom_point(size = 2.4, colour = "grey20") +
  geom_text(aes(label = sprintf("queue %.1f", queue)), vjust = -0.9,
            size = 2.8, colour = "grey40") +
  expand_limits(y = c(0, 0.27)) +
  scale_y_continuous(labels = function(x) sprintf("%.0f%%", 100 * x)) +
  labs(x = "hysteresis band (ranks between surface and clear)",
       y = "share of admitted orders flapping")
