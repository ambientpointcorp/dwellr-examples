# Generated from chapters/14-setting-thresholds.qmd by tools/extract.py -- do not edit by hand.
# Chapter: Setting Thresholds
#
# Run from the repository root:
#   Rscript scripts/14-setting-thresholds.R

library(dwellr)
library(ggplot2)
library(ranger)

source("R/theme.R")


## ---- population --------------------------------------------------
sim    <- simulate_lifecycles(n = 5000, seed = 11)
orders <- sim$orders
dwell  <- stage_dwell(sim)

total <- tapply(dwell$dwell_hours, dwell$order_no, sum)
lead  <- data.frame(
  order_no    = orders$order_no,
  stage       = "lead",
  dwell_hours = as.numeric(total[orders$order_no]),
  censored    = as.logical(tapply(dwell$censored, dwell$order_no,
                                  any)[orders$order_no]),
  stringsAsFactors = FALSE)

## ---- at-risk -----------------------------------------------------
c(orders = nrow(lead),
  still_open = sum(lead$censored),
  median_hours = round(dwell_quantile(lead, 0.5), 2),
  p90_hours = round(dwell_quantile(lead, 0.9), 2))

## ---- fractile-table ----------------------------------------------
ratios <- list(c(400, 10), c(400, 40), c(400, 400), c(40, 400), c(10, 400))

do.call(rbind, lapply(ratios, function(cc) {
  r <- threshold_from_dwell_quantile(lead, cost_miss = cc[1], cost_look = cc[2])
  data.frame(c_miss = cc[1], c_look = cc[2],
             p_star = round(r$fractile, 4),
             threshold_hours = round(r$hours, 2),
             flagged_share = round(r$flagged_share, 4))
}))

## ---- inverted ----------------------------------------------------
c(correct = round(threshold_from_dwell_quantile(lead, 400, 40)$hours, 2),
  inverted = round(dwell_quantile(lead, 400 / 440), 2))

## ---- fig-fractile ------------------------------------------------
grid <- data.frame(share = seq(0.02, 0.98, by = 0.02))
grid$hours <- vapply(grid$share, function(s) dwell_quantile(lead, s),
                     numeric(1))

ggplot(grid, aes(share, hours)) +
  geom_line(colour = "grey25") +
  geom_point(data = grid[grid$share %in% c(0.10, 0.50, 0.90), ],
             size = 2, colour = "grey20") +
  scale_y_log10() +
  scale_x_continuous(labels = function(x) sprintf("%.0f%%", 100 * x)) +
  labs(x = expression(paste(italic(p), "* = cost of a look as a share of a miss")),
       y = "threshold (hours, log scale)")

## ---- posterior ---------------------------------------------------
rel <- stage_dwell(sim, "released")
mix <- dwell_mixture(rel)

round(mix$weight, 3)

do.call(rbind, lapply(c(1, 6, 24, 72), function(t)
  data.frame(hours_waited = t,
             p_slow = round(mix$posterior(t, censored = TRUE), 4))))

## ---- breach-model ------------------------------------------------
breach <- 120
md <- data.frame(
  y       = factor(lead$dwell_hours > breach),
  special = as.integer(orders$special_order),
  channel = factor(orders$order_source),
  branch  = factor(orders$branch),
  lines   = orders$lines,
  log_value = log(orders$value),
  hour    = as.integer(format(orders$entered_at, "%H")))

set.seed(5)
tr <- sample(c(TRUE, FALSE), nrow(md), replace = TRUE, prob = c(0.6, 0.4))
fit <- ranger(y ~ ., md[tr, ], probability = TRUE, num.trees = 400, seed = 1)
pr  <- predict(fit, md[!tr, ])$predictions[, "TRUE"]
yv  <- md$y[!tr] == "TRUE"

auc <- function(s, y) {
  r <- rank(s); n1 <- sum(y); n0 <- length(y) - n1
  (sum(r[y == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0)
}

c(base_rate = round(mean(yv), 4),
  holdout_auc = round(auc(pr, as.integer(yv)), 4),
  mean_predicted = round(mean(pr), 4))

## ---- calibration-vs-discrimination -------------------------------
p_star <- 40 / (40 + 400)

do.call(rbind, lapply(list(c("as fitted", "1"), c("squashed", "2"),
                           c("inflated", "0.5")), function(x) {
  q <- pr ^ as.numeric(x[2])
  data.frame(scores = x[1],
             auc = round(auc(q, as.integer(yv)), 4),
             mean_probability = round(mean(q), 4),
             flagged = sum(q > p_star),
             precision = round(if (sum(q > p_star)) mean(yv[q > p_star])
                               else NA_real_, 4))
}))

## ---- predicted-breach --------------------------------------------
do.call(rbind, lapply(list(c(400, 400), c(200, 400), c(100, 400)), function(cc) {
  ps <- cc[2] / (cc[1] + cc[2])
  fire <- pr > ps
  data.frame(c_miss = cc[1], c_look = cc[2], p_star = round(ps, 4),
             fires_on = sum(fire),
             precision = round(if (sum(fire)) mean(yv[fire]) else NA_real_, 4),
             recall = round(sum(yv & fire) / sum(yv), 4))
}))

## ---- cohort-failure ----------------------------------------------
fire <- pr > 0.5
do.call(rbind, lapply(c(1, 0), function(lv) {
  i <- md$special[!tr] == lv
  data.frame(special = lv, orders = sum(i),
             fire_rate = round(mean(fire[i]), 3),
             actual_breach_rate = round(mean(yv[i]), 3))
}))

## ---- attention-budget --------------------------------------------
monitoring_budget(annual_value = 398300, cost_per_review = 4,
                  reviews_per_year = 30 * 52)

## ---- tiers -------------------------------------------------------
warn   <- threshold_from_dwell_quantile(lead, cost_miss = 400, cost_look = 40)
breach_t <- threshold_from_dwell_quantile(lead, cost_miss = 40, cost_look = 400)

rbind(warn = c(p_star = round(warn$fractile, 4),
               hours = round(warn$hours, 2),
               flagged = round(warn$flagged_share, 4)),
      breach = c(p_star = round(breach_t$fractile, 4),
                 hours = round(breach_t$hours, 2),
                 flagged = round(breach_t$flagged_share, 4)))

## ---- tier-band ---------------------------------------------------
c(in_band = sum(lead$dwell_hours > warn$hours &
                lead$dwell_hours <= breach_t$hours),
  share = round(mean(lead$dwell_hours > warn$hours &
                     lead$dwell_hours <= breach_t$hours), 4))

## ---- exposure ----------------------------------------------------
open <- orders[!orders$is_terminal, ]
c(open_orders = nrow(open),
  min_value = round(min(open$value), 2),
  median_value = round(median(open$value), 2),
  max_value = round(max(open$value), 2),
  ratio = round(max(open$value) / min(open$value)))

## ---- value-weighted ----------------------------------------------
c_m_i   <- 0.25 * open$value          # a miss costs a quarter of the order
p_star_i <- 400 / (c_m_i + 400)
thr_i   <- vapply(p_star_i, function(q) dwell_quantile(lead, q), numeric(1))

c(p_star_min = round(min(p_star_i), 4),
  p_star_max = round(max(p_star_i), 4),
  threshold_min_hours = round(min(thr_i), 1),
  threshold_max_hours = round(max(thr_i), 1),
  flat_threshold_hours = round(
    threshold_from_dwell_quantile(lead, mean(c_m_i), 400)$hours, 1))
