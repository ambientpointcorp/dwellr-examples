# Generated from chapters/15-attention-capacity.qmd by tools/extract.py -- do not edit by hand.
# Chapter: Attention Capacity
#
# Run from the repository root:
#   Rscript scripts/15-attention-capacity.R

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
  order_no    = orders$order_no, stage = "lead",
  dwell_hours = as.numeric(total[orders$order_no]),
  censored    = as.logical(tapply(dwell$censored, dwell$order_no,
                                  any)[orders$order_no]),
  stringsAsFactors = FALSE)

md <- data.frame(
  y = factor(lead$dwell_hours > 120),
  special = as.integer(orders$special_order),
  channel = factor(orders$order_source),
  branch  = factor(orders$branch),
  lines   = orders$lines,
  log_value = log(orders$value),
  hour    = as.integer(format(orders$entered_at, "%H")))

set.seed(5)
tr  <- sample(c(TRUE, FALSE), nrow(md), replace = TRUE, prob = c(0.6, 0.4))
fit <- ranger(y ~ ., md[tr, ], probability = TRUE, num.trees = 400, seed = 1)
score <- predict(fit, md[!tr, ])$predictions[, "TRUE"]
hit   <- md$y[!tr] == "TRUE"
value <- orders$value[!tr]

## ---- arrival-rates -----------------------------------------------
span <- as.numeric(difftime(max(orders$entered_at), min(orders$entered_at),
                            units = "days"))

do.call(rbind, lapply(list(c(400, 40), c(400, 400), c(40, 400), c(10, 400)),
  function(cc) {
    r <- threshold_from_dwell_quantile(lead, cc[1], cc[2])
    data.frame(c_miss = cc[1], c_look = cc[2],
               threshold_hours = round(r$hours, 2),
               flagged = round(r$flagged_share, 4),
               alerts_per_day = round(r$flagged_share * nrow(orders) / span, 1))
  }))

## ---- stability ---------------------------------------------------
do.call(rbind, lapply(c(37.9, 20.8, 6.4, 3.8), function(lam) {
  q <- queue_stability(alerts_per_day = lam, reviews_per_day = 12)
  data.frame(alerts_per_day = lam,
             utilization = round(q$utilization, 2),
             stable = q$stable,
             queue_length = if (is.finite(q$queue_length))
               round(q$queue_length, 1) else Inf)
}))

## ---- fig-utilization ---------------------------------------------
u <- data.frame(rho = seq(0.05, 0.95, by = 0.01))
u$queue <- u$rho / (1 - u$rho)
pts <- data.frame(rho = c(3.8, 6.4) / 12)
pts$queue <- pts$rho / (1 - pts$rho)

ggplot(u, aes(rho, queue)) +
  geom_line(colour = "grey25") +
  geom_point(data = pts, size = 2.4, colour = "grey20") +
  geom_text(data = pts, aes(label = sprintf("%.2f", rho)), vjust = -0.9,
            size = 2.9) +
  labs(x = "utilization (alerts per day / reviews per day)",
       y = "expected backlog (items)")

## ---- marginal ----------------------------------------------------
adm <- capacity_admit(score, capacity = 125, outcome = hit, value = value,
                      p_star = 0.5, band = 25)

head(round(adm$marginal, 4), 8)

## ---- fig-marginal ------------------------------------------------
m <- capacity_admit(score, capacity = length(score), outcome = hit,
                    band = 25)$marginal
m <- m[m$up_to <= 400, ]

ggplot(m, aes(up_to)) +
  geom_hline(yintercept = c(0.5, 0.25), linetype = 2, colour = "grey65") +
  geom_line(aes(y = cumulative_precision), colour = "grey25") +
  geom_point(aes(y = band_precision), size = 1.6, colour = "grey45") +
  annotate("text", x = 390, y = 0.52, label = "p* = 0.50", size = 2.8,
           hjust = 1, colour = "grey40") +
  annotate("text", x = 390, y = 0.27, label = "p* = 0.25", size = 2.8,
           hjust = 1, colour = "grey40") +
  labs(x = "capacity (orders reviewed)", y = "precision")

## ---- paying-capacity ---------------------------------------------
do.call(rbind, lapply(c(0.5, 0.25, 0.1210, 0.0909), function(ps) {
  a <- capacity_admit(score, capacity = 125, outcome = hit, p_star = ps,
                      band = 25)
  data.frame(p_star = ps, paying_capacity = a$paying_capacity)
}))

## ---- design-rate -------------------------------------------------
holdout_days <- span * 0.4

do.call(rbind, lapply(c(0.15, 0.07, 0.03), function(sh) {
  k <- round(sh * length(score))
  a <- capacity_admit(score, capacity = k, outcome = hit)
  data.frame(design_share = sh, queue = k,
             precision = round(a$precision, 4),
             recall = round(a$recall, 4),
             per_day = round(k / holdout_days, 1))
}))

## ---- silence-test ------------------------------------------------
do.call(rbind, lapply(c(3.0, 6.4), function(lam)
  data.frame(designed_per_day = lam,
             p_silent_day = round(exp(-lam), 4),
             p_three_silent_days = signif(exp(-lam)^3, 3))))

## ---- admission ---------------------------------------------------
small <- capacity_admit(score, capacity = 24, outcome = hit, value = value)
large <- capacity_admit(score, capacity = 125, outcome = hit, value = value)

rbind(
  two_reviewers = c(capacity = small$capacity,
                    precision = round(small$precision, 4),
                    recall = round(small$recall, 4),
                    value_share = round(small$value_share, 4)),
  ten_reviewers = c(capacity = large$capacity,
                    precision = round(large$precision, 4),
                    recall = round(large$recall, 4),
                    value_share = round(large$value_share, 4)))

## ---- abc-attention -----------------------------------------------
open <- orders[!orders$is_terminal, ]
v <- sort(open$value, decreasing = TRUE)
cum <- cumsum(v) / sum(v)

do.call(rbind, lapply(c(0.1, 0.2, 0.3, 0.5), function(sh)
  data.frame(top_share = sh,
             share_of_open_value = round(cum[round(sh * length(v))], 3))))

## ---- routing-concentration ---------------------------------------
adm24 <- capacity_admit(score, capacity = 24, outcome = hit)
sel <- adm24$admitted

c(admitted = sum(sel),
  specialty = sum(md$special[!tr][sel] == 1),
  stock = sum(md$special[!tr][sel] == 0))
