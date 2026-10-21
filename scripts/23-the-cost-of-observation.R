# Generated from chapters/23-the-cost-of-observation.qmd by tools/extract.py -- do not edit by hand.
# Chapter: The Cost of Observation
#
# Run from the repository root:
#   Rscript scripts/23-the-cost-of-observation.R

library(dwellr)
library(ggplot2)
library(ranger)

source("R/theme.R")


## ---- population --------------------------------------------------
sim    <- simulate_lifecycles(n = 5000, seed = 11)
orders <- sim$orders
dwell  <- stage_dwell(sim)
lead_h <- as.numeric(tapply(dwell$dwell_hours, dwell$order_no,
                            sum)[orders$order_no])
lead   <- data.frame(
  dwell_hours = lead_h,
  censored = as.logical(tapply(dwell$censored, dwell$order_no,
                               any)[orders$order_no]))

breach <- 120
y      <- lead_h > breach
c_look <- 40
days   <- as.numeric(diff(range(as.Date(orders$entered_at))))
open   <- !orders$is_terminal

## ---- cost-structure ----------------------------------------------
obs_per_day <- 24 * 6 + 24 + 1          # the three cadences of 17.3

do.call(rbind, lapply(c(1, 100), function(scale) {
  k <- observation_cost(fixed_annual = 40000, per_observation = 0.004,
                        observations_per_day = obs_per_day,
                        per_decision = 0.012,
                        decisions_per_day = 13.9 * scale)
  data.frame(scale = paste0(scale, "x"),
             instrumentation = round(k$annual[1]),
             observation = round(k$annual[2]),
             decision = round(k$annual[3]),
             decision_share = k$share[3],
             total = round(k$annual[4]))
}))

## ---- crossover ---------------------------------------------------
c(decisions_per_day_to_match_instrumentation =
    round(40000 / (0.012 * 365)),
  multiple_of_this_operation = round(40000 / (0.012 * 365) / 13.9))

## ---- cadence -----------------------------------------------------
rate     <- sum(y) / days               # breaches arising per day
lag_cost <- 400 / 24                    # cost of a day of not noticing

o <- cadence_optimal(scan_cost = 0.004, arrival_rate = rate,
                     lag_cost = lag_cost)
c(conditions_per_day = round(rate, 2),
  optimal_interval_minutes = round(o$interval * 24 * 60, 1),
  scan_component = round(o$scan, 4),
  lag_component = round(o$lag, 4))

## ---- cadence-sensitivity -----------------------------------------
do.call(rbind, lapply(c(0.5, 0.75, 1, 2, 4), function(f)
  data.frame(multiple_of_optimum = f,
             minutes = round(f * o$interval * 24 * 60, 1),
             cost_penalty = round(cadence_cost(f * o$interval, 0.004, rate,
                                               lag_cost) / o$cost - 1, 4))))

## ---- deadline-shaped ---------------------------------------------
deadline <- function(wait_days) if (wait_days < 0.25) 0 else 400
on <- cadence_optimal(0.004, rate, lag_cost = NA, lag_cost_fn = deadline)

c(interval_minutes = round(on$interval * 24 * 60, 1),
  closed_form = on$closed_form)

## ---- allocation --------------------------------------------------
feat <- data.frame(
  special   = as.integer(orders$special_order),
  channel   = factor(orders$order_source),
  branch    = factor(orders$branch),
  lines     = orders$lines,
  log_value = log(orders$value),
  hour      = as.integer(format(orders$entered_at, "%H")))
set.seed(5)
tr  <- sample(c(TRUE, FALSE), nrow(orders), replace = TRUE, prob = c(0.6, 0.4))
fit <- ranger(y ~ ., data.frame(feat[tr, ], y = factor(y[tr])),
              probability = TRUE, num.trees = 400, seed = 1)
p   <- predict(fit, feat)$predictions[, "TRUE"]

ho      <- !tr
exposed <- (y * orders$value)[ho]
ranked  <- order(-(p * orders$value)[ho])

do.call(rbind, lapply(c(50, 100, 200, 400, 800), function(budget) {
  k <- head(ranked, budget)
  data.frame(items_decided = budget, metered_cost = budget * 0.012,
             breached_value_reached = round(sum(exposed[k])),
             value_per_dollar = round(sum(exposed[k]) / (budget * 0.012)))
}))

## ---- model-cost --------------------------------------------------
lines <- c(retraining = 12 * 4 * 150, pipeline = 24 * 150,
           monitoring = 8000, incident = 6 * 400, analyst = 0.15 * 140000)
data.frame(line = names(lines), annual = as.numeric(lines),
           share = round(as.numeric(lines) / sum(lines), 3))

## ---- renewal -----------------------------------------------------
cycles <- 12; capacity <- 1000; gain <- 0.02    # 9.21's measured gain

do.call(rbind, lapply(c(75, 180, 400), function(cm) {
  benefit <- cycles * capacity * gain * cm
  data.frame(miss_cost = cm, annual_benefit = round(benefit),
             annual_cost = round(sum(lines)),
             net = round(benefit - sum(lines)),
             decision = if (benefit > sum(lines)) "renew" else "retire")
}))

c(break_even_precision_gain = round(sum(lines) / (cycles * capacity * 180), 4))

## ---- not-worth-looking -------------------------------------------
forms <- list(
  `whole order value` = orders$value[open],
  `margin times loss probability` = 0.22 * 0.15 * orders$value[open],
  `flat retention cost` = rep(180, sum(open)))

do.call(rbind, lapply(names(forms), function(f) {
  worth <- p[open] * forms[[f]] > c_look
  data.frame(miss_cost_form = f, below_the_bar = sum(!worth),
             share_of_items = round(mean(!worth), 4),
             their_value = round(sum(orders$value[open][!worth])),
             share_of_exposure = round(sum(orders$value[open][!worth]) /
                                       sum(orders$value[open]), 4))
}))

## ---- falling-cost ------------------------------------------------
do.call(rbind, lapply(c(1, 0.1, 0.01, 0.001), function(f)
  data.frame(scan_cost_multiple = f,
             interval_minutes = round(cadence_optimal(0.004 * f, rate,
                                                      lag_cost)$interval *
                                      24 * 60, 2))))

## ---- miss-cost ---------------------------------------------------
m <- miss_cost(order_value = round(mean(orders$value), 2), expedite = 75,
               margin_rate = 0.22, loss_probability = 0.15,
               lifetime_value = 18000, churn_probability = 0.01,
               cost_look = c_look)
m

## ---- threshold-spread --------------------------------------------
do.call(rbind, lapply(seq_len(nrow(m)), function(i) {
  t <- threshold_from_dwell_quantile(lead, cost_miss = m$miss_cost[i],
                                     cost_look = c_look)
  data.frame(approach = m$approach[i], miss_cost = m$miss_cost[i],
             p_star = round(t$fractile, 4),
             threshold_hours = round(t$hours, 2),
             flagged = sum(lead_h > t$hours))
}))

## ---- business-case -----------------------------------------------
coverage <- 0.5973                              # 21.11's defensible claim
breaches_per_year <- sum(y) / days * 365
annual_cost <- 40308

do.call(rbind, lapply(c(75, 180, 400), function(cm)
  do.call(rbind, lapply(c(0.05, 0.10, 0.20), function(eff) {
    benefit <- breaches_per_year * coverage * eff * cm
    data.frame(miss_cost = cm, effect = eff,
               breaches_addressed = round(breaches_per_year * coverage * eff, 1),
               annual_benefit = round(benefit), annual_cost = annual_cost,
               net = round(benefit - annual_cost),
               verdict = if (benefit > annual_cost) "positive" else "negative")
  }))))

c(breaches_per_year = round(breaches_per_year),
  break_even_effect_at_180 =
    round(annual_cost / (breaches_per_year * coverage * 180), 3))
