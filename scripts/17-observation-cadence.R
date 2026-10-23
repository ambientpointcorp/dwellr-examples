# Generated from chapters/17-observation-cadence.qmd by tools/extract.py -- do not edit by hand.
# Chapter: Observation Cadence
#
# Run from the repository root:
#   Rscript scripts/17-observation-cadence.R

library(dwellr)
library(ggplot2)

source("R/theme.R")


## ---- population --------------------------------------------------
sim    <- simulate_lifecycles(n = 5000, seed = 11)
orders <- sim$orders
events <- sim$events
dwell  <- stage_dwell(sim)

total <- tapply(dwell$dwell_hours, dwell$order_no, sum)
orders$lead <- as.numeric(total[orders$order_no])

t0 <- min(orders$entered_at)
orders$in_h  <- as.numeric(difftime(orders$entered_at, t0, units = "hours"))
orders$out_h <- orders$in_h + orders$lead
events$h <- as.numeric(difftime(events$occurred_at, t0, units = "hours"))

## ---- detection-lag -----------------------------------------------
threshold <- 120
breached  <- orders$lead > threshold
c(orders = nrow(orders), breaching = sum(breached))

do.call(rbind, lapply(c(0.25, 1, 4, 8, 24, 72), function(R) {
  breach_at <- orders$in_h[breached] + threshold
  detect_at <- ceiling(breach_at / R) * R
  finish_at <- orders$in_h[breached] + orders$lead[breached]
  data.frame(R_hours = R,
             mean_lag = round(mean(detect_at - breach_at), 2),
             p90_lag  = round(quantile(detect_at - breach_at, 0.9)[[1]], 2),
             alerts_after_the_order_finished =
               round(mean(detect_at > finish_at), 4))
}))

## ---- lag-share ---------------------------------------------------
do.call(rbind, lapply(c(8, 24, 120), function(th)
  do.call(rbind, lapply(c(1, 4, 24), function(R)
    data.frame(threshold_hours = th, R_hours = R,
               mean_lag_as_share_of_threshold =
                 sprintf("%.1f%%", 100 * (R / 2) / th))))))

## ---- interval-cost -----------------------------------------------
c_obs   <- 40     # cost of one observation cycle across the estate
c_stall <- 2      # cost per hour a breach goes undetected
days    <- max(orders$in_h) / 24

interval_cost <- function(R) {
  breach_at <- orders$in_h[breached] + threshold
  lag <- ceiling(breach_at / R) * R - breach_at
  c(observation = c_obs * 24 * days / R,
    undetected  = c_stall * sum(lag))
}

do.call(rbind, lapply(c(0.5, 1, 2, 4, 8, 12, 24, 48), function(R) {
  x <- interval_cost(R)
  data.frame(R_hours = R, observation = round(x[["observation"]]),
             undetected = round(x[["undetected"]]),
             total = round(sum(x)))
}))

## ---- fig-interval ------------------------------------------------
grid <- data.frame(R = seq(0.5, 48, by = 0.25))
parts <- t(vapply(grid$R, interval_cost, numeric(2)))
grid$observation <- parts[, 1]
grid$undetected  <- parts[, 2]
grid$total <- grid$observation + grid$undetected
best <- grid$R[which.min(grid$total)]

ggplot(grid, aes(R)) +
  geom_line(aes(y = observation), linetype = 2, colour = "grey60") +
  geom_line(aes(y = undetected), linetype = 3, colour = "grey60") +
  geom_line(aes(y = total), colour = "grey20") +
  geom_vline(xintercept = best, colour = "grey45") +
  scale_y_log10(labels = scales::comma) +
  labs(x = "review interval (hours)", y = "cost over the horizon (log scale)")

## ---- observation-cost --------------------------------------------
open_per_day <- vapply(seq_len(floor(days)), function(d)
  sum(orders$in_h <= d * 24 & orders$out_h > d * 24), numeric(1))
changed_per_day <- vapply(seq_len(floor(days)), function(d)
  sum(events$h > (d - 1) * 24 & events$h <= d * 24), numeric(1))

c(open_records_a_day = round(mean(open_per_day)),
  records_that_changed_a_day = round(mean(changed_per_day)),
  reads_polling_every_open_record_at_10_min =
    round(mean(open_per_day) * 144),
  reads_with_a_change_fingerprint = round(mean(changed_per_day) + 144))

## ---- stage-cadence -----------------------------------------------
do.call(rbind, lapply(c("received", "awaiting_supply", "credit_hold",
                        "released", "backordered"), function(st) {
  d <- stage_dwell(sim, st)
  p90 <- dwell_quantile(d, 0.9)
  data.frame(stage = st, visits = nrow(d),
             median_hours = round(dwell_quantile(d, 0.5), 2),
             p90_hours = round(p90, 2),
             cadence_for_10pc_lag = round(0.2 * p90, 2))
}))

## ---- push-blind --------------------------------------------------
sampled <- which(breached)[seq_len(min(500, sum(breached)))]
emitted <- vapply(sampled, function(i)
  sum(events$order_no == orders$order_no[i] &
      events$h >  orders$in_h[i] &
      events$h <= orders$in_h[i] + threshold), numeric(1))

c(sampled_orders = length(emitted),
  emitted_no_event_before_breaching = round(mean(emitted == 0), 3),
  median_events_emitted = median(emitted))

## ---- ordering ----------------------------------------------------
set.seed(3)
shuffled <- events[sample(nrow(events)), ]

rebuild <- function(ev) {
  d <- stage_dwell(structure(list(orders = orders, events = ev,
                                  lines = sim$lines, items = sim$items,
                                  as_of = sim$as_of, spec = sim$spec),
                             class = "lifecycle_sim"))
  d[order(d$order_no, d$stage, d$entered_at), ]
}

isTRUE(all.equal(rebuild(events[order(events$occurred_at,
                                      events$order_no), ]),
                 rebuild(shuffled), check.attributes = FALSE))

## ---- cold-start --------------------------------------------------
switch_on <- 60 * 24
live <- orders$in_h < switch_on & orders$out_h > switch_on
elapsed <- switch_on - orders$in_h[live]

c(orders_in_flight = sum(live),
  median_true_elapsed = round(median(elapsed), 1),
  p90_true_elapsed = round(quantile(elapsed, 0.9)[[1]], 1),
  max_true_elapsed = round(max(elapsed), 1),
  already_past_threshold = round(mean(elapsed > threshold), 3))
