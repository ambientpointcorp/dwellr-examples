# Generated from chapters/13-deadlines-and-date-fields.qmd by tools/extract.py -- do not edit by hand.
# Chapter: Deadlines and Date Fields
#
# Run from the repository root:
#   Rscript scripts/13-deadlines-and-date-fields.R

library(dwellr)
library(ggplot2)
library(survival)

source("R/theme.R")


## ---- population --------------------------------------------------
sim    <- simulate_lifecycles(n = 5000, seed = 11)
orders <- sim$orders
dwell  <- stage_dwell(sim)

total <- tapply(dwell$dwell_hours, dwell$order_no, sum)
orders$lead_hours  <- as.numeric(total[orders$order_no])
orders$censored    <- as.logical(tapply(dwell$censored, dwell$order_no,
                                        any)[orders$order_no])
orders$entered_day <- as.Date(orders$entered_at)

requested_lead <- as.integer(orders$requested_date - orders$entered_day)
promised_lead  <- as.integer(orders$promised_date - orders$entered_day)
has_promise    <- !is.na(orders$promised_date)

## ---- field-test --------------------------------------------------
c(equals_order_date = sum(requested_lead == 0),
  of_total = nrow(orders),
  share = round(mean(requested_lead == 0), 3))

table(pmin(requested_lead, 8))

## ---- field-stats -------------------------------------------------
c(mean_lead_days = round(mean(requested_lead), 2),
  sd_lead_days = round(sd(requested_lead), 2),
  mean_among_overridden = round(mean(requested_lead[requested_lead > 0]), 2))

## ---- promise-test ------------------------------------------------
c(orders_with_a_promise = sum(has_promise),
  share = round(mean(has_promise), 3),
  zero_lead_rows = sum(promised_lead[has_promise] == 0),
  mean_lead_days = round(mean(promised_lead[has_promise]), 2),
  sd_lead_days = round(sd(promised_lead[has_promise]), 2))

## ---- fig-leads ---------------------------------------------------
leads <- rbind(
  data.frame(field = "requested", lead = pmin(requested_lead, 12)),
  data.frame(field = "promised",
             lead = pmin(promised_lead[has_promise], 12)))

ggplot(leads, aes(lead)) +
  geom_histogram(binwidth = 1, fill = "grey35", colour = "white",
                 linewidth = 0.2) +
  facet_wrap(~ field, scales = "free_y") +
  labs(x = "lead time in days (truncated at 12)", y = "orders")

## ---- not-a-filter ------------------------------------------------
past_requested <- pmax(0, orders$lead_hours - 24 * requested_lead)

c(orders_past_requested = sum(past_requested > 0),
  share = round(mean(past_requested > 0), 3))

## ---- collinear ---------------------------------------------------
c(correlation_with_lead_time = round(cor(past_requested, orders$lead_hours), 4),
  rows_exactly_equal = sum(abs(past_requested - orders$lead_hours) < 1e-9),
  share = round(mean(abs(past_requested - orders$lead_hours) < 1e-9), 3))

## ---- saturation --------------------------------------------------
cap <- 100
severity <- pmin(cap, past_requested)
in_flight <- !orders$is_terminal

c(in_flight = sum(in_flight),
  at_the_cap = sum(severity[in_flight] >= cap),
  share_at_cap = round(mean(severity[in_flight] >= cap), 3),
  distinct_scores = length(unique(round(severity[in_flight], 4))))

## ---- derived -----------------------------------------------------
pick_p90 <- dwell_quantile(stage_dwell(sim, "released"), 0.9)
carrier_cutoff <- 4

c(pick_p90_hours = round(pick_p90, 2),
  carrier_cutoff_hours = carrier_cutoff,
  horizon_offset_hours = round(pick_p90 + carrier_cutoff, 2))

## ---- derived-counts ----------------------------------------------
promise_hours <- 24 * promised_lead
horizon_hours <- promise_hours - (pick_p90 + carrier_cutoff)

c(promised_orders = sum(has_promise),
  past_the_promise = sum(orders$lead_hours > promise_hours, na.rm = TRUE),
  past_the_horizon = sum(orders$lead_hours > horizon_hours, na.rm = TRUE))

## ---- hard-soft ---------------------------------------------------
hard <- has_promise & orders$lead_hours > promise_hours
soft <- orders$lead_hours > 24

c(hard_breaches = sum(hard, na.rm = TRUE),
  soft_breaches = sum(soft),
  both = sum(hard & soft, na.rm = TRUE),
  hard_only = sum(hard & !soft, na.rm = TRUE))

## ---- no-deadline -------------------------------------------------
c(no_promise = sum(!has_promise), share = round(mean(!has_promise), 3))

## ---- no-deadline-baseline ----------------------------------------
lead <- data.frame(
  order_no = orders$order_no, stage = "lead",
  dwell_hours = orders$lead_hours, censored = orders$censored,
  stringsAsFactors = FALSE)

cohort <- data.frame(
  special = ifelse(orders$special_order, "specialty", "stock"),
  stringsAsFactors = FALSE)

baseline_cohort(lead[!has_promise, ], cohort[!has_promise, , drop = FALSE],
                p = 0.9)$cells

## ---- predicted ---------------------------------------------------
md <- data.frame(
  t = pmax(orders$lead_hours, 0.01),
  e = as.integer(!orders$censored),
  special = orders$special_order,
  source = factor(orders$order_source),
  branch = factor(orders$branch),
  lines = orders$lines,
  log_value = log(orders$value))

set.seed(7)
tr <- sample(c(TRUE, FALSE), nrow(md), replace = TRUE, prob = c(0.6, 0.4))
aft <- survreg(Surv(t, e) ~ special + source + branch + lines + log_value,
               data = md[tr, ], dist = "lognormal")

h <- md[!tr, ]
done <- h$e == 1
p50 <- predict(aft, h, type = "quantile", p = 0.5)
p90 <- predict(aft, h, type = "quantile", p = 0.9)

c(completed_holdout = sum(done),
  exceeded_p50 = round(mean(h$t[done] > p50[done]), 4),
  exceeded_p90 = round(mean(h$t[done] > p90[done]), 4))

## ---- interval-width ----------------------------------------------
c(median_p50_hours = round(median(p50), 1),
  median_p90_hours = round(median(p90), 1),
  ratio = round(median(p90) / median(p50), 2))

## ---- fig-quantile-calibration ------------------------------------
cal <- do.call(rbind, lapply(seq(0.1, 0.95, by = 0.05), function(p) {
  qp <- predict(aft, h, type = "quantile", p = p)
  data.frame(nominal = 1 - p, realized = mean(h$t[done] > qp[done]))
}))

ggplot(cal, aes(nominal, realized)) +
  geom_abline(slope = 1, linetype = 2, colour = "grey60") +
  geom_point(size = 2, colour = "grey20") +
  scale_x_continuous(labels = function(x) sprintf("%.0f%%", 100 * x)) +
  scale_y_continuous(labels = function(x) sprintf("%.0f%%", 100 * x)) +
  labs(x = "nominal share expected to exceed",
       y = "realized share exceeding")
