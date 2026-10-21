# Generated from chapters/18-severity-and-routing.qmd by tools/extract.py -- do not edit by hand.
# Chapter: Severity and Routing
#
# Run from the repository root:
#   Rscript scripts/18-severity-and-routing.R

library(dwellr)
library(ggplot2)
library(ranger)

source("R/theme.R")


## ---- population --------------------------------------------------
sim    <- simulate_lifecycles(n = 5000, seed = 11)
orders <- sim$orders
dwell  <- stage_dwell(sim)

open <- orders[!orders$is_terminal, ]
open$elapsed <- as.numeric(difftime(sim$as_of, open$entered_at,
                                    units = "hours"))

## ---- components --------------------------------------------------
open$req_lead <- as.integer(open$requested_date - as.Date(open$entered_at))
promised_h <- as.numeric(difftime(as.POSIXct(open$promised_date),
                                  sim$as_of, units = "hours"))

comp <- list(dwell_tier = open$elapsed,
             deadline_proximity = promised_h,
             exposure = open$value,
             repeat_behavior = open$loops)

do.call(rbind, lapply(names(comp), function(n) {
  x <- comp[[n]]
  data.frame(component = n, present = sum(!is.na(x)),
             distinct_values = length(unique(x[!is.na(x)])),
             cv = round(sd(x, na.rm = TRUE) / abs(mean(x, na.rm = TRUE)), 3))
}))

## ---- counterexample ----------------------------------------------
past_requested <- pmax(0, open$elapsed - 24 * open$req_lead)

c(distinct_values = length(unique(round(past_requested, 4))),
  correlation_with_elapsed = round(cor(past_requested, open$elapsed), 4))

## ---- tiers -------------------------------------------------------
rules <- data.frame(
  key = c("received", "released", "awaiting_supply", "backordered",
          "credit_hold"),
  action = c("release the order", "chase the pick", "chase the vendor",
             "chase stock", "clear the credit hold"),
  recipient = c("warehouse", "warehouse", "buyer", "buyer", "finance"),
  urgency = c("notify", "notify", "escalate", "escalate", "escalate"),
  stringsAsFactors = FALSE)

routed <- route_action(open$stage, rules,
  default = data.frame(action = "review manually", recipient = "supervisor",
                       urgency = "notify", stringsAsFactors = FALSE))

table(routed$urgency)

## ---- by-recipient ------------------------------------------------
table(routed$recipient)

## ---- mismatch ----------------------------------------------------
warehouse_can_act <- open$stage %in% c("received", "released",
                                       "partially_shipped")

c(open_queue = nrow(open),
  warehouse_can_act = sum(warehouse_can_act),
  cannot = sum(!warehouse_can_act),
  share = round(mean(!warehouse_can_act), 3),
  value_misrouted = round(sum(open$value[!warehouse_can_act])),
  value_share = round(sum(open$value[!warehouse_can_act]) /
                      sum(open$value), 3))

## ---- ownership ---------------------------------------------------
c(web_store = sum(orders$order_source == "estore"),
  inside_sales = sum(orders$order_source == "inside_sales"),
  web_share = round(mean(orders$order_source == "estore"), 3),
  web_share_of_open_queue = round(mean(open$order_source == "estore"), 3))

## ---- learned-routing ---------------------------------------------
visits <- dwell[dwell$stage %in% rules$key, ]
oi <- match(visits$order_no, orders$order_no)

X <- data.frame(
  special = as.integer(orders$special_order[oi]),
  channel = factor(orders$order_source[oi]),
  branch  = factor(orders$branch[oi]),
  lines   = orders$lines[oi],
  log_value = log(orders$value[oi]),
  loops   = orders$loops[oi],
  shortfall = as.integer(orders$stock_shortfall[oi]),
  dwell_hours = visits$dwell_hours,
  y = factor(rules$action[match(visits$stage, rules$key)]))

set.seed(7)
tr <- sample(c(TRUE, FALSE), nrow(X), replace = TRUE, prob = c(0.7, 0.3))
fit <- ranger(y ~ ., X[tr, ], probability = TRUE, num.trees = 400, seed = 1)
p   <- predict(fit, X[!tr, ])$predictions
pred <- colnames(p)[max.col(p)]
conf <- apply(p, 1, max)
truth <- as.character(X$y[!tr])

c(visits = nrow(X), holdout = length(truth),
  accuracy = round(mean(pred == truth), 4),
  base_rate = round(max(table(truth)) / length(truth), 4))

## ---- confidence-floor --------------------------------------------
do.call(rbind, lapply(c(0, 0.5, 0.7, 0.9), function(fl) {
  k <- conf >= fl
  data.frame(floor = fl, auto_routed = round(mean(k), 3),
             accuracy_when_auto = round(if (sum(k)) mean(pred[k] == truth[k])
                                        else NA_real_, 4),
             left_to_rules = round(1 - mean(k), 3))
}))

## ---- by-action ---------------------------------------------------
k <- conf >= 0.70
do.call(rbind, lapply(sort(unique(truth)), function(a) {
  i <- truth == a
  data.frame(action = a, visits = sum(i),
             auto_routed = round(mean(k[i]), 3),
             accuracy = round(if (sum(k & i)) mean(pred[k & i] == truth[k & i])
                              else NA_real_, 4))
}))

## ---- gaps --------------------------------------------------------
g <- routing_gaps(open$stage, rules, value = open$value)

c(covered = round(g$covered, 3))
g$gaps

## ---- unmatched ---------------------------------------------------
r <- route_action(open$stage, rules)

c(unmatched = sum(!r$matched),
  highest_value_unmatched = round(max(open$value[!r$matched]), 2))
