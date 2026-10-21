# Generated from chapters/c-stochastic-dynamic-programming.qmd by tools/extract.py -- do not edit by hand.
# Chapter: Stochastic dynamic programming
#
# Run from the repository root:
#   Rscript scripts/c-stochastic-dynamic-programming.R

library(dwellr)
library(ggplot2)

source("R/theme.R")


## ---- setup-grid --------------------------------------------------
support <- 0:8
probs   <- dpois(support, 3)
probs   <- probs / sum(probs)
h <- 1; b <- 9

period_cost <- function(y)
  sum(probs * (h * pmax(y - support, 0) + b * pmax(support - y, 0)))

round(vapply(0:8, period_cost, numeric(1)), 3)

## ---- base-stock --------------------------------------------------
bs <- policy_base_stock(periods = 20, support = support, probs = probs,
                        holding_cost = h, shortage_cost = b)

newsvendor_level <- min(which(cumsum(probs) >= b / (b + h))) - 1
c(dp_level = tail(bs$S, 1), newsvendor_level = newsvendor_level,
  cost = round(bs$cost, 3))

## ---- ss-policy ---------------------------------------------------
do.call(rbind, lapply(c(0, 5, 20, 60), function(K) {
  p <- policy_ss(support, probs, order_cost = K, holding_cost = h,
                 shortage_cost = b, S_range = 1:30)
  data.frame(fixed_cost = K, s = p$s, S = p$S, band = p$S - p$s,
             cost = round(p$cost, 4))
}))

## ---- dp-growth ---------------------------------------------------
do.call(rbind, lapply(1:5, function(d)
  data.frame(dimensions = d, levels = 40,
             states = format(40^d, big.mark = ",", scientific = FALSE),
             feasible = 40^d <= 1e6)))
