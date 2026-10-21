# Generated from chapters/03-stochastic-models-and-service-levels.qmd by tools/extract.py -- do not edit by hand.
# Chapter: Stochastic Models and Service Levels
#
# Run from the repository root:
#   Rscript scripts/03-stochastic-models-and-service-levels.R

library(dwellr)
library(ggplot2)

source("R/theme.R")


## ---- newsvendor --------------------------------------------------
Q_star <- nv_quantity(overage = 1, underage = 5,
                      quantile_fn = function(p) qnorm(p, mean = 10, sd = 2))
c(critical_fractile = nv_critical_fractile(1, 5),
  Q_star            = Q_star,
  cost              = nv_cost(Q_star, 1, 5, mean = 10, sd = 2))

## ---- fig-newsvendor ----------------------------------------------
Q <- seq(4, 18, length.out = 400)
df <- rbind(
  data.frame(Q, cost = 1 * nv_leftover(Q, 10, 2),  part = "overage"),
  data.frame(Q, cost = 5 * nv_shortfall(Q, 10, 2), part = "underage"),
  data.frame(Q, cost = nv_cost(Q, 1, 5, mean = 10, sd = 2), part = "total"))

ggplot(df, aes(Q, cost, colour = part)) +
  geom_line() +
  geom_vline(xintercept = Q_star, linetype = 2, colour = "grey40") +
  labs(x = "quantity stocked Q", y = "expected cost", colour = NULL) +
  theme(legend.position = "top")

## ---- newsvendor-discrete -----------------------------------------
units <- 0:300
pmf   <- dpois(units, lambda = 100)

c(exact_Q  = qpois(5/6, 100),
  normal_Q = round(qnorm(5/6, mean = 100, sd = 10)),
  exact_cost  = nv_cost(110, 1, 5, support = units, probs = pmf),
  normal_cost = nv_cost(110, 1, 5, mean = 100, sd = 10))

## ---- service-levels ----------------------------------------------
at_optimum <- nv_service_levels(Q_star, mean = 10, sd = 2)
c(alpha = at_optimum$alpha, beta = at_optimum$beta)

## ---- service-targets ---------------------------------------------
targets <- c(0.90, 0.95, 0.99)
data.frame(
  target     = targets,
  `Q for alpha` = sapply(targets, nv_quantity_for_service,
                         mean = 10, sd = 2, type = "alpha"),
  `Q for beta`  = sapply(targets, nv_quantity_for_service,
                         mean = 10, sd = 2, type = "beta"),
  check.names = FALSE
)

## ---- simulate-base-stock -----------------------------------------
simulate_policy <- function(S, periods, lambda, h, b, seed = 42) {
  set.seed(seed)
  w <- Warehouse$new(S)
  cost <- numeric(periods)
  for (t in seq_len(periods) - 1L) {
    gap <- S - w$position()
    if (gap > 0) w$receive(gap, t)         # zero lead time
    w$ship(rpois(1, lambda), t)
    cost[t + 1L] <- h * w$pickable() + b * w$owed()
  }
  c(mean = mean(cost), sd = sd(cost), worst = max(cost))
}

simulate_policy(S = 13, periods = 2000, lambda = 10, h = 1, b = 5)

## ---- base-stock --------------------------------------------------
units <- 0:40
pmf   <- dpois(units, lambda = 10)

bs <- policy_base_stock(periods = 5, support = units, probs = pmf,
                        holding_cost = 1, shortage_cost = 5)
bs$S

## ---- base-stock-check --------------------------------------------
c(sdp = bs$S[1], newsvendor = qpois(5 / (1 + 5), lambda = 10))

## ---- ss-evaluate -------------------------------------------------
ss_cost(s = 6, S = 40, support = units, probs = pmf,
        order_cost = 64, holding_cost = 1, shortage_cost = 9)

## ---- ss-optimize -------------------------------------------------
best <- policy_ss(support = units, probs = pmf, order_cost = 64,
                  holding_cost = 1, shortage_cost = 9, S_range = 20:50)
unlist(best)

## ---- rs-policy ---------------------------------------------------
intervals <- 1:6
data.frame(
  R    = intervals,
  cost = sapply(intervals, policy_rs, S = 40, support = units, probs = pmf,
                order_cost = 64, holding_cost = 1, shortage_cost = 9)
)

## ---- policy-comparison -------------------------------------------
K <- 64; h <- 1; b <- 9

ss   <- policy_ss(units, pmf, K, h, b, S_range = 20:50)
bs13 <- policy_base_stock(5, units, pmf, h, b)$S[1]

data.frame(
  policy = c("(s, S)", "base-stock, K ignored", "(R, S), R = 3"),
  detail = c(sprintf("s = %d, S = %d", ss$s, ss$S),
             sprintf("S = %d, order every period", bs13),
             "S = 40, review every 3 periods"),
  cost   = c(ss$cost,
             ss_cost(bs13 - 1, bs13, units, pmf, K, h, b),
             policy_rs(3, 40, units, pmf, K, h, b))
)
