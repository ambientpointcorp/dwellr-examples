# Generated from chapters/02-deterministic-models.qmd by tools/extract.py -- do not edit by hand.
# Chapter: Deterministic Models
#
# Run from the repository root:
#   Rscript scripts/02-deterministic-models.R

library(dwellr)
library(ggplot2)

source("R/theme.R")


## ---- eoq-functions -----------------------------------------------
K <- 100; h <- 1; d <- 10

Q_star <- eoq(demand_rate = d, order_cost = K, holding_cost = h)
c(Q_star       = Q_star,
  cost         = eoq_cost(d, K, h, Q_star),
  cycle_length = Q_star / d)

## ---- eoq-balance -------------------------------------------------
c(ordering = K * d / Q_star,
  holding  = h * Q_star / 2)

## ---- fig-eoq -----------------------------------------------------
Q <- seq(5, 150, length.out = 400)
df <- rbind(
  data.frame(Q, cost = K * d / Q,          part = "ordering"),
  data.frame(Q, cost = h * Q / 2,          part = "holding"),
  data.frame(Q, cost = eoq_cost(d, K, h, Q), part = "total"))

ggplot(df, aes(Q, cost, colour = part)) +
  geom_line() +
  geom_vline(xintercept = Q_star, linetype = 2, colour = "grey40") +
  annotate("point", x = Q_star, y = eoq_cost(d, K, h, Q_star), size = 2) +
  labs(x = "order quantity Q", y = "cost per period", colour = NULL) +
  theme(legend.position = "top")

## ---- eoq-penalty -------------------------------------------------
ratios <- c(0.5, 0.75, 0.8, 1, 1.25, 1.5, 2)
data.frame(
  `Q / Q*`       = ratios,
  `Q`            = round(ratios * Q_star, 1),
  `cost penalty` = sprintf("%.2f%%", 100 * eoq_penalty(ratios)),
  check.names    = FALSE
)

## ---- eoq-rounding ------------------------------------------------
sprintf("%.2f%%", 100 * eoq_penalty(50 / Q_star))

## ---- cost-misestimate --------------------------------------------
f <- c(0.5, 0.6, 0.8, 1, 1.25, 2, 4)
data.frame(
  `K estimated as` = sprintf("%.0f%% of true", 100 * f),
  `Q error`        = sprintf("%+.0f%%", 100 * (sqrt(f) - 1)),
  `cost penalty`   = sprintf("%.2f%%", 100 * eoq_misestimate_penalty(f)),
  check.names      = FALSE
)

## ---- reorder-point -----------------------------------------------
L <- 0.5
c(reorder_point = reorder_point(demand_rate = d, lead_time = L),
  order_qty     = Q_star,
  cycle_length  = Q_star / d)

## ---- pow2 --------------------------------------------------------
T_star <- Q_star / d
T_pow  <- pow2_interval(optimal_interval = T_star, base_interval = 0.7)

c(T_star  = T_star,
  T_pow2  = T_pow,
  penalty = eoq_cost_interval(T_pow,  d, K, h) /
            eoq_cost_interval(T_star, d, K, h) - 1,
  bound   = pow2_bound())

## ---- all-units ---------------------------------------------------
cand <- eoq_quantity_discount(
  demand_rate = d, order_cost = K, holding_cost = h,
  breakpoints = c(0, 60, 120),
  prices      = c(5, 4.5, 4))
cand

## ---- all-units-best ----------------------------------------------
cand[which.min(cand$cost), ]

## ---- planned-backorders ------------------------------------------
bp <- backorder_policy(demand_rate = d, order_cost = K,
                       holding_cost = h, shortage_cost = 5)
c(Q = bp$Q, backordered_fraction = bp$fraction, cost = bp$cost,
  saving_vs_eoq = 1 - bp$cost / eoq_cost(d, K, h, Q_star))

## ---- epq ---------------------------------------------------------
ep <- epq(demand_rate = d, order_cost = K, holding_cost = h,
          production_rate = 20)
c(rho = ep$rho, Q = ep$Q, cost = ep$cost,
  eoq_Q = Q_star, eoq_cost = eoq_cost(d, K, h, Q_star))

## ---- elsp --------------------------------------------------------
items <- data.frame(
  item = c("A", "B", "C"),
  d    = c(10,  5,   8),
  P    = c(50,  40,  60),
  K    = c(100, 60,  80),
  h    = c(1,   2,   1.5))

sched <- elsp_common_cycle(items)
c(T_star = sched$interval, machine_busy_fraction = sched$busy_fraction)
sched$runs

## ---- jrp ---------------------------------------------------------
# A fast mover, a medium mover, and a slow mover that is cheap to hold.
res <- jrp(major_cost    = 80,
           minor_costs   = c(20,  15,  30),
           holding_costs = c(1,   2,   0.2),
           demand_rates  = c(10,  5,   0.5))
c(base_cycle = res$interval, cost = res$cost)
rbind(multiple  = res$multiples,
      own_cycle = round(res$multiples * res$interval, 2))

## ---- wagner-whitin -----------------------------------------------
demand <- c(20, 50, 10, 50, 30)
ww <- dls_wagner_whitin(demand, order_cost = 100, holding_cost = 1)
ww

## ---- ww-alternatives ---------------------------------------------
lot_for_lot <- length(demand) * 100
single_order <- 100 + sum((seq_along(demand) - 1) * demand)
c(optimal = ww$cost, lot_for_lot = lot_for_lot, single_order = single_order)
