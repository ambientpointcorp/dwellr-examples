# Generated from chapters/07-from-dwell-variance-to-safety-stock.qmd by tools/extract.py -- do not edit by hand.
# Chapter: From Dwell Variance to Safety Stock
#
# Run from the repository root:
#   Rscript scripts/07-from-dwell-variance-to-safety-stock.R

library(dwellr)
library(ggplot2)

source("R/theme.R")


## ---- population --------------------------------------------------
sim  <- simulate_lifecycles(n = 5000, seed = 11)
spec <- distribution_stage_spec()
dec  <- lead_time_decompose(sim, spec = spec)
round(attr(dec, "total")[c("mean", "sd")] / 24, 3)

## ---- contribution ------------------------------------------------
vc <- dwell_variance_contribution(dec, demand_mean = 10, demand_sd = 3,
                                  service_level = 0.95)
attr(vc, "total")

## ---- empirical-ltd -----------------------------------------------
done <- sim$orders$order_no[sim$orders$stage == "shipped"]
d_all <- stage_dwell(sim)
keep  <- d_all$order_no %in% done
L_obs <- tapply(d_all$dwell_hours[keep], d_all$order_no[keep], sum) / 24

emp <- lead_time_demand_empirical(L_obs, demand_mean = 10, demand_sd = 3,
                                  service_level = 0.95, n = 4e5)
round(unlist(emp), 2)

## ---- fig-ltd-shape -----------------------------------------------
Ld <- sample(L_obs, 2e5, replace = TRUE)
D  <- rnorm(length(Ld), 10 * Ld, 3 * sqrt(Ld))
mu <- 10 * mean(L_obs)
sg <- sqrt(mean(L_obs) * 9 + 100 * var(L_obs))

ggplot(data.frame(D = D), aes(D)) +
  geom_density(colour = "grey25") +
  stat_function(fun = dnorm, args = list(mean = mu, sd = sg),
                linetype = 2, colour = "grey45") +
  geom_vline(xintercept = 0, linewidth = 0.3, colour = "grey70") +
  coord_cartesian(xlim = c(-80, 200)) +
  labs(x = "lead-time demand (units)", y = NULL)

## ---- by-stage ----------------------------------------------------
vc

## ---- attribution-sums --------------------------------------------
c(marginal_sum     = round(sum(vc$marginal_units), 1),
  proportional_sum = round(sum(vc$proportional_units), 1),
  total_safety_stock = round(attr(vc, "total")[["safety_stock"]], 1))

## ---- fig-attribution ---------------------------------------------
long <- rbind(
  data.frame(stage = vc$stage, units = vc$proportional_units,
             reading = "proportional (sums to the total)"),
  data.frame(stage = vc$stage, units = vc$marginal_units,
             reading = "marginal (released if removed)"))
long$stage <- factor(long$stage, levels = rev(vc$stage))

ggplot(long, aes(units, stage, fill = reading)) +
  geom_col(position = "dodge", width = 0.7) +
  scale_fill_grey(start = 0.35, end = 0.7) +
  labs(x = "safety stock (units)", y = NULL, fill = NULL) +
  theme(legend.position = "bottom")

## ---- drivers -----------------------------------------------------
rel <- merge(stage_backlog(sim, "released"),
             sim$orders[, c("order_no", "branch", "order_source",
                            "lines", "value")],
             by = "order_no")
rel$log_value <- log(rel$value)

dv <- driver_variance(rel, c("branch", "order_source", "lines",
                             "log_value", "backlog"))
dv$ranking
c(r_squared = round(dv$r_squared, 4),
  unexplained = round(dv$unexplained, 3),
  max_correlation = round(dv$max_correlation, 3),
  n = dv$n)

## ---- admin-test --------------------------------------------------
internal <- c("branch", "order_source", "lines")
do.call(rbind, lapply(c("received", "released", "awaiting_supply",
                        "credit_hold"), function(st) {
  x <- merge(stage_dwell(sim, st),
             sim$orders[, c("order_no", internal)], by = "order_no")
  tab <- dwell_regression(x, internal)$table
  data.frame(stage = st, visits = nrow(x),
             branch_p  = signif(tab$cox_p[1], 2),
             channel_p = signif(tab$cox_p[2], 2),
             lines_p   = signif(tab$cox_p[3], 2))
}))

## ---- segments ----------------------------------------------------
special <- sim$orders$special_order[match(names(L_obs), sim$orders$order_no)]

seg <- function(label, x) {
  ltd <- lead_time_demand(10, 3, mean(x), sd(x))
  data.frame(segment = label, orders = length(x),
             lead_mean = round(mean(x), 2), lead_sd = round(sd(x), 2),
             cv = round(sd(x) / mean(x), 2),
             safety_stock = round(safety_stock(ltd, 0.95), 1))
}
rbind(seg("pooled", L_obs),
      seg("stock", L_obs[!special]),
      seg("specialty", L_obs[special]))

## ---- segment-total -----------------------------------------------
w <- mean(special)
pooled_ss <- safety_stock(lead_time_demand(10, 3, mean(L_obs), sd(L_obs)), 0.95)
seg_ss <- (1 - w) * safety_stock(
              lead_time_demand(10, 3, mean(L_obs[!special]),
                               sd(L_obs[!special])), 0.95) +
          w * safety_stock(
              lead_time_demand(10, 3, mean(L_obs[special]),
                               sd(L_obs[special])), 0.95)
c(pooled = round(pooled_ss, 1), segmented = round(seg_ss, 1),
  reduction = sprintf("%.1f%%", 100 * (1 - seg_ss / pooled_ss)))

## ---- reduction-by-stage ------------------------------------------
do.call(rbind, lapply(vc$stage, function(st) {
  v <- variance_reduction_value(vc, st, reduction = 0.5,
                                unit_cost = 120, holding_rate = 0.22)
  data.frame(stage = st, units_released = round(v$units, 2),
             annual_value = round(v$annual_value, 0))
}))

## ---- convexity ---------------------------------------------------
do.call(rbind, lapply(c(0.25, 0.5, 0.75, 1.0), function(r) {
  v <- variance_reduction_value(vc, "received", r, unit_cost = 120)
  full <- variance_reduction_value(vc, "received", 1, unit_cost = 120)$units
  data.frame(variance_removed = sprintf("%.0f%%", 100 * r),
             units = round(v$units, 2),
             annual_value = round(v$annual_value, 0),
             share_of_full = sprintf("%.0f%%", 100 * v$units / full))
}))

## ---- budget ------------------------------------------------------
b <- monitoring_budget(annual_value = 186, cost_per_review = 4,
                       reviews_per_year = 30)
unlist(b)

## ---- gap ---------------------------------------------------------
tot <- attr(dec, "total")
gap <- service_level_gap(demand_mean = 10, demand_sd = 3,
                         assumed_mean = 2.0, assumed_sd = 0.5,
                         actual_mean = tot[["mean"]] / 24,
                         actual_sd   = tot[["sd"]] / 24,
                         target = 0.95)
round(unlist(gap), 3)

## ---- valuation ---------------------------------------------------
# Restrict the corpus to stock items, so the stage shares and the lead time
# both describe the class being valued.
stock_orders <- sim$orders[!sim$orders$special_order, ]
stock_sim <- structure(
  list(orders = stock_orders,
       events = sim$events[sim$events$order_no %in% stock_orders$order_no, ],
       as_of  = sim$as_of),
  class = "lifecycle_sim")

dec_stock <- lead_time_decompose(stock_sim, spec = spec)
vc_stock <- dwell_variance_contribution(dec_stock, demand_mean = 10,
                                        demand_sd = 3, service_level = 0.95)
round(attr(vc_stock, "total")[c("lead_time_mean", "lead_time_sd",
                                "safety_stock")], 2)

internal_stages <- vc_stock$stage[vc_stock$class != "external supply"]
sum_of_marginals <- sum(vapply(internal_stages, function(st)
  variance_reduction_value(vc_stock, st, reduction = 0.5,
                           unit_cost = 120)$annual_value, 0))

# All of them at once, which is what a program halving internal dwell
# variance would actually do.
z     <- qnorm(0.95)
V     <- attr(vc_stock, "total")[["ltd_variance"]]
V_int <- sum(vc_stock$variance[vc_stock$class != "external supply"])
joint_units <- z * (sqrt(V) - sqrt(V - 0.5 * V_int))

c(sum_of_marginals = round(sum_of_marginals, 0),
  joint_per_item   = round(joint_units * 120 * 0.22, 0),
  class_annual     = round(850 * joint_units * 120 * 0.22, -2))

## ---- joint -------------------------------------------------------
c(sum_of_marginal_units = round(sum_of_marginals / (120 * 0.22), 2),
  joint_units           = round(joint_units, 2),
  understatement        = sprintf("%.0f%%",
                            100 * (joint_units /
                              (sum_of_marginals / (120 * 0.22)) - 1)))
