# Generated from chapters/06-measuring-dwell-time.qmd by tools/extract.py -- do not edit by hand.
# Chapter: Measuring Dwell Time
#
# Run from the repository root:
#   Rscript scripts/06-measuring-dwell-time.R

library(dwellr)
library(ggplot2)
library(survival)

source("R/theme.R")


## ---- population --------------------------------------------------
sim  <- simulate_lifecycles(n = 5000, seed = 11)
spec <- distribution_stage_spec()
rel  <- stage_dwell(sim, "released")
nrow(rel)

## ---- shape -------------------------------------------------------
x <- rel$dwell_hours[!rel$censored]
c(n        = length(x),
  mean     = round(mean(x), 2),
  median   = round(median(x), 2),
  sd       = round(sd(x), 2),
  max      = round(max(x), 1),
  skewness = round(mean((x - mean(x))^3) / sd(x)^3, 2))

## ---- shape-consequences ------------------------------------------
c(share_below_the_mean = round(mean(x < mean(x)), 3),
  mean_minus_2sd       = round(mean(x) - 2 * sd(x), 1),
  mean_plus_2sd        = round(mean(x) + 2 * sd(x), 1),
  share_beyond_that    = round(mean(x > mean(x) + 2 * sd(x)), 4))

## ---- censoring-bias ----------------------------------------------
windows <- c(20, 40, 60, 90, 120)
do.call(rbind, lapply(windows, function(days) {
  earlier <- observe_at(sim, sim$as_of - (120 - days) * 24 * 3600)
  m <- dwell_mean(stage_dwell(earlier, "awaiting_supply"), restrict_to = 400)
  data.frame(window_days = days, visits = m$n,
             censored = round(m$censored / m$n, 3),
             naive_mean = round(m$naive, 1),
             censoring_aware = round(m$restricted, 1),
             bias = sprintf("%+.1f%%", 100 * m$bias))
}))

## ---- fig-km-released ---------------------------------------------
fit <- dwell_survival(rel)
marks <- c(4, 8, 12, 24, 48, 72)
curve <- data.frame(hours = fit$time, surv = fit$surv)

ggplot(curve[curve$hours <= 96, ], aes(hours, surv)) +
  geom_step(colour = "grey25") +
  geom_point(data = data.frame(hours = marks,
                               surv = summary(fit, times = marks)$surv),
             size = 1.8, colour = "grey10") +
  scale_x_continuous(breaks = marks) +
  scale_y_continuous(labels = function(p) sprintf("%.0f%%", 100 * p)) +
  labs(x = "hours since release", y = "still in the stage")

## ---- hazard ------------------------------------------------------
dwell_hazard(rel, breaks = c(0, 1, 2, 4, 8, 24, 72, Inf))

## ---- parametric --------------------------------------------------
dwell_parametric(rel)

## ---- mixture -----------------------------------------------------
mix <- dwell_mixture(rel, families = c("exp", "lnorm"))
c(fast_share   = round(mix$weight[[1]], 3),
  stuck_share  = round(mix$weight[[2]], 3),
  fast_mean_h  = round(mix$params[[1]][1], 2),
  stuck_median = round(exp(mix$params[[2]][1]), 1),
  stuck_sigma  = round(mix$params[[2]][2], 2))

c(converged = mix$converged, iterations = mix$iterations)

## ---- mixture-quantiles -------------------------------------------
comp <- mix$component_quantile(c(0.5, 0.9))
rbind(fast = round(comp[1, ], 2),
      stuck = round(comp[2, ], 2),
      pooled = round(mix$quantile(c(0.5, 0.9)), 2))

## ---- fig-mixture -------------------------------------------------
single <- survreg(Surv(pmax(rel$dwell_hours, 1e-6), !rel$censored) ~ 1,
                  dist = "lognormal")
hrs <- seq(0.05, 48, length.out = 400)
cmp <- rbind(
  data.frame(hours = hrs, surv = summary(fit, times = hrs,
                                         extend = TRUE)$surv,
             source = "measured (Kaplan-Meier)"),
  data.frame(hours = hrs,
             surv = plnorm(hrs, coef(single)[1], single$scale,
                           lower.tail = FALSE),
             source = "single log-normal"),
  data.frame(hours = hrs,
             surv = mix$weight[[1]] *
                      pexp(hrs, 1 / mix$params[[1]][1], lower.tail = FALSE) +
                    mix$weight[[2]] *
                      plnorm(hrs, mix$params[[2]][1], mix$params[[2]][2],
                             lower.tail = FALSE),
             source = "two-component mixture"))

ggplot(cmp, aes(hours, surv, linetype = source)) +
  geom_line(colour = "grey25") +
  scale_y_continuous(labels = function(p) sprintf("%.0f%%", 100 * p)) +
  labs(x = "hours since release", y = "still in the stage", linetype = NULL) +
  theme(legend.position = "bottom")

## ---- field-comparison --------------------------------------------
data.frame(hours = marks,
           synthetic = sprintf("%.1f%%", 100 * summary(fit, times = marks)$surv),
           field = c("24.9%", "14.8%", "13.6%", "7.2%", "3.7%", "1.6%"))

## ---- sigma-coverage ----------------------------------------------
do.call(rbind, lapply(1:3, function(k) {
  thr <- mean(x) + k * sd(x)
  data.frame(rule = paste0("mean + ", k, " sd"),
             hours = round(thr, 1),
             covers = sprintf("%.1f%%", 100 * mean(x <= thr)))
}))

## ---- km-quantiles ------------------------------------------------
round(dwell_quantile(rel, c(0.5, 0.75, 0.9, 0.95, 0.99)), 2)

## ---- competing ---------------------------------------------------
exits <- stage_exit_type(sim, "released", spec)
table(exits$exit_type)
dwell_competing(exits, times = c(2, 4, 8, 24, 72))

## ---- competing-error ---------------------------------------------
wrong <- exits
wrong$censored <- exits$exit_type != "forward"
c(treating_backward_as_censored = round(1 - summary(dwell_survival(wrong),
                                                    times = 24)$surv, 4),
  cumulative_incidence          = round(dwell_competing(exits,
                                                        times = 24)$forward, 4))

## ---- regression-data ---------------------------------------------
rel_cov <- merge(stage_backlog(sim, "released"),
                 sim$orders[, c("order_no", "branch", "order_source",
                                "lines", "value")],
                 by = "order_no")
rel_cov$log_value <- log(rel_cov$value)
fit_rel <- dwell_regression(rel_cov, c("branch", "order_source", "lines",
                                       "log_value", "backlog"))
fit_rel$table

## ---- regression-received -----------------------------------------
rec_cov <- merge(stage_dwell(sim, "received"),
                 sim$orders[, c("order_no", "branch", "order_source", "lines")],
                 by = "order_no")
dwell_regression(rec_cov, c("branch", "order_source", "lines"))$table

## ---- ph-test -----------------------------------------------------
fit_rel$ph_test

## ---- survival-direct ---------------------------------------------
s <- Surv(time = pmax(rel$dwell_hours, 1e-6), event = !rel$censored)

km <- survfit(s ~ 1)
summary(km, times = c(4, 24, 72))$surv

quantile(km, probs = c(0.5, 0.9))$quantile

## ---- survival-strata ---------------------------------------------
by_branch <- survfit(Surv(pmax(dwell_hours, 1e-6), !censored) ~ branch,
                     data = rel_cov)
summary(by_branch, times = c(4, 24))$surv

survdiff(Surv(pmax(dwell_hours, 1e-6), !censored) ~ branch, data = rel_cov)
