# Generated from chapters/d-survival-analysis-reference.qmd by tools/extract.py -- do not edit by hand.
# Chapter: Survival analysis reference
#
# Run from the repository root:
#   Rscript scripts/d-survival-analysis-reference.R

library(dwellr)
library(ggplot2)
library(survival)

source("R/theme.R")


## ---- packages ----------------------------------------------------
sim    <- simulate_lifecycles(n = 5000, seed = 11)
dwell  <- stage_dwell(sim)
rec    <- dwell[dwell$stage == "received", ]

data.frame(visits = nrow(rec), censored = sum(rec$censored),
           censored_share = round(mean(rec$censored), 4))

## ---- km-against-truth --------------------------------------------
set.seed(8)
n <- 4000
meanlog <- log(40) - 0.5 * 1.1^2
truth <- qlnorm(c(0.5, 0.9), meanlog, 1.1)

dwell_true <- rlnorm(n, meanlog, 1.1)
window <- runif(n, 0, 120)               # how long each item has been watched
obs    <- pmin(dwell_true, window)
cens   <- dwell_true > window

km <- survfit(Surv(obs, !cens) ~ 1)
rbind(
  truth = round(truth, 2),
  kaplan_meier = round(as.numeric(quantile(km, probs = c(0.5, 0.9))$quantile), 2),
  drop_censored = round(unname(quantile(obs[!cens], c(0.5, 0.9))), 2),
  treat_as_complete = round(unname(quantile(obs, c(0.5, 0.9))), 2))

## ---- km-corpus ---------------------------------------------------
fit <- survfit(Surv(dwell_hours, !censored) ~ 1, data = rec)
q <- quantile(fit, probs = c(0.5, 0.9, 0.95))

out <- rbind(hours = as.numeric(q$quantile),
             lower = as.numeric(q$lower),
             upper = as.numeric(q$upper))
colnames(out) <- c("p50", "p90", "p95")
round(out, 2)

## ---- cox ---------------------------------------------------------
oi <- match(rec$order_no, sim$orders$order_no)
rec$special  <- sim$orders$special_order[oi]
rec$channel  <- sim$orders$order_source[oi]
rec$logvalue <- log(sim$orders$value[oi])

cx <- coxph(Surv(dwell_hours, !censored) ~ special + channel + logvalue,
            data = rec)
round(summary(cx)$coefficients[, c("coef", "exp(coef)", "Pr(>|z|)")], 4)

## ---- cox-diagnostics ---------------------------------------------
ph <- cox.zph(cx)
c(concordance = round(summary(cx)$concordance[[1]], 4),
  ph_global_p = round(ph$table["GLOBAL", "p"], 4))

## ---- parametric --------------------------------------------------
do.call(rbind, lapply(c("exponential", "weibull", "lognormal"), function(dist) {
  f <- survreg(Surv(dwell_hours, !censored) ~ special, data = rec, dist = dist)
  data.frame(distribution = dist, logLik = round(as.numeric(logLik(f)), 1),
             AIC = round(AIC(f), 1),
             special_coef = round(coef(f)[["specialTRUE"]], 4))
}))

## ---- competing ---------------------------------------------------
table(stage_exit_type(sim, "received")$exit_type)

## ---- cuminc ------------------------------------------------------
ex <- stage_exit_type(sim, "received")
ex$event <- factor(ifelse(ex$exit_type == "censored", "censored",
                   ifelse(ex$exit_type == "cancelled", "cancelled",
                          "forward")),
                   levels = c("censored", "forward", "cancelled"))
ci <- survfit(Surv(ex$dwell_hours, ex$event) ~ 1)

at <- c(24, 72, 168)
round(summary(ci, times = at)$pstate, 4)

## ---- censoring-by-stage ------------------------------------------
round(tapply(dwell$censored, dwell$stage, mean), 4)

## ---- censoring-balance -------------------------------------------
oi2 <- match(dwell$order_no, sim$orders$order_no)
dwell$special <- sim$orders$special_order[oi2]
dwell$value   <- sim$orders$value[oi2]

rbind(
  censored = c(specialty_share = round(mean(dwell$special[dwell$censored]), 4),
               median_value = round(median(dwell$value[dwell$censored]), 2)),
  complete = c(specialty_share = round(mean(dwell$special[!dwell$censored]), 4),
               median_value = round(median(dwell$value[!dwell$censored]), 2)))
