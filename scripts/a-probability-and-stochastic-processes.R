# Generated from chapters/a-probability-and-stochastic-processes.qmd by tools/extract.py -- do not edit by hand.
# Chapter: Probability and stochastic processes
#
# Run from the repository root:
#   Rscript scripts/a-probability-and-stochastic-processes.R

library(dwellr)
library(ggplot2)

source("R/theme.R")


## ---- poisson-check -----------------------------------------------
lambda <- 4
n_days <- 50000
counts <- rpois(n_days, lambda)

c(mean = round(mean(counts), 4), variance = round(var(counts), 4),
  ratio = round(var(counts) / mean(counts), 4))

## ---- renewal-check -----------------------------------------------
mu <- 2.5
horizon <- 20000
rate <- function(draw) {
  gaps <- draw(60000)
  length(cumsum(gaps)[cumsum(gaps) <= horizon]) / horizon
}

c(exponential = round(rate(function(k) rexp(k, rate = 1 / mu)), 4),
  gamma = round(rate(function(k) rgamma(k, shape = 4, scale = mu / 4)), 4),
  lognormal = round(rate(function(k)
    rlnorm(k, log(mu) - 0.5 * 0.6^2, 0.6)), 4),
  theoretical = round(1 / mu, 4))

## ---- compound-check ----------------------------------------------
lambda <- 3; t_len <- 10
size <- function(k) rgamma(k, shape = 2, scale = 5)   # E[Y] = 10

sim <- replicate(40000, {
  n <- rpois(1, lambda * t_len)
  if (n == 0) 0 else sum(size(n))
})

EY  <- 2 * 5
EY2 <- EY^2 + 2 * 5^2            # Var[Y] + E[Y]^2
c(mean_simulated = round(mean(sim), 3),
  mean_theory = round(lambda * t_len * EY, 3),
  var_simulated = round(var(sim), 1),
  var_theory = round(lambda * t_len * EY2, 1))

## ---- tails -------------------------------------------------------
q <- c(0.5, 0.9, 0.99, 0.999)
m <- 40                            # a common mean of 40 hours
v <- exp(1.1^2) - 1                # a common squared coefficient of variation
tails <- rbind(
  lognormal = qlnorm(q, log(m) - 0.5 * 1.1^2, 1.1),
  gamma = qgamma(q, shape = 1 / v, scale = m * v),
  exponential = qexp(q, rate = 1 / m))
colnames(tails) <- paste0("q", 100 * q)
round(tails, 1)

## ---- clt-check ---------------------------------------------------
reps <- 40000
target <- 0.95
coverage <- function(L, draw) {
  d <- replicate(reps, sum(draw(L)))
  z <- mean(d) + qnorm(target) * sd(d)
  mean(d <= z)
}

do.call(rbind, lapply(c(1, 3, 7, 30), function(L)
  data.frame(lead_periods = L,
             gamma_cv_0.71 = round(coverage(L, function(k)
               rgamma(k, shape = 2, scale = 5)), 4),
             lognormal_cv_1.31 = round(coverage(L, function(k)
               rlnorm(k, log(10) - 0.5 * 1^2, 1)), 4))))
