# Generated from chapters/g-reproducibility.qmd by tools/extract.py -- do not edit by hand.
# Chapter: Reproducibility
#
# Run from the repository root:
#   Rscript scripts/g-reproducibility.R

library(dwellr)
library(ggplot2)

source("R/theme.R")


## ---- environment -------------------------------------------------
pkgs <- c("dwellr", "survival", "ranger", "ggplot2", "R6",
          "quantreg", "isotree", "xgboost", "forecast", "digest")

env <- rbind(
  data.frame(component = "R", version = paste(R.version$major,
                                              R.version$minor, sep = ".")),
  data.frame(component = pkgs,
             version = vapply(pkgs, function(p)
               as.character(packageVersion(p)), character(1))))
rownames(env) <- NULL
env

## ---- seed-check --------------------------------------------------
a <- simulate_lifecycles(n = 400, seed = 11)
b <- simulate_lifecycles(n = 400, seed = 11)

c(orders_identical = identical(a$orders, b$orders),
  events_identical = identical(a$events, b$events),
  lines_identical  = identical(a$lines, b$lines))
