# Generated from chapters/f-the-dwellr-examples-repository.qmd by tools/extract.py -- do not edit by hand.
# Chapter: The `dwellr-examples` repository
#
# Run from the repository root:
#   Rscript scripts/f-the-dwellr-examples-repository.R

library(dwellr)
library(ggplot2)

source("R/theme.R")


## ---- generator ---------------------------------------------------
sim <- simulate_lifecycles(n = 5000, seed = 11)
vapply(sim, function(x) if (is.data.frame(x)) nrow(x) else length(x),
       numeric(1))

## ---- dwell-shape -------------------------------------------------
d <- stage_dwell(sim)
do.call(rbind, lapply(sort(unique(d$stage)), function(st) {
  x <- d$dwell_hours[d$stage == st]
  data.frame(stage = st, visits = length(x),
             p50 = round(median(x), 2),
             p90 = round(unname(quantile(x, 0.9)), 2),
             mean = round(mean(x), 2), cv = round(sd(x) / mean(x), 2))
}))

## ---- pathologies -------------------------------------------------
o <- sim$orders
c(censored_visits = sum(d$censored),
  partially_shipped = sum(o$partially_shipped),
  with_a_loop = sum(o$loops > 0),
  ticket_residue = sum(o$open_ticket_residue),
  specialty = sum(o$special_order))

## ---- own-extract -------------------------------------------------
own <- list(
  orders = data.frame(
    order_no    = c("A1", "A2"),
    entered_at  = as.POSIXct(c("2026-02-01 08:00", "2026-02-01 09:30"),
                             tz = "UTC"),
    stage       = c("received", "shipped"),
    is_terminal = c(FALSE, TRUE),
    branch      = c("30", "60"),
    stringsAsFactors = FALSE),
  events = data.frame(
    order_no    = c("A1", "A2", "A2"),
    event       = c("order_entered", "order_entered", "order_shipped"),
    stage       = c("received", "received", "shipped"),
    occurred_at = as.POSIXct(c("2026-02-01 08:00", "2026-02-01 09:30",
                               "2026-02-02 14:00"), tz = "UTC"),
    stringsAsFactors = FALSE),
  as_of = as.POSIXct("2026-02-03 07:00", tz = "UTC"))

stage_dwell(own)
