# Generated from chapters/05-decomposing-lead-time.qmd by tools/extract.py -- do not edit by hand.
# Chapter: Decomposing Lead Time
#
# Run from the repository root:
#   Rscript scripts/05-decomposing-lead-time.R

library(dwellr)
library(ggplot2)

source("R/theme.R")


## ---- population --------------------------------------------------
sim <- simulate_lifecycles(n = 5000, seed = 11)
sim

## ---- trace-plain -------------------------------------------------
trace <- function(id) {
  e <- sim$events[sim$events$order_no == id, c("event", "stage", "occurred_at")]
  e <- e[order(e$occurred_at), ]
  e$elapsed_h <- round(as.numeric(difftime(e$occurred_at,
                                           e$occurred_at[1], units = "hours")), 2)
  print(e, row.names = FALSE)
}
trace("S000001")

## ---- trace-long --------------------------------------------------
trace("S000007")

## ---- spec --------------------------------------------------------
spec <- distribution_stage_spec()
spec

## ---- validate ----------------------------------------------------
stage_validate(sim, spec)

## ---- validate-broken ---------------------------------------------
broken <- sim
drop <- which(broken$events$order_no == "S000001" &
              broken$events$event == "pick_ticket_released")
broken$events <- broken$events[-drop, ]
stage_validate(broken, spec)

## ---- decompose ---------------------------------------------------
dec <- lead_time_decompose(sim, spec = spec)
dec

## ---- independence ------------------------------------------------
total <- attr(dec, "total")
c(sum_of_own_variances = round(sum(dec$own_var)),
  variance_of_total    = round(total[["var"]]),
  ratio                = round(sum(dec$own_var) / total[["var"]], 3))

## ---- fig-mean-vs-variance ----------------------------------------
ggplot(dec, aes(mean_hours, var_share)) +
  geom_point(size = 2.4, colour = "grey25") +
  geom_text(aes(label = stage), hjust = -0.12, vjust = 0.4, size = 3) +
  scale_y_continuous(labels = function(x) sprintf("%.0f%%", 100 * x)) +
  expand_limits(x = c(0, 23), y = c(0, 0.37)) +
  labs(x = "mean contribution to lead time (hours)",
       y = "share of lead-time variance")

## ---- cv ----------------------------------------------------------
c(mean_hours = round(total[["mean"]], 2),
  sd_hours   = round(total[["sd"]], 2),
  mean_days  = round(total[["mean"]] / 24, 2),
  sd_days    = round(total[["sd"]] / 24, 2),
  cv         = round(total[["sd"]] / total[["mean"]], 2))

## ---- by-class ----------------------------------------------------
by_class <- aggregate(cbind(mean_hours, cov_with_total) ~ class,
                      data = dec, FUN = sum)
by_class$mean_share <- by_class$mean_hours / total[["mean"]]
by_class$var_share  <- by_class$cov_with_total / total[["var"]]
by_class <- by_class[order(-by_class$var_share),
                     c("class", "mean_hours", "mean_share", "var_share")]
print(transform(by_class,
                mean_hours = round(mean_hours, 2),
                mean_share = round(mean_share, 3),
                var_share  = round(var_share, 3)), row.names = FALSE)

## ---- degenerate-scan-empty ---------------------------------------
scan0 <- degenerate_stages(sim)
print(transform(scan0[, c("stage", "n_measurable", "zero_share",
                          "median_hours", "verdict")],
                zero_share   = round(zero_share, 3),
                median_hours = round(median_hours, 2)), row.names = FALSE)

## ---- degenerate-scan ---------------------------------------------
printed <- sim$events[sim$events$event == "pick_ticket_released", ]
printed$event <- "pick_ticket_printed"
printed$stage <- "printed"
printed$occurred_at <- printed$occurred_at - 1   # one second earlier

configured <- sim
configured$events <- rbind(sim$events, printed)
configured$events <- configured$events[order(configured$events$order_no,
                                             configured$events$occurred_at), ]

scan <- degenerate_stages(configured)
print(transform(scan[, c("stage", "zero_share", "collapses_onto",
                         "verdict")],
                zero_share = round(zero_share, 3)), row.names = FALSE)

## ---- missing-stage -----------------------------------------------
has_ticket <- unique(sim$events$order_no[
  sim$events$event == "pick_ticket_released"])

in_flight <- sim$orders[!sim$orders$is_terminal, ]
flagged   <- in_flight$order_no[!in_flight$order_no %in% has_ticket]

table(actual_stage = in_flight$stage[in_flight$order_no %in% flagged])

## ---- conflation-cost ---------------------------------------------
d <- stage_dwell(sim)
sapply(c("received", "awaiting_supply"), function(s) {
  x <- d$dwell_hours[d$stage == s]
  round(c(n = length(x), median = median(x),
          p90 = unname(quantile(x, 0.9))), 2)
})
