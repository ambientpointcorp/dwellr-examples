# Generated from chapters/08-lifecycles-that-are-not-chains.qmd by tools/extract.py -- do not edit by hand.
# Chapter: Lifecycles That Are Not Chains
#
# Run from the repository root:
#   Rscript scripts/08-lifecycles-that-are-not-chains.R

library(dwellr)
library(ggplot2)

source("R/theme.R")


## ---- population --------------------------------------------------
sim  <- simulate_lifecycles(n = 5000, seed = 11)
spec <- distribution_stage_spec()
dwell <- stage_dwell(sim)

## ---- loops -------------------------------------------------------
r <- revisit_count(sim)
c(instances_with_a_revisit = nrow(r),
  share = round(nrow(r) / nrow(sim$orders), 3),
  stages_affected = length(unique(r$stage)),
  max_visits = max(r$visits))

## ---- time-in-stage -----------------------------------------------
t <- time_in_stage(sim, "released")
looped <- t[t$visits > 1, ]
round(c(median_since_entry = median(looped$since_entry_hours),
        median_total       = median(looped$total_hours),
        median_understated = median(looped$understated),
        worst_understated  = max(looped$understated)), 3)

## ---- fig-loop ----------------------------------------------------
ggplot(looped, aes(since_entry_hours, total_hours)) +
  geom_abline(slope = 1, linetype = 2, colour = "grey65") +
  geom_point(alpha = 0.45, size = 1.1, colour = "grey25") +
  scale_x_log10() + scale_y_log10() +
  labs(x = "hours since the most recent entry", y = "hours across both visits")

## ---- loop-counts -------------------------------------------------
ever_released <- length(unique(dwell$order_no[dwell$stage == "released"]))
c(release_events = sum(dwell$stage == "released"),
  orders_ever_released = ever_released,
  inflation = round(sum(dwell$stage == "released") / ever_released, 3))

## ---- partial -----------------------------------------------------
o <- sim$orders
c(partially_shipped = sum(o$partially_shipped),
  share = round(mean(o$partially_shipped), 3),
  closed_short = sum(o$closed_short),
  share_of_shipped = round(mean(o$closed_short[o$stage == "shipped"]), 3))

## ---- residue -----------------------------------------------------
q <- queue_reconcile(sim)
unlist(lapply(q, function(x) round(x, 3)))

## ---- split-dwell -------------------------------------------------
ev <- sim$events[order(sim$events$order_no, sim$events$occurred_at), ]
at <- function(e) {
  x <- ev$occurred_at[ev$event == e]
  stats::setNames(x, ev$order_no[ev$event == e])
}
entered <- at("order_entered"); first <- at("partial_shipment")
final <- at("order_shipped")

ids <- intersect(names(first),
                 sim$orders$order_no[sim$orders$stage == "shipped"])
to_first <- as.numeric(difftime(first[ids], entered[ids], units = "hours"))
to_final <- as.numeric(difftime(final[ids], entered[ids], units = "hours"))

rbind(to_first_shipment = c(median = median(to_first),
                            p90 = unname(quantile(to_first, 0.9)),
                            breached_24h = mean(to_first > 24)),
      to_final_shipment = c(median = median(to_final),
                            p90 = unname(quantile(to_final, 0.9)),
                            breached_24h = mean(to_final > 24)))

## ---- rework ------------------------------------------------------
set.seed(3)
base <- stage_dwell(sim, "received")
pick <- sample(nrow(base), nrow(base) %/% 3)
adjust <- data.frame(
  order_no = base$order_no[pick], event = "quantity_adjusted",
  stage = "received",
  occurred_at = base$entered_at[pick] +
    3600 * base$dwell_hours[pick] * runif(length(pick), 0.2, 0.8),
  stringsAsFactors = FALSE)

reworked <- sim
reworked$events <- rbind(sim$events, adjust)
reworked$events <- reworked$events[order(reworked$events$order_no,
                                         reworked$events$occurred_at), ]
after <- stage_dwell(reworked, "received")

rbind(before = c(visits = nrow(base),
                 median = median(base$dwell_hours),
                 p90 = unname(quantile(base$dwell_hours, 0.9)),
                 over_24h = mean(base$dwell_hours > 24)),
      after  = c(visits = nrow(after),
                 median = median(after$dwell_hours),
                 p90 = unname(quantile(after$dwell_hours, 0.9)),
                 over_24h = mean(after$dwell_hours > 24)))

## ---- rework-detect -----------------------------------------------
stage_validate(reworked, spec)

## ---- loop-signal -------------------------------------------------
total <- tapply(dwell$dwell_hours, dwell$order_no, sum)
loops <- sim$orders$loops[match(names(total), sim$orders$order_no)]

do.call(rbind, lapply(0:1, function(k) data.frame(
  loops = k, instances = sum(loops == k),
  median_hours = round(median(total[loops == k]), 1),
  p90_hours = round(unname(quantile(total[loops == k], 0.9)), 1),
  over_24h = round(mean(total[loops == k] > 24), 3))))

## ---- variants ----------------------------------------------------
variants <- lifecycle_variants(sim, min_share = 0.005)
variants

## ---- fig-variants ------------------------------------------------
all_v <- lifecycle_variants(sim, min_share = 0)
all_v$rank <- seq_len(nrow(all_v))
all_v$cumulative <- cumsum(all_v$share)

ggplot(all_v, aes(rank, cumulative)) +
  geom_step(colour = "grey25") +
  geom_hline(yintercept = 0.92, linetype = 2, colour = "grey65") +
  scale_y_continuous(labels = function(p) sprintf("%.0f%%", 100 * p)) +
  labs(x = "variant, by frequency", y = "cumulative share of instances")

## ---- separation --------------------------------------------------
stage_separation(sim, spec)

## ---- separation-fail ---------------------------------------------
set.seed(3)
ev <- sim$events
rel <- which(ev$stage == "released")
half <- sample(rel, length(rel) %/% 2)
ev$stage[half] <- "released_a"
ev$stage[setdiff(rel, half)] <- "released_b"

split_sim <- sim
split_sim$events <- ev
split_spec <- stage_spec(
  stages = c("released_a", "released_b", "shipped"),
  transitions = c("released_a -> released_b", "released_b -> released_a",
                  "released_a -> shipped", "released_b -> shipped"),
  terminal = "shipped")

stage_separation(split_sim, split_spec)
