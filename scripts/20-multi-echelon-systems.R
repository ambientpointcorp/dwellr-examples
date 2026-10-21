# Generated from chapters/20-multi-echelon-systems.qmd by tools/extract.py -- do not edit by hand.
# Chapter: Multi-Echelon Systems
#
# Run from the repository root:
#   Rscript scripts/20-multi-echelon-systems.R

library(dwellr)
library(ggplot2)

source("R/theme.R")


## ---- serial-identities -------------------------------------------
mu <- 10; sd <- 3
L  <- c(1, 2)        # transit periods into stage 1, and from the source
h  <- c(1, 0.6)      # installation holding, downstream first
b  <- 9              # backorder penalty at stage 1

ample <- echelon_simulate(c(28, 59), mu, sd, L, h, b, periods = 6000)
ample$identity
ample$net_inventory_identity

## ---- starved-identity --------------------------------------------
tight <- echelon_simulate(c(28, 49), mu, sd, L, h, b, periods = 6000)
tight$identity
tight$net_inventory_identity

## ---- echelon-against-installation --------------------------------
tune <- function(...) echelon_optimize(mu, sd, L, h, b, periods = 2000,
                                       steps = c(8, 2), span = 3, ...)
oe <- tune()
oi <- tune(policy = "installation")

ev <- function(o, pol) {
  r <- echelon_simulate(o$levels, mu, sd, L, h, b, periods = 6000,
                        policy = pol)
  data.frame(policy = pol, levels = paste(o$levels, collapse = "/"),
             cost = round(r$cost, 3), fill_rate = round(r$fill_rate, 4),
             on_hand_1 = round(r$on_hand[1], 2),
             on_hand_2 = round(r$on_hand[2], 2))
}
rbind(ev(oe, "echelon"), ev(oi, "installation"))

## ---- decomposition -----------------------------------------------
grid <- seq(22, 33)
do.call(rbind, lapply(c(45, 55, 65, 80), function(y2) {
  cost <- vapply(grid, function(y1)
    echelon_simulate(c(y1, y2), mu, sd, L, h, b, periods = 6000)$cost, 0)
  data.frame(upstream_level = y2, best_downstream = grid[which.min(cost)],
             cost = round(min(cost), 3))
}))

c(echelon_margin = round(mu * (L[1] + 1) +
    qnorm(b / (b + h[1] - h[2])) * sd * sqrt(L[1] + 1), 2),
  installation_margin = round(mu * (L[1] + 1) +
    qnorm(b / (b + h[1])) * sd * sqrt(L[1] + 1), 2))

## ---- assembly ----------------------------------------------------
sim    <- simulate_lifecycles(n = 5000, seed = 11)
orders <- sim$orders
lines  <- sim$lines
items  <- sim$items

set.seed(4)
li <- match(lines$item_no, items$item_no)
lines$ready <- pmax(0, rnorm(nrow(lines), items$lead_time_mean[li],
                             items$lead_time_sd[li]))
per_order <- do.call(rbind, tapply(lines$ready, lines$order_no,
                                  function(x) c(max = max(x), k = length(x))))

c(mean_line_ready = round(mean(lines$ready), 3),
  mean_order_ready = round(mean(per_order[, "max"]), 3),
  inflation = round(mean(per_order[, "max"]) / mean(lines$ready), 3))

## ---- assembly-by-lines -------------------------------------------
do.call(rbind, lapply(1:8, function(k) {
  i <- per_order[, "k"] == k
  data.frame(lines = k, orders = sum(i),
             mean_ready = round(mean(per_order[i, "max"]), 2),
             p90_ready = round(unname(quantile(per_order[i, "max"], 0.9)), 2))
}))

## ---- allocation --------------------------------------------------
allocate_sim <- function(yw, yb, mu, sd, Lw, Lb, h_w, h_b, b,
                         rule = "proportional", periods = 6000,
                         warmup = 500, seed = 1) {
  set.seed(seed)
  n <- length(mu)
  d  <- mapply(function(m, s) pmax(0, rnorm(periods, m, s)), mu, sd)
  pw <- rep(sum(mu), Lw)
  pb <- matrix(mu, nrow = Lb, ncol = n, byrow = TRUE)
  Iw <- yw - sum(yb); Ib <- yb - mu * Lb
  acc <- 0; kept <- 0; fill <- numeric(n); dem <- numeric(n); spread <- 0

  for (t in seq_len(periods)) {
    Iw <- Iw + pw[1]; pw <- c(pw[-1], 0)
    Ib <- Ib + pb[1, ]; pb <- rbind(pb[-1, , drop = FALSE], 0)
    posb <- Ib + colSums(pb)
    pw[Lw] <- pw[Lw] + max(0, yw - (Iw + sum(pw) + sum(posb)))
    want <- pmax(0, yb - posb)
    ship <- if (sum(want) <= max(Iw, 0)) want else {
      avail <- max(Iw, 0)
      switch(rule,
             proportional = avail * want / sum(want),
             high_volume_first = {
               s <- numeric(n); left <- avail
               for (k in order(-mu)) { g <- min(want[k], left)
                 s[k] <- g; left <- left - g }
               s },
             by_cover = { risk <- want / pmax(mu, 1e-9)
                          avail * risk / sum(risk) })
    }
    Iw <- Iw - sum(ship); pb[Lb, ] <- pb[Lb, ] + ship
    served <- pmin(pmax(Ib, 0), d[t, ]); Ib <- Ib - d[t, ]
    if (t > warmup) {
      kept <- kept + 1
      acc <- acc + h_w * max(Iw, 0) + sum(h_b * pmax(Ib, 0)) +
             b * sum(pmax(-Ib, 0))
      fill <- fill + served; dem <- dem + d[t, ]
      spread <- spread + abs(diff(pmax(Ib, 0) / pmax(mu, 1e-9)))
    }
  }
  list(cost = acc / kept, fill = fill / dem, cover_spread = spread / kept)
}

mub <- c(3, 7); sdb <- c(1.5, 2.5)
do.call(rbind, lapply(c("proportional", "high_volume_first", "by_cover"),
                      function(rule) {
  r <- allocate_sim(48, c(12, 20), mub, sdb, 2, 1, 0.6, 1, 9, rule = rule)
  data.frame(rule = rule, cost = round(r$cost, 3),
             fill_small = round(r$fill[1], 4), fill_large = round(r$fill[2], 4),
             cover_spread = round(r$cover_spread, 3))
}))

## ---- pooling -----------------------------------------------------
pooled <- allocate_sim(48, c(32, 0), c(sum(mub), 1e-9),
                       c(sqrt(sum(sdb^2)), 1e-9), 2, 1, 0.6, 1, 9)
c(pooled_cost = round(pooled$cost, 3),
  pooled_fill = round(pooled$fill[1], 4))

## ---- state-space -------------------------------------------------
do.call(rbind, lapply(2:6, function(n)
  data.frame(stages = n, levels_per_stage = 40,
             states = format(40^n, big.mark = ",", scientific = FALSE))))

## ---- administrative-share ----------------------------------------
dwell  <- stage_dwell(sim)
by_stage <- tapply(dwell$dwell_hours, dwell$stage, sum)
round(100 * by_stage / sum(by_stage), 2)

admin <- c("received", "credit_hold", "released")
c(administrative_share = round(sum(by_stage[admin]) / sum(by_stage), 4))

## ---- stall-against-its-mean --------------------------------------
stall <- list(stage = 1, probability = 0.5, periods = 2)
os <- tune(stall = stall)
od <- echelon_optimize(mu, sd, c(2, 2), h, b, periods = 2000,
                       steps = c(8, 2), span = 3)

rbind(
  stalled = data.frame(levels = paste(os$levels, collapse = "/"),
    cost = round(echelon_simulate(os$levels, mu, sd, L, h, b, periods = 6000,
                                  stall = stall)$cost, 3)),
  deterministic = data.frame(levels = paste(od$levels, collapse = "/"),
    cost = round(echelon_simulate(od$levels, mu, sd, c(2, 2), h, b,
                                  periods = 6000)$cost, 3)))

## ---- stall-and-bullwhip ------------------------------------------
base <- echelon_simulate(oe$levels, mu, sd, L, h, b, periods = 6000)
with_stall <- echelon_simulate(oe$levels, mu, sd, L, h, b, periods = 6000,
                               stall = list(stage = 1, probability = 0.1,
                                            periods = 4))
rbind(
  no_stall = c(bullwhip = round(bullwhip(base$demand, base$orders)$ratio, 3),
               fill = round(base$fill_rate, 4)),
  stalled  = c(bullwhip = round(bullwhip(with_stall$demand,
                                         with_stall$orders)$ratio, 3),
               fill = round(with_stall$fill_rate, 4)))

## ---- review-amplification ----------------------------------------
do.call(rbind, lapply(c(1, 2, 5, 10), function(R) {
  o <- tune(review = R)
  r <- echelon_simulate(o$levels, mu, sd, L, h, b, periods = 6000, review = R)
  data.frame(review_periods = R, levels = paste(o$levels, collapse = "/"),
             cost = round(r$cost, 3), fill_rate = round(r$fill_rate, 4),
             bullwhip = round(bullwhip(r$demand, r$orders)$ratio, 2))
}))

## ---- fidelity ----------------------------------------------------
tuned_without <- echelon_simulate(oe$levels, mu, sd, L, h, b, periods = 6000,
                                  stall = stall)
tuned_with <- echelon_simulate(os$levels, mu, sd, L, h, b, periods = 6000,
                               stall = stall)
c(tuned_without_the_stall = round(tuned_without$cost, 3),
  fill = round(tuned_without$fill_rate, 4),
  tuned_with_the_stall = round(tuned_with$cost, 3),
  fill_with = round(tuned_with$fill_rate, 4),
  penalty = round((tuned_without$cost - tuned_with$cost) /
                  tuned_with$cost, 3))

## ---- stall-placement ---------------------------------------------
do.call(rbind, lapply(1:2, function(j) {
  stj <- list(stage = j, probability = 0.1, periods = 4)
  oj <- tune(stall = stj)
  rj <- echelon_simulate(oj$levels, mu, sd, L, h, b, periods = 6000,
                         stall = stj)
  data.frame(stall_at_stage = j, levels = paste(oj$levels, collapse = "/"),
             cost = round(rj$cost, 3),
             cost_above_clean = round(rj$cost - base$cost, 3),
             fill_rate = round(rj$fill_rate, 4))
}))

## ---- ownership ---------------------------------------------------
special <- orders$order_no[orders$special_order]
ds <- dwell[dwell$order_no %in% special, ]
bs <- tapply(ds$dwell_hours, ds$stage, sum)
round(100 * bs / sum(bs), 2)

vendor <- c("awaiting_supply", "backordered")
c(vendor_share_specialty = round(sum(bs[vendor]) / sum(bs), 4),
  vendor_variance_rank = which(names(sort(
    tapply(dwell$dwell_hours, dwell$stage, var), decreasing = TRUE)) ==
    "awaiting_supply"))
