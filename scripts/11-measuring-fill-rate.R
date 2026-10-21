# Generated from chapters/11-measuring-fill-rate.qmd by tools/extract.py -- do not edit by hand.
# Chapter: Measuring Fill Rate
#
# Run from the repository root:
#   Rscript scripts/11-measuring-fill-rate.R

library(dwellr)
library(ggplot2)
library(ranger)

source("R/theme.R")


## ---- population --------------------------------------------------
sim    <- simulate_lifecycles(n = 5000, seed = 11)
orders <- fill_rate(sim$orders)
lines  <- sim$lines

## ---- constructed -------------------------------------------------
ex <- data.frame(
  line        = 1:3,
  qty_ordered = c(1L, 3L, 4L),
  unit_price  = c(400, 20, 15),
  qty_shipped = c(0L, 2L, 4L))

c(units = sum(ex$qty_shipped) / sum(ex$qty_ordered),
  lines = mean(ex$qty_shipped >= ex$qty_ordered),
  value = sum(ex$qty_shipped * ex$unit_price) /
          sum(ex$qty_ordered * ex$unit_price))

## ---- measured-order ----------------------------------------------
shipped <- orders[orders$fill_defined, ]
pick <- shipped$order_no[which(shipped$fill_binding < 1 & shipped$lines == 3)][1]
one <- lines[lines$order_no == pick, ]

data.frame(line = one$line_no, qty = one$qty_ordered,
           price = round(one$unit_price, 2), shipped = one$qty_shipped,
           extended = round(one$qty_ordered * one$unit_price, 2))

c(units = round(sum(one$qty_shipped) / sum(one$qty_ordered), 4),
  lines = round(mean(one$qty_shipped >= one$qty_ordered), 4),
  value = round(sum(one$qty_shipped * one$unit_price) /
                sum(one$qty_ordered * one$unit_price), 4))

## ---- dimensions --------------------------------------------------
sl <- lines[lines$order_no %in% shipped$order_no, ]
d  <- fill_dimensions(sl)
round(unlist(d[c("units", "lines", "value")]), 4)

## ---- denominator -------------------------------------------------
c(defined = sum(orders$fill_defined), not_defined = sum(!orders$fill_defined))

table(orders$stage[!orders$fill_defined])

## ---- wrong-denominators ------------------------------------------
all_lines <- fill_dimensions(lines)

c(correct   = round(d$units, 4),
  all_lines = round(all_lines$units, 4),
  open_as_1 = round(mean(ifelse(is.na(orders$fill_units), 1,
                                orders$fill_units)), 4))

## ---- binding -----------------------------------------------------
short <- orders[which(orders$fill_binding < 1), ]

c(short_orders = nrow(short),
  share_of_defined = round(nrow(short) / sum(orders$fill_defined), 4))

table(short$fill_binding_dimension)

## ---- spread ------------------------------------------------------
spread <- pmax(short$fill_units, short$fill_lines, short$fill_value) -
          pmin(short$fill_units, short$fill_lines, short$fill_value)

round(c(median = median(spread), p90 = quantile(spread, 0.9)[[1]],
        max = max(spread)), 4)

## ---- fig-axes ----------------------------------------------------
ggplot(short, aes(fill_units, fill_value)) +
  geom_abline(slope = 1, linetype = 2, colour = "grey60") +
  geom_point(alpha = 0.5, size = 1.2, colour = "grey20") +
  scale_x_continuous(labels = function(x) sprintf("%.0f%%", 100 * x)) +
  scale_y_continuous(labels = function(x) sprintf("%.0f%%", 100 * x)) +
  labs(x = "unit fill", y = "value fill")

## ---- direction ---------------------------------------------------
c(value_worse_than_units = sum(short$fill_value < short$fill_units),
  value_better = sum(short$fill_value > short$fill_units))

## ---- alpha-beta --------------------------------------------------
c(alpha = round(mean(shipped$fill_binding >= 1), 4),
  beta  = round(d$units, 4))

## ---- weighting ---------------------------------------------------
rbind(line_weighted  = round(unlist(d[c("units", "lines", "value")]), 4),
      order_weighted = round(d$order_weighted, 4))

## ---- by-size -----------------------------------------------------
sz <- pmin(shipped$lines, 6)
do.call(rbind, lapply(sort(unique(sz)), function(k) {
  ids <- shipped$order_no[sz == k]
  s   <- sl[sl$order_no %in% ids, ]
  data.frame(lines_on_order = ifelse(k == 6, "6+", as.character(k)),
             orders = length(ids),
             unit_fill = round(sum(s$qty_shipped) / sum(s$qty_ordered), 4),
             share_complete = round(mean(shipped$fill_binding[sz == k] >= 1), 4))
}))

## ---- perfect -----------------------------------------------------
on_time <- as.numeric(difftime(shipped$finished_at, shipped$entered_at,
                               units = "hours")) <= 24
on_time[is.na(on_time)] <- FALSE

components <- cbind(complete   = shipped$fill_binding >= 1,
                    on_time    = on_time,
                    no_rework  = shipped$loops == 0,
                    no_residue = !shipped$open_ticket_residue)

round(colMeans(components), 4)

## ---- perfect-rate ------------------------------------------------
perfect <- apply(components, 1, all)
marg    <- colMeans(components)

c(perfect_rate = round(mean(perfect), 4),
  product_of_marginals = round(prod(marg), 4))

## ---- fragility ---------------------------------------------------
round(mean(perfect) - prod(marg - 0.01), 4)

## ---- fig-perfect -------------------------------------------------
bars <- data.frame(
  measure = c(names(marg), "perfect order", "product of marginals"),
  value   = c(as.numeric(marg), mean(perfect), prod(marg)),
  kind    = c(rep("component", 4), "combined", "combined"))
bars$measure <- factor(bars$measure, levels = bars$measure)

ggplot(bars, aes(measure, value, fill = kind)) +
  geom_col(width = 0.6) +
  scale_fill_manual(values = c(component = "grey70", combined = "grey25"),
                    guide = "none") +
  scale_y_continuous(labels = function(x) sprintf("%.0f%%", 100 * x)) +
  labs(x = NULL, y = "share of shipped orders") +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))

## ---- detect ------------------------------------------------------
open_lines <- lines[lines$order_no %in% orders$order_no[!orders$is_terminal], ]
stage_of   <- orders$stage[match(open_lines$order_no, orders$order_no)]

table(ifelse(stage_of %in% c("backordered", "awaiting_supply"),
             "cannot be allocated", "not yet allocated"))

c(open_lines = nrow(open_lines), open_units = sum(open_lines$qty_ordered))

## ---- predict-setup -----------------------------------------------
term <- lines[lines$order_no %in% shipped$order_no, ]
term$short <- factor(term$qty_shipped < term$qty_ordered)
oi <- match(term$order_no, shipped$order_no)

X <- data.frame(
  branch  = factor(shipped$branch[oi]),
  channel = factor(shipped$order_source[oi]),
  lines   = shipped$lines[oi],
  special = as.integer(shipped$special_order[oi]),
  qty     = term$qty_ordered,
  price   = log(term$unit_price),
  y       = term$short)

c(short_lines = sum(term$short == "TRUE"), total_lines = nrow(term),
  rate = round(mean(term$short == "TRUE"), 4))

## ---- predict -----------------------------------------------------
auc <- function(p, y) {
  r <- rank(p); n1 <- sum(y); n0 <- length(y) - n1
  (sum(r[y == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0)
}

set.seed(3)
tr <- sample(unique(term$order_no), 0.7 * length(unique(term$order_no)))
g  <- term$order_no %in% tr

fit <- ranger(y ~ ., X[g, ], probability = TRUE, num.trees = 400, seed = 1)
p   <- predict(fit, X[!g, ])$predictions[, "TRUE"]

c(auc = round(auc(p, as.integer(X$y[!g]) - 1), 4),
  mean_predicted = round(mean(p), 4),
  observed = round(mean(X$y[!g] == "TRUE"), 4))

## ---- predict-stock -----------------------------------------------
items <- sim$items
ii <- match(term$item_no, items$item_no)

Z <- cbind(X, data.frame(
  on_hand    = term$on_hand_at_entry,
  on_order   = term$on_order_at_entry,
  cover      = term$cover_ratio,
  demand_30d = term$demand_30d,
  promised   = ifelse(is.na(term$promised_in_days), -1,
                      term$promised_in_days),
  item_class = factor(items$item_class[ii]),
  lt_mean    = items$lead_time_mean[ii],
  lt_sd      = items$lead_time_sd[ii]))

stock_fit <- ranger(y ~ ., Z[g, ], probability = TRUE, num.trees = 400,
                    seed = 1)
ps <- predict(stock_fit, Z[!g, ])$predictions[, "TRUE"]

rule <- as.integer(Z$on_hand[!g] < Z$qty[!g])
obs  <- as.integer(Z$y[!g]) - 1

c(order_attributes = round(auc(p, obs), 4),
  on_hand_vs_qty   = round(auc(rule, obs), 4),
  with_stock_state = round(auc(ps, obs), 4))

## ---- predict-capacity --------------------------------------------
ord <- order(ps, decreasing = TRUE)
yv  <- Z$y[!g] == "TRUE"
rule_ord <- order(rule, decreasing = TRUE)

rbind(model = c(top_100 = round(mean(yv[ord[1:100]]), 3),
                top_500 = round(mean(yv[ord[1:500]]), 3)),
      rule  = c(top_100 = round(mean(yv[rule_ord[1:100]]), 3),
                top_500 = round(mean(yv[rule_ord[1:500]]), 3)))
