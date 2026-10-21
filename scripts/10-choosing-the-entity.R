# Generated from chapters/10-choosing-the-entity.qmd by tools/extract.py -- do not edit by hand.
# Chapter: Choosing the Entity
#
# Run from the repository root:
#   Rscript scripts/10-choosing-the-entity.R

library(dwellr)
library(ggplot2)
library(ranger)

source("R/theme.R")


## ---- population --------------------------------------------------
sim    <- simulate_lifecycles(n = 5000, seed = 11)
orders <- sim$orders
events <- sim$events

## ---- grains ------------------------------------------------------
lines   <- order_lines(sim)
tickets <- events[events$event == "pick_ticket_released", ]
tickets$ticket_no <- sprintf("T%06d", seq_len(nrow(tickets)))
orders$customer_no <- sprintf("C%04d", (seq_len(nrow(orders)) * 7919L) %% 900L + 1L)

c(events = nrow(events), orders = nrow(orders), lines = nrow(lines),
  tickets = nrow(tickets), customers = length(unique(orders$customer_no)))

## ---- entity-test -------------------------------------------------
ev_cust <- events
ev_cust$customer_no <- orders$customer_no[match(events$order_no, orders$order_no)]
customers <- data.frame(customer_no = unique(orders$customer_no),
                        stringsAsFactors = FALSE)

row <- function(nm, x) data.frame(
  entity = nm, instances = x$n_instances, attached = x$attached,
  unattached = x$unattached, ambiguous = x$ambiguous,
  attach_rate = round(x$attach_rate, 4), unique_fit = x$unique)

rbind(
  row("order header", entity_check(events,  orders,    "order_no")),
  row("order line",   entity_check(events,  lines,     "order_no")),
  row("pick ticket",  entity_check(events,  tickets,   "order_no")),
  row("customer",     entity_check(ev_cust, customers, "customer_no")))

## ---- obligation --------------------------------------------------
orders$owes    <- !orders$is_terminal
customers$owes <- rep(TRUE, nrow(customers))

hdr <- entity_check(events,  orders,    "order_no",    owed = "owes")
cst <- entity_check(ev_cust, customers, "customer_no", owed = "owes")
c(header_owing = hdr$owing, header_knows = hdr$knows_owed,
  customer_knows = cst$knows_owed)

## ---- weighting ---------------------------------------------------
rel <- stage_dwell(sim, "released")
m   <- merge(rel, orders[, c("order_no", "lines")], by = "order_no")

c(order_weighted = round(mean(m$dwell_hours), 3),
  line_weighted  = round(sum(m$dwell_hours * m$lines) / sum(m$lines), 3),
  inflation      = round(sum(m$dwell_hours * m$lines) / sum(m$lines) /
                         mean(m$dwell_hours) - 1, 4))

## ---- fig-grain ---------------------------------------------------
g <- data.frame(
  lines = as.integer(names(tapply(m$dwell_hours, pmin(m$lines, 6), mean))),
  mean_h = as.numeric(tapply(m$dwell_hours, pmin(m$lines, 6), mean)),
  n = as.numeric(table(pmin(m$lines, 6))))

ggplot(g, aes(lines, mean_h)) +
  geom_hline(yintercept = mean(m$dwell_hours), linetype = 2, colour = "grey55") +
  geom_hline(yintercept = sum(m$dwell_hours * m$lines) / sum(m$lines),
             linetype = 1, colour = "grey25") +
  geom_point(aes(size = n), alpha = 0.7, colour = "grey20") +
  scale_size_area(max_size = 7, guide = "none") +
  scale_x_continuous(breaks = 1:6, labels = c(1:5, "6+")) +
  labs(x = "lines on the order", y = "mean dwell in released (hours)")

## ---- identifiers -------------------------------------------------
estore <- orders[orders$order_source == "estore", ]
ext <- data.frame(ext_ref = sprintf("E%07d", seq_len(nrow(estore))),
                  order_no = estore$order_no, stringsAsFactors = FALSE)

# Thirty collisions: thirty references reused for a second order.
set.seed(7)
dup <- sample(nrow(ext), 60)
ext$ext_ref[dup[1:30]] <- ext$ext_ref[dup[31:60]]

# Eighty fragmentations: eighty orders re-entered under a second reference.
frag <- sample(nrow(ext), 80)
ids  <- c(ext$ext_ref,  sprintf("X%07d", seq_along(frag)))
ents <- c(ext$order_no, ext$order_no[frag])

a <- identifier_audit(ids, ents)
unlist(a[c("n_records", "n_ids", "n_entities", "collisions",
           "collision_records", "fragmentations", "fragmented_records")])

## ---- linkage -----------------------------------------------------
second <- data.frame(amount = round(estore$value),
                     day = as.Date(estore$entered_at),
                     truth = estore$order_no, stringsAsFactors = FALSE)
order_day <- as.Date(orders$entered_at)

cand <- lapply(seq_len(nrow(second)), function(i)
  which(abs(orders$value - second$amount[i]) <= 0.5 &
        order_day == second$day[i]))

score <- function(hit) {
  ok <- !is.na(hit); tp <- sum(hit[ok] == second$truth[ok])
  c(linked = sum(ok), precision = round(tp / sum(ok), 4),
    recall = round(tp / nrow(second), 4))
}

accept_any <- vapply(cand, function(k)
  if (length(k)) orders$order_no[k[1]] else NA_character_, character(1))
accept_unique <- vapply(cand, function(k)
  if (length(k) == 1L) orders$order_no[k] else NA_character_, character(1))

rbind(any_candidate    = score(accept_any),
      unique_candidate = score(accept_unique))

## ---- durable -----------------------------------------------------
first_order <- tapply(as.numeric(orders$entered_at), orders$customer_no, min)
age_hours   <- (as.numeric(sim$as_of) - first_order) / 3600

cust_dwell <- data.frame(order_no = names(first_order), stage = "customer",
                         dwell_hours = age_hours, censored = TRUE)

c(customers = nrow(cust_dwell),
  share_censored = mean(cust_dwell$censored),
  median_age_days = round(median(cust_dwell$dwell_hours) / 24, 1),
  km_median = dwell_quantile(cust_dwell, 0.5),
  km_p10 = dwell_quantile(cust_dwell, 0.10))

## ---- segmentation ------------------------------------------------
seg_branch <- orders$branch[match(rel$order_no, orders$order_no)]
seg_source <- orders$order_source[match(rel$order_no, orders$order_no)]

b <- segment_decision_shift(rel, seg_branch, cost_miss = 40, cost_look = 400)
s <- segment_decision_shift(rel, seg_source, cost_miss = 40, cost_look = 400)

rbind(
  data.frame(key = "branch",       pooled = round(b$pooled, 2), b$per_level,
             changed = b$n_changed, share = round(b$share_changed, 4)),
  data.frame(key = "order_source", pooled = round(s$pooled, 2), s$per_level,
             changed = s$n_changed, share = round(s$share_changed, 4)))

## ---- fig-thresholds ----------------------------------------------
curves <- do.call(rbind, lapply(c("estore", "inside_sales"), function(lv) {
  k <- stage_dwell(sim, "released")
  k <- k[seg_source == lv, ]
  f <- dwell_survival(k)
  data.frame(channel = lv, time = f$time, surv = f$surv)
}))

ggplot(curves, aes(time, surv, colour = channel)) +
  geom_step(linewidth = 0.5) +
  geom_vline(xintercept = b$pooled, linetype = 2, colour = "grey45") +
  geom_vline(xintercept = s$per_level$hours, linetype = 1,
             colour = "grey25", linewidth = 0.3) +
  scale_x_log10() +
  scale_colour_manual(values = c(estore = "grey20", inside_sales = "grey60")) +
  labs(x = "hours in released", y = "share still in stage", colour = NULL) +
  theme(legend.position = "bottom")

## ---- segmentation-ratios -----------------------------------------
ratios <- list(c(40, 400), c(100, 100), c(400, 100), c(400, 40))
do.call(rbind, lapply(ratios, function(cc) {
  r <- segment_decision_shift(rel, seg_source, cc[1], cc[2])
  data.frame(cost_miss = cc[1], cost_look = cc[2],
             pooled_hours = round(r$pooled, 2), changed = r$n_changed,
             share = round(r$share_changed, 4))
}))

## ---- flags -------------------------------------------------------
table(lines$disposition)

c(derived_shortfall = sum(lines$shortfall),
  flagged_by_source = sum(lines$disposition == "cancelled_at_close"),
  ratio = round(sum(lines$shortfall) /
                sum(lines$disposition == "cancelled_at_close"), 2))

## ---- flags-orders ------------------------------------------------
c(ever_short = sum(orders$stock_shortfall | orders$closed_short),
  closed_short = sum(orders$closed_short))

table(orders$stage[orders$closed_short])

## ---- grain-leakage -----------------------------------------------
X <- lines[, c("branch", "order_source", "lines", "value_share")]
X$branch       <- factor(X$branch)
X$order_source <- factor(X$order_source)
X$y            <- factor(lines$shortfall)

auc <- function(p, y) {
  r <- rank(p); n1 <- sum(y); n0 <- length(y) - n1
  (sum(r[y == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0)
}

set.seed(2)
i  <- sample(nrow(X), 0.7 * nrow(X))
f1 <- ranger(y ~ ., X[i, ], probability = TRUE, num.trees = 300, seed = 1)

tr <- sample(unique(lines$order_no), 0.7 * length(unique(lines$order_no)))
g  <- lines$order_no %in% tr
f2 <- ranger(y ~ ., X[g, ], probability = TRUE, num.trees = 300, seed = 1)

c(random_line_split  = round(auc(predict(f1, X[-i, ])$predictions[, "TRUE"],
                                 as.integer(X$y[-i]) - 1), 4),
  order_grouped_split = round(auc(predict(f2, X[!g, ])$predictions[, "TRUE"],
                                  as.integer(X$y[!g]) - 1), 4))
