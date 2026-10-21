# Generated from chapters/21-evaluating-a-monitoring-policy.qmd by tools/extract.py -- do not edit by hand.
# Chapter: Evaluating a Monitoring Policy
#
# Run from the repository root:
#   Rscript scripts/21-evaluating-a-monitoring-policy.R

library(dwellr)
library(ggplot2)
library(ranger)

source("R/theme.R")


## ---- population --------------------------------------------------
sim    <- simulate_lifecycles(n = 5000, seed = 11)
orders <- sim$orders
dwell  <- stage_dwell(sim)
lead_h <- as.numeric(tapply(dwell$dwell_hours, dwell$order_no,
                            sum)[orders$order_no])

breach <- 120
y      <- lead_h > breach
p_star <- 40 / (40 + 400)
c_look <- 40
c_miss <- 400

key  <- paste(orders$special_order, orders$order_source, sep = "/")
set.seed(5)
tr   <- sample(c(TRUE, FALSE), nrow(orders), replace = TRUE, prob = c(0.6, 0.4))
rate <- tapply(y[tr], key[tr], mean)

tier <- function(p) ifelse(p > 2 * p_star, "escalate",
                    ifelse(p > p_star, "notify", "none"))
rule_p  <- as.numeric(rate[key])
current <- tier(rule_p)

## ---- the-four-cells ----------------------------------------------
table(flagged = current != "none", breached = y)

## ---- cost-of-invisible-errors ------------------------------------
fp <- sum(current != "none" & !y)
fn <- sum(current == "none" & y)

data.frame(
  error = c("false positive", "false negative"),
  leaves_a_record = c(TRUE, FALSE),
  items = c(fp, fn),
  unit_cost = c(c_look, c_miss),
  total_cost = c(fp * c_look, fn * c_miss),
  share_of_error_cost = round(c(fp * c_look, fn * c_miss) /
                              (fp * c_look + fn * c_miss), 4))

## ---- complaint-tuning --------------------------------------------
threshold <- p_star
history <- NULL
for (k in 1:8) {
  d  <- rule_p > threshold
  tp <- sum(d & y)
  history <- rbind(history, data.frame(
    round = k, threshold = round(threshold, 4), flagged = sum(d),
    false_positives = sum(d & !y), missed = sum(!d & y),
    precision = round(if (sum(d)) tp / sum(d) else NA_real_, 4),
    recall = round(tp / sum(y), 4)))
  if (sum(d & !y) > 300) threshold <- threshold * 1.3 else break
}
history

## ---- selection ---------------------------------------------------
acted <- current != "none"
c(population = round(mean(y), 4),
  among_reviewed = round(mean(y[acted]), 4),
  among_unreviewed = round(mean(y[!acted]), 4))

## ---- off-policy --------------------------------------------------
feat <- data.frame(
  special   = as.integer(orders$special_order),
  channel   = factor(orders$order_source),
  branch    = factor(orders$branch),
  lines     = orders$lines,
  log_value = log(orders$value),
  hour      = as.integer(format(orders$entered_at, "%H")))
fit <- ranger(y ~ ., data.frame(feat[tr, ], y = factor(y[tr])),
              probability = TRUE, num.trees = 400, seed = 1)
p <- predict(fit, feat)$predictions[, "TRUE"]
proposed <- tier(p)

set.seed(21)
eps      <- 0.10                       # the share of decisions randomized
rule_act <- current != "none"
explore  <- runif(nrow(orders)) < eps
logged   <- ifelse(explore, runif(nrow(orders)) < 0.5, rule_act)
prop     <- ifelse(logged == rule_act, (1 - eps) + eps * 0.5, eps * 0.5)
reward   <- -(logged * c_look + (!logged) * y * c_miss)
target   <- proposed != "none"

qm <- cbind("TRUE" = rep(-c_look, nrow(orders)), "FALSE" = -p * c_miss)
target_prop <- ifelse(target == rule_act, (1 - eps) + eps * 0.5, eps * 0.5)
v <- offpolicy_value(as.character(logged), reward, prop, as.character(target),
                     target_propensity = target_prop, reward_model = qm)

c(true_value = round(mean(-(target * c_look + (!target) * y * c_miss)), 3),
  ips = round(v$ips, 3), ips_se = round(v$ips_se, 3),
  dr = round(v$dr, 3), dr_se = round(v$dr_se, 3),
  logged_policy = round(v$on_policy, 3),
  evidence = round(v$evidence, 4), positivity = round(v$positivity, 4))

## ---- no-exploration ----------------------------------------------
v0 <- offpolicy_value(as.character(rule_act),
                      -(rule_act * c_look + (!rule_act) * y * c_miss),
                      propensity = rep(1, nrow(orders)),
                      target = as.character(target),
                      target_propensity = as.numeric(target == rule_act))
c(ips = round(v0$ips, 3), evidence = round(v0$evidence, 4),
  positivity = round(v0$positivity, 4))

## ---- live-state --------------------------------------------------
open <- !orders$is_terminal
e <- evaluate_policy(current[open], proposed[open],
                     id = orders$order_no[open], value = orders$value[open])
e$transition
e$volume

## ---- three-lists -------------------------------------------------
round(e$exposure, 2)
head(e$ceasing, 3)

## ---- harness-drift -----------------------------------------------
harness <- ifelse(rule_p > 2.2 * p_star, "escalate",
           ifelse(rule_p > 1.1 * p_star, "notify", "none"))

c(open_items = sum(open),
  disagreements = sum(harness[open] != current[open]),
  share = round(mean(harness[open] != current[open]), 4))

## ---- replay ------------------------------------------------------
days <- as.numeric(diff(range(as.Date(orders$entered_at))))
c(days = days,
  current_per_day = round(sum(acted) / days, 1),
  proposed_per_day = round(sum(proposed != "none") / days, 1),
  arrivals_per_day = round(nrow(orders) / days, 1))

## ---- reachability ------------------------------------------------
rules <- data.frame(
  name  = c("aged credit hold", "high-value stall", "promised date breach"),
  field = c("stage", "order_value", "promised_date"),
  op    = c("in", ">", "<"),
  value = c("credit_hold", "1000", "100"),
  stringsAsFactors = FALSE)

population <- data.frame(stage = orders$stage[open],
                         value = orders$value[open],
                         promised_date = orders$promised_date[open])
rule_reachable(rules, population)

## ---- stage-list --------------------------------------------------
stage_list <- c("received", "released", "awaiting_supply", "backordered")
rule_reachable(rules[1, ],
               data.frame(stage = intersect(population$stage, stage_list)))

## ---- power -------------------------------------------------------
per_day <- nrow(orders) / days
do.call(rbind, lapply(c(0.30, 0.20, 0.15, 0.10), function(rel) {
  pw <- power.prop.test(p1 = mean(y), p2 = mean(y) * (1 - rel),
                        power = 0.8, sig.level = 0.05)
  data.frame(relative_reduction = rel,
             orders_per_arm = ceiling(pw$n),
             days = ceiling(2 * pw$n / per_day))
}))

## ---- selection-trap ----------------------------------------------
received <- dwell[dwell$stage == "received", ]
i <- match(received$order_no, orders$order_no)

c(flagged_mean_hours = round(mean(received$dwell_hours[current[i] != "none"]), 2),
  unflagged_mean_hours = round(mean(received$dwell_hours[current[i] == "none"]), 2))
