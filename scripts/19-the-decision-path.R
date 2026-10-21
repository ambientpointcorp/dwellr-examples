# Generated from chapters/19-the-decision-path.qmd by tools/extract.py -- do not edit by hand.
# Chapter: The Decision Path
#
# Run from the repository root:
#   Rscript scripts/19-the-decision-path.R

library(dwellr)
library(ggplot2)
library(ranger)

source("R/theme.R")


## ---- population --------------------------------------------------
sim    <- simulate_lifecycles(n = 5000, seed = 11)
orders <- sim$orders
dwell  <- stage_dwell(sim)

total  <- tapply(dwell$dwell_hours, dwell$order_no, sum)
lead_h <- as.numeric(total[orders$order_no])
breach <- 120
y      <- lead_h > breach
p_star <- 40 / (40 + 400)

feat <- data.frame(
  special   = as.integer(orders$special_order),
  channel   = factor(orders$order_source),
  branch    = factor(orders$branch),
  lines     = orders$lines,
  log_value = log(orders$value),
  hour      = as.integer(format(orders$entered_at, "%H")))

set.seed(5)
tr <- sample(c(TRUE, FALSE), nrow(feat), replace = TRUE, prob = c(0.6, 0.4))
ho <- !tr

auc <- function(s, yy) {
  r <- rank(s); n1 <- sum(yy); n0 <- length(yy) - n1
  (sum(r[yy == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0)
}

## ---- tiers -------------------------------------------------------
key  <- paste(orders$special_order, orders$order_source, sep = "/")
rate <- tapply(y[tr], key[tr], mean)
round(rate, 4)

tier <- function(p) ifelse(p > 2 * p_star, "escalate",
                    ifelse(p > p_star, "notify", "none"))
rule_p <- as.numeric(rate[key])
rule   <- tier(rule_p)

fit   <- ranger(y ~ ., data.frame(feat[tr, ], y = factor(y[tr])),
                probability = TRUE, num.trees = 400, seed = 1)
p     <- predict(fit, feat)$predictions[, "TRUE"]
model <- tier(p)

rbind(rule = table(rule), model = table(model))

## ---- cost-per-decision -------------------------------------------
t1 <- system.time(for (i in 1:20) tier(as.numeric(rate[key])))
t2 <- system.time(for (i in 1:5) predict(fit, feat)$predictions[, "TRUE"])

us <- c(rule  = t1[["elapsed"]] / 20 / nrow(feat) * 1e6,
        model = t2[["elapsed"]] / 5  / nrow(feat) * 1e6)

c(signif(us, 1), ratio = round(us[["model"]] / us[["rule"]], -2))

## ---- rule-against-model ------------------------------------------
acted  <- function(d) d != "none"
summarize <- function(d, yy) {
  data.frame(acted = sum(acted(d)),
             precision = round(mean(yy[acted(d)]), 4),
             recall = round(sum(yy & acted(d)) / sum(yy), 4),
             escalated = sum(d == "escalate"),
             escalate_precision = round(mean(yy[d == "escalate"]), 4))
}
rbind(rule = summarize(rule[ho], y[ho]), model = summarize(model[ho], y[ho]))

## ---- auc-compare -------------------------------------------------
c(rule_auc  = round(auc(rule_p[ho], as.integer(y[ho])), 4),
  model_auc = round(auc(p[ho], as.integer(y[ho])), 4))

## ---- policies ----------------------------------------------------
policies <- lapply(c("raise_only", "replace", "advise"), function(po)
  decision_path(rule[ho], model[ho], policy = po))
names(policies) <- c("raise_only", "replace", "advise")

do.call(rbind, lapply(names(policies), function(nm) {
  d <- policies[[nm]]
  data.frame(policy = nm, acted = sum(acted(d$decided)),
             precision = round(mean(y[ho][acted(d$decided)]), 4),
             recall = round(sum(y[ho] & acted(d$decided)) / sum(y[ho]), 4),
             raised = sum(d$raised), lowered = sum(d$lowered))
}))

## ---- what-each-policy-costs --------------------------------------
dr   <- policies$replace
drop <- acted(dr$rule) & !acted(dr$decided)
ra   <- policies$raise_only
add  <- !acted(ra$rule) & acted(ra$decided)

rbind(
  dropped_by_replace = c(orders = sum(drop), breached = sum(y[ho][drop]),
                         rate = round(mean(y[ho][drop]), 4)),
  added_by_raise_only = c(orders = sum(add), breached = sum(y[ho][add]),
                          rate = round(mean(y[ho][add]), 4)))

## ---- audit -------------------------------------------------------
a <- decision_audit(policies$raise_only)
c(changed = round(a$changed, 4), raised = round(a$raised, 4),
  lowered = round(a$lowered, 4))

## ---- decompose ---------------------------------------------------
risk <- fit
timing <- ranger(h ~ ., data.frame(feat[tr, ], h = lead_h[tr]),
                 num.trees = 400, seed = 1)
hhat   <- predict(timing, feat)$predictions
value  <- orders$value

single <- ranger(z ~ ., data.frame(feat[tr, ],
                                   z = as.numeric(y[tr]) * value[tr]),
                 num.trees = 400, seed = 1)
zhat   <- predict(single, feat)$predictions

cap <- 240
exposed <- y * value
captured <- function(s) {
  i <- order(-s[ho])[1:cap]
  data.frame(breaches = sum(y[ho][i]),
             value_captured = round(sum(exposed[ho][i])),
             share = round(sum(exposed[ho][i]) / sum(exposed[ho]), 4))
}

do.call(rbind, lapply(list(risk_only = p, value_only = value,
                           risk_times_value = p * value,
                           slack_from_timing = -(breach - hhat),
                           single_model = zhat), captured))

## ---- timing-quality ----------------------------------------------
c(mae_hours = round(mean(abs(hhat[ho] - lead_h[ho])), 2),
  correlation = round(cor(hhat[ho], lead_h[ho]), 4))

## ---- drafting-volume ---------------------------------------------
d <- policies$raise_only$decided
c(decisions = length(d), escalated = sum(d == "escalate"),
  share_drafted = round(mean(d == "escalate"), 4))

## ---- tool-surface ------------------------------------------------
exports <- getNamespaceExports("dwellr")
fns     <- exports[vapply(exports, function(n)
             is.function(get(n, asNamespace("dwellr"))), TRUE)]
code    <- vapply(fns, function(n)
             paste(deparse(get(n, asNamespace("dwellr"))), collapse = "\n"), "")

refuses <- grepl("stopifnot\\(|stop\\(|not_yet\\(", code)
outside <- c("write\\.", "writeLines", "saveRDS", "readRDS", "file\\(",
             "url\\(", "download\\.", "Sys\\.setenv", "unlink\\(",
             "system\\(")
touches <- grepl(paste(outside, collapse = "|"), code)

c(functions = length(fns), refuse_bad_input = sum(refuses),
  refusal_share = round(mean(refuses), 3),
  touch_anything_outside_arguments = sum(touches))

## ---- budget ------------------------------------------------------
days  <- as.numeric(diff(range(as.Date(orders$entered_at))))
per_day <- c(orders = nrow(orders) / days,
             escalate = sum(tier(p) == "escalate") / days,
             notify = sum(tier(p) == "notify") / days)
round(per_day, 1)

do.call(rbind, lapply(c(1, 12), function(s) {
  b <- inference_budget(calls = round((per_day[["escalate"]] +
                                       per_day[["notify"]]) * s),
                        cost_per_call = 0.012, cap = 1.00,
                        essential = round(per_day[["escalate"]] * s))
  data.frame(arrival_scale = paste0(s, "x"), calls = b$calls,
             uncapped_cost = round(b$calls * 0.012, 2),
             affordable = b$affordable, cost = round(b$cost, 2),
             share_metered = round(b$share_metered, 3),
             essential_covered = round(b$essential_covered, 3))
}))

## ---- budget-too-tight --------------------------------------------
unlist(inference_budget(calls = 166, cost_per_call = 0.012, cap = 0.50,
                        essential = 70))["essential_covered"]

## ---- fallback-vocabulary -----------------------------------------
fallback_reasons()

## ---- fallback-measured -------------------------------------------
set.seed(3)
n   <- sum(ho)
m   <- tier(p[ho])
why <- rep(NA_character_, n)

off <- orders$order_source[ho] == "estore" & runif(n) < 0.18
to  <- !off & runif(n) < 0.03
th  <- !off & !to & runif(n) < 0.012
un  <- !off & !to & !th & runif(n) < 0.004
m[off | to | th | un] <- NA
why[off] <- "model_disabled";    why[to] <- "model_timeout"
why[th]  <- "model_throttled";   why[un] <- "model_unreachable"

path <- decision_path(rule[ho], m, reason = why)
audit <- decision_audit(path)
c(without_model = round(audit$without_model, 4))
audit$reasons

## ---- malformed ---------------------------------------------------
tryCatch(decision_path("none", model = "CRITICAL"),
         error = function(e) conditionMessage(e))

## ---- stale-inputs ------------------------------------------------
o     <- order(orders$entered_at)
early <- o[1:1500]
late  <- o[(length(o) - 1499):length(o)]

r_early <- tapply(y[early], key[early], mean)
r_late  <- tapply(y[late],  key[late],  mean)
rbind(early = round(r_early, 4), late = round(r_late, 4))

stale <- tier(as.numeric(r_early[key[late]]))
fresh <- tier(as.numeric(r_late[key[late]]))

q <- function(d, yy) data.frame(acted = sum(acted(d)),
  precision = round(mean(yy[acted(d)]), 4),
  recall = round(sum(yy & acted(d)) / sum(yy), 4))
rbind(stale = q(stale, y[late]), refit = q(fresh, y[late]))
c(decisions_that_differ = sum(stale != fresh), of = length(late))

## ---- selection-bias ----------------------------------------------
reviewed <- acted(rule)
c(reviewed_share = round(mean(reviewed), 4),
  breach_rate_reviewed = round(mean(y[reviewed]), 4),
  breach_rate_unreviewed = round(mean(y[!reviewed]), 4),
  breach_rate_all = round(mean(y), 4))

## ---- biased-model ------------------------------------------------
sel <- tr & reviewed
fit_sel <- ranger(y ~ ., data.frame(feat[sel, ], y = factor(y[sel])),
                  probability = TRUE, num.trees = 400, seed = 1)
p_sel <- predict(fit_sel, feat)$predictions[, "TRUE"]

c(training_rows = sum(sel),
  auc_on_all_holdout = round(auc(p_sel[ho], as.integer(y[ho])), 4),
  auc_full_training = round(auc(p[ho], as.integer(y[ho])), 4),
  mean_predicted = round(mean(p_sel[ho]), 4),
  observed = round(mean(y[ho]), 4))
