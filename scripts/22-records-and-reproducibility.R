# Generated from chapters/22-records-and-reproducibility.qmd by tools/extract.py -- do not edit by hand.
# Chapter: Records and Reproducibility
#
# Run from the repository root:
#   Rscript scripts/22-records-and-reproducibility.R

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
key    <- paste(orders$special_order, orders$order_source, sep = "/")
open   <- !orders$is_terminal

tier <- function(p) ifelse(p > 2 * p_star, "escalate",
                    ifelse(p > p_star, "notify", "none"))

## ---- two-versions ------------------------------------------------
o     <- order(orders$entered_at)
early <- o[1:2500]
late  <- o[2501:5000]

v1 <- tapply(y[early], key[early], mean)
v2 <- tapply(y[late],  key[late],  mean)
rbind(`cohort-v1` = round(v1, 4), `cohort-v2` = round(v2, 4))

d1 <- tier(as.numeric(v1[key]))
d2 <- tier(as.numeric(v2[key]))
c(open_items = sum(open),
  decisions_that_differ = sum(d1[open] != d2[open]),
  share = round(mean(d1[open] != d2[open]), 4))

## ---- hash-chain --------------------------------------------------
chain <- function(r) {
  h <- character(nrow(r)); prev <- ""
  for (i in seq_len(nrow(r))) {
    row <- paste(r$id[i], r$decided[i], r$policy_version[i], sep = "|")
    h[i] <- substr(digest::digest(paste0(prev, row), algo = "sha256"), 1, 12)
    prev <- h[i]
  }
  h
}

ledger <- decision_record(decided = c("notify", "none", "escalate", "notify"),
                          id = c("A", "B", "C", "D"), at = sim$as_of,
                          policy_version = "cohort-v2", source = "rule")
edited <- ledger
edited$decided[2] <- "notify"

data.frame(id = ledger$id, published = chain(ledger),
           after_the_edit = chain(edited),
           intact = chain(ledger) == chain(edited))

## ---- the-record --------------------------------------------------
feat <- data.frame(
  special   = as.integer(orders$special_order),
  channel   = factor(orders$order_source),
  branch    = factor(orders$branch),
  lines     = orders$lines,
  log_value = log(orders$value),
  hour      = as.integer(format(orders$entered_at, "%H")))
set.seed(5)
tr  <- sample(c(TRUE, FALSE), nrow(orders), replace = TRUE, prob = c(0.6, 0.4))
fit <- ranger(y ~ ., data.frame(feat[tr, ], y = factor(y[tr])),
              probability = TRUE, num.trees = 400, seed = 1)
p   <- predict(fit, feat)$predictions[, "TRUE"]

set.seed(3)
n   <- sum(open)
m   <- tier(p[open])
why <- rep(NA_character_, n)
off <- orders$order_source[open] == "estore" & runif(n) < 0.18
m[off] <- NA
why[off] <- "model_disabled"

path <- decision_path(d2[open], m, reason = why)
rec  <- decision_record(
  decided        = path$decided,
  id             = orders$order_no[open],
  at             = sim$as_of,
  policy_version = "cohort-v2",
  source         = path$source,
  model_version  = ifelse(path$source == "model", "forest-2026-10", NA),
  score          = ifelse(path$source == "model", round(p[open], 4), NA),
  reason         = path$reason,
  basis          = "trailing 60-day cohort rate, p* = 0.0909")

table(rec$source)
data.frame(with_a_fallback_reason = sum(!is.na(rec$reason)),
           conformant = record_conformance(rec)$ok)

## ---- replay ------------------------------------------------------
pinned  <- replay_check(rec, path$decided)
drifted <- replay_check(rec, decision_path(d1[open], m, reason = why)$decided)

set.seed(99)
tr2  <- sample(c(TRUE, FALSE), nrow(orders), replace = TRUE, prob = c(0.6, 0.4))
fit2 <- ranger(y ~ ., data.frame(feat[tr2, ], y = factor(y[tr2])),
               probability = TRUE, num.trees = 400, seed = 2)
p2   <- predict(fit2, feat)$predictions[, "TRUE"]
m2   <- tier(p2[open]); m2[off] <- NA
refit <- replay_check(rec, decision_path(d2[open], m2, reason = why)$decided)

do.call(rbind, lapply(list(pinned = pinned, wrong_config = drifted,
                           refitted_model = refit), function(k)
  data.frame(agreement = round(k$agreement, 4), diverged = nrow(k$diverged))))

## ---- conformance -------------------------------------------------
broken <- rec
broken$policy_version[c(3, 9)] <- NA
broken$decided[5] <- "URGENT"
record_conformance(broken)$problems

## ---- version-guard -----------------------------------------------
publish <- function(store, draft, base_version) {
  if (store$version != base_version)
    stop(sprintf("refused: store is at %s, draft was based on %s",
                 store$version, base_version), call. = FALSE)
  list(version = paste0("v", as.integer(sub("v", "", store$version)) + 1L),
       rule = draft)
}

store <- list(version = "v2", rule = "value > 1000")
store <- publish(store, "value > 2000", "v2")
store$version

tryCatch(publish(store, "value > 3000", "v2"),
         error = function(e) conditionMessage(e))

## ---- confirmation-gate -------------------------------------------
fields <- c(stage = "character", value = "numeric", lines = "numeric")
pop <- data.frame(stage = orders$stage[open], value = orders$value[open],
                  lines = orders$lines[open], stringsAsFactors = FALSE)

do.call(rbind, lapply(c("stage in credit_hold,awaiting_supply and value > 1000",
                        "value > 100",
                        "stage in credit_hold and value > 100000"),
                      function(txt) {
  r   <- rule_compile(txt, fields)
  hit <- rule_apply(r, pop)
  q   <- queue_stability(alerts_per_day = sum(hit), reviews_per_day = 24)
  data.frame(catches = sum(hit), utilization = round(q$utilization, 2),
             rule = txt,
             gate = if (sum(hit) == 0) "catches nothing"
                    else if (!q$stable) "exceeds capacity" else "passes",
             stringsAsFactors = FALSE)
}))

## ---- grammar -----------------------------------------------------
rule_grammar()[c("operators", "connectives")]

## ---- vocabulary --------------------------------------------------
rule_compile("order_value > 1000", fields)$errors

## ---- limits ------------------------------------------------------
rule_grammar()$cannot_express

## ---- refusals ----------------------------------------------------
c(negation = rule_compile("not stage in shipped", fields)$errors[1],
  arithmetic = rule_compile("due_date - 2 < 0",
                            c(due_date = "numeric"))$errors[1])
