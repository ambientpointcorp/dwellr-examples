# Generated from chapters/24-worked-case-a-business-to-business-distributor.qmd by tools/extract.py -- do not edit by hand.
# Chapter: Worked Case: A Business-to-Business Distributor
#
# Run from the repository root:
#   Rscript scripts/24-worked-case-a-business-to-business-distributor.R

library(dwellr)
library(ggplot2)

source("R/theme.R")


## ---- case-spec ---------------------------------------------------
spec <- stage_spec(
  stages = c("received", "credit_hold", "awaiting_supply", "released",
             "partially_shipped", "shipped", "cancelled"),
  transitions = c(
    "received -> credit_hold", "received -> awaiting_supply",
    "received -> released", "credit_hold -> received",
    "awaiting_supply -> released", "released -> partially_shipped",
    "released -> shipped", "partially_shipped -> shipped",
    "received -> cancelled", "awaiting_supply -> cancelled"),
  terminal = c("shipped", "cancelled"),
  class = c(received = "queueing", credit_hold = "approval",
            awaiting_supply = "external supply", released = "queueing",
            partially_shipped = "queueing"))

c(stages = length(spec$stages), transitions = nrow(spec$transitions),
  terminal = length(spec$terminal))

## ---- case-admission ----------------------------------------------
fields <- c(stage = "character", dwell_hours = "numeric",
            value = "numeric", special_order = "character")

clauses <- c(
  "stage in received,credit_hold and dwell_hours > 24",
  "stage in awaiting_supply and dwell_hours > 168",
  "stage in released,partially_shipped and dwell_hours > 48")

do.call(rbind, lapply(clauses, function(txt) {
  r <- rule_compile(txt, fields)
  data.frame(compiles = r$ok, terms = nrow(r$terms),
             connective = r$connective, clause = txt,
             stringsAsFactors = FALSE)
}))

## ---- case-routing ------------------------------------------------
rules <- data.frame(
  key = c("received", "credit_hold", "awaiting_supply", "released",
          "partially_shipped"),
  action = c("release the order", "clear the credit hold",
             "chase the vendor", "chase the pick",
             "release what is picked"),
  recipient = c("branch", "finance", "purchasing", "warehouse", "warehouse"),
  urgency = c("notify", "escalate", "escalate", "notify", "notify"),
  stringsAsFactors = FALSE)

observed <- c("received", "credit_hold", "awaiting_supply", "released",
              "partially_shipped", "awaiting_supply", "received")
g <- routing_gaps(observed, rules)
c(covered = g$covered, unused_rules = length(g$unused))

## ---- case-register -----------------------------------------------
register <- data.frame(
  quantity = c("look cost", "miss cost", "awaiting_supply threshold",
               "credit_hold threshold", "review capacity",
               "seasonal term"),
  value_in_force = c("$4 direct, $12 loaded", "$180", "168 h", "24 h",
                     "9 items a day", "none"),
  basis = c("ten-minute review at a loaded rate",
            "retention estimate, one assumed churn rate",
            "90th percentile of a snapshot distribution",
            "operational judgment, not measured",
            "stated availability, not observed",
            "no history to estimate one"),
  settled_by = c("time study on a sample of reviews",
                 "expediting ledger plus a churn analysis",
                 "re-cut from 90 days of captured transitions",
                 "same, once credit holds are logged",
                 "disposition timestamps from the queue",
                 "four quarters of capture"),
  stringsAsFactors = FALSE)

register[, c("quantity", "value_in_force", "settled_by")]
