# Measuring a lifecycle the book did not generate.
#
# Hand-written, unlike scripts/, and the reason is in README.md: this example
# illustrates the package rather than a chapter, so there is no manuscript
# chunk for tools/extract.py to generate it from.
#
# Every script in scripts/ starts from simulate_lifecycles(), which produces a
# corpus whose stages are the book's own -- received, released, shipped. An
# operation being measured for real has its own stage names and its own idea
# of what an ending is, and the package reads both out of a stage_spec()
# rather than assuming them. This shows that path end to end.
#
# Run from the repository root:
#   Rscript recorded/measuring-a-recorded-log.R

library(dwellr)

set.seed(1)

## ---- the operation's own stage set ---------------------------------

# Freight consolidation: a truck for a lane on a departure date. Nothing here
# shares a stage name with the book's distribution corpus, which is the point.
#
# `fulfilling` is declared separately from `terminal` because the two are
# different questions. A load that departed has delivered its work and has
# ended; a load that was cancelled has ended and delivered nothing. Functions
# that ask whether the work was done read the first, functions that ask
# whether the lifecycle stopped read the second.
spec <- stage_spec(
  stages      = c("open", "accumulating", "tendered", "staged",
                  "departed", "cancelled"),
  transitions = c("open -> accumulating",
                  "accumulating -> tendered",
                  "tendered -> accumulating",   # a rejected tender bounces back
                  "tendered -> staged",
                  "staged -> departed",
                  "open -> cancelled",
                  "accumulating -> cancelled"),
  terminal    = c("departed", "cancelled"),
  fulfilling  = "departed",
  class       = c(open         = "queueing",
                  accumulating = "batching",
                  tendered     = "external supply",
                  staged       = "handling"))

print(spec)

## ---- the recorded log ----------------------------------------------

# Stand-in for an extract from an operation's own records. A real one arrives
# as a file; the three columns below are the whole contract that as_lifecycle()
# asks for, and the instant the data was cut.
as_of <- as.POSIXct("2026-03-01 06:00", tz = "UTC")

draw_load <- function(id, born) {
  hrs <- function(lo, hi) stats::runif(1, lo, hi)
  at <- born
  path <- data.frame(stage = "open", occurred_at = at,
                     stringsAsFactors = FALSE)
  step <- function(stage, hours) {
    at <<- at + hours * 3600
    path <<- rbind(path, data.frame(stage = stage, occurred_at = at,
                                    stringsAsFactors = FALSE))
  }
  if (stats::runif(1) < 0.08) {           # abandoned before it ever filled
    step("cancelled", hrs(2, 30))
  } else {
    step("accumulating", hrs(1, 20))
    # The dwell in a stage runs from entering it to entering the next one, so
    # the wait IN accumulating is the length of the step that leaves it. Most
    # loads fill in a day; a long tail waits on freight that has not arrived,
    # and those are the visits still open at the cut -- the ones a naive
    # average over completed visits throws away.
    step("tendered", if (stats::runif(1) < 0.3) hrs(40, 500) else hrs(2, 26))
    if (stats::runif(1) < 0.22) {         # tender rejected: back and out again
      step("accumulating", hrs(1, 9))
      step("tendered", hrs(1, 12))
    }
    step("staged", hrs(1, 7))
    step("departed", hrs(1, 5))
  }
  path$order_no <- id
  path
}

births <- as_of - stats::runif(400, 0, 24) * 24 * 3600
log <- do.call(rbind, Map(draw_load, sprintf("L%04d", seq_along(births)),
                          births))
# Events after the cut have not been observed yet, so they are not in the
# extract. Dropping them is what leaves the open visits censored.
log <- log[log$occurred_at <= as_of, c("order_no", "stage", "occurred_at")]

## ---- build the population ------------------------------------------

# The specification is required rather than defaulted. A stage set cannot be
# inferred from a log, and the wrong one fails silently: `departed` would be
# read as an ordinary wait, so every finished load would carry a long open
# visit and the stage would look like the worst in the operation.
loads <- as_lifecycle(log, spec, as_of = as_of)
print(loads)

# The same three checks stage_validate() reports on anywhere else. An
# undeclared stage is refused at the door by as_lifecycle(); an illegal move
# and an event after an ending are found here.
print(stage_validate(loads, spec))

## ---- measure it ------------------------------------------------------

d <- stage_dwell(loads, "accumulating")
cat(sprintf("\naccumulating: %d visits, %.1f%% still open at the cut\n",
            nrow(d), 100 * mean(d$censored)))

# The estimator that uses the censored visits rather than dropping them.
fit <- dwell_survival(d)
cat("\nmean dwell, naive against censoring-aware (hours):\n")
str(dwell_mean(d))
cat("\noperating quantiles (hours):\n")
print(round(dwell_quantile(d, c(0.5, 0.9, 0.95)), 2))

# A threshold read off the operation's own distribution rather than chosen.
# The fractile comes from the cost ratio -- looking at a load costs a planner
# a few minutes, missing one that strands freight costs a great deal more --
# and the hours come from the measured dwell at that fractile.
cat("\nthreshold, from the cost ratio and this stage's own dwell:\n")
str(threshold_from_dwell_quantile(d, cost_miss = 400, cost_look = 25))
# Worth reading rather than skipping: at this ratio the rule says look at
# nearly everything, because a look is cheap against a stranded load. That is
# the correct answer to the question asked, and it is also the point at which
# the threshold stops being what rations attention -- reviewer capacity does,
# which is the subject of Chapter 15 and of capacity_admit().

# `through` is not supplied: it is inferred as the stages the specification
# declares both terminal and fulfilling, which here is `departed`.
cat("\n")
print(lead_time_decompose(loads, spec = spec))

## ---- the failure this guards against ---------------------------------

# Measured against the book's stage set, which has never heard of `departed`,
# the ending becomes a wait. The row count is the tell.
wrong <- stage_dwell(loads, spec = distribution_stage_spec())
right <- stage_dwell(loads)
cat(sprintf(
  "\nvisits measured with this operation's spec: %d\n", nrow(right)))
cat(sprintf(
  "visits measured with the book's spec:       %d  (%d of them endings read as waits)\n",
  nrow(wrong), nrow(wrong) - nrow(right)))
stopifnot(!"departed" %in% right$stage, "departed" %in% wrong$stage)
