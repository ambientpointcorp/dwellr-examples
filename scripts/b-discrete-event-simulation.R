# Generated from chapters/b-discrete-event-simulation.qmd by tools/extract.py -- do not edit by hand.
# Chapter: Discrete-event simulation
#
# Run from the repository root:
#   Rscript scripts/b-discrete-event-simulation.R

library(dwellr)
library(ggplot2)

source("R/theme.R")


## ---- engine ------------------------------------------------------
simulate_events <- function(handlers, initial, until, seed = 1,
                            capacity = 64L) {
  set.seed(seed)
  at <- numeric(capacity); kind <- character(capacity)
  live <- logical(capacity); n_slots <- capacity

  state <- initial
  entered <- numeric(1e6); left <- numeric(1e6); k <- 0L

  schedule <- function(when, what) {
    if (when > until) return(invisible(NULL))
    i <- which(!live)[1]
    if (is.na(i)) {                       # grow once, geometrically
      at <<- c(at, numeric(n_slots)); kind <<- c(kind, character(n_slots))
      live <<- c(live, logical(n_slots))
      i <- n_slots + 1L; n_slots <<- 2L * n_slots
    }
    at[i] <<- when; kind[i] <<- what; live[i] <<- TRUE
    invisible(NULL)
  }
  state$schedule <- schedule
  for (nm in names(state$seed_events)) schedule(state$seed_events[[nm]], nm)

  while (any(live)) {
    idx <- which(live)
    i <- idx[which.min(at[idx])]          # the earliest scheduled event
    live[i] <- FALSE
    state$now <- at[i]
    out <- handlers[[kind[i]]](state, NULL)
    state <- out$state
    if (!is.null(out$record)) {
      k <- k + 1L
      entered[k] <- out$record[1]; left[k] <- out$record[2]
    }
  }
  list(state = state,
       log = if (k) data.frame(entered = entered[seq_len(k)],
                               left = left[seq_len(k)]) else NULL)
}

## ---- mm1 ---------------------------------------------------------
mm1 <- function(lambda, service, until, seed = 1) {
  arrive <- function(state, ev) {
    state$schedule(state$now + rexp(1, lambda), "arrive")
    state$queue <- c(state$queue, state$now)
    if (!state$busy) {
      state$busy <- TRUE
      state$schedule(state$now + service(), "finish")
    }
    list(state = state, record = NULL)
  }
  finish <- function(state, ev) {
    started <- state$queue[1]
    state$queue <- state$queue[-1]
    if (length(state$queue))
      state$schedule(state$now + service(), "finish")
    else state$busy <- FALSE
    list(state = state, record = c(started, state$now))
  }
  simulate_events(list(arrive = arrive, finish = finish),
                  initial = list(queue = numeric(0), busy = FALSE,
                                 seed_events = list(arrive = 0)),
                  until = until, seed = seed)
}

run  <- mm1(lambda = 0.8, service = function() rexp(1, 1), until = 50000)
done <- run$log
done <- done[done$entered > 2000, ]          # discard the warm-up

c(served = nrow(done),
  mean_time_simulated = round(mean(done$left - done$entered), 4),
  mean_time_analytic = round(1 / (1.0 - 0.8), 4))

## ---- littles-law -------------------------------------------------
c(little = round(0.8 * mean(done$left - done$entered), 4),
  analytic = round(0.8 / (1 - 0.8), 4))

## ---- variable-service --------------------------------------------
mean_time <- function(service, until = 30000) {
  r <- mm1(0.8, service, until = until)
  d <- r$log
  d <- d[d$entered > 2000, ]
  mean(d$left - d$entered)
}

s2 <- log(1 + 4)                             # a lognormal with cv = 2
pk <- function(cv2) 1 + 0.8 / (1 - 0.8) * (1 + cv2) / 2

do.call(rbind, lapply(
  list(list("exponential", function() rexp(1, 1), 1),
       list("deterministic", function() 1, 0),
       list("lognormal, cv 2", function() rlnorm(1, -0.5 * s2, sqrt(s2)), 4)),
  function(x) data.frame(service = x[[1]],
                         simulated = round(mean_time(x[[2]]), 3),
                         pollaczek_khinchine = pk(x[[3]]))))
