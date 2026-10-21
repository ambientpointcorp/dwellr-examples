# Generated from chapters/01-inventory-systems.qmd by tools/extract.py -- do not edit by hand.
# Chapter: Inventory Systems
#
# Run from the repository root:
#   Rscript scripts/01-inventory-systems.R

library(dwellr)
library(ggplot2)
library(R6)

source("R/theme.R")


## ---- warehouse-class ---------------------------------------------
Warehouse <- R6Class("Warehouse",
  public = list(
    stock       = NULL,  # signed level: held, less owed
    outstanding = NULL,  # ordered, not yet received

    initialize = function(opening = 0, on_order = 0) {
      self$stock <- opening
      self$outstanding <- on_order
      private$ledger <- list()
      private$post(0, "opening", opening)
    },

    # An order is placed: position rises, level does not.
    place = function(qty, period) {
      self$outstanding <- self$outstanding + qty
      private$post(period, "order placed", qty)
      invisible(self)
    },

    # Receipts land at the START of a period.
    receive = function(qty, period) {
      self$outstanding <- max(0, self$outstanding - qty)
      self$stock <- self$stock + qty
      private$post(period, "receipt", qty)
      invisible(self)
    },

    # Issues land at the END, hence period + 1.
    ship = function(qty, period) {
      self$stock <- self$stock - qty
      private$post(period + 1, "issue", -qty)
      invisible(self)
    },

    pickable = function() max(0,  self$stock),   # I+
    owed     = function() max(0, -self$stock),   # I-
    position = function() self$stock + self$outstanding,

    history = function() {
      d <- do.call(rbind, lapply(private$ledger, as.data.frame))
      rownames(d) <- NULL
      d
    }
  ),
  private = list(
    ledger = NULL,
    post = function(period, event, qty) {
      private$ledger[[length(private$ledger) + 1L]] <-
        list(period = period, event = event, qty = qty,
             level    = self$stock,
             pickable = max(0,  self$stock),
             owed     = max(0, -self$stock),
             on_order = self$outstanding,
             position = self$stock + self$outstanding)
    }
  )
)

## ---- plot-helper -------------------------------------------------
plot_inventory <- function(w, series = "level") {
  h <- w$history()
  df <- do.call(rbind, lapply(series, function(v) {
    data.frame(period = h$period, items = h[[v]], series = v)
  }))
  df$series <- factor(df$series, levels = series)
  ticks <- seq(0, max(df$period))

  ggplot(df, aes(x = period, y = items, colour = series)) +
    geom_step(direction = "hv") +
    geom_point(size = 1.4) +
    scale_x_continuous(breaks = ticks, labels = ticks + 1) +
    labs(x = "period", y = "items", colour = NULL) +
    theme(legend.position = if (length(series) > 1) "top" else "none")
}

## ---- fig-run-down ------------------------------------------------
N <- 20
demand_rate <- 5

w <- Warehouse$new(100)
for (t in 0:(N - 1)) {
  w$ship(demand_rate, t)
}
plot_inventory(w)

## ---- fig-replenished ---------------------------------------------
w <- Warehouse$new(50)
for (t in 0:(N - 1)) {
  if (t == 10) w$receive(50, t)
  w$ship(demand_rate, t)
}
plot_inventory(w)

## ---- fig-stockout ------------------------------------------------
w <- Warehouse$new(50)
for (t in 0:(N - 1)) {
  if (t == 10) w$receive(40, t)
  w$ship(demand_rate, t)
}
plot_inventory(w, series = c("pickable", "level"))

## ---- stockout-numbers --------------------------------------------
h <- w$history()
h[h$period %in% c(19, 20) & h$event == "issue",
  c("period", "event", "level", "pickable", "owed")]

## ---- fig-lean ----------------------------------------------------
w <- Warehouse$new(0)
for (t in 0:(N - 1)) {
  w$receive(demand_rate, t)
  w$ship(demand_rate, t)
}
plot_inventory(w)

## ---- lead-time-sim -----------------------------------------------
L <- 2
w <- Warehouse$new(L * demand_rate)   # prime the pipeline
arrivals <- numeric(N + L + 1)        # indexed by period + 1

for (t in 0:(N - 1)) {
  if (arrivals[t + 1] > 0) w$receive(arrivals[t + 1], t)
  if (t + L <= N - 1) {
    w$place(demand_rate, t)
    arrivals[t + L + 1] <- demand_rate
  }
  w$ship(demand_rate, t)
}

range(w$history()$level)

## ---- position-steady ---------------------------------------------
h <- w$history()
h[h$period %in% 6:7,
  c("period", "event", "level", "on_order", "position")]

## ---- lead-time-cold-start ----------------------------------------
w0 <- Warehouse$new(0)               # no priming
arrivals <- numeric(N + L + 1)
for (t in 0:(N - 1)) {
  if (arrivals[t + 1] > 0) w0$receive(arrivals[t + 1], t)
  if (t + L <= N - 1) arrivals[t + L + 1] <- demand_rate
  w0$ship(demand_rate, t)
}
range(w0$history()$level)
