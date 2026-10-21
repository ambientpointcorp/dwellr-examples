# Generated from chapters/12-baselines.qmd by tools/extract.py -- do not edit by hand.
# Chapter: Baselines
#
# Run from the repository root:
#   Rscript scripts/12-baselines.R

library(dwellr)
library(ggplot2)
library(quantreg)
library(isotree)

source("R/theme.R")


## ---- population --------------------------------------------------
sim   <- simulate_lifecycles(n = 5000, seed = 11)
orders <- sim$orders
dwell  <- stage_dwell(sim)

total <- tapply(dwell$dwell_hours, dwell$order_no, sum)
lead  <- data.frame(
  order_no    = names(total),
  stage       = "lead",
  dwell_hours = as.numeric(total),
  censored    = as.logical(tapply(dwell$censored, dwell$order_no, any)),
  stringsAsFactors = FALSE)

oo <- orders[match(lead$order_no, orders$order_no), ]
oo$special    <- ifelse(oo$special_order, "specialty", "stock")
oo$value_band <- as.character(cut(oo$value,
  quantile(orders$value, c(0, 0.5, 0.9, 1)),
  labels = c("low", "mid", "high"), include.lowest = TRUE))

## ---- global ------------------------------------------------------
global <- dwell_quantile(lead, 0.9)
round(global, 2)

## ---- heterogeneity -----------------------------------------------
by_key <- function(key) {
  lv <- sort(unique(as.character(oo[[key]])))
  do.call(rbind, lapply(lv, function(l) {
    i <- as.character(oo[[key]]) == l
    data.frame(key = key, level = l, n = sum(i),
               own_p90 = round(dwell_quantile(lead[i, ], 0.9), 2),
               exceedance = round(mean(lead$dwell_hours[i] > global), 4))
  }))
}

do.call(rbind, lapply(c("special", "order_source", "branch", "value_band"),
                      by_key))

## ---- fig-cohorts -------------------------------------------------
curves <- do.call(rbind, lapply(c("stock", "specialty"), function(l) {
  f <- dwell_survival(lead[oo$special == l, ])
  data.frame(cohort = l, time = f$time, surv = f$surv)
}))

ggplot(curves, aes(time, surv, colour = cohort)) +
  geom_step(linewidth = 0.5) +
  geom_vline(xintercept = global, linetype = 2, colour = "grey45") +
  scale_x_log10() +
  scale_colour_manual(values = c(stock = "grey60", specialty = "grey20")) +
  labs(x = "hours of total lead time", y = "share not yet finished",
       colour = NULL) +
  theme(legend.position = "bottom")

## ---- granularity -------------------------------------------------
set.seed(4)
fit <- sample(c(TRUE, FALSE), nrow(lead), replace = TRUE, prob = c(0.6, 0.4))

schemes <- list(
  "special"                   = "special",
  "special + source"          = c("special", "order_source"),
  "special + source + branch" = c("special", "order_source", "branch"),
  "all four keys"             = c("special", "order_source", "branch",
                                  "value_band"))

pooled_thr <- dwell_quantile(lead[fit, ], 0.9)
h <- lead[!fit, ]
pooled_row <- data.frame(
  scheme = "pooled", cells = 1L, fallbacks = 0L,
  median_cell_n = NA_integer_,
  overall = round(mean(h$dwell_hours[!h$censored |
                       h$dwell_hours > pooled_thr] > pooled_thr), 4),
  worst_cohort_error = NA_real_)

rows <- lapply(names(schemes), function(nm) {
  k <- schemes[[nm]]
  b <- baseline_cohort(lead[fit, ], oo[fit, k, drop = FALSE], p = 0.9,
                       min_n = 50)
  e <- baseline_exceedance(lead[!fit, ], oo[!fit, k, drop = FALSE], b)
  data.frame(scheme = nm, cells = nrow(b$cells),
             fallbacks = sum(b$cells$fell_back),
             median_cell_n = as.integer(median(b$cells$n)),
             overall = round(e$overall, 4),
             worst_cohort_error = round(e$worst_cohort_error, 4))
})
rbind(pooled_row, do.call(rbind, rows))

## ---- key-ranking -------------------------------------------------
do.call(rbind, lapply(c("special", "order_source", "branch", "value_band"),
  function(k) {
    r <- segment_decision_shift(lead, oo[[k]], cost_miss = 40,
                                cost_look = 400, min_n = 50)
    data.frame(key = k,
               spread_hours = round(max(r$per_level$hours, na.rm = TRUE) -
                                    min(r$per_level$hours, na.rm = TRUE), 2),
               reclassified = r$n_changed,
               share = round(r$share_changed, 4))
  }))

## ---- seasonality -------------------------------------------------
weekday <- as.integer(format(oo$entered_at, "%u"))
week    <- as.integer(format(oo$entered_at, "%V"))

c(weekday_p = signif(kruskal.test(lead$dwell_hours ~ factor(weekday))$p.value, 3),
  week_p    = signif(kruskal.test(lead$dwell_hours ~ factor(week))$p.value, 3))

## ---- censoring-gradient ------------------------------------------
round(tapply(lead$censored, week, mean), 2)

## ---- censoring-artifact ------------------------------------------
done  <- !lead$censored
early <- week <= sort(unique(week))[length(unique(week)) - 3]

c(completed_only_p = signif(
    kruskal.test(lead$dwell_hours[done] ~ factor(week[done]))$p.value, 3),
  excluding_last_3_weeks_p = signif(
    kruskal.test(lead$dwell_hours[early] ~ factor(week[early]))$p.value, 3))

## ---- cold-start --------------------------------------------------
set.seed(8)
hold <- sample(c(TRUE, FALSE), nrow(lead), replace = TRUE, prob = c(0.3, 0.7))
pool <- dwell_quantile(lead[!hold, ], 0.9)

do.call(rbind, lapply(c("stock", "specialty"), function(l) {
  hi <- hold & oo$special == l
  pooled_err <- abs(mean(lead$dwell_hours[hi] > pool) - 0.1)
  ix <- which(!hold & oo$special == l)
  do.call(rbind, lapply(c(25, 50, 100, 200, 300), function(nn) {
    e <- replicate(40, {
      s <- sample(ix, nn)
      thr <- dwell_quantile(lead[s, ], 0.9)
      abs(mean(lead$dwell_hours[hi] > thr) - 0.1)
    })
    data.frame(cohort = l, cohort_n = nn,
               own_error = round(mean(e, na.rm = TRUE), 4),
               pooled_error = round(pooled_err, 4))
  }))
}))

## ---- stability ---------------------------------------------------
baseline_stability(lead, oo$entered_at, widths = c(7, 14, 30, 60), p = 0.9,
                   step = 7)

## ---- responsiveness ----------------------------------------------
day <- as.numeric(difftime(oo$entered_at, min(oo$entered_at), units = "days"))
shift_day <- 60
shifted <- lead
shifted$dwell_hours <- ifelse(day > shift_day, lead$dwell_hours * 1.40,
                              lead$dwell_hours)

before <- dwell_quantile(shifted[day <= shift_day, ], 0.9)
after  <- dwell_quantile(shifted[day > shift_day, ], 0.9)
halfway <- (before + after) / 2

lag_days <- function(w) {
  for (e in seq(shift_day, max(day), by = 1)) {
    i <- day > e - w & day <= e
    if (sum(i) < 20) next
    q <- dwell_quantile(shifted[i, ], 0.9)
    if (!is.na(q) && q >= halfway) return(e - shift_day)
  }
  NA_integer_
}

c(before = round(before, 2), after = round(after, 2),
  halfway = round(halfway, 2))

data.frame(width_days = c(7, 14, 30, 60),
           days_to_detect = vapply(c(7, 14, 30, 60), lag_days, numeric(1)))

## ---- fig-window --------------------------------------------------
st <- baseline_stability(lead, oo$entered_at, widths = c(7, 14, 30, 60),
                         p = 0.9, step = 7)
st$days_to_detect <- vapply(st$width_days, lag_days, numeric(1))

ggplot(st, aes(days_to_detect, cv)) +
  geom_path(colour = "grey70") +
  geom_point(size = 2.4, colour = "grey20") +
  geom_text(aes(label = paste0(width_days, "d")), vjust = -0.9, size = 3) +
  expand_limits(y = 0, x = c(5, 33)) +
  labs(x = "days to detect a 40% step change",
       y = "coefficient of variation of the estimate")

## ---- modeled -----------------------------------------------------
lead$log_h <- log(pmax(lead$dwell_hours, 0.01))
md <- data.frame(
  log_h     = lead$log_h,
  special   = oo$special_order,
  source    = factor(oo$order_source),
  branch    = factor(oo$branch),
  lines     = oo$lines,
  log_value = log(oo$value))

set.seed(6)
mfit <- sample(c(TRUE, FALSE), nrow(md), replace = TRUE, prob = c(0.6, 0.4))
q90 <- rq(log_h ~ special + source + branch + lines + log_value, tau = 0.9,
          data = md[mfit, ])
predicted <- exp(predict(q90, md[!mfit, ]))
hold <- lead[!mfit, ]

c(overall = round(mean(hold$dwell_hours > predicted), 4), target = 0.10)

## ---- modeled-by-cohort -------------------------------------------
do.call(rbind, lapply(c("stock", "specialty"), function(l) {
  i <- oo$special[!mfit] == l
  data.frame(cohort = l, n = sum(i),
             exceedance = round(mean(hold$dwell_hours[i] > predicted[i]), 4))
}))

## ---- anomaly -----------------------------------------------------
feat <- data.frame(
  lines     = oo$lines,
  log_value = log(oo$value),
  special   = as.integer(oo$special_order),
  loops     = oo$loops,
  hour      = as.integer(format(oo$entered_at, "%H")))

set.seed(5)
iso   <- isolation.forest(feat, ntrees = 200, nthreads = 1)
score <- predict(iso, feat)
late  <- lead$dwell_hours > global

auc <- function(p, y) {
  r <- rank(p); n1 <- sum(y); n0 <- length(y) - n1
  (sum(r[y == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0)
}
round(auc(score, as.integer(late)), 4)

## ---- contamination -----------------------------------------------
do.call(rbind, lapply(c(0.01, 0.05, 0.10), function(cr) {
  k <- round(cr * length(score))
  idx <- order(score, decreasing = TRUE)[1:k]
  data.frame(contamination = cr, flagged = k,
             share_late = round(mean(late[idx]), 3))
}))

## ---- sigma -------------------------------------------------------
m <- mean(lead$dwell_hours)
s <- sd(lead$dwell_hours)

c(mean = round(m, 2), sd = round(s, 2),
  skewness = round(mean((lead$dwell_hours - m)^3) / s^3, 2),
  mean_minus_2sd = round(m - 2 * s, 2),
  mean_plus_2sd = round(m + 2 * s, 2),
  percentile_of_mean_plus_2sd = round(100 * mean(lead$dwell_hours <= m + 2 * s), 1))

## ---- sigma-by-cohort ---------------------------------------------
do.call(rbind, lapply(c("stock", "specialty"), function(l) {
  x <- lead$dwell_hours[oo$special == l]
  data.frame(cohort = l,
             own_p90 = round(dwell_quantile(lead[oo$special == l, ], 0.9), 2),
             sd_above_mean = round((dwell_quantile(lead[oo$special == l, ], 0.9) -
                                    mean(x)) / sd(x), 2))
}))

## ---- drift-setup -------------------------------------------------
v <- tapply(lead$dwell_hours, oo$special, mean)
w <- as.numeric(table(oo$special) / nrow(oo))
names(w) <- names(table(oo$special))

rbind(mean_hours = round(v, 2), share = round(w, 4))

## ---- drift -------------------------------------------------------
mix_only     <- baseline_drift_decompose(v, v, w,
                                         c(specialty = 0.20, stock = 0.80))
process_only <- baseline_drift_decompose(v, v * 1.25, w, w)
both         <- baseline_drift_decompose(v, v * 1.25, w,
                                         c(specialty = 0.20, stock = 0.80))

do.call(rbind, lapply(list(mix_only = mix_only, process_only = process_only,
                           both = both), function(r)
  data.frame(base = round(r$base, 2), now = round(r$now, 2),
             change = round(r$change, 2), process = round(r$process, 2),
             mix = round(r$mix, 2))))

## ---- renormalization ---------------------------------------------
creep <- lead
creep$dwell_hours <- lead$dwell_hours * (1 + 0.004 * day)   # 0.4% a day

rolling <- do.call(rbind, lapply(seq(40, max(day), by = 10), function(e) {
  trail <- day > e - 30 & day <= e
  now   <- day > e & day <= e + 10
  if (sum(trail) < 50 || sum(now) < 20) return(NULL)
  thr <- dwell_quantile(creep[trail, ], 0.9)
  data.frame(day = e, baseline_p90 = round(thr, 1),
             exceedance_next_10d = round(mean(creep$dwell_hours[now] > thr), 3))
}))
rolling
