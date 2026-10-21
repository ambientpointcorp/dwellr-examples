# Generated from chapters/09-machine-learning-for-lifecycle-data.qmd by tools/extract.py -- do not edit by hand.
# Chapter: Machine Learning for Lifecycle Data
#
# Run from the repository root:
#   Rscript scripts/09-machine-learning-for-lifecycle-data.R

library(dwellr)
library(ggplot2)
library(tidymodels)
library(censored)
library(probably)
library(vetiver)
library(pins)
library(isotree)
library(survival)
library(ranger)
library(xgboost)
library(quantreg)
library(pROC)
library(dbscan)

source("R/theme.R")


## ---- corpus ------------------------------------------------------
sim <- simulate_lifecycles(n = 20000, seed = 11)
dwell <- stage_dwell(sim)
sim

## ---- modeling-frame ----------------------------------------------
o <- sim$orders
total <- tapply(dwell$dwell_hours, dwell$order_no, sum)

md <- data.frame(
  order_no = o$order_no, entered = o$entered_at,
  branch   = factor(o$branch),
  channel  = factor(o$order_source),
  lines    = o$lines,
  log_value = log(o$value),
  special  = as.integer(o$special_order),
  hour     = as.integer(format(o$entered_at, "%H")),
  dow      = as.integer(format(o$entered_at, "%u")),
  stringsAsFactors = FALSE)

# as.numeric() because tapply returns a one-dimensional array, which some
# modeling functions accept and others reject (Section 9.21).
md$total    <- as.numeric(total[match(md$order_no, names(total))])
md$shipped  <- o$stage == "shipped"
md$finished <- o$is_terminal

## ---- label -------------------------------------------------------
H <- 24
elapsed <- as.numeric(difftime(sim$as_of, md$entered, units = "hours"))

md$breach <- NA
md$breach[md$shipped] <- md$total[md$shipped] > H
md$breach[!md$finished & elapsed > H] <- TRUE
md <- md[!is.na(md$breach), ]

# A factor with the event first is what the tidymodels metrics and the
# classification models expect; the logical stays for the base-R calls.
md$outcome <- factor(md$breach, levels = c(TRUE, FALSE),
                     labels = c("breach", "ok"))

c(orders = nrow(o), labelled = nrow(md),
  share = round(nrow(md) / nrow(o), 3),
  breach_rate = round(mean(md$breach), 3))

## ---- split -------------------------------------------------------
# Temporal split, for the reason Section 9.12 gives. initial_time_split()
# takes the first `prop` of the rows as training, so the frame has to be in
# time order before it is called, which is the one way to misuse it.
md <- md[order(md$entered), ]
split <- initial_time_split(md, prop = 0.70)
train <- training(split)
test  <- testing(split)
boundary <- as.numeric(max(train$entered))

features <- c("branch", "channel", "lines", "log_value", "special",
              "hour", "dow")
auc_of <- function(y, p) as.numeric(suppressMessages(pROC::auc(y, p, quiet = TRUE)))
c(train = nrow(train), test = nrow(test),
  train_rate = round(mean(train$breach), 3),
  test_rate = round(mean(test$breach), 3))

## ---- promotion ---------------------------------------------------
rule <- auc_of(test$breach, test$special)

logit <- glm(breach ~ ., data = train[c("breach", features)],
             family = binomial)
logit_auc <- auc_of(test$breach, predict(logit, test, type = "response"))

forest <- ranger(as.factor(breach) ~ ., data = train[c("breach", features)],
                 probability = TRUE, num.trees = 300, seed = 1)
p_forest <- predict(forest, test)$predictions[, "TRUE"]
forest_auc <- auc_of(test$breach, p_forest)

data.frame(
  candidate = c("rule on `special` alone", "logistic, all features",
                "random forest, all features"),
  auc = round(c(rule, logit_auc, forest_auc), 4),
  gain_over_rule = round(c(0, logit_auc - rule, forest_auc - rule), 4))

## ---- censored-target ---------------------------------------------
# The duration task has its own usable population: every order with an
# observed or censored time, not only those with a resolved breach label.
duration_frame <- function(s) {
  oo <- s$orders
  dd <- stage_dwell(s)
  tt <- tapply(dd$dwell_hours, dd$order_no, sum)
  f <- data.frame(branch = factor(oo$branch),
                  channel = factor(oo$order_source),
                  lines = oo$lines, log_value = log(oo$value),
                  special = as.integer(oo$special_order))
  f$observed <- as.numeric(tt[match(oo$order_no, names(tt))])
  f$event <- oo$stage == "shipped"
  open_now <- !oo$is_terminal
  f$observed[open_now] <- as.numeric(difftime(s$as_of,
                                      oo$entered_at[open_now], units = "hours"))
  f$event[open_now] <- FALSE
  f[!is.na(f$observed) & f$observed > 0, ]
}

compare_targets <- function(s, label) {
  f <- duration_frame(s)
  naive <- lm(log(observed) ~ branch + channel + lines + log_value + special,
              data = f[f$event, ])
  aft <- survreg(Surv(observed, event) ~ branch + channel + lines +
                   log_value + special, data = f, dist = "lognormal")
  data.frame(window = label, instances = nrow(f),
             censored = round(mean(!f$event), 3),
             naive_hours = round(exp(coef(naive)[[1]]), 1),
             survival_hours = round(exp(coef(aft)[[1]]), 1),
             understatement = sprintf("%.1f%%",
               100 * (1 - exp(coef(naive)[[1]]) / exp(coef(aft)[[1]]))))
}

rbind(compare_targets(sim, "full window"),
      compare_targets(observe_at(sim, sim$as_of - 90 * 24 * 3600),
                      "observed 90 days earlier"))

## ---- aft-carried -------------------------------------------------
dur <- duration_frame(sim)
aft <- survreg(Surv(observed, event) ~ branch + channel + lines + log_value +
                 special, data = dur, dist = "lognormal")
c(instances = nrow(dur), censored = round(mean(!dur$event), 3))

## ---- feature-signal ----------------------------------------------
breach_by <- function(v, lab) data.frame(
  feature = lab,
  group = names(tapply(md$breach, v, mean)),
  breach_rate = round(as.numeric(tapply(md$breach, v, mean)), 3))

rbind(breach_by(md$special, "special"),
      breach_by(md$branch, "branch"),
      breach_by(md$channel, "channel"),
      breach_by(cut(md$lines, c(0, 1, 2, 3, 100)), "lines"))

## ---- leakage -----------------------------------------------------
rec <- dwell[dwell$stage == "received", ]
md$received_hours <- rec$dwell_hours[match(md$order_no, rec$order_no)]
train$received_hours <- md$received_hours[match(train$order_no, md$order_no)]
test$received_hours  <- md$received_hours[match(test$order_no,  md$order_no)]

leaky <- ranger(as.factor(breach) ~ .,
                data = train[c("breach", features, "received_hours")],
                probability = TRUE, num.trees = 300, seed = 1)
leaky_auc <- auc_of(test$breach, predict(leaky, test)$predictions[, "TRUE"])

c(honest_auc = round(forest_auc, 4),
  with_leaked_feature = round(leaky_auc, 4),
  inflation = round(leaky_auc - forest_auc, 4),
  correlation_with_target = round(cor(md$received_hours, md$total,
                                      use = "complete.obs"), 3))

## ---- survival-families -------------------------------------------
cox <- coxph(Surv(observed, event) ~ branch + channel + lines + log_value +
               special, data = dur)
c(cox_concordance = round(summary(cox)$concordance[[1]], 4),
  aft_log_lik = round(as.numeric(aft$loglik[2]), 1))

## ---- discrete-hazard ---------------------------------------------
breaks <- c(0, 6, 12, 24, 48, 96)
person_period <- do.call(rbind, lapply(seq_len(length(breaks) - 1), function(k) {
  at_risk <- dur$observed > breaks[k]
  data.frame(period = k,
             left = dur$event[at_risk] & dur$observed[at_risk] <= breaks[k + 1],
             branch = dur$branch[at_risk], channel = dur$channel[at_risk],
             lines = dur$lines[at_risk], log_value = dur$log_value[at_risk],
             special = dur$special[at_risk])
}))

haz <- glm(left ~ factor(period) + branch + channel + lines + log_value +
             special, data = person_period, family = binomial)
c(rows = nrow(person_period), instances = nrow(dur),
  expansion = round(nrow(person_period) / nrow(dur), 2))
round(plogis(coef(haz)[[1]] + c(0, coef(haz)[2:5])), 3)

## ---- quantile-model ----------------------------------------------
done <- dur[dur$event, ]
q50 <- rq(log(observed) ~ branch + channel + lines + log_value + special,
          data = done, tau = 0.5)
q90 <- rq(log(observed) ~ branch + channel + lines + log_value + special,
          data = done, tau = 0.9)

c(special_on_median = round(exp(coef(q50)[["special"]]), 2),
  special_on_p90    = round(exp(coef(q90)[["special"]]), 2),
  mean_fitted_median = round(mean(exp(fitted(q50))), 1),
  mean_fitted_p90    = round(mean(exp(fitted(q90))), 1),
  coverage_of_p90    = round(mean(done$observed <= exp(fitted(q90))), 3))

## ---- monotone ----------------------------------------------------
X <- model.matrix(~ branch + channel + lines + log_value + special + hour +
                    dow - 1, train)
X_test <- model.matrix(~ branch + channel + lines + log_value + special +
                         hour + dow - 1, test)
dtrain <- xgb.DMatrix(X, label = as.numeric(train$breach))
dtest  <- xgb.DMatrix(X_test)

fit_xgb <- function(mono = NULL) {
  pars <- list(objective = "binary:logistic", max_depth = 4, eta = 0.1,
               nthread = 2)
  if (!is.null(mono))
    pars$monotone_constraints <- paste0("(", paste(mono, collapse = ","), ")")
  xgb.train(params = pars, data = dtrain, nrounds = 120, verbose = 0)
}

mono <- rep(0, ncol(X))
mono[colnames(X) == "lines"] <- 1

data.frame(
  model = c("unconstrained", "lines constrained upward"),
  auc = round(c(auc_of(test$breach, predict(fit_xgb(), dtest)),
                auc_of(test$breach, predict(fit_xgb(mono), dtest))), 4))

## ---- unsupervised ------------------------------------------------
cols <- c("lines", "log_value", "hour", "dow")

# nthreads = 1 because this build of isotree has no OpenMP support; the
# default emits a warning and falls back to one thread anyway.
forest_scores <- predict(
  isolation.forest(train[cols], ntrees = 120, nthreads = 1), test[cols])
lof_scores <- dbscan::lof(scale(test[cols]), minPts = 20)

c(isolation_forest_auc = round(auc_of(test$breach, forest_scores), 4),
  local_outlier_factor_auc = round(auc_of(test$breach, lof_scores), 4),
  flagged_at_1pct = sum(forest_scores >= quantile(forest_scores, 0.99)),
  flagged_at_5pct = sum(forest_scores >= quantile(forest_scores, 0.95)),
  flagged_at_10pct = sum(forest_scores >= quantile(forest_scores, 0.90)))

## ---- ranking -----------------------------------------------------
precision_at <- function(y, p, k) {
  ord <- order(-p)
  mean(y[ord][seq_len(k)])
}
data.frame(
  reviews = c(50, 200, 1000),
  forest = round(sapply(c(50, 200, 1000),
                        function(k) precision_at(test$breach, p_forest, k)), 3),
  rule = round(sapply(c(50, 200, 1000),
                      function(k) precision_at(test$breach, test$special, k)), 3),
  base_rate = round(mean(test$breach), 3))

## ---- split-comparison --------------------------------------------
set.seed(7)
idx <- sample(nrow(md), round(0.70 * nrow(md)))
r_train <- md[idx, ]; r_test <- md[-idx, ]
r_forest <- ranger(as.factor(breach) ~ ., data = r_train[c("breach", features)],
                   probability = TRUE, num.trees = 300, seed = 1)

c(random_split = round(auc_of(r_test$breach,
    predict(r_forest, r_test)$predictions[, "TRUE"]), 4),
  temporal_split = round(forest_auc, 4))

## ---- drift-split -------------------------------------------------
drifted <- md
frac <- (as.numeric(drifted$entered) - min(as.numeric(drifted$entered))) /
          diff(range(as.numeric(drifted$entered)))
set.seed(11)
drifted$breach <- as.logical(pmin(1, drifted$breach +
                                    (runif(nrow(drifted)) < 0.35 * frac)))

d_tr <- drifted[as.numeric(drifted$entered) <= boundary, ]
d_te <- drifted[as.numeric(drifted$entered) >  boundary, ]
set.seed(7); j <- sample(nrow(drifted), round(0.70 * nrow(drifted)))

m_temporal <- ranger(as.factor(breach) ~ ., data = d_tr[c("breach", features)],
                     probability = TRUE, num.trees = 300, seed = 1)
m_random <- ranger(as.factor(breach) ~ .,
                   data = drifted[j, c("breach", features)],
                   probability = TRUE, num.trees = 300, seed = 1)

c(breach_rate_early = round(mean(d_tr$breach), 3),
  breach_rate_late = round(mean(d_te$breach), 3),
  random_split = round(auc_of(drifted[-j, ]$breach,
    predict(m_random, drifted[-j, ])$predictions[, "TRUE"]), 4),
  temporal_split = round(auc_of(d_te$breach,
    predict(m_temporal, d_te)$predictions[, "TRUE"]), 4))

## ---- resampling --------------------------------------------------
# Rolling origin over the training period, assessing one week at a time.
origins <- sliding_period(train, index = entered, period = "week",
                          lookback = Inf, assess_stop = 1)

# Group-aware: the person-period expansion of Section 9.7, split so
# that no order appears on both sides of the boundary.
pp_frame <- do.call(rbind, lapply(1:3, function(k) transform(md, period = k)))
grouped <- group_initial_split(pp_frame, group = order_no, prop = 0.70)

# Nested: an inner loop for tuning inside each outer temporal fold.
nested <- nested_cv(train,
                    outside = rolling_origin(initial = 8000, assess = 1500,
                                             skip = 1500, cumulative = TRUE),
                    inside = vfold_cv(v = 3))

c(rolling_origin_resamples = nrow(origins),
  first_assessment = nrow(assessment(origins$splits[[1]])),
  person_period_rows = nrow(pp_frame),
  orders_on_both_sides = length(intersect(training(grouped)$order_no,
                                          testing(grouped)$order_no)),
  outer_folds = nrow(nested),
  inner_per_outer = nrow(nested$inner_resamples[[1]]))

## ---- rare-metrics ------------------------------------------------
# A rarer event, built from the same labelled frame: shipped, and slower than
# five days. An order still open is excluded, since its duration is unknown
# and 120 hours has not yet elapsed for most of them.
md$rare <- md$shipped & md$total > 120
rare_tr <- md[as.numeric(md$entered) <= boundary, ]
rare_te <- md[as.numeric(md$entered) >  boundary, ]

rare_rf <- ranger(as.factor(rare) ~ branch + channel + lines + log_value +
                    special, data = rare_tr, probability = TRUE,
                  num.trees = 300, seed = 1)
p_rare <- predict(rare_rf, rare_te)$predictions[, "TRUE"]

ord <- order(-p_rare)
y_ord <- rare_te$rare[ord]
prec <- cumsum(y_ord) / seq_along(y_ord)
rec <- cumsum(y_ord) / sum(y_ord)

c(base_rate = round(mean(rare_te$rare), 4),
  roc_auc = round(auc_of(rare_te$rare, p_rare), 4),
  pr_auc = round(sum(diff(c(0, rec)) * prec), 4),
  precision_at_50 = round(mean(y_ord[seq_len(50)]), 3),
  precision_at_200 = round(mean(y_ord[seq_len(200)]), 3))

## ---- yardstick-metrics -------------------------------------------
scored <- data.frame(truth = test$outcome, .pred_breach = p_forest)
scored$.pred_ok <- 1 - scored$.pred_breach

metric_set(roc_auc, pr_auc, brier_class)(
  scored, truth = truth, .pred_breach, event_level = "first")

## ---- survival-metrics --------------------------------------------
surv_frame <- dur
surv_frame$surv <- Surv(surv_frame$observed, surv_frame$event)
set.seed(9)
s_split <- initial_split(surv_frame, prop = 0.70)

cox_fit <- proportional_hazards() |>
  set_engine("survival") |> set_mode("censored regression") |>
  fit(surv ~ branch + channel + lines + log_value + special,
      data = training(s_split))

# Evaluated on 500 of the held-back rows. A survival Brier score builds a
# predicted curve per row, which costs about twenty-three seconds at this size
# and exhausts memory on the full six thousand, so the subsample is stated
# rather than hidden.
set.seed(9)
eval_rows <- testing(s_split)[sample(nrow(testing(s_split)), 500), ]
horizons <- c(6, 12, 24, 48, 96)
surv_pred <- augment(cox_fit, eval_rows, eval_time = horizons)

list(
  evaluated_on = nrow(eval_rows),
  concordance = round(concordance_survival(surv_pred, truth = surv,
                                           estimate = .pred_time)$.estimate, 4),
  integrated_brier = round(brier_survival_integrated(surv_pred, truth = surv,
                                                     .pred)$.estimate, 4),
  brier_by_horizon = round(brier_survival(surv_pred, truth = surv,
                                          .pred)$.estimate, 4))

## ---- calibration-check -------------------------------------------
bins <- cut(p_forest, quantile(p_forest, seq(0, 1, 0.2)), include.lowest = TRUE)
cal <- data.frame(
  bin = levels(bins),
  predicted = round(as.numeric(tapply(p_forest, bins, mean)), 3),
  observed = round(as.numeric(tapply(as.numeric(test$breach), bins, mean)), 3),
  n = as.integer(table(bins)))
cal

## ---- calibration-break -------------------------------------------
# Fit on part of the training period and calibrate on the rest, so the repair
# never sees the evaluation period. Calibrating on the test set would report a
# flattering figure for the same reason Section 9.12 gives.
inner_split <- initial_time_split(train, prop = 0.80)
fit_part <- training(inner_split)
cal_part <- testing(inner_split)

design <- function(df) model.matrix(~ branch + channel + lines + log_value +
                                      special + hour + dow - 1, df)
weighted_fit <- xgb.train(
  params = list(objective = "binary:logistic", max_depth = 4, eta = 0.1,
                scale_pos_weight = 8, nthread = 2),
  data = xgb.DMatrix(design(fit_part), label = as.numeric(fit_part$breach)),
  nrounds = 120, verbose = 0)

as_pred <- function(df) {
  p <- predict(weighted_fit, xgb.DMatrix(design(df)))
  data.frame(outcome = df$outcome, .pred_breach = p, .pred_ok = 1 - p)
}
cal_pred <- as_pred(cal_part)
test_pred <- as_pred(test)

isotonic <- cal_estimate_isotonic(cal_pred, truth = outcome,
                                  estimate = starts_with(".pred_"))
platt <- cal_estimate_logistic(cal_pred, truth = outcome,
                               estimate = starts_with(".pred_"), smooth = FALSE)

repaired_iso <- cal_apply(test_pred, isotonic)$.pred_breach
repaired_platt <- cal_apply(test_pred, platt)$.pred_breach

data.frame(
  scaling = c("weighted 8:1, raw", "isotonic repair", "Platt repair"),
  mean_predicted = round(c(mean(test_pred$.pred_breach),
                           mean(repaired_iso), mean(repaired_platt)), 3),
  observed = round(mean(test$breach), 3),
  auc = round(c(auc_of(test$breach, test_pred$.pred_breach),
                auc_of(test$breach, repaired_iso),
                auc_of(test$breach, repaired_platt)), 4))

## ---- permutation-importance --------------------------------------
baseline <- auc_of(rare_te$rare, p_rare)
imp <- vapply(c("branch", "channel", "lines", "log_value", "special"),
              function(v) {
                shuffled <- rare_te
                set.seed(2)
                shuffled[[v]] <- sample(shuffled[[v]])
                baseline - auc_of(rare_te$rare,
                  predict(rare_rf, shuffled)$predictions[, "TRUE"])
              }, numeric(1))
round(sort(imp, decreasing = TRUE), 4)

## ---- vetiver-model -----------------------------------------------
scoring_recipe <- recipe(outcome ~ branch + channel + lines + log_value +
                           special + hour + dow, data = train) |>
  step_dummy(all_nominal_predictors()) |>
  step_zv(all_predictors())

scoring_wf <- workflow() |>
  add_recipe(scoring_recipe) |>
  add_model(rand_forest(trees = 300) |> set_engine("ranger") |>
              set_mode("classification"))

fitted_wf <- fit(scoring_wf, train)

deployable <- vetiver_model(
  fitted_wf, "breach-24h",
  metadata = list(horizon_hours = 24,
                  training_window = format(range(train$entered), "%Y-%m-%d")))
deployable

## ---- vetiver-prototype -------------------------------------------
# The prototype is a schema contract, and it is what catches the skew.
deployable$prototype

missing_column <- test[, setdiff(names(test), "lines")]
inherits(try(predict(deployable, missing_column), silent = TRUE), "try-error")

## ---- drift-test --------------------------------------------------
early <- md$lines[as.numeric(md$entered) <= boundary]
late  <- md$lines[as.numeric(md$entered) >  boundary]
shifted <- late + rbinom(length(late), 2, 0.3)

rbind(
  observed = c(statistic = round(suppressWarnings(ks.test(early, late))$statistic, 4),
               p = round(suppressWarnings(ks.test(early, late))$p.value, 3)),
  with_injected_shift = c(
    statistic = round(suppressWarnings(ks.test(early, shifted))$statistic, 4),
    p = signif(suppressWarnings(ks.test(early, shifted))$p.value, 3)))

## ---- vetiver-metrics ---------------------------------------------
monitored <- data.frame(
  entered = test$entered, truth = test$outcome,
  estimate = factor(p_forest > 0.5, levels = c(TRUE, FALSE),
                    labels = c("breach", "ok")))

by_week <- vetiver_compute_metrics(
  monitored, entered, "week", truth, estimate,
  metric_set = metric_set(accuracy, precision, recall))
head(as.data.frame(by_week), 6)

## ---- vetiver-pin -------------------------------------------------
board <- board_temp(versioned = TRUE)
vetiver_pin_write(board, deployable)

# A second fit, on a shorter window, written to the same name.
shorter <- vetiver_model(
  fit(scoring_wf, train[seq_len(6000), ]), "breach-24h",
  metadata = list(horizon_hours = 24, training_window = "first 6000 orders"))
vetiver_pin_write(board, shorter)

recorded <- pin_meta(board, "breach-24h")
list(versions = nrow(pin_versions(board, "breach-24h")),
     recorded_fields = names(recorded$user),
     horizon = recorded$user$horizon_hours,
     required_packages = recorded$user$required_pkgs)

## ---- end-to-end --------------------------------------------------
# 1. a calibration period carved out of the training period, so the repair is
#    not fitted on the evaluation period
inner <- initial_time_split(train, prop = 0.80)
fit_part <- training(inner)
cal_part <- testing(inner)

# 2. one workflow: the recipe and the model travel together
final_wf <- workflow() |>
  add_recipe(recipe(outcome ~ branch + channel + lines + log_value + special +
                      hour + dow, data = fit_part) |>
               step_dummy(all_nominal_predictors()) |> step_zv(all_predictors())) |>
  add_model(rand_forest(trees = 300) |> set_engine("ranger") |>
              set_mode("classification"))
final_fit <- fit(final_wf, fit_part)

# 3. calibrate on the held-back slice, which the model has not seen
p_cal <- predict(final_fit, cal_part, type = "prob")$.pred_breach
cal_map <- as.stepfun(isoreg(p_cal, as.numeric(cal_part$breach)))

# 4. score and report on the untouched test period
p_raw <- predict(final_fit, test, type = "prob")$.pred_breach
p_final <- cal_map(p_raw)

c(auc = round(auc_of(test$breach, p_raw), 4),
  mean_predicted_raw = round(mean(p_raw), 3),
  mean_predicted_calibrated = round(mean(p_final), 3),
  observed = round(mean(test$breach), 3),
  precision_at_200 = round(precision_at(test$breach, p_final, 200), 3))

## ---- last-fit ----------------------------------------------------
collect_metrics(last_fit(final_wf, split,
                         metrics = metric_set(roc_auc, pr_auc)))
