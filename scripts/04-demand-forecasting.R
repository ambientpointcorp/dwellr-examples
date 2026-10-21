# Generated from chapters/04-demand-forecasting.qmd by tools/extract.py -- do not edit by hand.
# Chapter: Demand Forecasting
#
# Run from the repository root:
#   Rscript scripts/04-demand-forecasting.R

library(dwellr)
library(ggplot2)
library(forecast)
library(modeltime)
library(timetk)
library(tidymodels)
library(dplyr)

source("R/theme.R")


## ---- fig-series --------------------------------------------------
set.seed(42)
n <- 72
t <- 1:n
demand <- round(300 * 1.012^t *
                (1 + 0.25 * sin(2 * pi * (t - 2) / 12)) *
                exp(rnorm(n, 0, 0.08)))

dates <- seq(as.Date("2019-01-01"), by = "month", length.out = n)
y     <- ts(demand, start = c(2019, 1), frequency = 12)
train <- window(y, end = c(2023, 12))
test  <- window(y, start = c(2024, 1))

ggplot(data.frame(date = dates, demand), aes(date, demand)) +
  geom_line(colour = "grey35") +
  geom_vline(xintercept = dates[60], linetype = 2) +
  labs(x = NULL, y = "units")

## ---- series-properties -------------------------------------------
c(mean_first_year = mean(demand[1:12]),
  mean_last_year  = mean(demand[61:72]),
  sd_first_two_yrs = round(sd(demand[1:24]), 1),
  sd_last_two_yrs  = round(sd(demand[49:72]), 1))

## ---- benchmarks --------------------------------------------------
h     <- length(test)
hist_ <- as.numeric(train)
act   <- as.numeric(test)

forecasts <- list(
  mean   = fc_mean(hist_, h),
  naive  = fc_naive(hist_, h),
  drift  = fc_drift(hist_, h),
  snaive = fc_seasonal_naive(hist_, h, period = 12))

round(sapply(forecasts, head, 4), 1)

## ---- accuracy ----------------------------------------------------
round(t(sapply(forecasts, fc_accuracy,
               actual = act, train = hist_, scale_lag = 1)), 3)

## ---- rolling-origin ----------------------------------------------
round(rbind(
  naive  = fc_rolling_origin(demand, fc_naive, h = 1, n_origins = 24),
  snaive = fc_rolling_origin(demand,
             function(z, k) fc_seasonal_naive(z, k, 12),
             h = 1, n_origins = 24)), 3)

## ---- box-cox -----------------------------------------------------
lambda <- BoxCox.lambda(train)
round(lambda, 4)

## ---- ets ---------------------------------------------------------
fit_ets <- ets(train)
fit_ets$method

## ---- arima -------------------------------------------------------
fit_arima <- auto.arima(train)
ord <- arimaorder(fit_arima)
sprintf("ARIMA(%d,%d,%d)(%d,%d,%d)[%d]",
        ord[1], ord[2], ord[3], ord[4], ord[5], ord[6], ord[7])

## ---- modeltime-table ---------------------------------------------
demand_tbl <- tibble(date = dates, value = demand)
splits     <- time_series_split(demand_tbl, assess = "12 months",
                                cumulative = TRUE)

# Classical candidates, via modeltime's parsnip-style interface.
m_ets   <- exp_smoothing() |> set_engine("ets")        |>
             fit(value ~ date, training(splits))
m_arima <- arima_reg()     |> set_engine("auto_arima") |>
             fit(value ~ date, training(splits))
m_proph <- prophet_reg()   |> set_engine("prophet")    |>
             fit(value ~ date, training(splits))

# A gradient-boosted candidate, assembled with tidymodels.
rec <- recipe(value ~ date, data = training(splits)) |>
  step_timeseries_signature(date) |>
  step_rm(matches("(iso)|(xts)|(hour)|(minute)|(second)|(am.pm)")) |>
  step_dummy(all_nominal_predictors(), one_hot = TRUE) |>
  step_normalize(all_numeric_predictors()) |>
  step_rm(date)

m_boost <- workflow() |>
  add_recipe(rec) |>
  add_model(boost_tree(trees = 500, learn_rate = 0.05, tree_depth = 4) |>
              set_engine("xgboost") |> set_mode("regression")) |>
  fit(training(splits))

calibrated <- modeltime_table(m_ets, m_arima, m_proph, m_boost) |>
  modeltime_calibrate(testing(splits), quiet = TRUE)

calibrated |>
  modeltime_accuracy() |>
  select(.model_desc, mae, rmse, mape, mase) |>
  as.data.frame()

## ---- selected ----------------------------------------------------
selected <- fit_arima

## ---- fig-residuals -----------------------------------------------
res <- residuals(selected)
par(mfrow = c(2, 2), mar = c(4, 4, 2, 1))
plot(res, ylab = "residual", xlab = "year", main = "Residuals")
abline(h = 0, lty = 2)
acf(res, main = "ACF")
hist(res, breaks = 15, main = "Distribution", xlab = "residual")
qqnorm(res, main = "Normal Q-Q"); qqline(res)
par(mfrow = c(1, 1))

## ---- ljung-box ---------------------------------------------------
c(mean_residual = round(mean(res), 4),
  ljung_box_p   = round(Box.test(res, lag = 24,
                                 type = "Ljung-Box")$p.value, 4))

## ---- fig-intervals -----------------------------------------------
fc <- forecast(selected, h = 12)
autoplot(fc) +
  autolayer(test, series = "actual") +
  labs(x = NULL, y = "units") +
  theme(legend.position = "top")

## ---- interval-widths ---------------------------------------------
w <- as.numeric(fc$upper[, 2] - fc$lower[, 2])
c(h1 = round(w[1], 1), h6 = round(w[6], 1), h12 = round(w[12], 1),
  ratio_12_to_1 = round(w[12] / w[1], 2))

## ---- modeltime-intervals -----------------------------------------
mt_width <- calibrated |>
  modeltime_forecast(new_data = testing(splits), conf_interval = 0.95) |>
  filter(.model_id == 2) |>
  mutate(width = .conf_hi - .conf_lo) |>
  pull(width)

c(h1 = round(mt_width[1], 1), h12 = round(mt_width[12], 1),
  ratio_12_to_1 = round(mt_width[12] / mt_width[1], 2))

## ---- intermittent-series -----------------------------------------
set.seed(7)
inter <- rbinom(120, 1, 0.25) * rpois(120, 6)
c(zero_periods = mean(inter == 0),
  mean_demand  = round(mean(inter), 4),
  mean_size_when_nonzero = round(mean(inter[inter > 0]), 3))

## ---- croston -----------------------------------------------------
cr <- croston(inter, h = 6)
c(croston     = round(as.numeric(cr$mean)[1], 4),
  sample_mean = round(mean(inter), 4))

## ---- ltd ---------------------------------------------------------
ltd <- lead_time_demand(demand_mean = 10, demand_sd = 3,
                        lead_time_mean = 10, lead_time_sd = 2)
unlist(ltd)

## ---- ss-comparison -----------------------------------------------
base <- safety_stock(ltd, service_level = 0.95)

improve <- function(d_sd, L_sd) {
  safety_stock(lead_time_demand(10, d_sd, 10, L_sd), service_level = 0.95)
}

data.frame(
  scenario = c("as is",
               "halve demand variability",
               "halve lead-time variability",
               "eliminate lead-time variability"),
  safety_stock = c(base, improve(1.5, 2), improve(3, 1), improve(3, 0)),
  reduction = sprintf("%.1f%%", 100 * (1 - c(base, improve(1.5, 2),
                                             improve(3, 1), improve(3, 0)) / base))
)

## ---- ltd-own-series ----------------------------------------------
sigma_d <- sd(residuals(selected))      # one-period forecast error

own <- lead_time_demand(demand_mean = mean(train), demand_sd = sigma_d,
                        lead_time_mean = 2, lead_time_sd = 0.5)
c(sigma_d = round(sigma_d, 1),
  lead_time_share = round(own$lead_time_share, 3),
  safety_stock = round(safety_stock(own, service_level = 0.95), 1))
