rm(list = ls())
cat("\014")

# Load libraries
library(quantmod)
library(tidyverse)
library(lubridate)
library(PerformanceAnalytics)
library(moments)
library(writexl)

# A1: Download MCX.NS price data
ticker <- "MCX.NS"
start_date <- as.Date("2015-01-01")
end_date   <- as.Date("2025-12-31")

getSymbols(ticker, from = start_date, to = end_date, src = "yahoo")
data <- get(ticker)

price <- Ad(data)

df <- data.frame(
  Date = index(price),
  Price = as.numeric(price)
)

# A2: Compute log returns
df <- df %>%
  arrange(Date) %>%
  mutate(
    LogReturn = log(Price / lag(Price)),
    Source = "Yahoo Finance"
  )

df <- na.omit(df)

write_xlsx(df, "MCX_Data.xlsx")
tools::md5sum("MCX_Data.xlsx")

# A3: Summary statistics
mean_ret   <- mean(df$LogReturn)
median_ret <- median(df$LogReturn)
sd_ret     <- sd(df$LogReturn)
skew_ret   <- skewness(df$LogReturn)
kurt_ret   <- kurtosis(df$LogReturn) - 3

min_ret <- min(df$LogReturn)
max_ret <- max(df$LogReturn)

min_date <- df$Date[which.min(df$LogReturn)]
max_date <- df$Date[which.max(df$LogReturn)]

summary_stats <- data.frame(
  Metric = c("Mean","Median","SD","Skewness","Excess Kurtosis","Min","Max"),
  Value  = c(mean_ret, median_ret, sd_ret, skew_ret, kurt_ret, min_ret, max_ret)
)

print(summary_stats)
cat("\nMin Return Date:", min_date)
cat("\nMax Return Date:", max_date)

# A4: Price and return plots
plot(df$Date, df$Price, type = "l", col = "blue",
     main = "MCX.NS Price Series",
     xlab = "Date", ylab = "Price")

plot(df$Date, df$LogReturn, type = "l", col = "red",
     main = "MCX.NS Log Returns",
     xlab = "Date", ylab = "Log Return")

hist(df$LogReturn, breaks = 50, probability = TRUE,
     main = "MCX.NS Return Distribution",
     xlab = "Log Return",
     col = "lightblue")

x <- seq(min(df$LogReturn), max(df$LogReturn), length = 100)
y <- dnorm(x, mean = mean_ret, sd = sd_ret)
lines(x, y, col = "darkred", lwd = 2)

# A5: Sub-period analysis
df$Period <- case_when(
  df$Date < as.Date("2020-03-01") ~ "Pre-COVID",
  df$Date >= as.Date("2020-03-01") & df$Date <= as.Date("2021-12-31") ~ "COVID",
  df$Date >= as.Date("2022-01-01") ~ "Post-COVID"
)

sub_stats <- df %>%
  group_by(Period) %>%
  summarise(
    Mean = mean(LogReturn),
    Median = median(LogReturn),
    SD = sd(LogReturn),
    Skewness = skewness(LogReturn),
    Excess_Kurtosis = kurtosis(LogReturn) - 3
  )

knitr::kable(sub_stats, digits = 4)

# A6: Largest one-day drop
cat("\nLargest one-day drop:")
cat("\nDate:", as.character(min_date))
cat("\nReturn:", min_ret)

# B1: ADF test
library(tseries)

adf_result <- adf.test(df$LogReturn) 
adf_summary <- data.frame(
  Test = "ADF",
  Statistic = round(adf_result$statistic, 3),
  Lag = adf_result$parameter,
  `p-value` = "< 0.01",
  Conclusion = "Stationary"
)

knitr::kable(adf_summary)

# B2: ACF and PACF
par(mfrow = c(2,1))
acf(df$LogReturn, lag.max = 20,
    main = "MCX.NS ACF of Log Returns")

pacf(df$LogReturn, lag.max = 20,
     main = "MCX.NS PACF of Log Returns")

# B3: ARIMA model comparison
library(forecast)

model_000 <- Arima(df$LogReturn, order = c(0,0,0))
model_100 <- Arima(df$LogReturn, order = c(1,0,0))
model_010 <- Arima(df$LogReturn, order = c(0,0,1))
model_110 <- Arima(df$LogReturn, order = c(1,0,1))

model_comparison <- data.frame(
  Model = c("ARIMA(0,0,0)", "ARIMA(1,0,0)", "ARIMA(0,0,1)", "ARIMA(1,0,1)"),
  AIC = c(AIC(model_000), AIC(model_100), AIC(model_010), AIC(model_110)),
  BIC = c(BIC(model_000), BIC(model_100), BIC(model_010), BIC(model_110)),
  LogLik = c(logLik(model_000), logLik(model_100), logLik(model_010), logLik(model_110))
)

print(model_comparison)

# B4: Model diagnostics
summary(model_000)

Box.test(residuals(model_000), lag = 5, type = "Ljung-Box")
Box.test(residuals(model_000), lag = 10, type = "Ljung-Box")
Box.test(residuals(model_000), lag = 20, type = "Ljung-Box")

# B5: 10-day forecast
forecast_10 <- forecast(model_000, h = 10)
print(forecast_10)

last_60 <- tail(df$LogReturn, 60)

time_index <- 1:60
future_index <- 61:70

plot(time_index, last_60, type = "l", col = "blue",
     xlim = c(1, 70),
     ylim = range(c(last_60, forecast_10$lower, forecast_10$upper)),
     main = "MCX.NS 10-Day Return Forecast",
     xlab = "Time", ylab = "Log Return")

lines(future_index, forecast_10$mean, col = "red", lwd = 2)
lines(future_index, forecast_10$lower[,2], col = "darkgreen", lty = 2)
lines(future_index, forecast_10$upper[,2], col = "darkgreen", lty = 2)

# B6: Forecast comparison
forecast_alt <- forecast(model_100, h = 10)

print(forecast_10$mean)
print(forecast_alt$mean)

comparison <- data.frame(
  Day = 1:10,
  ARIMA_000 = as.numeric(forecast_10$mean),
  ARIMA_100 = as.numeric(forecast_alt$mean)
)

print(comparison)

# C1: ARCH-LM test
library(FinTS)

arch_test_5 <- ArchTest(df$LogReturn, lags = 5)
print(arch_test_5)

arch_test_10 <- ArchTest(df$LogReturn, lags = 10)
print(arch_test_10)

# C2: GARCH(1,1)
library(rugarch)

spec <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
  mean.model = list(armaOrder = c(0,0)),
  distribution.model = "norm"
)

garch_fit <- ugarchfit(spec = spec, data = df$LogReturn)
show(garch_fit)

# C3: Alternative GARCH
spec_12 <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1,2)),
  mean.model = list(armaOrder = c(0,0)),
  distribution.model = "norm"
)

garch_12 <- ugarchfit(spec = spec_12, data = df$LogReturn)

spec_21 <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(2,1)),
  mean.model = list(armaOrder = c(0,0)),
  distribution.model = "norm"
)

garch_21 <- ugarchfit(spec = spec_21, data = df$LogReturn)

show(garch_12)
show(garch_21)

# C4: GARCH comparison
garch_comparison <- data.frame(
  Model = c("GARCH(1,1)", "GARCH(1,2)", "GARCH(2,1)"),
  AIC = c(infocriteria(garch_fit)[1],
          infocriteria(garch_12)[1],
          infocriteria(garch_21)[1]),
  BIC = c(infocriteria(garch_fit)[2],
          infocriteria(garch_12)[2],
          infocriteria(garch_21)[2]),
  LogLik = c(likelihood(garch_fit),
             likelihood(garch_12),
             likelihood(garch_21))
)

print(garch_comparison)

# C5: Conditional volatility
volatility <- sigma(garch_fit)

plot(df$Date, volatility, type = "l", col = "blue",
     main = "MCX.NS Conditional Volatility (GARCH)",
     xlab = "Date", ylab = "Volatility")

# C6: Rolling volatility forecast
roll_spec <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
  mean.model = list(armaOrder = c(0,0)),
  distribution.model = "norm"
)

roll <- ugarchroll(
  spec = roll_spec,
  data = df$LogReturn,
  n.ahead = 1,
  forecast.length = 30,
  refit.every = 1,
  refit.window = "moving"
)

forecast_vol <- as.numeric(roll@forecast$density[,"Sigma"])

realised_abs <- abs(tail(df$LogReturn, 30))

dates_forecast <- tail(df$Date, 30)

plot(dates_forecast, forecast_vol, type = "l", col = "blue", lwd = 2,
     main = "30-Day Out-of-Sample Forecast vs Realised Absolute Returns",
     xlab = "Date", ylab = "Volatility")

lines(dates_forecast, realised_abs, col = "red")

legend("topright", legend = c("Forecasted Volatility", "Realised Absolute Return"),
       col = c("blue", "red"), lty = 1)

# C7: Highest volatility spikes
vol_df <- data.frame(
  Date = df$Date,
  Volatility = volatility
)

top_vol <- vol_df %>%
  arrange(desc(Volatility)) %>%
  head(3)

print(top_vol)

# D1: Value at Risk
mu <- mean(df$LogReturn)
sigma <- sd(df$LogReturn)

VaR_95_param <- -(mu + sigma * qnorm(0.05))
VaR_99_param <- -(mu + sigma * qnorm(0.01))

library(zoo)

VaR_95_hist_roll <- rollapply(
  df$LogReturn,
  252,
  function(x) -quantile(x, 0.05, na.rm = TRUE),
  align = "right",
  fill = NA
)

VaR_99_hist_roll <- rollapply(
  df$LogReturn,
  252,
  function(x) -quantile(x, 0.01, na.rm = TRUE),
  align = "right",
  fill = NA
)

VaR_95_hist <- tail(na.omit(VaR_95_hist_roll), 1)
VaR_99_hist <- tail(na.omit(VaR_99_hist_roll), 1)

VaR_results <- data.frame(
  Method = c("Parametric", "Parametric", "Historical", "Historical"),
  Confidence = c("95%", "99%", "95%", "99%"),
  VaR = c(VaR_95_param, VaR_99_param, VaR_95_hist, VaR_99_hist)
)

print(VaR_results)

# D3: Expected Shortfall
ES_95 <- -mean(df$LogReturn[df$LogReturn <= quantile(df$LogReturn, 0.05)])
ES_99 <- -mean(df$LogReturn[df$LogReturn <= quantile(df$LogReturn, 0.01)])

ES_results <- data.frame(
  Measure = c("VaR", "VaR", "ES", "ES"),
  Confidence = c("95%", "99%", "95%", "99%"),
  Value = c(VaR_95_hist, VaR_99_hist, ES_95, ES_99)
)

print(ES_results)

# D4: VaR backtesting
df_bt <- df %>%
  mutate(
    VaR_99 = VaR_99_hist_roll,
    Violation = ifelse(LogReturn < -VaR_99, 1, 0)
  )

total_obs <- nrow(na.omit(df_bt$VaR_99))
num_violations <- sum(df_bt$Violation, na.rm = TRUE)

violation_rate <- num_violations / total_obs

violation_dates <- df_bt$Date[df_bt$Violation == 1]

cat("Total observations:", total_obs, "\n")
cat("Number of violations:", num_violations, "\n")
cat("Violation rate:", violation_rate, "\n")

print(violation_dates)

# D5: Post-COVID VaR
df_post <- df %>% filter(Date >= as.Date("2022-01-01"))

mu_post <- mean(df_post$LogReturn)
sigma_post <- sd(df_post$LogReturn)

VaR_95_param_post <- -(mu_post + sigma_post * qnorm(0.05))
VaR_99_param_post <- -(mu_post + sigma_post * qnorm(0.01))

VaR_95_hist_post <- -quantile(df_post$LogReturn, 0.05)
VaR_99_hist_post <- -quantile(df_post$LogReturn, 0.01)

VaR_post_results <- data.frame(
  Method = c("Parametric", "Parametric", "Historical", "Historical"),
  Confidence = c("95%", "99%", "95%", "99%"),
  VaR = c(VaR_95_param_post, VaR_99_param_post, VaR_95_hist_post, VaR_99_hist_post)
)

print(VaR_post_results)

# D6: Largest VaR violation
df_bt$Gap <- df_bt$LogReturn + df_bt$VaR_99

violations <- df_bt %>% filter(Violation == 1)

largest_violation <- violations[which.min(violations$Gap), ]

print(largest_violation[, c("Date", "LogReturn", "VaR_99", "Gap")])

# E1: Black-Scholes Inputs

S <- tail(df$Price, 1)
K <- S
T <- 30/252
r <- 0.0518   # 91-day T-bill yield

# Use LAST forecasted volatility from Part C6
sigma_bs <- tail(forecast_vol, 1) * sqrt(252)

cat("Black-Scholes Model Inputs:\n\n")
cat("Spot Price (S)      :", round(S, 4), "\n")
cat("Strike Price (K)    :", round(K, 4), "\n")
cat("Time to Maturity (T):", round(T, 6), "\n")
cat("Risk-free Rate (r)  :", round(r, 4), "\n")
cat("Volatility (sigma)  :", round(sigma_bs, 6), "\n")


# E2: Black-Scholes Pricing

d1 <- (log(S/K) + (r + 0.5 * sigma_bs^2) * T) / (sigma_bs * sqrt(T))
d2 <- d1 - sigma_bs * sqrt(T)

call_price <- S * pnorm(d1) - K * exp(-r * T) * pnorm(d2)
put_price  <- K * exp(-r * T) * pnorm(-d2) - S * pnorm(-d1)

cat("\nBlack-Scholes Pricing Results:\n\n")
cat("Call Price (C) :", round(call_price, 4), "\n")
cat("Put Price (P)  :", round(put_price, 4), "\n")

# Put-Call Parity
LHS <- call_price - put_price
RHS <- S - K * exp(-r * T)

cat("\nPut-Call Parity Check:\n\n")
cat("LHS (C - P)        :", round(LHS, 4), "\n")
cat("RHS (S - K e^{-rT}):", round(RHS, 4), "\n")
cat("Difference         :", round(LHS - RHS, 6), "\n")


# E3: Greeks

delta_call <- pnorm(d1)
delta_put  <- pnorm(d1) - 1

gamma <- dnorm(d1) / (S * sigma_bs * sqrt(T))

theta_call <- (-S * dnorm(d1) * sigma_bs / (2 * sqrt(T))) -
  r * K * exp(-r * T) * pnorm(d2)

theta_put <- (-S * dnorm(d1) * sigma_bs / (2 * sqrt(T))) +
  r * K * exp(-r * T) * pnorm(-d2)

vega <- S * dnorm(d1) * sqrt(T)

rho_call <- K * T * exp(-r * T) * pnorm(d2)
rho_put  <- -K * T * exp(-r * T) * pnorm(-d2)
theta_call <- theta_call / 365
theta_put  <- theta_put / 365
vega <- vega / 100

cat("\nOption Greeks:\n\n")
cat("Delta (Call) :", round(delta_call, 6), "\n")
cat("Delta (Put)  :", round(delta_put, 6), "\n")
cat("Gamma        :", round(gamma, 6), "\n")
cat("Theta (Call) :", round(theta_call, 4), "\n")
cat("Theta (Put)  :", round(theta_put, 4), "\n")
cat("Vega         :", round(vega, 4), "\n")
cat("Rho (Call)   :", round(rho_call, 4), "\n")
cat("Rho (Put)    :", round(rho_put, 4), "\n")


# E4: Strike Sensitivity

K_seq <- seq(0.8*S, 1.2*S, by = 0.01*S)

call_prices <- c()
deltas <- c()

for (K_i in K_seq) {
  
  d1_i <- (log(S/K_i) + (r + 0.5 * sigma_bs^2) * T) / (sigma_bs * sqrt(T))
  d2_i <- d1_i - sigma_bs * sqrt(T)
  
  C_i <- S * pnorm(d1_i) - K_i * exp(-r * T) * pnorm(d2_i)
  delta_i <- pnorm(d1_i)
  
  call_prices <- c(call_prices, C_i)
  deltas <- c(deltas, delta_i)
}

# Plot 1: Call Price vs Strike
plot(K_seq, call_prices, type = "l", col = "blue", lwd = 2,
     main = "Call Price vs Strike",
     xlab = "Strike (K)", ylab = "Call Price")

# Plot 2: Delta vs Strike
plot(K_seq, deltas, type = "l", col = "red", lwd = 2,
     main = "Delta vs Strike",
     xlab = "Strike (K)", ylab = "Delta")


# E5: Volatility Sensitivity

sigma_seq <- seq(0.5 * sigma_bs, 2 * sigma_bs, length.out = 20)

call_vol <- c()

for (sig in sigma_seq) {
  
  d1_i <- (log(S/K) + (r + 0.5 * sig^2) * T) / (sig * sqrt(T))
  d2_i <- d1_i - sig * sqrt(T)
  
  C_i <- S * pnorm(d1_i) - K * exp(-r * T) * pnorm(d2_i)
  
  call_vol <- c(call_vol, C_i)
}

plot(sigma_seq, call_vol, type = "l", col = "blue", lwd = 2,
     main = "Call Price vs Volatility",
     xlab = "Volatility (sigma)", ylab = "Call Price")

abline(v = sigma_bs, col = "red", lty = 2, lwd = 2)