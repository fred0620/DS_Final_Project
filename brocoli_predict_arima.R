library(randomForest)
library(ISOweek)
library(dplyr)
library(tseries)
library(ggplot2)

data <- read.csv("weekly_veg_dataset.csv", header = TRUE)
data$'品項' <- as.factor(data$'品項')
data$'市場' <- as.factor(data$'市場')
data <- data[data$'品項' == "花椰菜（青梗）", ]

# NA補值
# 數值型欄位用中位數補值
numeric_vars <- names(data)[sapply(data, is.numeric)]

# 類別型欄位用眾數補值
categorical_vars <- names(data)[sapply(data, function(x) is.character(x) || is.factor(x))]

for (var in numeric_vars) {
  median_val <- median(data[[var]], na.rm = TRUE)
  data[[var]][is.na(data[[var]])] <- median_val
}

# 建立 ISO 週格式字串（例："2024-W12"）
summary_price$iso_week <- sprintf("%04d-W%02d", summary_price$year, summary_price$week)

# 轉換成週一日期（用 ISOweek2date）
summary_price$year_week_date <- ISOweek2date(paste0(summary_price$iso_week, "-1"))
#6個市場
summary_price_6 <- data %>%
  group_by(year, week, 市場) %>%
  summarise(
    price = 平均價_元每公斤,
    .groups = "drop"
  )
#最終預測使用的3個市場
summary_price_3 <- data %>%
  filter(市場 %in% c("512 永靖鄉", "514 溪湖鎮", "648 西螺鎮")) %>%
  group_by(year, week, 市場) %>%
  summarise(
    price = 平均價_元每公斤,
    .groups = "drop"
  )
# year + week → 日期
summary_price_6$iso_week <- sprintf("%04d-W%02d", summary_price_6$year, summary_price_6$week)
summary_price_6$year_week_date <- ISOweek2date(paste0(summary_price_6$iso_week, "-1"))

summary_price_3$iso_week <- sprintf("%04d-W%02d", summary_price_3$year, summary_price_3$week)
summary_price_3$year_week_date <- ISOweek2date(paste0(summary_price_3$iso_week, "-1"))

######不同市場每週加權平均價格趨勢####
ggplot(summary_price_6, aes(x = year_week_date, y = price)) +
  geom_line(color = "steelblue") +
  labs(
    title = "不同市場每週平均價格趨勢",
    x = "週",
    y = "價格（元/公斤）"
  ) +
  facet_wrap(~ 市場, scales = "free_y") +  # 每個品種一格，y軸自動調整
  theme_minimal()

ggplot(summary_price_3, aes(x = year_week_date, y = price)) +
  geom_line(color = "steelblue") +
  labs(
    title = "不同市場每週平均價格趨勢",
    x = "週",
    y = "價格（元/公斤）"
  ) +
  facet_wrap(~ 市場, scales = "free_y") + 
  theme_minimal()

#####512永靖鄉#####

summary_price512 <- data %>%
  filter(市場 == "512 永靖鄉") %>%
  group_by(year, week) %>%
  summarise(
    temperature = mean(平均地溫100cm..._w.3, na.rm = TRUE),
    price = mean(平均價_元每公斤, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(year, week) %>%
  mutate(week_num = row_number())
ts.plot(summary_price512$price, ylab = "price", main = "512永靖鄉每週批發價格趨勢圖")

train_end <- floor(nrow(summary_price512)*0.85)
num_forecast <- (train_end+1):nrow(summary_price512) 
summary_price512_fit <- summary_price512[-num_forecast, ]
summary_price512_forecast <- summary_price512[num_forecast, ]

ts.plot(summary_price512_fit$price, ylab = "price", main = "512 永靖鄉fit data每週批發價格趨勢圖")
#variance不是每個時期一致 -> 對預測目標取對數
summary_price512_fit$log_price <- log(summary_price512_fit$price)
ts.plot(summary_price512_fit$log_price, ylab = "log(price)")
lm_log <- lm(log_price ~ temperature, data = summary_price512_fit)
summary(lm_log)
acf(lm_log$residuals)
pacf(lm_log$residuals)
# 殘差建 ARIMA
fitModel <- arima(lm_log$residuals, order = c(3,0,1))
#Ljungbox test
#H0:fitted model adequate
residual_analysis <- residuals(fitModel)
B_text_p_value = c(0,0)
for(hh in 1:20){
  B_text_p_value[hh] = Box.test(residual_analysis, lag=hh, type="Ljung-Box")$p.value
}
plot(1:20, B_text_p_value[1:20], type="p", 
     main="p values for Ljung-Box statistic", 
     xlab="lag", ylab="p value", ylim=c(0,1))
abline(h=0.05, lty=2, col=4)

#prediction
model.est <- function(residual ,t) {
  result <- 0.35042 + t*0.10998 + residual
  return(result)
}
len_forecast <- length(num_forecast)
x.pred <- predict(fitModel, n.ahead = len_forecast)

# 還原預測（log + 殘差 → exp）
model.est(x.pred$pred, summary_price512_forecast$temperature)
pred <- exp(model.est(x.pred$pred, summary_price512_forecast$temperature))

# 預測結果與實際值比較
actual <- summary_price512_forecast$price
# MAE
mae <- mean(abs(actual - pred))
# RMSE
rmse <- sqrt(mean((actual - pred)^2))
# R-squared
ss_total <- sum((actual - mean(actual))^2)
ss_res <- sum((actual - pred)^2)
r_squared <- 1 - ss_res / ss_total

cat("MAE: ", round(mae, 4), "\n")
cat("RMSE: ", round(rmse, 4), "\n")
cat("R-squared: ", round(r_squared, 4), "\n")

ts.plot(c(summary_price512$price[440:460], rep(NA,len_forecast)), ylim=c(0,90), ylab='價格（元/公斤）', main = "「512 永靖鄉」市場價格預測")
lines(length(summary_price512$price[440:460])+(1:len_forecast), pred, col=2)
lines(length(summary_price512$price[440:460])+(1:len_forecast), summary_price512$price[461:542], col=4)
legend("topright", inset = c(0, 0), xpd = TRUE,
       legend = c("Forecast", "Actual"),
       col = c(2, 4), lty = c(1, 1), bty = "n", cex = 0.8)

#####514溪湖鎮#####
summary_price514 <- data %>%
  filter(市場 == "514 溪湖鎮") %>%
  group_by(year, week) %>%
  summarise(
    temperature = mean(平均地溫100cm..._w.3, na.rm = TRUE),
    price = mean(平均價_元每公斤, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(year, week) %>%
  mutate(week_num = row_number())
ts.plot(summary_price514$price, ylab = "price", main = "514溪湖鎮每週批發價格趨勢圖")

train_end <- floor(nrow(summary_price514)*0.85)
num_forecast <- (train_end+1):nrow(summary_price514) 
summary_price514_fit <- summary_price514[-num_forecast, ]
summary_price514_forecast <- summary_price514[num_forecast, ]
ts.plot(summary_price514_fit$price, ylab = "price", main = "514溪湖鎮fit data每週批發價格趨勢圖")

summary_price514_fit$log_price <- log(summary_price514_fit$price)
ts.plot(summary_price514_fit$log_price, ylab = "log(price)")
lm_log <- lm(log_price ~ temperature, data = summary_price514_fit)
summary(lm_log)
acf(lm_log$residuals)
pacf(lm_log$residuals)
# 殘差建 ARIMA
fitModel <- arima(lm_log$residuals, order = c(3,0,1))
#Ljungbox test
#H0:fitted model adequate
residual_analysis <- residuals(fitModel)
B_text_p_value = c(0,0)
for(hh in 1:20){
  B_text_p_value[hh] = Box.test(residual_analysis, lag=hh, type="Ljung-Box")$p.value
}
plot(1:20, B_text_p_value[1:20], type="p", 
     main="p values for Ljung-Box statistic", 
     xlab="lag", ylab="p value", ylim=c(0,1))
abline(h=0.05, lty=2, col=4)

#prediction
model.est <- function(residual ,t) {
  result <- -0.035445+ t*0.131235 + residual
  return(result)
}
len_forecast <- length(num_forecast)
x.pred <- predict(fitModel, n.ahead = len_forecast)

# 還原預測（log + 殘差 → exp）
#model.est(x.pred$pred, summary_price514_forecast$temperature)
pred <- exp(model.est(x.pred$pred, summary_price514_forecast$temperature))

# 預測結果與實際值比較
actual <- summary_price514_forecast$price
# MAE
mae <- mean(abs(actual - pred))
# RMSE
rmse <- sqrt(mean((actual - pred)^2))
# R-squared
ss_total <- sum((actual - mean(actual))^2)
ss_res <- sum((actual - pred)^2)
r_squared <- 1 - ss_res / ss_total

cat("MAE: ", round(mae, 4), "\n")
cat("RMSE: ", round(rmse, 4), "\n")
cat("R-squared: ", round(r_squared, 4), "\n")

ts.plot(c(summary_price514$price[440:460], rep(NA,len_forecast)), ylim=c(0,120), ylab='價格（元/公斤）', main = "「514 溪湖鎮」市場價格預測")
lines(length(summary_price514$price[440:460])+(1:len_forecast), pred, col=2)
lines(length(summary_price514$price[440:460])+(1:len_forecast), summary_price514$price[461:542], col=4)
legend("topright", inset = c(0, 0), xpd = TRUE,
       legend = c("Forecast", "Actual"),
       col = c(2, 4), lty = c(1, 1), bty = "n", cex = 0.8)


#####648 西螺鎮#####
summary_price648 <- data %>%
  filter(市場 == "648 西螺鎮") %>%
  group_by(year, week) %>%
  summarise(
    temperature = mean(平均地溫100cm..._w.3, na.rm = TRUE),
    price = mean(平均價_元每公斤, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(year, week) %>%
  mutate(week_num = row_number())
ts.plot(summary_price648$price, ylab = "price", main = "648西螺鎮每週批發價格趨勢圖")

train_end <- floor(nrow(summary_price648)*0.85)
num_forecast <- (train_end+1):nrow(summary_price648) 
summary_price648_fit <- summary_price648[-num_forecast, ]
summary_price648_forecast <- summary_price648[num_forecast, ]
ts.plot(summary_price648_fit$price, ylab = "price", main = "648西螺鎮fit data每週批發價格趨勢圖")

summary_price648_fit$log_price <- log(summary_price648_fit$price)
ts.plot(summary_price648_fit$log_price, ylab = "log(price)")
lm_log <- lm(log_price ~ temperature, data = summary_price648_fit)
summary(lm_log)
acf(lm_log$residuals)
pacf(lm_log$residuals)
# 殘差建 ARIMA
fitModel <- arima(lm_log$residuals, order = c(3,0,1))
#Ljungbox test
#H0:fitted model adequate
residual_analysis <- residuals(fitModel)
B_text_p_value = c(0,0)
for(hh in 1:20){
  B_text_p_value[hh] = Box.test(residual_analysis, lag=hh, type="Ljung-Box")$p.value
}
plot(1:20, B_text_p_value[1:20], type="p", 
     main="p values for Ljung-Box statistic", 
     xlab="lag", ylab="p value", ylim=c(0,1))
abline(h=0.05, lty=2, col=4)

#prediction
model.est <- function(residual ,t) {
  result <- 0.305461 + t*0.120230  + residual
  return(result)
}
len_forecast <- length(num_forecast)
x.pred <- predict(fitModel, n.ahead = len_forecast)

# 還原預測（log + 殘差 → exp）
model.est(x.pred$pred, summary_price648_forecast$temperature)
pred <- exp(model.est(x.pred$pred, summary_price648_forecast$temperature))
# 預測結果與實際值比較
actual <- summary_price648_forecast$price

# MAE
mae <- mean(abs(actual - pred))
# RMSE
rmse <- sqrt(mean((actual - pred)^2))
# R-squared
ss_total <- sum((actual - mean(actual))^2)
ss_res <- sum((actual - pred)^2)
r_squared <- 1 - ss_res / ss_total

cat("MAE: ", round(mae, 4), "\n")
cat("RMSE: ", round(rmse, 4), "\n")
cat("R-squared: ", round(r_squared, 4), "\n")

ts.plot(c(summary_price648$price[440:460], rep(NA,len_forecast)), ylim=c(0,150), ylab='價格（元/公斤）', main = "「648 西螺鎮」市場價格預測")
lines(length(summary_price648$price[440:460])+(1:len_forecast), pred, col=2)
lines(length(summary_price648$price[440:460])+(1:len_forecast), summary_price648$price[461:542], col=4)
legend("topright", inset = c(0, 0), xpd = TRUE,
       legend = c("Forecast", "Actual"),
       col = c(2, 4, 3), lty = c(1, 1), bty = "n", cex = 0.8)



