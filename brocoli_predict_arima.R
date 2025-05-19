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

trian_end <- floor(nrow(summary_price512*0.85))
num_forecast <- train_end:nrow(summary_price512) 
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
fitModel <- arima(lm_log$residuals, order = c(2,0,1))
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
  result <- 0.293048 + t*0.112706 + residual
  return(result)
}
len_forecast <- length(num_forecast)
x.pred <- predict(fitModel, n.ahead = len_forecast)

# 還原預測（log + 殘差 → exp）
model.est(x.pred$pred, summary_price512_forecast$temperature)
pred <- exp(model.est(x.pred$pred, summary_price512_forecast$temperature))
U <- x.pred$pred + 1.96*x.pred$se
L <- x.pred$pred - 1.96*x.pred$se
pred.U <- exp(model.est(U, summary_price512_forecast$temperature))
pred.L <- exp(model.est(L, summary_price512_forecast$temperature))

# 設定 layout：畫面分為兩欄，左邊畫主圖，右邊畫 legend
layout(matrix(c(1, 2), nrow = 1), widths = c(5, 1))
# 設定邊界：主圖
par(mar = c(5, 4, 4, 2))  # 下左上右的邊距（右邊留小一點）
ts.plot(c(summary_price512$price[350:378], rep(NA,len_forecast)), ylim=c(0,90), ylab='price', main = "「512 永靖鄉」市場價格預測")
lines(length(summary_price512$price[350:378])+(1:len_forecast), pred, col=2)
lines(length(summary_price512$price[350:378])+(1:len_forecast), pred.U, col=3, lty=2)
lines(length(summary_price512$price[350:378])+(1:len_forecast), pred.L, col=3, lty=2)
lines(length(summary_price512$price[350:378])+(1:len_forecast), summary_price512$price[379:542], col=4)
# 換到第2欄畫 legend
par(mar = c(0, 0, 2, 0))

plot.new()  # 新增空白畫面
legend("center", inset = c(0, 0), xpd = TRUE,
       legend = c("Forecast", "Actual", "95% CI"),
       col = c(2, 4, 3), lty = c(2, 1, 2), bty = "n")
dev.off()

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

trian_end <- floor(nrow(summary_price514*0.85))
num_forecast <- train_end:nrow(summary_price514) 
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
fitModel <- arima(lm_log$residuals, order = c(2,0,1))
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
  result <- -0.013675+ t* 0.129134  + residual
  return(result)
}
len_forecast <- length(num_forecast)
x.pred <- predict(fitModel, n.ahead = len_forecast)

# 還原預測（log + 殘差 → exp）
model.est(x.pred$pred, summary_price514_forecast$temperature)
pred <- exp(model.est(x.pred$pred, summary_price514_forecast$temperature))
model.est(x.pred$pred, summary_price514_forecast$temperature)
U <- x.pred$pred + 1.96*x.pred$se
L <- x.pred$pred - 1.96*x.pred$se
pred.U <- exp(model.est(U, summary_price514_forecast$temperature))
pred.L <- exp(model.est(L, summary_price514_forecast$temperature))

# 設定 layout：畫面分為兩欄，左邊畫主圖，右邊畫 legend
layout(matrix(c(1, 2), nrow = 1), widths = c(5, 1))  # 4:1 的寬度比例
par(mar = c(5, 4, 4, 2))  # 下左上右的邊距（右邊留小一點）
ts.plot(c(summary_price514$price[350:378], rep(NA,len_forecast)), ylim=c(0,120), ylab='price', main = "「514 溪湖鎮」市場價格預測")
lines(length(summary_price514$price[350:378])+(1:len_forecast), pred, col=2)
lines(length(summary_price514$price[350:378])+(1:len_forecast), pred.U, col=3, lty=2)
lines(length(summary_price514$price[350:378])+(1:len_forecast), pred.L, col=3, lty=2)
lines(length(summary_price514$price[350:378])+(1:len_forecast), summary_price514$price[379:542], col=4)
# 換到第2欄畫 legend
par(mar = c(0, 0, 2, 0))
plot.new()  # 新增空白畫面
legend("center", inset = c(0, 0), xpd = TRUE,
       legend = c("Forecast", "Actual", "95% CI"),
       col = c(2, 4, 3), lty = c(2, 1, 2), bty = "n")
dev.off()

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

trian_end <- floor(nrow(summary_price648*0.85))
num_forecast <- train_end:nrow(summary_price648) 
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
fitModel <- arima(lm_log$residuals, order = c(2,0,1))
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
  result <- 0.302641 + t* 0.119348  + residual
  return(result)
}
len_forecast <- length(num_forecast)
x.pred <- predict(fitModel, n.ahead = len_forecast)

# 還原預測（log + 殘差 → exp）
model.est(x.pred$pred, summary_price648_forecast$temperature)
pred <- exp(model.est(x.pred$pred, summary_price648_forecast$temperature))
U <- x.pred$pred + 1.96*x.pred$se
L <- x.pred$pred - 1.96*x.pred$se
pred.U <- exp(model.est(U, summary_price648_forecast$temperature))
pred.L <- exp(model.est(L, summary_price648_forecast$temperature))

# 設定 layout：畫面分為兩欄，左邊畫主圖，右邊畫 legend
layout(matrix(c(1, 2), nrow = 1), widths = c(5, 1))  
par(mar = c(5, 4, 4, 2))  # 下左上右的邊距（右邊留小一點）
ts.plot(c(summary_price648$price[350:378], rep(NA,len_forecast)), ylim=c(0,150), ylab='price', main = "「648 西螺鎮」市場價格預測")
lines(length(summary_price648$price[350:378])+(1:len_forecast), pred, col=2)
lines(length(summary_price648$price[350:378])+(1:len_forecast), pred.U, col=3, lty=2)
lines(length(summary_price648$price[350:378])+(1:len_forecast), pred.L, col=3, lty=2)
lines(length(summary_price648$price[350:378])+(1:len_forecast), summary_price648$price[379:542], col=4)
# 換到第2欄畫 legend
par(mar = c(0, 0, 2, 0))
plot.new()  # 新增空白畫面
legend("center", inset = c(0, 0), xpd = TRUE,
       legend = c("Forecast", "Actual", "95% CI"),
       col = c(2, 4, 3), lty = c(2, 1, 2), bty = "n")
dev.off()

