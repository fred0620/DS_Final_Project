# 載入套件
library(tidyverse)
library(lubridate)
library(xgboost)
library(caret)
library(dplyr)
library(showtext)
library(sysfonts)

# 載入 Google 的 Noto Sans TC（支援繁體中文）
font_add_google("Noto Sans TC", "noto")
showtext_auto()

# 設定 ggplot 預設字型
theme_set(theme_minimal(base_family = "noto"))

# 1. 讀取 CSV 檔案
cauliflower_data <- read.csv("cauliflower_cleaned.csv", fileEncoding = "UTF-8")

# 假設你的資料框叫 df，week 欄位是數字
cauliflower_data $week <- sprintf("%02d", cauliflower_data $week)

#####512 永靖鄉#####

# 篩選市場
df_market <- cauliflower_data %>%
  filter(市場 == "512 永靖鄉")
colnames(df_market)

# 合併 year 和 week 欄位，創建時間欄位
df <- df_market %>%
  mutate(date = paste(year, week, sep = "-"))


# 提取指定欄位創建新資料集
model_data <- df %>%
  select(
    date,
    "平均氣溫_w.3", "最高氣溫_w.3", "最低氣溫_w.3",
    "平均地溫5cm_w.3", "平均地溫10cm_w.3", "平均地溫20cm_w.3",
    "平均地溫50cm_w.3", "平均地溫100cm_w.3", "平均露點溫度_w.3",
    "上價", "中價", "下價", "交易量_公斤", "平均價_元每公斤"
  )

# 分割資料集為訓練集、驗證集和測試集 (使用時間順序，前70%訓練、中間15%驗證、後15%測試)
# 確保資料按照日期排序
model_data <- model_data[order(model_data$date), ]

# 計算分割點
n <- nrow(model_data)
train_end <- floor(n * 0.7)
val_end <- floor(n * 0.85)

# 按時間順序分割
train_data <- model_data[1:train_end, ]
val_data <- model_data[(train_end+1):val_end, ]
test_data <- model_data[(val_end+1):n, ]

# 顯示各資料集的時間範圍和樣本數
cat("訓練集時間範圍:", as.character(min(train_data$date)), "到", as.character(max(train_data$date)), 
    "，樣本數:", nrow(train_data), "\n")
cat("驗證集時間範圍:", as.character(min(val_data$date)), "到", as.character(max(val_data$date)), 
    "，樣本數:", nrow(val_data), "\n")
cat("測試集時間範圍:", as.character(min(test_data$date)), "到", as.character(max(test_data$date)), 
    "，樣本數:", nrow(test_data), "\n")

# 準備XGBoost的輸入資料
features <- c(
  "平均氣溫_w.3", "最高氣溫_w.3", "最低氣溫_w.3",
  "平均地溫5cm_w.3", "平均地溫10cm_w.3", "平均地溫20cm_w.3",
  "平均地溫50cm_w.3", "平均地溫100cm_w.3", "平均露點溫度_w.3"
) 

# 訓練集
X_train <- as.matrix(train_data[, features])
y_train <- train_data$`平均價_元每公斤`

# 驗證集
X_val <- as.matrix(val_data[, features])
y_val <- val_data$`平均價_元每公斤`

# 測試集
X_test <- as.matrix(test_data[, features])
y_test <- test_data$`平均價_元每公斤`

# 轉換為XGBoost的DMatrix格式
dtrain <- xgb.DMatrix(data = X_train, label = y_train)
dval <- xgb.DMatrix(data = X_val, label = y_val)
dtest <- xgb.DMatrix(data = X_test)

# 設定XGBoost參數
params <- list(
  objective = "reg:squarederror",
  eval_metric = "rmse",
  eta = 0.1,
  max_depth = 6,
  min_child_weight = 1,
  subsample = 0.8,
  colsample_bytree = 0.8
)

# 使用訓練集訓練模型，並監控驗證集表現
xgb_model <- xgb.train(
  params = params,
  data = dtrain,
  nrounds = 1000,
  watchlist = list(train = dtrain, validation = dval),
  early_stopping_rounds = 50,
  verbose = 1
)

# 獲取最佳迭代次數
best_iteration <- xgb_model$best_iteration
cat("\n最佳迭代次數:", best_iteration, "\n")

# 在測試集上進行預測
y_pred <- predict(xgb_model, X_test)

# 計算評估指標
rmse <- sqrt(mean((y_pred - y_test)^2))
mae <- mean(abs(y_pred - y_test))
r_squared <- 1 - sum((y_test - y_pred)^2) / sum((y_test - mean(y_test))^2)

# 印出評估結果
cat("\n測試集評估結果：\n")
cat("RMSE：", rmse, "\n")
cat("MAE：", mae, "\n")
cat("R-squared：", r_squared, "\n")

# 將預測結果與實際值合併以進行比較和繪圖
results <- data.frame(
  date = test_data$date,
  actual = y_test,
  predicted = y_pred
)

# 印出實際值與預測值的比較結果
cat("\n實際值與預測值比較(前10筆)：\n")
comparison <- head(results, 10)
print(comparison)

# 計算預測誤差
results$error <- results$predicted - results$actual
results$abs_error <- abs(results$error)
results$error_percent <- (results$error / results$actual) * 100

# 輸出誤差統計資訊
cat("\n誤差統計：\n")
cat("平均絕對誤差 (MAE):", mean(results$abs_error), "\n")
cat("平均絕對百分比誤差 (MAPE):", mean(abs(results$error_percent)), "%\n")
cat("最大誤差:", max(results$abs_error), "\n")
cat("最小誤差:", min(results$abs_error), "\n")

layout(matrix(c(1, 2), nrow = 1), widths = c(5, 1))  
par(mar = c(5, 4, 4, 2))  
ts.plot(c(tail(y_val, 29), rep(NA,nrow(test_data))), ylim=c(0,90), ylab='price', main = "「512 永靖鄉」市場價格預測")
lines(29+(1:nrow(test_data)), results$predicted, col=2, lty = 2)

lines(29+(1:nrow(test_data)), results$actual, col=4)

par(mar = c(0, 0, 2, 0))
plot.new() 
legend("center", inset = c(0, 0), xpd = TRUE,
       legend = c("Forecast", "Actual"),
       col = c(2, 4), lty = c(2, 1), bty = "n")
dev.off()
#####514 溪湖鎮#####

# 篩選市場
df_market <- cauliflower_data %>%
  filter(市場 == "514 溪湖鎮")

# 合併 year 和 week 欄位，創建時間欄位
df <- df_market %>%
  mutate(date = paste(year, week, sep = "-"))


# 提取指定欄位創建新資料集
model_data <- df %>%
  select(
    date,
    "平均氣溫_w.3", "最高氣溫_w.3", "最低氣溫_w.3",
    "平均地溫5cm_w.3", "平均地溫10cm_w.3", "平均地溫20cm_w.3",
    "平均地溫50cm_w.3", "平均地溫100cm_w.3", "平均露點溫度_w.3",
    "上價", "中價", "下價", "交易量_公斤", "平均價_元每公斤"
  )

# 分割資料集為訓練集、驗證集和測試集 (使用時間順序，前70%訓練、中間15%驗證、後15%測試)
# 確保資料按照日期排序
model_data <- model_data[order(model_data$date), ]

# 計算分割點
n <- nrow(model_data)
train_end <- floor(n * 0.7)
val_end <- floor(n * 0.85)

# 按時間順序分割
train_data <- model_data[1:train_end, ]
val_data <- model_data[(train_end+1):val_end, ]
test_data <- model_data[(val_end+1):n, ]

# 顯示各資料集的時間範圍和樣本數
cat("訓練集時間範圍:", as.character(min(train_data$date)), "到", as.character(max(train_data$date)), 
    "，樣本數:", nrow(train_data), "\n")
cat("驗證集時間範圍:", as.character(min(val_data$date)), "到", as.character(max(val_data$date)), 
    "，樣本數:", nrow(val_data), "\n")
cat("測試集時間範圍:", as.character(min(test_data$date)), "到", as.character(max(test_data$date)), 
    "，樣本數:", nrow(test_data), "\n")

# 準備XGBoost的輸入資料
features <- c(
  "平均氣溫_w.3", "最高氣溫_w.3", "最低氣溫_w.3",
  "平均地溫5cm_w.3", "平均地溫10cm_w.3", "平均地溫20cm_w.3",
  "平均地溫50cm_w.3", "平均地溫100cm_w.3", "平均露點溫度_w.3"
) 

# 訓練集
X_train <- as.matrix(train_data[, features])
y_train <- train_data$`平均價_元每公斤`

# 驗證集
X_val <- as.matrix(val_data[, features])
y_val <- val_data$`平均價_元每公斤`

# 測試集
X_test <- as.matrix(test_data[, features])
y_test <- test_data$`平均價_元每公斤`

# 轉換為XGBoost的DMatrix格式
dtrain <- xgb.DMatrix(data = X_train, label = y_train)
dval <- xgb.DMatrix(data = X_val, label = y_val)
dtest <- xgb.DMatrix(data = X_test)

# 設定XGBoost參數
params <- list(
  objective = "reg:squarederror",
  eval_metric = "rmse",
  eta = 0.1,
  max_depth = 6,
  min_child_weight = 1,
  subsample = 0.8,
  colsample_bytree = 0.8
)

# 使用訓練集訓練模型，並監控驗證集表現
xgb_model <- xgb.train(
  params = params,
  data = dtrain,
  nrounds = 1000,
  watchlist = list(train = dtrain, validation = dval),
  early_stopping_rounds = 50,
  verbose = 1
)

# 獲取最佳迭代次數
best_iteration <- xgb_model$best_iteration
cat("\n最佳迭代次數:", best_iteration, "\n")

# 在測試集上進行預測
y_pred <- predict(xgb_model, X_test)

# 計算評估指標
rmse <- sqrt(mean((y_pred - y_test)^2))
mae <- mean(abs(y_pred - y_test))
r_squared <- 1 - sum((y_test - y_pred)^2) / sum((y_test - mean(y_test))^2)

# 印出評估結果
cat("\n測試集評估結果：\n")
cat("RMSE：", rmse, "\n")
cat("MAE：", mae, "\n")
cat("R-squared：", r_squared, "\n")

# 將預測結果與實際值合併以進行比較和繪圖
results <- data.frame(
  date = test_data$date,
  actual = y_test,
  predicted = y_pred
)

# 印出實際值與預測值的比較結果
cat("\n實際值與預測值比較(前10筆)：\n")
comparison <- head(results, 10)
print(comparison)

# 計算預測誤差
results$error <- results$predicted - results$actual
results$abs_error <- abs(results$error)
results$error_percent <- (results$error / results$actual) * 100

# 輸出誤差統計資訊
cat("\n誤差統計：\n")
cat("平均絕對誤差 (MAE):", mean(results$abs_error), "\n")
cat("平均絕對百分比誤差 (MAPE):", mean(abs(results$error_percent)), "%\n")
cat("最大誤差:", max(results$abs_error), "\n")
cat("最小誤差:", min(results$abs_error), "\n")

layout(matrix(c(1, 2), nrow = 1), widths = c(5, 1))  
par(mar = c(5, 4, 4, 2))  
ts.plot(c(tail(y_val, 29), rep(NA,nrow(test_data))), ylim=c(0,120), ylab='price', main = "「514 溪湖鎮」市場價格預測")
lines(29+(1:nrow(test_data)), results$predicted, col=2, lty = 2)
lines(29+(1:nrow(test_data)), results$actual, col=4)

par(mar = c(0, 0, 2, 0))
plot.new() 
legend("center", inset = c(0, 0), xpd = TRUE,
       legend = c("Forecast", "Actual"),
       col = c(2, 4), lty = c(2, 1), bty = "n")
dev.off()
#####648 西螺鎮#####
# 篩選市場
df_market <- cauliflower_data %>%
  filter(市場 == "648 西螺鎮")

# 合併 year 和 week 欄位，創建時間欄位
df <- df_market %>%
  mutate(date = paste(year, week, sep = "-"))


# 提取指定欄位創建新資料集
model_data <- df %>%
  select(
    date,
    "平均氣溫_w.3", "最高氣溫_w.3", "最低氣溫_w.3",
    "平均地溫5cm_w.3", "平均地溫10cm_w.3", "平均地溫20cm_w.3",
    "平均地溫50cm_w.3", "平均地溫100cm_w.3", "平均露點溫度_w.3",
    "上價", "中價", "下價", "交易量_公斤", "平均價_元每公斤"
  )

# 分割資料集為訓練集、驗證集和測試集 (使用時間順序，前70%訓練、中間15%驗證、後15%測試)
# 確保資料按照日期排序
model_data <- model_data[order(model_data$date), ]

# 計算分割點
n <- nrow(model_data)
train_end <- floor(n * 0.7)
val_end <- floor(n * 0.85)

# 按時間順序分割
train_data <- model_data[1:train_end, ]
val_data <- model_data[(train_end+1):val_end, ]
test_data <- model_data[(val_end+1):n, ]

# 顯示各資料集的時間範圍和樣本數
cat("訓練集時間範圍:", as.character(min(train_data$date)), "到", as.character(max(train_data$date)), 
    "，樣本數:", nrow(train_data), "\n")
cat("驗證集時間範圍:", as.character(min(val_data$date)), "到", as.character(max(val_data$date)), 
    "，樣本數:", nrow(val_data), "\n")
cat("測試集時間範圍:", as.character(min(test_data$date)), "到", as.character(max(test_data$date)), 
    "，樣本數:", nrow(test_data), "\n")

# 準備XGBoost的輸入資料
features <- c(
  "平均氣溫_w.3", "最高氣溫_w.3", "最低氣溫_w.3",
  "平均地溫5cm_w.3", "平均地溫10cm_w.3", "平均地溫20cm_w.3",
  "平均地溫50cm_w.3", "平均地溫100cm_w.3", "平均露點溫度_w.3"
) 

# 訓練集
X_train <- as.matrix(train_data[, features])
y_train <- train_data$`平均價_元每公斤`

# 驗證集
X_val <- as.matrix(val_data[, features])
y_val <- val_data$`平均價_元每公斤`

# 測試集
X_test <- as.matrix(test_data[, features])
y_test <- test_data$`平均價_元每公斤`

# 轉換為XGBoost的DMatrix格式
dtrain <- xgb.DMatrix(data = X_train, label = y_train)
dval <- xgb.DMatrix(data = X_val, label = y_val)
dtest <- xgb.DMatrix(data = X_test)

# 設定XGBoost參數
params <- list(
  objective = "reg:squarederror",
  eval_metric = "rmse",
  eta = 0.1,
  max_depth = 6,
  min_child_weight = 1,
  subsample = 0.8,
  colsample_bytree = 0.8
)

# 使用訓練集訓練模型，並監控驗證集表現
xgb_model <- xgb.train(
  params = params,
  data = dtrain,
  nrounds = 1000,
  watchlist = list(train = dtrain, validation = dval),
  early_stopping_rounds = 50,
  verbose = 1
)

# 獲取最佳迭代次數
best_iteration <- xgb_model$best_iteration
cat("\n最佳迭代次數:", best_iteration, "\n")

# 在測試集上進行預測
y_pred <- predict(xgb_model, X_test)

# 計算評估指標
rmse <- sqrt(mean((y_pred - y_test)^2))
mae <- mean(abs(y_pred - y_test))
r_squared <- 1 - sum((y_test - y_pred)^2) / sum((y_test - mean(y_test))^2)

# 印出評估結果
cat("\n測試集評估結果：\n")
cat("RMSE：", rmse, "\n")
cat("MAE：", mae, "\n")
cat("R-squared：", r_squared, "\n")

# 將預測結果與實際值合併以進行比較和繪圖
results <- data.frame(
  date = test_data$date,
  actual = y_test,
  predicted = y_pred
)

# 印出實際值與預測值的比較結果
cat("\n實際值與預測值比較(前10筆)：\n")
comparison <- head(results, 10)
print(comparison)

# 計算預測誤差
results$error <- results$predicted - results$actual
results$abs_error <- abs(results$error)
results$error_percent <- (results$error / results$actual) * 100

# 輸出誤差統計資訊
cat("\n誤差統計：\n")
cat("平均絕對誤差 (MAE):", mean(results$abs_error), "\n")
cat("平均絕對百分比誤差 (MAPE):", mean(abs(results$error_percent)), "%\n")
cat("最大誤差:", max(results$abs_error), "\n")
cat("最小誤差:", min(results$abs_error), "\n")

layout(matrix(c(1, 2), nrow = 1), widths = c(5, 1))  
par(mar = c(5, 4, 4, 2))
ts.plot(c(tail(y_val, 29), rep(NA,nrow(test_data))), ylim=c(0,150), ylab='price', main = "「648 西螺鎮」市場價格預測")
lines(29+(1:nrow(test_data)), results$predicted, col=2, lty = 2)
lines(29+(1:nrow(test_data)), results$actual, col=4)
par(mar = c(0, 0, 2, 0))
plot.new() 
legend("center", inset = c(0, 0), xpd = TRUE,
       legend = c("Forecast", "Actual"),
       col = c(2, 4), lty = c(2, 1), bty = "n")
dev.off()
