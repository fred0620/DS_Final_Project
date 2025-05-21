# --- 1. 環境設定 ---
library(tidyverse)
library(lubridate)
library(here)
library(ranger)
library(Metrics)
library(zoo)

# 創建資料夾
dirs <- c("data_processed", "plots", "plots/predictions", "models_saved")
for(dir in dirs) {
  if(!dir.exists(here(dir))) dir.create(here(dir), recursive = TRUE)
}

# --- 2. 載入與處理數據 ---
tong_ho_daily_raw <- read_csv(here("vegdata", "茼蒿.csv"), locale = locale(encoding = "UTF-8"))
weather_daily_raw <- read_csv(here("weatherdata", "daily_weather.csv"), locale = locale(encoding = "UTF-8"))

# 處理茼蒿數據
tong_ho_processed <- tong_ho_daily_raw %>%
  mutate(
    market_code = str_extract(`市場`, "^[0-9A-Za-z]+"),
    market_name = str_replace(`市場`, "^[0-9A-Za-z]+\\s*", ""),
    crop_code_combined = str_extract(`產品`, "^[^\\s]+"),
    crop_name_full = str_replace(`產品`, "^[^\\s]+\\s*", "")
  ) %>%
  filter(str_detect(crop_name_full, "茼蒿")) %>%
  mutate(crop_name_standard = "茼蒿")

# 清理數據
tong_ho_cleaned <- tong_ho_processed %>%
  rename(
    original_date_str = `日期`,
    price_upper = `上價`,
    price_middle = `中價`,
    price_lower = `下價`,
    avg_price = `平均價(元/公斤)`,
    transaction_volume = `交易量(公斤)`
  ) %>%
  mutate(crop_name = crop_name_standard)

# 更健壯的日期處理
tong_ho_cleaned <- tong_ho_cleaned %>%
  mutate(
    year_roc_str = str_sub(original_date_str, 1, str_locate(original_date_str, "/")[,1] - 1),
    month_day_str = str_sub(original_date_str, str_locate(original_date_str, "/")[,1] + 1),
    year_roc = as.integer(year_roc_str),
    year_ad = year_roc + 1911,
    month_str = str_sub(month_day_str, 1, str_locate(month_day_str, "/")[,1] - 1),
    day_str = str_sub(month_day_str, str_locate(month_day_str, "/")[,1] + 1),
    month = as.integer(month_str),
    day = as.integer(day_str),
    date = ymd(paste(year_ad, sprintf("%02d", month), sprintf("%02d", day), sep="-"), quiet = TRUE)
  )

# 檢查日期NA值
na_dates <- sum(is.na(tong_ho_cleaned$date))
if(na_dates > 0) {
  cat("警告：日期欄位有", na_dates, "個NA值\n")
}

# 更詳細的數據類型轉換
tong_ho_cleaned <- tong_ho_cleaned %>%
  mutate(
    avg_price = as.numeric(avg_price),
    transaction_volume = as.numeric(transaction_volume),
    price_upper = as.numeric(price_upper),
    price_middle = as.numeric(price_middle),
    price_lower = as.numeric(price_lower),
    market_code = as.factor(market_code),
    market_name = as.factor(market_name),
    crop_name = as.factor(crop_name)
  ) %>%
  distinct(date, market_name, .keep_all = TRUE)

# 市場檢查
unique_markets <- unique(tong_ho_cleaned$market_name)
cat("數據集中的市場:", paste(unique_markets, collapse=", "), "\n")

# 清理天氣數據
weather_cleaned <- weather_daily_raw %>%
  rename(date = `觀測時間`) %>%
  select(
    date,
    `平均氣溫(℃)`, `最高氣溫(℃)`, `最低氣溫(℃)`,
    `平均相對溼度(%)`, `最低相對溼度(%)`,
    `累計雨量(mm)`,
    matches("日照時數"),
    `平均風速(m/s)`,
    # 為茼蒿添加可能相關的額外地溫數據
    `平均地溫0cm(℃)`, `平均地溫5cm(℃)`, `平均地溫10cm(℃)`
  ) %>%
  rename_with(
    ~ str_replace_all(., c("\\(℃\\)" = "_c", "\\(%\\)" = "_pct",
                           "\\(mm\\)" = "_mm", "\\(m/s\\)" = "_mps",
                           "平均氣溫" = "temp_avg", "最高氣溫" = "temp_max", "最低氣溫" = "temp_min",
                           "平均相對溼度" = "rh_avg", "最低相對溼度" = "rh_min",
                           "累計雨量" = "precip_accumulated",
                           "平均地溫0cm" = "soil_temp_0cm", "平均地溫5cm" = "soil_temp_5cm", 
                           "平均地溫10cm" = "soil_temp_10cm"))
  )

# 合併數據
merged_data <- left_join(tong_ho_cleaned, weather_cleaned, by = "date")

# --- 3. 探索性分析與特徵工程 ---
# 查看主要市場的數據量
market_counts <- table(tong_ho_cleaned$market_name)
cat("各市場數據量:\n")
print(market_counts)

# 明確指定台中、豐原與西螺三個市場
target_markets <- c("台中市", "豐原區", "西螺鎮")
cat("將使用以下市場進行預測:", paste(target_markets, collapse=", "), "\n")

# 檢查指定市場是否存在於數據中
missing_markets <- setdiff(target_markets, unique(tong_ho_cleaned$market_name))
if(length(missing_markets) > 0) {
  cat("警告: 以下市場在數據中不存在:", paste(missing_markets, collapse=", "), "\n")
  target_markets <- intersect(target_markets, unique(tong_ho_cleaned$market_name))
  cat("修正後的目標市場列表:", paste(target_markets, collapse=", "), "\n")
}

model_results_list <- list()

for (market_n in target_markets) {
  cat(paste("\n處理市場:", market_n, "\n"))
  
  # 篩選市場數據
  market_data <- merged_data %>%
    filter(market_name == market_n) %>%
    arrange(date)
  
  # 基本統計資訊
  cat("市場數據行數:", nrow(market_data), "\n")
  cat("日期範圍:", min(market_data$date, na.rm=TRUE), "到", max(market_data$date, na.rm=TRUE), "\n")
  
  # 添加特徵
  market_data <- market_data %>%
    mutate(
      year = year(date),
      month = month(date),
      day_of_month = mday(date),
      day_of_week = wday(date, label = FALSE, week_start = 1),
      day_of_year = yday(date),
      week_of_year = isoweek(date),
      quarter = quarter(date),
      is_weekend = ifelse(day_of_week %in% c(6, 7), 1, 0)
    )
  
  # 添加滞后特徵和滾動統計
  market_data <- market_data %>%
    arrange(date) %>%
    mutate(
      avg_price_lag_1 = lag(avg_price, 1),
      avg_price_lag_7 = lag(avg_price, 7),
      avg_price_lag_14 = lag(avg_price, 14),
      avg_price_lag_30 = lag(avg_price, 30),  # 特別為茼蒿添加的30天滞后
      temp_avg_c_lag_7 = lag(temp_avg_c, 7),
      rh_avg_pct_lag_7 = lag(rh_avg_pct, 7),
      precip_accumulated_mm_lag_7 = lag(precip_accumulated_mm, 7),  # 為茼蒿額外添加降雨滞后
      soil_temp_5cm_c_lag_7 = lag(soil_temp_5cm_c, 7),  # 為茼蒿添加地溫滞后
      avg_price_roll_mean_7 = rollmean(avg_price, k=7, fill=NA, align="right"),
      avg_price_roll_mean_14 = rollmean(avg_price, k=14, fill=NA, align="right"),
      avg_price_roll_sd_7 = rollapply(avg_price, width=7, FUN=sd, fill=NA, align="right")  # 價格波動特徵
    )
  
  # 創建對數價格
  market_data <- market_data %>%
    mutate(log_avg_price = ifelse(avg_price > 0, log(avg_price), NA))
  
  # 處理NA值
  market_data_na_handled <- market_data %>%
    filter(!is.na(avg_price)) # 僅移除價格為NA的行
  
  # 處理關鍵特徵的NA值
  market_data_na_handled <- market_data_na_handled %>%
    mutate(across(contains(c("_lag_", "_roll_", "temp", "rh", "precip", "soil")), 
                  ~ifelse(is.na(.), median(., na.rm=TRUE), .)))
  
  # 檢查數據充足性
  if (nrow(market_data_na_handled) < 50) {
    cat(paste("警告: 處理NA後，市場", market_n, "的數據不足（僅有", nrow(market_data_na_handled), "行）\n"))
    cat("跳過此市場並繼續\n")
    next
  }
  
  # --- 4. 構建模型 ---
  # 分割數據
  train_ratio <- 0.8
  train_size <- floor(train_ratio * nrow(market_data_na_handled))
  train_data <- market_data_na_handled[1:train_size, ]
  test_data <- market_data_na_handled[(train_size + 1):nrow(market_data_na_handled), ]
  
  cat("訓練集行數:", nrow(train_data), ", 測試集行數:", nrow(test_data), "\n")
  
  # 茼蒿的初始特徵集 - 包含更多潜在相關的天氣和地溫特徵
  model_features <- c(
    "year", "month", "day_of_week", "is_weekend", 
    "avg_price_lag_1", "avg_price_lag_7", "avg_price_lag_14", "avg_price_lag_30",
    "avg_price_roll_mean_7", "avg_price_roll_mean_14", "avg_price_roll_sd_7",
    "temp_avg_c", "temp_max_c", "temp_min_c", "temp_avg_c_lag_7",
    "rh_avg_pct", "rh_min_pct", "rh_avg_pct_lag_7",
    "precip_accumulated_mm", "precip_accumulated_mm_lag_7",
    "soil_temp_5cm_c", "soil_temp_5cm_c_lag_7"
  )
  
  # 確保所有特徵存在
  missing_features <- setdiff(model_features, colnames(train_data))
  if (length(missing_features) > 0) {
    cat("警告: 缺少特徵:", paste(missing_features, collapse=", "), "\n")
    model_features <- intersect(model_features, colnames(train_data))
    cat("修改後的特徵列表:", paste(model_features, collapse=", "), "\n")
  }
  
  # 訓練Ranger模型
  ranger_model <- ranger(
    formula = avg_price ~ .,
    data = train_data %>% select(avg_price, all_of(model_features)),
    num.trees = 500,
    importance = 'permutation',
    seed = 123
  )
  
  # 預測
  test_data$ranger_pred_orig <- predict(ranger_model, data = test_data)$predictions
  
  # 評估
  rmse <- rmse(test_data$avg_price, test_data$ranger_pred_orig)
  mae <- mae(test_data$avg_price, test_data$ranger_pred_orig)
  mape <- mean(abs((test_data$avg_price - test_data$ranger_pred_orig) / test_data$avg_price), na.rm = TRUE) * 100
  
  cat(paste("Ranger模型 RMSE:", round(rmse, 2), "MAE:", round(mae, 2), "MAPE:", round(mape, 2), "%\n"))
  
  # 特徵重要性
  imp <- ranger::importance(ranger_model)
  importance_df <- data.frame(
    Feature = names(imp),
    Importance = imp
  ) %>% arrange(desc(Importance))
  
  top_features <- head(importance_df, 10)
  cat("最重要的10個特徵:\n")
  print(top_features)
  
  # 儲存結果
  model_results_list[[market_n]] <- list(
    ranger = list(
      model = ranger_model,
      importance = imp,
      rmse = rmse,
      mae = mae,
      mape = mape
    ),
    data_splits = list(
      train_data_for_plotting = train_data,
      test_data_with_all_preds = test_data
    )
  )
  
  # --- 5. 繪製預測圖 ---
  # 獲取市場代碼
  market_code <- unique(as.character(train_data$market_code))[1]
  
  # 繪製預測圖
  layout(matrix(c(1, 2), nrow = 1), widths = c(5, 1))
  par(mar = c(5, 4, 4, 2))
  
  # 僅選擇最近30個訓練數據點
  recent_train <- tail(train_data$avg_price, 30)
  
  # 設定Y軸範圍
  y_max <- max(c(recent_train, test_data$avg_price, test_data$ranger_pred_orig), na.rm=TRUE) * 1.1
  
  # 繪製基本圖形
  ts.plot(c(recent_train, rep(NA, nrow(test_data))), 
          ylim = c(0, y_max),
          ylab = '價格 (元/公斤)', 
          main = paste0("「", market_code, " ", market_n, "」市場茼蒿價格預測"))
  
  # 添加預測線和實際值線
  lines(length(recent_train) + (1:nrow(test_data)), test_data$ranger_pred_orig, col = "red", lty = 2, lwd = 1.5)
  lines(length(recent_train) + (1:nrow(test_data)), test_data$avg_price, col = "blue", lwd = 1.5)
  
  # 添加圖例
  par(mar = c(0, 0, 2, 0))
  plot.new()
  legend("center", inset = c(0, 0), xpd = TRUE,
         legend = c("預測價格", "實際價格"),
         col = c("red", "blue"), lty = c(2, 1), lwd = c(1.5, 1.5), bty = "n")
  
  # 保存圖片
  dev.copy(png, filename = here("plots/predictions", paste0(market_code, "市場茼蒿價格預測_ranger.png")),
           width = 800, height = 500, res = 100)
  dev.off()
  
  cat(paste("預測圖已儲存:", market_n, "\n"))
  
  # --- 6. 特徵重要性視覺化 ---
  # 繪製特徵重要性圖
  top_n_features <- 10
  importance_plot_data <- head(importance_df, top_n_features)
  
  png(filename = here("plots/predictions", paste0(market_code, "市場茼蒿特徵重要性.png")),
      width = 800, height = 500, res = 100)
  
  par(mar = c(5, 10, 4, 2))  # 調整邊距以容納長特徵名稱
  barplot(importance_plot_data$Importance, 
          names.arg = importance_plot_data$Feature,
          horiz = TRUE, 
          las = 1,
          main = paste0(market_n, " 茼蒿價格預測特徵重要性"),
          xlab = "重要性分數")
  
  dev.off()
  cat(paste("特徵重要性圖已儲存:", market_n, "\n"))
}

# 儲存模型結果和合併數據以供後續使用
saveRDS(model_results_list, file = here("models_saved", "tong_ho_model_results.rds"))
saveRDS(merged_data, file = here("data_processed", "tong_ho_merged_data.rds"))
cat("\n茼蒿價格預測模型訓練和預測圖生成完成！\n")