# --- 花椰菜價格預測（Ranger隨機森林模型） ---
library(tidyverse)
library(lubridate)
library(here)
library(ranger)
library(Metrics)
library(zoo)

# 創建資料夾
dirs <- c("data_processed", "plots", "plots/predictions", "plots/predictions/cauliflower", "models_saved")
for(dir in dirs) {
  if(!dir.exists(here(dir))) dir.create(here(dir), recursive = TRUE)
}

# --- 載入與處理數據 ---
cauliflower_daily_raw <- read_csv(here("vegdata", "花椰菜 青梗.csv"), locale = locale(encoding = "UTF-8"))
weather_daily_raw <- read_csv(here("weatherdata", "daily_weather.csv"), locale = locale(encoding = "UTF-8"))

# 處理花椰菜數據
cauliflower_processed <- cauliflower_daily_raw %>%
  mutate(
    market_code = str_extract(`市場`, "^[0-9A-Za-z]+"),
    market_name = str_replace(`市場`, "^[0-9A-Za-z]+\\s*", ""),
    crop_code_combined = str_extract(`產品`, "^[^\\s]+"),
    crop_name_full = str_replace(`產品`, "^[^\\s]+\\s*", "")
  ) %>%
  filter(str_detect(crop_name_full, "花椰菜") & str_detect(crop_name_full, "青梗")) %>%
  mutate(crop_name_standard = "花椰菜 青梗")

# 清理數據
cauliflower_cleaned <- cauliflower_processed %>%
  rename(
    original_date_str = `日期`,
    price_upper = `上價`,
    price_middle = `中價`,
    price_lower = `下價`,
    avg_price = `平均價(元/公斤)`,
    transaction_volume = `交易量(公斤)`
  ) %>%
  mutate(crop_name = crop_name_standard)

# 日期處理
cauliflower_cleaned <- cauliflower_cleaned %>%
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

# 數據類型轉換
cauliflower_cleaned <- cauliflower_cleaned %>%
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
unique_markets <- unique(cauliflower_cleaned$market_name)
cat("數據集中的花椰菜市場:", paste(unique_markets, collapse=", "), "\n")

# 檢查每個市場的數據量
market_counts <- cauliflower_cleaned %>% 
  count(market_name) %>% 
  arrange(desc(n))
print(market_counts)

# 清理天氣數據
weather_cleaned <- weather_daily_raw %>%
  rename(date = `觀測時間`) %>%
  select(
    date,
    `平均氣溫(℃)`, `最高氣溫(℃)`, `最低氣溫(℃)`,
    `平均相對溼度(%)`, `最低相對溼度(%)`,
    `累計雨量(mm)`,
    matches("日照時數"),
    `平均風速(m/s)`
  ) %>%
  rename_with(
    ~ str_replace_all(., c("\\(℃\\)" = "_c", "\\(%\\)" = "_pct",
                           "\\(mm\\)" = "_mm", "\\(m/s\\)" = "_mps",
                           "平均氣溫" = "temp_avg", "最高氣溫" = "temp_max", "最低氣溫" = "temp_min",
                           "平均相對溼度" = "rh_avg", "最低相對溼度" = "rh_min",
                           "累計雨量" = "precip_accumulated"))
  )

# 合併數據
merged_data <- left_join(cauliflower_cleaned, weather_cleaned, by = "date")

# --- 特徵工程與模型訓練 ---
# 修改為包含所有五個市場
target_markets <- c("台中市", "豐原區", "永靖鄉", "溪湖鎮", "西螺鎮")
model_results_list <- list()

# 檢查目標市場是否存在於數據集中
missing_markets <- setdiff(target_markets, unique_markets)
if(length(missing_markets) > 0) {
  cat("警告: 以下市場在花椰菜數據中不存在:", paste(missing_markets, collapse=", "), "\n")
}

for (market_n in target_markets) {
  cat(paste("\n處理市場:", market_n, "\n"))
  
  # 篩選市場數據
  market_data <- merged_data %>%
    filter(market_name == market_n) %>%
    arrange(date)
  
  # 檢查數據量
  if(nrow(market_data) == 0) {
    cat("警告: 無法找到", market_n, "的花椰菜數據，跳過處理\n")
    next
  }
  
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
      avg_price_lag_30 = lag(avg_price, 30),
      temp_avg_c_lag_7 = lag(temp_avg_c, 7),
      rh_avg_pct_lag_7 = lag(rh_avg_pct, 7),
      precip_accumulated_mm_lag_7 = lag(precip_accumulated_mm, 7),
      avg_price_roll_mean_7 = rollmean(avg_price, k=7, fill=NA, align="right"),
      avg_price_roll_mean_14 = rollmean(avg_price, k=14, fill=NA, align="right"),
      avg_price_roll_sd_7 = rollapply(avg_price, width=7, FUN=sd, fill=NA, align="right")
    )
  
  # 創建對數價格
  market_data <- market_data %>%
    mutate(log_avg_price = ifelse(avg_price > 0, log(avg_price), NA))
  
  # 處理NA值
  market_data_na_handled <- market_data %>%
    filter(!is.na(avg_price)) # 僅移除價格為NA的行
  
  # 處理關鍵特徵的NA值
  market_data_na_handled <- market_data_na_handled %>%
    mutate(across(contains(c("_lag_", "_roll_", "temp", "rh", "precip")), 
                  ~ifelse(is.na(.), median(., na.rm=TRUE), .)))
  
  # 檢查數據充足性
  if (nrow(market_data_na_handled) < 50) {
    cat(paste("警告: 處理NA後，市場", market_n, "的花椰菜數據不足（僅有", nrow(market_data_na_handled), "行）\n"))
    cat("跳過此市場並繼續\n")
    next
  }
  
  # 分割數據
  train_ratio <- 0.8
  train_size <- floor(train_ratio * nrow(market_data_na_handled))
  train_data <- market_data_na_handled[1:train_size, ]
  test_data <- market_data_na_handled[(train_size + 1):nrow(market_data_na_handled), ]
  
  cat("訓練集行數:", nrow(train_data), ", 測試集行數:", nrow(test_data), "\n")
  
  # 花椰菜的特徵集
  model_features <- c(
    "year", "month", "day_of_week", "is_weekend", 
    "avg_price_lag_1", "avg_price_lag_7", "avg_price_lag_14", "avg_price_lag_30",
    "avg_price_roll_mean_7", "avg_price_roll_mean_14", "avg_price_roll_sd_7",
    "temp_avg_c", "temp_max_c", "temp_min_c", "temp_avg_c_lag_7",
    "rh_avg_pct", "rh_min_pct", "rh_avg_pct_lag_7",
    "precip_accumulated_mm", "precip_accumulated_mm_lag_7"
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
  
  # 繪製預測圖
  market_code <- unique(as.character(train_data$market_code))[1]
  
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
          main = paste0("「", market_code, " ", market_n, "」市場花椰菜價格預測"))
  
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
  dev.copy(png, filename = here("plots/predictions/cauliflower", paste0(market_code, "市場花椰菜價格預測_ranger.png")),
           width = 800, height = 500, res = 100)
  dev.off()
  
  # 特徵重要性視覺化
  png(filename = here("plots/predictions/cauliflower", paste0(market_code, "市場花椰菜特徵重要性.png")),
      width = 800, height = 500, res = 100)
  
  par(mar = c(5, 10, 4, 2))  # 調整邊距以容納長特徵名稱
  barplot(top_features$Importance, 
          names.arg = top_features$Feature,
          horiz = TRUE, 
          las = 1,
          main = paste0(market_n, " 花椰菜價格預測特徵重要性"),
          xlab = "重要性分數")
  
  dev.off()
  
  cat(paste("預測圖和特徵重要性圖已儲存:", market_n, "\n"))
}

# 儲存模型結果
saveRDS(model_results_list, file = here("models_saved", "cauliflower_model_results.rds"))
saveRDS(merged_data, file = here("data_processed", "cauliflower_merged_data.rds"))

# 生成模型比較表格
comparison_data <- data.frame(
  Market = character(0),
  RMSE = numeric(0),
  MAE = numeric(0),
  MAPE = numeric(0)
)

for (market_n in names(model_results_list)) {
  if (!is.null(model_results_list[[market_n]][["ranger"]])) {
    comparison_data <- rbind(comparison_data, data.frame(
      Market = market_n,
      RMSE = round(model_results_list[[market_n]][["ranger"]]$rmse, 2),
      MAE = round(model_results_list[[market_n]][["ranger"]]$mae, 2),
      MAPE = round(model_results_list[[market_n]][["ranger"]]$mape, 2)
    ))
  }
}

# 保存比較表格
if (nrow(comparison_data) > 0) {
  write.csv(comparison_data, file = here("models_saved", "cauliflower_model_comparison.csv"), row.names = FALSE)
  cat("\n花椰菜各市場預測結果比較:\n")
  print(comparison_data)
}

cat("\n花椰菜價格預測模型訓練和預測圖生成完成！\n")