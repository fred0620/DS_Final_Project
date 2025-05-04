# Data Science Final Project: 台灣蔬菜價格與天氣資料分析
---

## 📊 Data Collection

### 農產品價格資料

* **資料來源**：農產品批發市場交易行情站
  🔗 [https://amis.afa.gov.tw/veg/VegProdDayTransInfo.aspx](https://amis.afa.gov.tw/veg/VegProdDayTransInfo.aspx)
* **收集期間**：2015/01/01 \~ 2025/04/30
* **收集品項**：花椰菜（青梗）、茼蒿、甜豌豆

| 品種  | 日資料筆數  | 週資料筆數 |
| --- | ------ | ----- |
| 花椰菜 | 14,983 | 764   |
| 茼蒿  | 4,091  | 764   | 
| 甜豌豆 | 4,228  | 764   |

---

### 天氣觀測資料

* **資料來源**：農業氣象觀測網監測系統
  🔗 [https://agr.cwa.gov.tw/history/station\_day](https://agr.cwa.gov.tw/history/station_day)
* **收集期間**：2014/10/01 \~ 2025/04/30

#### 對應蔬菜與觀測站

| 品種  | 觀測站地點               | 生育日數 |
| --- | ----------------------- | ------|
| 花椰菜 | 台中農改、 南改斗南分場  | 60 天 |
| 茼蒿  | 台中農改、 南改斗南分場   | 45 天 |
| 甜豌豆 | 台中農改、 南改斗南分場  | 90 天 |

---

### 產地與觀測站分布圖（嵌入 Google Map）

📍 [點此查看地理分布互動地圖](https://www.google.com/maps/d/u/0/edit?mid=1ReIEOk9rDv4Jogp6OP7GNVv825XCBh0&usp=sharing)

---

### 參考資料來源

| 主題         | 連結                                                                                        |
| ---------- | ----------------------------------------------------------------------------------------- |
| 食農教育資訊整合平台 | [https://fae.moa.gov.tw/map/county\_agri.php](https://fae.moa.gov.tw/map/county_agri.php) |
| 花椰菜產地說明    | [台中區農改場](https://www.tcdares.gov.tw/theme_data.php?theme=news&sub_theme=event&id=13643)   |
| 茼蒿產地說明     | [美食網](https://food.ltn.com.tw/article/1282)                                  |
| 豌豆產地報導     | [農傳媒](https://www.agriharvest.tw/archives/73963)                                          |

---

##  Data Integration

### 整合方法（Method）
