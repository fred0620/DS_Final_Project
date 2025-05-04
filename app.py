#大家可以用這個看一下天氣資料狀況，也幫助你們比較好分析

import streamlit as st
import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt
from scipy.stats import kurtosis, skew
import chardet
import matplotlib.pyplot as plt
import matplotlib.font_manager as fm

plt.rcParams['font.family'] = 'Noto Sans CJK TC'
plt.rcParams['axes.unicode_minus'] = False


columns = [
    "測站代碼", "觀測時間", "平均氣壓(hPa)", "海平面氣壓(hPa)", "日最高氣壓(hPa)", "最高氣壓時間",
    "日最低氣壓(hPa)", "最低氣壓時間", "平均氣溫(℃)", "最高氣溫(℃)", "最高氣溫時間",
    "最低氣溫(℃)", "最低氣溫時間", "平均露點溫度(℃)", "平均相對溼度(%)", "最低相對溼度(%)",
    "最低相對溼度時間", "平均風速(m/s)", "平均風向(°)", "最大陣風風速(m/s)", "最大陣風風向(°)",
    "最大陣風時間", "累計雨量(mm)", "最大10分鐘降水量(mm)", "最大10分鐘降水量起始時間",
    "最大60分鐘降水量(mm)", "最大60分鐘降水量起始時間", "累積日照時數(hr)", "累積日射量(MJ/m2)",
    "累積蒸發量(mm)", "平均地溫0cm(℃)", "平均地溫5cm(℃)", "平均地溫10cm(℃)",
    "平均地溫20cm(℃)", "平均地溫50cm(℃)", "平均地溫100cm(℃)"
]

# 模擬資料讀取（換成你的 CSV 即可）
with open('weatherdata/雲林天氣.csv', 'rb') as f:
    result = chardet.detect(f.read())
print(result)  # 印出編碼類型


@st.cache_data
def load_data():
    df = pd.read_csv(
    'weatherdata/雲林天氣.csv',
    encoding=result['encoding'],
    usecols=range(36),
    names=columns,
    header=0 
)
    return df

df = load_data()
time_col = "觀測時間"

st.title("📊 氣象資料互動報告")

# 下拉式選單選欄位
columns = [col for col in df.columns if col != time_col]
selected_col = st.selectbox("請選擇欄位進行分析", columns)

series = df[selected_col]

if pd.api.types.is_numeric_dtype(series):
    st.subheader("📌 數值欄位統計")
    st.write({
        "資料列": len(series),
        "相異值": series.nunique(),
        "遺漏值": series.isna().sum(),
        "平均值": series.mean(),
        "標準差": series.std(),
        "下限": series.min(),
        "25% 分位數": series.quantile(0.25),
        "中位數": series.median(),
        "75% 分位數": series.quantile(0.75),
        "上限": series.max(),
        "峰態": kurtosis(series.dropna()),
        "扭曲": skew(series.dropna())
    })

    # 畫圖
    fig, axes = plt.subplots(1, 2, figsize=(12, 4))
    sns.histplot(series.dropna(), bins=30, ax=axes[0], kde=True)
    axes[0].set_title("直方圖")

    if time_col in df.columns:
        axes[1].plot(df[time_col], series)
        axes[1].set_title("時間序列")
        axes[1].tick_params(axis='x', rotation=45)
    
    st.pyplot(fig)

else:
    st.subheader("📌 類別欄位統計")
    mode = series.mode().iloc[0]
    freq = series.value_counts().iloc[0]
    st.write({
        "資料列": len(series),
        "相異值": series.nunique(),
        "遺漏值": series.isna().sum(),
        "最常用的值": mode,
        "頻率": freq
    })
