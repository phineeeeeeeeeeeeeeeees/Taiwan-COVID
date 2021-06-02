<img src="resources/covid-icon.jpg/covid_icon.jpg" width = "50">

# Taiwan-COVID-Dashboard
An R-Shiny based online application for up-to-date data aggregation and visualization of the COVID-19 epidemic in Taiwan based on government open data. 
整合政府開放資料，提供嚴重特殊傳染性肺炎(COVID-19)台灣疫情即時資訊整理與視覺化

### Link to the dashboard: 
https://tzeliliu.shinyapps.io/Taiwan-COVID19-dashboard/

### This GitHub Repo contains the source code to retrieve, analyze, and visualize the data

* `ui.R`: the user interface of the Shiny website
* `server.R`: the calculation and visualization in the background
* `/data`: (part of) the data used in the application; the others are directly obtained to the original data source
* `get-vaccination-data.R`: find the link to download the daily vaccination report (PDF), decode PDF to csv plus a metadata of data version; `source()` in `server.R`.
* `shapefile_clean.R`: preprocess the shapefile (simplify)
* `populationXLS_clean.R`: clean the population data (from .xls to .csv)

### Data sources: 
* 衛福部疾管署：
	* [地區年齡性別統計表](https://data.cdc.gov.tw/dataset/agsdctable-day-19cov)
	* [台灣COVID-19冠狀病毒檢測每日送驗數](https://data.cdc.gov.tw/dataset/daily-cases-suspected-sars-cov-2-infection_tested)
	* [COVID-19疫苗統計資料](https://www.cdc.gov.tw/Category/Page/9jFXNbCe-sFK9EImRRi2Og)
* 內政部
	* [鄉鎮戶數及人口數(2021/04)](ris.gov.tw/app/portal/346)
	* [鄉鎮市區界線圖資](https://whgis.nlsc.gov.tw/Opendata/Files.aspx)