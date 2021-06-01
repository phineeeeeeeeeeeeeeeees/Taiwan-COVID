#####################################################################################################
# Taiwan-COVID
# Data dashboard: UI
# 2021-06-01
#####################################################################################################

# =====================================
# required packages
# =====================================
library(shiny)
library(shinydashboard)
library(sf)
library(dplyr) ; library(tidyr) 
library(ggplot2) ; library(plotly) ; library(leaflet) ; library(ggsci)
library(lubridate) ; library(stringr)

# =====================================
# UI
# =====================================
header <- dashboardHeader(
    title = "嚴重特殊傳染性肺炎(COVID-19)資料平台" , titleWidth = 400
)

body <- dashboardBody(
    fluidRow(
        # left graph panel
        column(width = 8,
               # map box (COVID map)
               box(width = NULL, status = "warning" , 
                   valueBox(value = textOutput("summary_total_confirmed") , 
                            subtitle = "累計確診" , 
                            width = 3 , 
                            icon = icon("stethoscope") , 
                            color = "light-blue") , 
                   valueBox(value = textOutput("summary_yesterday_confirmed") , 
                            subtitle = "昨日確診" , 
                            width = 3 , 
                            icon = icon("ambulance") , 
                            color = "light-blue") , 
                   valueBox(value = textOutput("summary_total_mortality") , 
                            subtitle = "累計死亡" , 
                            width = 3 , 
                            icon = icon("biohazard") , 
                            color = "light-blue") , 
                   valueBox(value = textOutput("summary_total_recovered") , 
                            subtitle = "解除隔離" , 
                            width = 3 , 
                            icon = icon("walking") , 
                            color = "light-blue") ) , 
               box(width = NULL, solidHeader = TRUE,
                   # map
                   leafletOutput("map_covid", height = 450) , 
                   # control panel 
                   # select variable: 7-day incidence/ accumulated cases/ num. new cases
                   # select date
                   absolutePanel(class = "panel panel-default" , 
                                 top = "auto" , left = 15 , right = "auto" , bottom = 30 , 
                                 width = 200 , height = "auto" , 
                                 p(strong("地圖瀏覽設定")) , 
                                 selectInput("adm_level" , "選擇行政區層級" , 
                                             choices = c("縣市" = "adm2" , "鄉鎮市區" = "adm3") , 
                                             selected = c("鄉鎮市區" = "adm3")) ,
                                 radioButtons("select_variable" , "選擇疫情指標" , 
                                              choices = c("近七日發生率(每十萬人)" = "incidence" , 
                                                          "累計確診數(每十萬人)" = "totalcase") , 
                                              selected = c("近七日發生率(每十萬人)" = "incidence")) , 
                                 p(class = "text-muted" , "以上二指標皆只適用本土案例") , 
                                 dateInput("select_date" , "選擇近七日發生率計算日期" , 
                                           language = "zh-TW" , 
                                           value = as_date(Sys.Date())-2 , 
                                           min = as_date("2020-02-01") , 
                                           max = as_date(Sys.Date())-1))
               ),
               column(width = 6 , 
                      box(width = NULL, status = "warning" , 
                          p(strong("點擊地圖中的行政區顯示更多資訊")) , 
                          plotlyOutput("plot_curve_selected_area" , height = 300) , 
                          tableHTML::tableHTML_output("table_selected_area") 
                      )) , 
               column(width = 6 , 
                      box(width = NULL, status = "warning" , 
                          p(strong("行政區依七日發生率排序")) , 
                          textOutput("table_incidence_date") , 
                          tableOutput("table_incidence" )
                      ))
        ),
        # right panel
        column(width = 4,
               # box1: graph of total cases
               box(
                   width = NULL, status = "warning" , 
                   p(strong("全台疫情概況")) , 
                   plotlyOutput("plot_total_curve" , height = 300) , 
                   plotlyOutput("plot_age_gender" , height = 300) , 
                   plotlyOutput("plot_tested" , height = 300) , 
                   tableOutput("table_summary") 
               )
        # end of column
        )
    ) , # end of fluidRow 
    hr() , 
    p(class = "text-muted",
      "確診資料及檢驗資料來源: 疾病管制署 (data.cdc.gov.tw/dataset/agsdctable-day-19cov, data.cdc.gov.tw/dataset/daily-cases-suspected-sars-cov-2-infection_tested)" ) , 
    p(class = "text-muted" , 
      "行政區人口數資料來源: 內政部2021年4月鄉鎮戶數及人口數(ris.gov.tw/app/portal/346)") , 
    p(class = "text-muted" , "Author: Tze-Li Liu (若有建議或疑問歡迎不吝指教 tze-li.liu@swisstph.ch)")
) # end of dashboardBody

# =====================================
# compile
# =====================================
dashboardPage(
    header,
    dashboardSidebar(disable = TRUE),
    body 
)