#####################################################################################################
# Taiwan-COVID
# Data dashboard: UI
# 2021-06-01
#####################################################################################################

# =====================================
# required packages
# =====================================
library(shiny)
library(shinydashboard) ; library(shinycssloaders)
library(sf)
library(dplyr) ; library(tidyr) 
library(ggplot2) ; library(plotly) ; library(leaflet) ; library(ggsci)
library(lubridate) ; library(stringr)
library(tabulizer) ; library(rvest)


# =====================================
# UI
# =====================================
# Options for Spinner
options(spinner.color="#0275D8", spinner.color.background="#ffffff", spinner.size=2)

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
                   withSpinner(leafletOutput("map_covid", height = 450) , type = 2) , 
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
                                                          "累計確診數(每十萬人)" = "totalcase" , 
                                                          "單日新增確診數" = "newcases") , 
                                              selected = c("近七日發生率(每十萬人)" = "incidence")) , 
                                 p(class = "text-muted" , "以上三指標皆只適用本土案例") , 
                                 dateInput("select_date" , "指定特定日期資料" , 
                                           language = "zh-TW" , 
                                           value = as_date(Sys.Date())-2 , 
                                           min = as_date("2020-02-01") , 
                                           max = as_date(Sys.Date())-1))
               ),
               column(width = 6 , 
                      # box: detail of the selected adm
                      box(width = NULL, status = "warning" , 
                          p(strong("點選地圖中的行政區可顯示該行政區之詳細資訊")) , 
                          plotlyOutput("plot_curve_selected_area" , height = 300) ,
                          br() , 
                          tableOutput("table_selected_area") , 
                          p(class = "text-muted" , "（單日新增確診之日期為地圖設定所選日期）") ,
                          br() ), 
                      # box: data source
                      box(width = NULL, status = "warning" , 
                          p(strong("資料來源")) , 
                          data.frame(
                            source = c("https://data.cdc.gov.tw/dataset/covid19_tw__stats" ,
                                       "https://data.cdc.gov.tw/dataset/agsdctable-day-19cov" , 
                                       "https://data.cdc.gov.tw/dataset/daily-cases-suspected-sars-cov-2-infection_tested" , 
                                       "https://www.cdc.gov.tw/Category/Page/9jFXNbCe-sFK9EImRRi2Og" , 
                                       "ris.gov.tw/app/portal/346") , 
                            row.names = c("COVID-19台灣最新病例與檢驗統計" , 
                                          "地區年齡性別統計表" , 
                                          "台灣COVID-19冠狀病毒檢測每日送驗數" , 
                                          "COVID-19疫苗統計資料" , 
                                          "內政部2021年4月鄉鎮戶數及人口數")
                          ) %>% tableHTML::tableHTML() , 
                          br() , 
                          p(class = "text-muted" , "資料僅供參考 正式數據皆以疾管署為準") , 
                          br()
                          )
                      ) , 
               column(width = 6 , 
                      # box: adm by incidence
                      box(width = NULL, status = "warning" , 
                          p(strong("行政區依七日發生率排序")) ,
                          p(class = "text-muted" , "(近七日每十萬人確診人數)") , 
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
                   withSpinner(plotlyOutput("plot_total_curve" , height = 270) , type = 2) ,
                   selectInput("select_total_curve_variable" , label = NULL , 
                               choices = c("每日新增確診數(境外移入/本土個案)" = "total" , 
                                           "每日確診數七日移動平均" = "movingavg") , 
                               selected = c("每日新增確診數(境外移入/本土個案)" = "total")) , 
                   plotlyOutput("plot_age_gender" , height = 270) , 
                   plotlyOutput("plot_tested" , height = 270) , 
                   tableOutput("table_summary") 
               ) , 
               box(
                 width = NULL, status = "warning" ,
                 p(strong("全台疫苗接種現況")) , 
                 valueBox(value = textOutput("vaccinated_total") , 
                          subtitle = "累計接種" , 
                          width = 6 , 
                          icon = icon("syringe") , 
                          color = "light-blue") , 
                 valueBox(value = textOutput("vaccinated_today") , 
                          subtitle = "本日接種" , 
                          width = 6 , 
                          icon = icon("syringe") , 
                          color = "light-blue")
               ) , 
               box(
                 width = NULL, status = "warning" , 
                 p(strong("各縣市疫苗接種情形")) , 
                 selectInput("vaccination_variable" , label = NULL , 
                             choices = c("累計接種人次" = "vaccinated" , "累計配送劑數" = "delivered") , 
                             selected = c("累計接種人次" = "vaccinated")), 
                 withSpinner(plotlyOutput("plot_vaccination" , height = 400) , type = 2) , 
                 textOutput("vaccine_date_update")
               )
        # end of column
        )
    ) , # end of fluidRow 
    hr() , 
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