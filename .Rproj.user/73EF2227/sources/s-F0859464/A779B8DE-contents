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
library(leaflet)

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
               box(width = NULL, solidHeader = TRUE,
                   leafletOutput("map_covid", height = 500)
               ),
               # control panel 
               # select variable: 7-day incidence/ accumulated cases/ num. new cases
               # select date
               column(width = 6 , 
                      box(width = NULL, status = "warning",
                          p(strong("地圖瀏覽設定")) , 
                          selectInput("adm_level" , "選擇行政區層級" , 
                                      choices = c("縣市" = "adm2" , "鄉鎮市區" = "adm3") , 
                                      selected = c("鄉鎮市區" = "adm3")) ,
                          radioButtons("select_variable" , "選擇疫情指標" , 
                                       choices = c("近七日發生率(每十萬人)" = "incidence" , 
                                                   "累計確診數(每十萬人)" = "totalcase") , 
                                       selected = c("近七日發生率(每十萬人)" = "incidence")) , 
                          p(class = "text-muted" , "註：以上二指標皆只適用本土案例") , 
                          dateInput("select_date" , "選擇顯示日期(發生率)" , 
                                    value = as_date(Sys.Date())-1 , 
                                    min = as_date("2020-02-01") , 
                                    max = as_date(Sys.Date())-1)
                      )) , 
               column(width = 6 , 
                      box(width = NULL, status = "warning" , 
                          p(strong("點擊地圖中的行政區顯示更多資訊")) , 
                          plotOutput("plot_curve_selected_area" , height = 200) , 
                          tableHTML::tableHTML_output("table_selected_area")
                      ))
        ),
        # right panel
        column(width = 4,
               # box1: graph of total cases
               box(
                   width = NULL, status = "warning" , 
                   p(strong("全台疫情概況")) , 
                   plotlyOutput("plot_total_curve" , height = 250) , 
                   plotlyOutput("plot_age_gender" , height = 300)
               ) , 
               # box 2
               box(width = NULL, status = "warning" , 
                   p(strong("行政區依七日發生率")) , 
                   textOutput("table_incidence_date") , 
                   tableOutput("table_incidence" )
               ) # end of box
        # end of column
        )
    ) , # end of fluidRow 
    p(class = "text-muted",
      paste("資料來源: 疾病管制署" , 
            "(https://data.cdc.gov.tw/dataset/agsdctable-day-19cov)") )
) # end of dashboardBody

# =====================================
# compile
# =====================================
dashboardPage(
    header,
    dashboardSidebar(disable = TRUE),
    body 
)