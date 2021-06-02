#####################################################################################################
# Taiwan-COVID
# Data dashboard: server
# 2021-06-01
#####################################################################################################

# =====================================
# required packages
# =====================================
library(shinydashboard)
library(sf)
library(dplyr) ; library(tidyr) 
library(ggplot2) ; library(plotly) ; library(leaflet) ; library(ggsci)
library(lubridate) ; library(stringr)
library(tabulizer) ; library(rvest)

# =====================================
# import data
# =====================================
# 鄉鎮以年齡及性別分組確診數
COVID_df <- read.csv("https://data.cdc.gov.tw/download?resourceid=3c1e263d-16ec-4d70-b56c-21c9e2171fc7&dataurl=https://od.cdc.gov.tw/eic/Day_Confirmation_Age_County_Gender_19CoV.csv" , 
                     stringsAsFactors = FALSE) %>% 
    setNames(c("disease" , "date_diagnostic" , "adm2" , "adm3" , "gender" , "imported" , "age" , "n_cases")) %>% 
    select(-disease) %>% 
    # cleaan table
    mutate(date_diagnostic = lubridate::as_date(as.character(date_diagnostic))) %>% 
    mutate(adm2 = ifelse(adm2 == "空值" , NA , adm2) , 
           adm3 = ifelse(adm3 == "空值" , NA , adm3) , 
           imported = ifelse(imported == "是" , TRUE , FALSE)) %>% 
    mutate(adm2 = str_replace_all(adm2 , "台" , "臺") ) %>% 
    # complete time-series (days without confirmed cases)
    right_join(
        data.frame(date_diagnostic = seq(min(.$date_diagnostic) , 
                                         max(.$date_diagnostic) , 
                                         "1 day"))
    ) %>% 
    arrange(date_diagnostic) %>% 
    mutate(n_cases = ifelse(is.na(n_cases) , 0 , n_cases))

# 最新統計
summary_df <- read.csv("https://data.cdc.gov.tw/download?resourceid=52eb9a7d-813d-48b1-b462-384a7c84a746&dataurl=https://od.cdc.gov.tw/eic/covid19/covid19_tw_stats.csv")

# 採檢送驗數
tested_df <- read.csv("https://data.cdc.gov.tw/download?resourceid=7ee40c7d-a14c-47b3-bf27-a5de4c278782&dataurl=https://od.cdc.gov.tw/eic/covid19/covid19_tw_specimen.csv") %>% 
    #setNames(c("date" , "enhanced_surveillance" , "quarantine" , "reported_cases" , "total")) %>% 
    mutate(通報日 = as_date(通報日)) %>% 
    filter(通報日 <= Sys.Date())


# 疫苗統計資料
# 先嘗試下載最新的資料 若不成功則用已下載下來的資料
# try to download from CDC website
download_vaccine_data_try <- FALSE
while(!download_vaccine_data_try){
    try({
        source("get-vaccination-data.R")
        download_vaccine_data_try <- TRUE
    })
}
# if not successful: load local files
if(!download_vaccine_data_try){
    # all the files
    in_files_vaccine <- list.files("data/vaccination" , full.names = TRUE , pattern = ".txt$|.csv")
    # filter the files of the latest date
    in_files_vaccine_date <- str_extract(in_files_vaccine , "\\d{4}-\\d{2}-\\d{2}") %>% as_date()
    in_files_vaccine <- in_files_vaccine[str_detect(in_files_vaccine , as.character(max(in_files_vaccine_date)))]
    # read the files
    vaccine_df <- read.csv(grep(".csv$" , in_files_vaccine , value = TRUE) , stringsAsFactors = FALSE)
    vaccine_today_date <- max(in_files_vaccine_date)
    vaccine_date_update <- read.table(grep("metadata.txt$" , in_files_vaccine , value = TRUE) , 
                                      sep = "," , header = TRUE , stringsAsFactors = FALSE)[1,1]
}


# Taiwan shapefile
TWN_adm3 <- read_sf("data/Taiwan_adm3/TWN_adm3_4326.shp")
TWN_adm2 <- read_sf("data/Taiwan_adm3/TWN_adm2_4326.shp")

# adm2 and adm3 population
# source("populationXLS_clean.R")
population_adm2 <- read.csv("data/population_adm2.csv" , stringsAsFactors = FALSE)
population_adm3 <- read.csv("data/population_adm3.csv" , stringsAsFactors = FALSE)



# =====================================
# 1) total curve
# =====================================
plot_total_curve <- COVID_df %>% 
    group_by(date_diagnostic , imported) %>% 
    summarize(n_cases = sum(n_cases)) %>% # number of cases per day
    ungroup() %>% 
    filter(!is.na(imported)) %>% 
    mutate(imported = ifelse(imported , "境外移入" , "本土感染")) %>% 
    # visualization
    ggplot(aes(x = date_diagnostic , y = n_cases)) +
    geom_bar(aes(fill = imported) , 
             position = "dodge" , stat = "identity") +
    labs(x = "個案研判日" , y = "確診病例數" , fill = "感染源" , 
         title = "COVID-19全台灣每日新增確診數") +
    scale_x_date(date_breaks = "1 month" , date_labels = "%Y/%m" , date_minor_breaks = "1 week") +
    #scale_y_continuous(breaks = seq(0,700,100) , minor_breaks = seq(0,700,50)) +
    ggthemes::theme_economist_white() +
    theme(text = element_text(family = "Noto Sans CJK TC") , 
          axis.text.x = element_text(angle = 30 , vjust = 1 , hjust = 1 , size = 7))
plot_movingavg_curve <- COVID_df %>% 
    group_by(date_diagnostic) %>% 
    summarize(n_cases = sum(n_cases)) %>% 
    ungroup() %>% 
    arrange(date_diagnostic) %>% 
    mutate(moving_average = zoo::rollapply(n_cases , 
                                           width = 7 , 
                                           FUN = mean , 
                                           align = "right" , 
                                           fill = NA)) %>% 
    ggplot(aes(x = date_diagnostic)) +
    geom_bar(aes(y = n_cases) , stat = "identity" , fill = "azure3") +
    geom_line(aes(y = moving_average) , color = "deeppink3") +
    labs(x = "個案研判日" , y = "總確診病例數" , 
         title = "每日確診數七日移動平均") +
    scale_x_date(date_breaks = "1 month" , date_labels = "%Y/%m" , date_minor_breaks = "1 week") +
    ggthemes::theme_economist_white() +
    theme(text = element_text(family = "Noto Sans CJK TC") , 
          axis.text.x = element_text(angle = 30 , vjust = 1 , hjust = 1 , size = 7))

# =====================================
# 2) by age and gender
# =====================================
plot_age_gender <- COVID_df %>% 
    # clean age
    filter(!is.na(age)) %>% 
    mutate(age = ifelse(age %in% as.character(0:4) , "0-4" , age)) %>% 
    mutate(age = factor(age , 
                        levels = c(paste(seq(0,65,5) , seq(4,69,5) , sep = "-") , "70+"))) %>% 
    # total cases in each gender-age group
    group_by(gender , age) %>% 
    summarize(n_cases = sum(n_cases)) %>% 
    ungroup() %>% 
    ggplot(aes(x = age , y = n_cases)) +
    geom_bar(aes(fill = gender) , stat = "identity" , position = "dodge") +
    scale_fill_jco() +
    scale_y_continuous(breaks = seq(0,650,100)) +
    theme_bw() +
    coord_flip() +
    labs(x = "" , y = "確診病例數" , 
         fill = "性別" , title = "各年齡層分性別確診人數統計") +
    ggthemes::theme_economist_white() +
    theme(text = element_text(family = "Noto Sans CJK TC") , 
          axis.text.y = element_text(size = 7))

# =====================================
# 3) 7-day incidence (of domestic cases)
# =====================================
# by adm2 -----------------------------
incidence_daily_adm2 <- COVID_df %>% 
    filter(imported == FALSE) %>% 
    group_by(date_diagnostic , adm2) %>% 
    summarize(n_cases = sum(n_cases)) %>% 
    ungroup() %>% 
    # complete time series
    right_join(
        data.frame(date_diagnostic = seq(min(COVID_df$date_diagnostic) , 
                                         max(COVID_df$date_diagnostic) , 
                                         "1 day"))
    ) %>% 
    arrange(date_diagnostic) %>% 
    # complete adm2 list
    pivot_wider(id_cols = c(date_diagnostic , adm2) , names_from = adm2 , values_from = n_cases) %>% 
    select(-`NA`) %>% 
    pivot_longer(cols = -date_diagnostic , names_to = "adm2" , values_to = "n_cases") %>% 
    mutate(n_cases = ifelse(is.na(n_cases) , 0 , n_cases)) %>% 
    # join population
    left_join(population_adm2 , by = "adm2") %>% 
    # 7-day cases
    group_by(adm2) %>% 
    mutate(n_cases_7day = zoo::rollapply(n_cases , 
                                         width = 7 , 
                                         FUN = sum , 
                                         align = "right" , 
                                         fill = NA)) %>% 
    ungroup() %>% 
    # 7-day incidence
    mutate(incidence_7day = n_cases_7day / population * 100000)


# by adm3 -----------------------------
incidence_daily_adm3 <- COVID_df %>% 
    filter(imported == FALSE) %>% 
    group_by(date_diagnostic , adm2 , adm3) %>% 
    summarize(n_cases = sum(n_cases)) %>% 
    ungroup() %>% 
    # complete time series
    right_join(
        data.frame(date_diagnostic = seq(min(COVID_df$date_diagnostic) , 
                                         max(COVID_df$date_diagnostic) , 
                                         "1 day"))
    ) %>% 
    arrange(date_diagnostic) %>% 
    # complete adm3 list
    pivot_wider(id_cols = c(date_diagnostic , adm2 , adm3) , names_from = c(adm2 , adm3) , names_sep = "_" , values_from = n_cases) %>% 
    select(-contains("NA")) %>% 
    pivot_longer(cols = -date_diagnostic , names_to = "adm3" , values_to = "n_cases") %>% 
    separate(col = adm3 , into = c("adm2" , "adm3") , sep = "_") %>% 
    mutate(n_cases = ifelse(is.na(n_cases) , 0 , n_cases)) %>% 
    # join population
    left_join(population_adm3 , by = c("adm2" , "adm3")) %>% 
    # 7-day cases
    group_by(adm2 , adm3) %>% 
    mutate(n_cases_7day = zoo::rollapply(n_cases , 
                                         width = 7 , 
                                         FUN = sum , 
                                         align = "right" , 
                                         fill = NA)) %>% 
    ungroup() %>% 
    # 7-day incidence
    mutate(incidence_7day = n_cases_7day / population * 100000)





# =====================================
# 4) accumulated cases for adm2 and adm3
# =====================================
# by adm2 -----------------------------
totalcases_adm2_sf <- COVID_df %>% 
    filter(imported == FALSE) %>% 
    group_by(adm2) %>% 
    summarize(n_cases = sum(n_cases)) %>% 
    ungroup() %>% 
    # complete adm2 list
    full_join(TWN_adm2 , by = c("adm2" = "COUNTYNAME")) %>% 
    distinct() %>% 
    # join population
    left_join(population_adm2 , by = "adm2") %>% 
    # number of cases per 100,000 
    mutate(n_cases = ifelse(is.na(n_cases) , 0 , n_cases)) %>% 
    mutate(n_cases_population = n_cases / population * 100000) %>% 
    select(adm2 , n_cases , population , n_cases_population , geometry) %>% 
    st_as_sf()

# by adm3 -----------------------------
totalcases_adm3_sf <- COVID_df %>% 
    filter(imported == FALSE) %>% 
    group_by(adm2 , adm3) %>% 
    summarize(n_cases = sum(n_cases)) %>% 
    ungroup() %>% 
    # complete adm2 list
    full_join(TWN_adm3 , by = c("adm2" = "COUNTYNAME" , "adm3" = "TOWNNAME")) %>% 
    distinct() %>% 
    # join population
    left_join(population_adm3 , by = c("adm2" , "adm3")) %>% 
    # number of cases per 100,000 
    mutate(n_cases = ifelse(is.na(n_cases) , 0 , n_cases)) %>% 
    mutate(n_cases_population = n_cases / population * 100000) %>% 
    select(adm2 , adm3 , n_cases , population , n_cases_population , geometry) %>% 
    st_as_sf()

# =====================================
# 5) tested curve
# =====================================
plot_total_tested <- tested_df %>% 
    pivot_longer(cols = c("法定傳染病通報" , "居家檢疫送驗" , "擴大監測送驗") , 
                 names_to = "Source" , values_to = "Number") %>% 
    # visualization
    ggplot(aes(x = 通報日)) +
    geom_area(aes(y = Number , fill = Source)) +
    geom_line(aes(y = Total) , alpha = 0.5) +
    geom_point(aes(y = Total) , alpha = 0.5 , size = 0.5) +
    labs(x = "通報日" , y = "每日送驗數" , 
         title = "病毒檢測每日送驗數") +
    scale_x_date(date_breaks = "1 month" , date_labels = "%Y/%m" , date_minor_breaks = "1 week") +
    ggthemes::theme_economist_white() +
    theme(text = element_text(family = "Noto Sans CJK TC") , 
          axis.text.x = element_text(angle = 30 , vjust = 1 , hjust = 1 , size = 7))

# =====================================
# 6) vaccination  plot
# =====================================
plot_vaccination <- vaccine_df %>% 
    # order adm2 by total vaccinated number
    mutate(adm2 = factor(adm2 , levels = adm2[order(total_tilltoday , decreasing = FALSE)])) %>% 
    # 
    pivot_longer(cols = c(total_tillyesterday, today) , names_to = "time" , values_to = "vaccinated") %>% 
    mutate(time = ifelse(time == "today" , "今日接種" , "昨日以前累計接種")) %>% 
    # visualization
    ggplot(aes(x = adm2 , y = vaccinated , fill = time)) +
    geom_bar(stat = "identity" , position = "stack") +
    coord_flip() +
    labs(x = "縣市" , y = "接種人次" , fill = "接種日" , 
         title = paste(vaccine_today_date , "累計接種人次") , 
         subtitle = paste("資料統計截止時間:" , vaccine_date_update)) +
    ggthemes::theme_fivethirtyeight() +
    theme(text = element_text(family = "Noto Sans CJK TC") , 
          axis.text.x = element_text(angle = 30 , vjust = 1 , hjust = 1 , size = 8) , 
          axis.text.y = element_text(size = 8))

plot_vaccination_delivered <- vaccine_df %>% 
    # order adm2 by total vaccinated number
    mutate(adm2 = factor(adm2 , levels = adm2[order(total_tilltoday , decreasing = FALSE)])) %>% 
    # visualization
    ggplot(aes(x = adm2 , y = delivered_tilltotday)) +
    geom_bar(stat = "identity" , fill = "dodgerblue3") +
    coord_flip() +
    labs(x = "縣市" , y = "劑數" , 
         title = paste(vaccine_today_date , "累計配送劑數") , 
         subtitle = paste("資料統計截止時間:" , vaccine_date_update)) +
    ggthemes::theme_fivethirtyeight() +
    theme(text = element_text(family = "Noto Sans CJK TC") , 
          axis.text.x = element_text(angle = 30 , vjust = 1 , hjust = 1 , size = 8) , 
          axis.text.y = element_text(size = 8))


# =====================================
# compile server
# =====================================
function(input, output, session) {
    
    # table_summary
    output$table_summary <- renderTable({
        summary_df %>% 
            select(`送驗` , `排除` , `昨日送驗` , `昨日排除`)
    })
    
    # summary_total_confirmed
    output$summary_total_confirmed <- renderText(summary_df$`確診`)
    
    # summary_yesterday_confirmed
    output$summary_yesterday_confirmed <- renderText(summary_df$`昨日確診`)
    
    # summary_total_mortality
    output$summary_total_mortality <- renderText(summary_df$`死亡`)
    
    # summary_total_recovered
    output$summary_total_recovered <- renderText(summary_df$`解除隔離`)
    
    # plot_total_curve
    output$plot_total_curve <- renderPlotly({
        if(input$select_total_curve_variable == "total"){
            ggplotly(plot_total_curve , tooltip = c("x" , "y")) %>% 
                layout(legend = list(x = 0.05, y = 0.95 , font = list(size = 9)))
        }else if(input$select_total_curve_variable == "movingavg"){
            ggplotly(plot_movingavg_curve , tooltip = c("x" , "y")) %>% 
                layout(legend = list(x = 0.05, y = 0.95 , font = list(size = 9)))
        }
    })
    
    # plot_age_gender
    output$plot_age_gender <- renderPlotly({
        ggplotly(plot_age_gender) %>% 
            layout(legend = list(x = 0.8, y = 0.1 , font = list(size = 9)))
    })
    
    # plot_tested
    output$plot_tested <- renderPlotly({
        ggplotly(plot_total_tested) %>% 
            layout(legend = list(x = 0.05, y = 0.95 , font = list(size = 9)))
    })
    
    # table_incidence
    output$table_incidence <- renderTable({
        incidence_daily_adm3 %>% 
            filter(date_diagnostic == max(date_diagnostic)) %>% 
            arrange(-incidence_7day) %>% 
            select(incidence_7day , n_cases_7day , adm2 , adm3) %>% 
            mutate(incidence_7day = round(incidence_7day , 1)) %>% 
            mutate(n_cases_7day = as.integer(n_cases_7day)) %>% 
            rename(`七日發生率` = incidence_7day , 
                   `七日確診數` = n_cases_7day , 
                   `縣市` = adm2 , `鄉鎮市區` = adm3) %>% 
            slice(1:30)
    })
    output$table_incidence_date <- renderText({
        sprintf("發生率統計日期: %s" , max(incidence_daily_adm3$date_diagnostic))
    })
    
    # map_covid
    output$map_covid <- renderLeaflet({
        selected_date <- input$select_date
        if(input$adm_level == "adm2"){
            if(input$select_variable == "incidence"){
                # adm 2 incidence -------------------------------------------------
                # the incidence of one particular day as sf
                incidence_oneday_adm2_sf <- TWN_adm2 %>% 
                    left_join(
                        incidence_daily_adm2 %>% 
                            filter(date_diagnostic == selected_date) %>% 
                            select(adm2 , incidence_7day , n_cases , population , n_cases_7day), 
                        by = c("COUNTYNAME" = "adm2")
                    ) %>% 
                    mutate(incidence_7day = ifelse(is.na(incidence_7day) , 0 , incidence_7day))
                pal_incidence_adm2 <- colorBin("YlOrRd", 
                                               domain = incidence_oneday_adm2_sf$incidence_7day , 
                                               na.color = "white" , 
                                               bins = c(1,5,10,25,50,75,Inf))
                label_incidence_adm2 <- sprintf("%s\n近七日發生率=%s\n近七日新增確診=%s" , 
                                                incidence_oneday_adm2_sf$COUNTYNAME , 
                                                round(incidence_oneday_adm2_sf$incidence_7day,1), 
                                                incidence_oneday_adm2_sf$n_cases_7day)
                
                incidence_oneday_adm2_sf %>% 
                    leaflet(options = leafletOptions(minZoom = 6 , maxZoom = 11)) %>% 
                    setView(120.972812 , 23.847964, 7) %>% 
                    addProviderTiles("CartoDB.VoyagerNoLabels") %>% 
                    addPolygons(
                        fillColor = ~pal_incidence_adm2(incidence_7day) , 
                        weight = 1 , 
                        opacity = 0.3 , 
                        color = "black" , 
                        fillOpacity = 0.7 , 
                        highlight = highlightOptions(
                            weight = 3,
                            color = "#666",
                            fillOpacity = 0.7,
                            bringToFront = TRUE) , 
                        label = label_incidence_adm2 , 
                        labelOptions = labelOptions(
                            style = list("font-weight" = "normal", padding = "3px 8px"),
                            textsize = "12px",
                            direction = "auto")
                    ) %>% 
                    addLegend(pal = pal_incidence_adm2, values = ~incidence_7day, 
                              opacity = 0.7, 
                              title = "近七日發生率(每十萬人)",
                              position = "bottomright")
            }else if(input$select_variable == "totalcase"){
                # adm 2 total case -------------------------------------------------
                pal_totalcase_adm2 <- colorBin("RdPu", 
                                               domain = totalcases_adm2_sf$n_cases_population , 
                                               na.color = "white" , 
                                               bins = c(1,10,25,50,75,100,150,Inf))
                label_totalcase_adm2 <- sprintf("%s: 每十萬人累計%s人確診 總確診人數=%s" , 
                                                totalcases_adm2_sf$adm2 , 
                                                round(totalcases_adm2_sf$n_cases_population,1), 
                                                totalcases_adm2_sf$n_cases)
                totalcases_adm2_sf %>% 
                    leaflet(options = leafletOptions(minZoom = 6 , maxZoom = 11)) %>% 
                    setView(120.972812 , 23.847964, 7) %>% 
                    addProviderTiles("CartoDB.VoyagerNoLabels") %>% 
                    addPolygons(
                        fillColor = ~pal_totalcase_adm2(n_cases_population) , 
                        weight = 0.5 , 
                        opacity = 0.3 , 
                        color = "black" , 
                        fillOpacity = 0.7 , 
                        highlight = highlightOptions(
                            weight = 3,
                            color = "#666",
                            fillOpacity = 0.7,
                            bringToFront = TRUE) , 
                        label = label_totalcase_adm2 , 
                        labelOptions = labelOptions(
                            style = list("font-weight" = "normal", padding = "3px 8px"),
                            textsize = "12px",
                            direction = "auto")
                    ) %>% 
                    addLegend(pal = pal_totalcase_adm2, values = ~n_cases_population, 
                              opacity = 0.7, 
                              title = "累計確診人數(每十萬人)",
                              position = "bottomright")
            }else if(input$select_variable == "newcases"){
                newcases_adm2_sf <- COVID_df %>% 
                    filter(imported == FALSE) %>% 
                    filter(date_diagnostic == selected_date) %>% 
                    group_by(adm2) %>% 
                    summarize(n_cases = sum(n_cases)) %>% 
                    ungroup() %>% 
                    right_join(TWN_adm2 , by = c("adm2" = "COUNTYNAME")) %>% 
                    mutate(n_cases = ifelse(is.na(n_cases) , 0 , n_cases)) %>% 
                    st_as_sf() %>% 
                    arrange(n_cases)
                # visualization
                label_newcase <- sprintf("%s%s新增確診%s例" , 
                                         newcases_adm2_sf$adm2 ,
                                         selected_date , 
                                         newcases_adm2_sf$n_cases)
                newcases_adm2_sf %>% 
                    leaflet(options = leafletOptions(minZoom = 6 , maxZoom = 11)) %>% 
                    setView(120.972812 , 23.847964, 7) %>% 
                    addProviderTiles("CartoDB.VoyagerNoLabels") %>% 
                    addPolygons(
                        fillColor = "white" , 
                        weight = 0.5 , 
                        fillOpacity = 0.1 , 
                        color = "black" , 
                        highlight = highlightOptions(
                            weight = 3,
                            color = "#666",
                            fillOpacity = 0.7,
                            bringToFront = FALSE) 
                    ) %>% 
                    addCircles(
                        data = newcases_adm2_sf %>% 
                            st_centroid() , 
                        radius = ~(n_cases*50000)^0.62 , 
                        fillColor = "red" , 
                        fillOpacity = 0.5 , 
                        stroke = FALSE , 
                        highlight = highlightOptions(
                            weight = 3,
                            color = "#666",
                            fillOpacity = 0.5,
                            bringToFront = TRUE) , 
                        label = label_newcase , 
                        labelOptions = labelOptions(
                            style = list("font-weight" = "normal", padding = "3px 8px"),
                            textsize = "12px",
                            direction = "auto")
                    ) 
            }
        }else if(input$adm_level == "adm3"){
            if(input$select_variable == "incidence"){
                # adm 3 incidence -------------------------------------------------
                # the incidence of one particular day as sf
                incidence_oneday_adm3_sf <- TWN_adm3 %>% 
                    select(COUNTYNAME, TOWNNAME) %>% 
                    left_join(
                        incidence_daily_adm3 %>% 
                            filter(date_diagnostic == selected_date) %>% 
                            select(adm2 , adm3 , incidence_7day , n_cases , population , n_cases_7day), 
                        by = c("COUNTYNAME" = "adm2" , "TOWNNAME" = "adm3")
                    ) %>% 
                    mutate(incidence_7day = ifelse(is.na(incidence_7day) , 0 , incidence_7day))
                pal_incidence_adm3 <- colorBin("YlOrRd", 
                                               domain = incidence_oneday_adm3_sf$incidence_7day , 
                                               na.color = "white" , 
                                               bins = c(1,5,10,25,50,75,Inf))
                label_incidence_adm3 <- sprintf("%s%s: \n近七日發生率=%s\n近七日新增確診=%s" , 
                                                incidence_oneday_adm3_sf$COUNTYNAME , 
                                                incidence_oneday_adm3_sf$TOWNNAME , 
                                                round(incidence_oneday_adm3_sf$incidence_7day,1), 
                                                incidence_oneday_adm3_sf$n_cases_7day)
                
                incidence_oneday_adm3_sf %>% 
                    leaflet(options = leafletOptions(minZoom = 6 , maxZoom = 11)) %>% 
                    setView(120.972812 , 23.847964, 7) %>% 
                    addProviderTiles("CartoDB.VoyagerNoLabels") %>% 
                    addPolygons(
                        fillColor = ~pal_incidence_adm3(incidence_7day) , 
                        weight = 0.5 , 
                        opacity = 0.3 , 
                        color = "black" , 
                        fillOpacity = 0.7 , 
                        highlight = highlightOptions(
                            weight = 3,
                            color = "#666",
                            fillOpacity = 0.7,
                            bringToFront = TRUE) , 
                        label = label_incidence_adm3 , 
                        labelOptions = labelOptions(
                            style = list("font-weight" = "normal", padding = "3px 8px"),
                            textsize = "12px",
                            direction = "auto")
                    ) %>%
                    addPolygons(
                        data = TWN_adm2 , 
                        fill = NA , 
                        weight = 1 , 
                        opacity = 0.3 , 
                        color = "black"
                    ) %>% 
                    addLegend(pal = pal_incidence_adm3, values = ~incidence_7day, 
                              opacity = 0.7, 
                              title = "近七日發生率(每十萬人)",
                              position = "bottomright")
            }else if(input$select_variable == "totalcase"){
                # adm 3 total case -------------------------------------------------
                pal_totalcase_adm3 <- colorBin("RdPu", 
                                               domain = totalcases_adm3_sf$n_cases_population , 
                                               na.color = "white" , 
                                               bins = c(1,10,25,50,75,100,150,Inf))
                label_totalcase_adm3 <- sprintf("%s%s: 每十萬人累計%s人確診 總確診人數=%s" , 
                                                totalcases_adm3_sf$adm2 , 
                                                totalcases_adm3_sf$adm3 , 
                                                round(totalcases_adm3_sf$n_cases_population,1), 
                                                totalcases_adm3_sf$n_cases)
                totalcases_adm3_sf %>% 
                    leaflet(options = leafletOptions(minZoom = 6 , maxZoom = 11)) %>% 
                    setView(120.972812 , 23.847964, 7) %>% 
                    addProviderTiles("CartoDB.VoyagerNoLabels") %>% 
                    addPolygons(
                        fillColor = ~pal_totalcase_adm3(n_cases_population) , 
                        weight = 0.5 , 
                        opacity = 0.3 , 
                        color = "black" , 
                        fillOpacity = 0.7 , 
                        highlight = highlightOptions(
                            weight = 3,
                            color = "#666",
                            fillOpacity = 0.7,
                            bringToFront = TRUE) , 
                        label = label_totalcase_adm3 , 
                        labelOptions = labelOptions(
                            style = list("font-weight" = "normal", padding = "3px 8px"),
                            textsize = "12px",
                            direction = "auto")
                    ) %>%
                    addPolygons(
                        data = TWN_adm2 , 
                        fill = NA , 
                        weight = 1 , 
                        opacity = 0.3 , 
                        color = "black"
                    ) %>% 
                    addLegend(pal = pal_totalcase_adm3, values = ~n_cases_population, 
                              opacity = 0.7, 
                              title = "累計確診人數(每十萬人)",
                              position = "bottomright")
            }else if(input$select_variable == "newcases"){
                newcases_adm3_sf <- COVID_df %>% 
                    filter(imported == FALSE) %>% 
                    filter(date_diagnostic == selected_date) %>% 
                    group_by(adm2 , adm3) %>% 
                    summarize(n_cases = sum(n_cases)) %>% 
                    ungroup() %>% 
                    right_join(TWN_adm3 , by = c("adm2" = "COUNTYNAME" , "adm3" = "TOWNNAME")) %>% 
                    mutate(n_cases = ifelse(is.na(n_cases) , 0 , n_cases)) %>% 
                    st_as_sf() %>% 
                    arrange(n_cases)
                # visualization
                label_newcase <- sprintf("%s%s%s新增確診%s例" , 
                                         newcases_adm3_sf$adm2 , 
                                         newcases_adm3_sf$adm3 , 
                                         selected_date , 
                                         newcases_adm3_sf$n_cases)
                newcases_adm3_sf %>% 
                    leaflet(options = leafletOptions(minZoom = 6 , maxZoom = 11)) %>% 
                    setView(120.972812 , 23.847964, 7) %>% 
                    addProviderTiles("CartoDB.VoyagerNoLabels") %>% 
                    addPolygons(
                        fillColor = "white" , 
                        weight = 0.5 , 
                        fillOpacity = 0.1 , 
                        color = "black" , 
                        highlight = highlightOptions(
                            weight = 3,
                            color = "#666",
                            fillOpacity = 0.7,
                            bringToFront = FALSE) 
                    ) %>%
                    addPolygons(
                        data = TWN_adm2 , 
                        fill = NA , 
                        weight = 1 , 
                        opacity = 0.3 , 
                        color = "black"
                    ) %>% 
                    addCircles(
                        data = newcases_adm3_sf %>% 
                            st_centroid() , 
                        radius = ~(n_cases*80000)^0.55 , 
                        fillColor = "red" , 
                        fillOpacity = 0.5 , 
                        stroke = FALSE , 
                        highlight = highlightOptions(
                            weight = 3,
                            color = "#666",
                            fillOpacity = 0.5,
                            bringToFront = TRUE) , 
                        label = label_newcase , 
                        labelOptions = labelOptions(
                            style = list("font-weight" = "normal", padding = "3px 8px"),
                            textsize = "12px",
                            direction = "auto")
                    ) 
            }
        }
    })
    
    
    # plot_curve_selected_area
    output$plot_curve_selected_area <- renderPlotly({
        if(!is.null(input$map_covid_click)){
            # the selected point from the map
            selected_point <- data.frame(x = input$map_covid_shape_click$lng , 
                                         y = input$map_covid_shape_click$lat) %>% 
                st_as_sf(coords = c("x","y")) %>% 
                st_set_crs(st_crs(TWN_adm2))
            if(input$adm_level == "adm2"){
                # check in which administration zone the selected point locates
                contains_selected_point <- st_contains(TWN_adm2 , selected_point , sparse = FALSE)
                # make the plot with valid selected points (within at least one zone)
                if(any(contains_selected_point)){
                    selected_adm <- TWN_adm2 %>% 
                        filter(contains_selected_point)
                    selected_adm_daily <- COVID_df %>% 
                        right_join(selected_adm , by = c("adm2"="COUNTYNAME")) %>% 
                        group_by(date_diagnostic) %>% 
                        summarize(n_cases = sum(n_cases)) %>% 
                        ungroup() 
                    plot_selected_adm_daily <- selected_adm_daily %>% 
                        ggplot(aes(x = date_diagnostic , y = n_cases)) +
                        geom_bar(stat = "identity") +
                        labs(x = "個案研判日" , y = "確診病例數" , 
                             title = sprintf("%s每日新增確診數" , selected_adm$COUNTYNAME)) +
                        scale_x_date(date_labels = "%Y/%m/%d") +
                        ggthemes::theme_economist_white() +
                        theme(text = element_text(family = "Noto Sans CJK TC") , 
                              axis.text.x = element_text(angle = 30 , vjust = 1 , hjust = 1 , size = 7))
                    if(all(is.na(selected_adm_daily))){
                        ggplotly(
                            plot_selected_adm_daily + 
                                labs(title = sprintf("%s無確診" , selected_adm$COUNTYNAME))
                        )
                    }else{
                        ggplotly(plot_selected_adm_daily)
                    }
                }
            }else if(input$adm_level == "adm3"){
                # check in which administration zone the selected point locates
                contains_selected_point <- st_contains(TWN_adm3 , selected_point , sparse = FALSE)
                # make the plot with valid selected points (within at least one zone)
                if(any(contains_selected_point)){
                    selected_adm <- TWN_adm3 %>% 
                        filter(contains_selected_point)
                    selected_adm_daily <- COVID_df %>% 
                        right_join(selected_adm , by = c("adm2"="COUNTYNAME" , "adm3" = "TOWNNAME")) %>% 
                        group_by(date_diagnostic) %>% 
                        summarize(n_cases = sum(n_cases)) %>% 
                        ungroup()
                    plot_selected_adm_daily <- selected_adm_daily %>% 
                        ggplot(aes(x = date_diagnostic , y = n_cases)) +
                        geom_bar(stat = "identity") +
                        labs(x = "個案研判日" , y = "確診病例數" , 
                             title = sprintf("%s%s每日新增確診數" , selected_adm$COUNTYNAME , selected_adm$TOWNNAME)) +
                        scale_x_date(date_labels = "%Y/%m/%d") +
                        ggthemes::theme_economist_white() +
                        theme(text = element_text(family = "Noto Sans CJK TC") , 
                              axis.text.x = element_text(angle = 30 , vjust = 1 , hjust = 1 , size = 7))
                    if(all(is.na(selected_adm_daily))){
                        ggplotly({
                            plot_selected_adm_daily + 
                                labs(title = sprintf("%s%s無確診" , selected_adm$COUNTYNAME , selected_adm$TOWNNAME))
                        })
                    }else{
                        ggplotly(plot_selected_adm_daily)
                    }
                }
            }
        }
    })
    
    # table_selected_area
    output$table_selected_area <- renderTable({
        selected_date <- input$select_date
        if(!is.null(input$map_covid_click)){
            # the selected point from the map
            selected_point <- data.frame(x = input$map_covid_shape_click$lng , 
                                         y = input$map_covid_shape_click$lat) %>% 
                st_as_sf(coords = c("x","y")) %>% 
                st_set_crs(st_crs(TWN_adm2))
            if(input$adm_level == "adm2"){
                # check in which administration zone the selected point locates
                contains_selected_point <- st_contains(TWN_adm2 , selected_point , sparse = FALSE)
                # make the plot with valid selected points (within at least one zone)
                if(any(contains_selected_point)){
                    selected_adm <- TWN_adm2 %>% 
                        filter(contains_selected_point) %>% 
                        select(COUNTYNAME)
                    # total cases
                    selected_adm_total_cases <- COVID_df %>% 
                        right_join(selected_adm , by = c("adm2" = "COUNTYNAME")) %>% 
                        summarize(n_cases = sum(n_cases)) %>% 
                        .$n_cases %>% 
                        ifelse(is.na(.) , 0 , .)
                    # new cases
                    selected_adm_new_cases <- COVID_df %>% 
                        right_join(selected_adm , by = c("adm2" = "COUNTYNAME")) %>% 
                        filter(date_diagnostic == selected_date) %>% 
                        summarize(n_cases = sum(n_cases)) %>% 
                        .$n_cases %>% 
                        ifelse(is.na(.) , 0 , .)
                    # 7-day incidence
                    selected_adm_incidence <- incidence_daily_adm3 %>% 
                        right_join(selected_adm , by = c("adm2" = "COUNTYNAME")) %>% 
                        filter(date_diagnostic == selected_date) %>% 
                        .$incidence_7day %>% 
                        ifelse(identical(numeric(0) , .) , 0 , .) %>% round(1)
                    # population
                    selected_adm_population <- population_adm2 %>% 
                        right_join(selected_adm , by = c("adm2" = "COUNTYNAME")) %>% 
                        .$population %>% 
                        ifelse(is.na(.) , 0 , .)
                    # output: data.frame
                    data.frame(
                        已點選行政區 = c("縣市" , "總確診數" , paste0(selected_date , "新增確診") , "近七日每十萬人確診數" , "總人口數") , 
                        Detail = c(selected_adm$COUNTYNAME , 
                                   selected_adm_total_cases , 
                                   selected_adm_new_cases , 
                                   selected_adm_incidence , 
                                   selected_adm_population)
                    ) 
                }
            }else if(input$adm_level == "adm3"){
                # check in which administration zone the selected point locates
                contains_selected_point <- st_contains(TWN_adm3 , selected_point , sparse = FALSE)
                # make the plot with valid selected points (within at least one zone)
                if(any(contains_selected_point)){
                    selected_adm <- TWN_adm3 %>% 
                        filter(contains_selected_point) %>% 
                        select(COUNTYNAME , TOWNNAME)
                    # total cases
                    selected_adm_total_cases <- COVID_df %>% 
                        right_join(selected_adm , by = c("adm2" = "COUNTYNAME" , "adm3" = "TOWNNAME")) %>% 
                        summarize(n_cases = sum(n_cases)) %>% 
                        .$n_cases %>% 
                        ifelse(is.na(.) , 0 , .)
                    # new cases
                    selected_adm_new_cases <- COVID_df %>% 
                        right_join(selected_adm , by = c("adm2" = "COUNTYNAME" , "adm3" = "TOWNNAME")) %>% 
                        filter(date_diagnostic == selected_date) %>% 
                        summarize(n_cases = sum(n_cases)) %>% 
                        .$n_cases %>% 
                        ifelse(is.na(.) , 0 , .)
                    # 7-day incidence
                    selected_adm_incidence <- incidence_daily_adm3 %>% 
                        right_join(selected_adm , by = c("adm2" = "COUNTYNAME" , "adm3" = "TOWNNAME")) %>% 
                        filter(date_diagnostic == selected_date) %>% 
                        .$incidence_7day %>% 
                        ifelse(identical(numeric(0) , .) , 0 , .) %>% round(1)
                    # population
                    selected_adm_population <- population_adm3 %>% 
                        right_join(selected_adm , by = c("adm2" = "COUNTYNAME" , "adm3" = "TOWNNAME")) %>% 
                        .$population %>% 
                        ifelse(is.na(.) , 0 , .)
                    # output: data.frame
                    data.frame(
                        已點選行政區 = c("縣市" , "鄉鎮市區" , "總確診數" , paste0(selected_date , "新增確診") , "近七日每十萬人確診數" , "總人口數") , 
                        Detail = c(selected_adm$COUNTYNAME , 
                                   selected_adm$TOWNNAME , 
                                   selected_adm_total_cases , 
                                   selected_adm_new_cases , 
                                   selected_adm_incidence , 
                                   selected_adm_population)
                    ) 
                }
                
            }
        }
    })
    
    # vaccine_date_update
    output$vaccine_date_update <- renderText(paste("資料統計截止時間:" , vaccine_date_update))
    
    # vaccinated_total
    output$vaccinated_total <- renderText(sum(vaccine_df$total_tilltoday))
    
    # vaccinated_today
    output$vaccinated_today <- renderText(sum(vaccine_df$today))
    
    # plot_vaccination
    output$plot_vaccination <- renderPlotly({
        if(input$vaccination_variable == "vaccinated" ){
            ggplotly(plot_vaccination) %>% 
                layout(legend = list(x = 0.6, y = 0.1 , font = list(size = 9)))
        }else if(input$vaccination_variable == "delivered"){
            ggplotly(plot_vaccination_delivered) %>% 
                layout(legend = list(x = 0.6, y = 0.1 , font = list(size = 9)))
        }
    })
    
    
    # # Route select input box
    # output$routeSelect <- renderUI({
    #     live_vehicles <- getMetroData("VehicleLocations/0")
    #     
    #     routeNums <- sort(unique(as.numeric(live_vehicles$Route)))
    #     # Add names, so that we can add all=0
    #     names(routeNums) <- routeNums
    #     routeNums <- c(All = 0, routeNums)
    #     selectInput("routeNum", "Route", choices = routeNums, selected = routeNums[2])
    # })
    
}
