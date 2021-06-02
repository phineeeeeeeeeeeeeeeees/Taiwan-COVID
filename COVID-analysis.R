#####################################################################################################
# Taiwan-COVID
# Data analysis
# 2021-05-31
#####################################################################################################


# =====================================
# required packages
# =====================================
library(sf)
library(dplyr) ; library(tidyr) 
library(ggplot2) ; library(plotly) ; library(leaflet) ; library(ggsci)
library(lubridate) ; library(stringr)

# =====================================
# import data
# =====================================
# Covid dataset from CDC
COVID_df <- read.csv("https://data.cdc.gov.tw/download?resourceid=3c1e263d-16ec-4d70-b56c-21c9e2171fc7&dataurl=https://od.cdc.gov.tw/eic/Day_Confirmation_Age_County_Gender_19CoV.csv" , 
                      stringsAsFactors = FALSE) %>% 
  setNames(c("disease" , "date_diagnostic" , "adm2" , "adm3" , "gender" , "imported" , "age" , "n_cases")) %>% 
  select(-disease) %>% 
  # cleaan table
  mutate(date_diagnostic = as_date(as.character(date_diagnostic))) %>% 
  mutate(adm2 = ifelse(adm2 == "空值" , NA , adm2) , 
         adm3 = ifelse(adm3 == "空值" , NA , adm3) , 
         imported = ifelse(imported == "是" , TRUE , FALSE)) %>% 
  mutate(adm2 = str_replace_all(adm2 , "台" , "臺") ) %>% 
  # complete time-series (days without confirmed cases)
  right_join(
    data.frame(date_diagnostic = seq(min(.$date_diagnostic) , 
                                     max(.$date_diagnostic) , 
                                     "1 day") , 
               by = "date_diagnostic")
  ) %>% 
  arrange(date_diagnostic) %>% 
  mutate(n_cases = ifelse(is.na(n_cases) , 0 , n_cases))

# Taiwan shapefile
# source("shapefile_clean.R")
TWN_adm3 <- read_sf("data/Taiwan_adm3/TWN_adm3_4326.shp")
TWN_adm2 <- read_sf("data/Taiwan_adm3/TWN_adm2_4326.shp")

# adm2 and adm3 population
# source("populationXLS_clean.R")
population_adm2 <- read.csv("data/population_adm2.csv" , stringsAsFactors = FALSE)
population_adm3 <- read.csv("data/population_adm3.csv" , stringsAsFactors = FALSE)


# =====================================
# total curve
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
  scale_y_continuous(breaks = seq(0,700,100) , minor_breaks = seq(0,700,50)) +
  ggthemes::theme_economist_white() +
  theme(text = element_text(family = "Noto Sans CJK TC") , 
        axis.text.x = element_text(angle = 45 , vjust = 1 , hjust = 1 , size = 7))
ggplotly(plot_total_curve , tooltip = c("x" , "y"))


# =====================================
# total cases by age group and gender
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
ggplotly(plot_age_gender)


# =====================================
# 7-day incidence (of domestic cases)
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
# the incidence of one particular day as sf
selected_date <- max(COVID_df$date_diagnostic)
incidence_oneday_adm2_sf <- TWN_adm2 %>% 
  left_join(
    incidence_daily_adm2 %>% 
      filter(date_diagnostic == selected_date) %>% 
      select(adm2 , incidence_7day), 
    by = c("COUNTYNAME" = "adm2")
  ) %>% 
  mutate(incidence_7day = ifelse(is.na(incidence_7day) , 0 , incidence_7day))
# map
incidence_oneday_adm2_sf %>% 
  ggplot() +
  geom_sf(aes(fill = incidence_7day)) +
  scale_fill_gradientn(colors = c("grey96" , RColorBrewer::brewer.pal(7,"YlOrRd")) , 
                       limits = c(0,105) , oob = scales::squish) +
  labs(fill = "近七日發生率\n(每十萬人)" , subtitle = selected_date) +
  theme_bw() +
  theme(text = element_text(family = "Noto Sans CJK TC"))

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
# map
ggplot(incidence_oneday_adm3_sf) +
  geom_sf(aes(fill = incidence_7day) , size = 0.1) +
  geom_sf(data = TWN_adm2 , fill = NA , size = 0.6) + 
  scale_fill_gradientn(colors = c("grey96" , RColorBrewer::brewer.pal(7,"YlOrRd")) , 
                       limits = c(0,105) , oob = scales::squish) +
  labs(fill = "近七日發生率\n(每十萬人)") +
  ggthemes::theme_map() +
  theme(text = element_text(family = "Noto Sans CJK TC"))
{
  pal_incidence <- colorBin("YlOrRd", 
                            domain = incidence_oneday_adm3_sf$incidence_7day , 
                            na.color = "white" , 
                            bins = c(1,5,10,25,50,75,Inf))
  label_incidence <- sprintf("%s%s: \n近七日發生率=%s\n近七日新增確診=%s" , 
                             incidence_oneday_adm3_sf$COUNTYNAME , 
                             incidence_oneday_adm3_sf$TOWNNAME , 
                             round(incidence_oneday_adm3_sf$incidence_7day,1), 
                             incidence_oneday_adm3_sf$n_cases_7day)
  
  incidence_oneday_adm3_sf %>% 
    leaflet(options = leafletOptions(minZoom = 6 , maxZoom = 11)) %>% 
    setView(120.972812 , 23.847964, 7) %>% 
    addProviderTiles("CartoDB.VoyagerNoLabels") %>% 
    addPolygons(
      fillColor = ~pal_incidence(incidence_7day) , 
      weight = 0.5 , 
      opacity = 0.3 , 
      color = "black" , 
      fillOpacity = 0.7 , 
      highlight = highlightOptions(
        weight = 3,
        color = "#666",
        fillOpacity = 0.7,
        bringToFront = TRUE) , 
      label = label_incidence , 
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
    addLegend(pal = pal_incidence, values = ~incidence_7day, 
              opacity = 0.7, 
              title = "近七日發生率(每十萬人)",
              position = "bottomright")
}



# =====================================
# accumulated cases for adm2 and adm3
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

{
  pal_totalcase <- colorBin("RdPu", 
                            domain = totalcases_adm3_sf$n_cases_population , 
                            na.color = "white" , 
                            bins = c(1,10,25,50,75,100,150,Inf))
  label_totalcase <- sprintf("%s%s: 每十萬人累計%s人確診 總確診人數=%s" , 
                             totalcases_adm3_sf$adm2 , 
                             totalcases_adm3_sf$adm3 , 
                             round(totalcases_adm3_sf$n_cases_population,1), 
                             totalcases_adm3_sf$n_cases)
  
  totalcases_adm3_sf %>% 
    leaflet(options = leafletOptions(minZoom = 6 , maxZoom = 11)) %>% 
    setView(120.972812 , 23.847964, 7) %>% 
    addProviderTiles("CartoDB.VoyagerNoLabels") %>% 
    addPolygons(
      fillColor = ~pal_totalcase(n_cases_population) , 
      weight = 0.5 , 
      opacity = 0.3 , 
      color = "black" , 
      fillOpacity = 0.7 , 
      highlight = highlightOptions(
        weight = 3,
        color = "#666",
        fillOpacity = 0.7,
        bringToFront = TRUE) , 
      label = label_totalcase , 
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
    addLegend(pal = pal_totalcase, values = ~n_cases_population, 
              opacity = 0.7, 
              title = "累計確診人數(每十萬人)",
              position = "bottomright")
}


# =====================================
# daily new cases for adm2 and adm3
# =====================================
# by adm2 -----------------------------
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

# by adm3 -----------------------------
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



