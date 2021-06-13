#####################################################################################################
# Taiwan-COVID
# Data retrieval: vaccine 
# 2021-06-02
#####################################################################################################


# =====================================
# required packages
# =====================================
library(dplyr) 
library(lubridate) ; library(stringr)
library(tabulizer)
library(rvest)


# =====================================
# link to the update vaccine daily report pdf file
# =====================================
url_CDC_page <- "https://www.cdc.gov.tw/Category/Page/9jFXNbCe-sFK9EImRRi2Og"
{ # getting the .pdf link by web crawling is unstable (fails sometimes) --> try 5 times
  url_vaccine_try <- 1
  while(url_vaccine_try < 5){
    url_vaccine <- url_CDC_page %>% 
      # COVID-19疫苗統計資料網頁
      read_html() %>% 
      # 連結到附件
      html_element(css = ".download a") %>% 
      html_attr("href") %>% 
      sprintf("https://www.cdc.gov.tw%s" , .) %>% # 開啟附件連結網址
      # pdf預覽網頁
      read_html() %>% 
      # 取得"下載"連結
      html_nodes("a") %>% html_attr("href") %>% # get href-class objects
      sprintf("https://www.cdc.gov.tw%s" , .)
    if(length(url_vaccine) != 0) break
  }
}
# url_vaccine <- "https://www.cdc.gov.tw/Uploads/cc5adc0a-24c6-49ab-b99a-4ccbe8369113.pdf"
# url_vaccine <- "https://www.cdc.gov.tw/Uploads/802feb39-f452-4bc5-8c1c-f9c921b6f38f.pdf"

# =====================================
# download the latest vaccine daily report pdf file as temporary file
# =====================================
vaccine_temp_pdf <- "data/vaccination/temp.pdf"
download.file(url = url_vaccine , 
              destfile = vaccine_temp_pdf)

# =====================================
# clean data from the temporary file
# =====================================
# main table
vaccine_df <- extract_tables(vaccine_temp_pdf)[[1]] %>% 
  as.data.frame() %>% 
  slice(-1:-3) %>% select(-2) %>% 
  setNames(c("adm2" , "total_tillyesterday" , "today" , "total_tilltoday" , "delivered_tilltotday")) %>% 
  filter(str_detect(adm2 , "縣|市")) %>% 
  mutate(across(-adm2 , str_replace_all , pattern = "," , "")) %>% 
  mutate(across(-adm2 , as.integer))
# 統計資料日期
vaccine_today_date <- extract_text(vaccine_temp_pdf) %>% 
  str_extract("\\d+/\\d+/\\d+ COVID-19 疫苗統計資料") %>% 
  str_extract("\\d+/\\d+/\\d+") %>% 
  as_date(format = "%Y/%m/%d") + years(1911)
# 資料統計截止時間
vaccine_date_update <- extract_text(vaccine_temp_pdf)%>% 
  str_extract("資料統計截止時間 \\d+/\\d+/\\d+ \\d+:\\d+") %>% 
  str_extract("\\d+/\\d+/\\d+ \\d+:\\d+") %>% 
  as_datetime(format = "%Y/%m/%d %H:%M" , tz = "Asia/Taipei") + years(1911)

# =====================================
# export the cleaned data (PDF and csv)
# =====================================
# pdf file
file.rename(vaccine_temp_pdf , 
            paste0(dirname(vaccine_temp_pdf) , "/vaccine_daily_report_" , vaccine_today_date , ".pdf") )
# csv file
write.csv(vaccine_df , 
          paste0(dirname(vaccine_temp_pdf) , "/vaccine_daily_report_" , vaccine_today_date , ".csv") , 
          row.names = FALSE)
# metadata: 資料統計截止時間
data.frame(資料統計截止時間 = vaccine_date_update) %>% 
  write.table(paste0(dirname(vaccine_temp_pdf) , "/vaccine_daily_report_" , vaccine_today_date , "_metadata.txt") , 
              row.names = FALSE)

# clean
rm(url_CDC_page , url_vaccine , url_vaccine_try , vaccine_temp_pdf)

