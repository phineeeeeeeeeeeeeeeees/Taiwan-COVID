#####################################################################################################
# Taiwan-COVID
# Data preprocessing: clean the population data
# 2021-05-31
#####################################################################################################

library(readxl)

# one sheet for each county
in_filepath_excelsheet <- "data/population.xls"
sheetnames <- excel_sheets(in_filepath_excelsheet)
# read all the sheets
for(i in 2:length(sheetnames)){
  if(i == 2){
    population_adm2 <- read_excel(in_filepath_excelsheet , 
                                  sheet = i , 
                                  skip = 2 , n_max = 1) %>% 
      setNames(c("adm2" , "n_household" , "population" , "male" , "female"))
    population_adm3 <- read_excel(in_filepath_excelsheet , 
                                  sheet = i , 
                                  skip = 3) %>% 
      setNames(c("adm3" , "n_household" , "population" , "male" , "female")) %>% 
      mutate(adm2 = sheetnames[i])
  }else{ # append
    population_adm2 <- bind_rows(
      population_adm2 , 
      read_excel(in_filepath_excelsheet , 
                 sheet = i , 
                 skip = 2 , n_max = 1) %>% 
        setNames(c("adm2" , "n_household" , "population" , "male" , "female"))
    )
    population_adm3 <- bind_rows(
      population_adm3 , 
      read_excel(in_filepath_excelsheet , 
                 sheet = i , 
                 skip = 3) %>% 
        setNames(c("adm3" , "n_household" , "population" , "male" , "female")) %>% 
        mutate(adm2 = sheetnames[i])
    )
  }
}
# clean table
population_adm3 <- population_adm3 %>% 
  # remove comment row
  filter(!is.na(n_household)) %>% 
  # clean names
  mutate(adm3 = str_remove_all(adm3 , "※")) %>% 
  mutate(adm3 = str_remove_all(adm3 , "[:space:]"))

# export cleaned population data
write.csv(population_adm2 , "data/population_adm2.csv" , row.names = FALSE)
write.csv(population_adm3 , "data/population_adm3.csv" , row.names = FALSE)

rm(i , sheetnames , in_filepath_excelsheet)
