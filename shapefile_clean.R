#####################################################################################################
# Taiwan-COVID
# Data preprocessing: clean the shapefile
# 2021-05-31
#####################################################################################################

library(sf) ; library(dplyr)

TWN_adm3 <- read_sf("data/Taiwan_adm3/TOWN_MOI_1100415.shp") %>% 
  st_simplify(preserveTopology = TRUE , dTolerance = 0.001) %>% 
  st_crop(xmin = 118 , xmax = 122.5 , ymin = 21 , ymax = 27) %>% 
  st_transform(st_crs(4326))
TWN_adm2 <- read_sf("data/Taiwan_adm3/TOWN_MOI_1100415.shp") %>% 
  group_by(COUNTYNAME) %>% 
  summarize() %>% 
  ungroup %>% 
  st_simplify(preserveTopology = FALSE , dTolerance = 0.001) %>% 
  st_crop(xmin = 118 , xmax = 122.5 , ymin = 21 , ymax = 27) %>% 
  st_transform(st_crs(4326))

write_sf(TWN_adm3 , "data/Taiwan_adm3/TWN_adm3_4326.shp", layer_options = "ENCODING=UTF-8", delete_layer = TRUE)
write_sf(TWN_adm2 , "data/Taiwan_adm3/TWN_adm2_4326.shp", layer_options = "ENCODING=UTF-8", delete_layer = TRUE)
