library(tidyverse)
library(sf)
setwd("C:/Users/wenjing.xu/Google Drive/RESEARCH/Pronghorn/BaBA_Season2/")

GPS <- st_read("./misc/2021_Summer_FieldChecks/Field_Data/raw_results/eTrex_all_Pts.kml") %>% 
  select (-Description)
GPS <- GPS %>%
  dplyr::mutate(lat = sf::st_coordinates(.)[,1],
                lon = sf::st_coordinates(.)[,2])
st_geometry(GPS) <- NULL
GPS <- GPS %>% mutate (GPS_ID = as.numeric(as.character(Name))) 

meas.pts <- read_csv("./misc/2021_Summer_FieldChecks/Field_Data/MeaurementPts.csv") %>% 
  select (-'Point ID', -measurements) %>% filter (!is.na(Structure)) %>% 
  left_join(GPS, by = "GPS_ID") %>% relocate(GPS_ID, lat, lon)
write_csv(meas.pts, "./misc/2021_Summer_FieldChecks/Field_Data/FieldMeasPts_clean.csv")

valid.pts <- read_csv("./misc/2021_Summer_FieldChecks/Field_Data/ValidationPts.csv") %>%
  rename(GPS_ID = 'GPS Point IDs' ) %>% left_join(GPS, by = "GPS_ID") %>% 
  relocate(GPS_ID, lat, lon)
which()

