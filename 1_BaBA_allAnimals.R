#baba around 2 with all animals with 2h intervals
setwd("/Users/Mushy 1/Google Drive (wenjing.xu@berkeley.edu)/RESEARCH/Pronghorn/BaBA_Season2")
target.crs <- "+init=epsg:32612"
setwd("C:/Users/wenjing.xu/Google Drive/RESEARCH/Pronghorn/BaBA_Season2")

library(tidyverse)
library(lubridate)
library(BaBA)
source("./code/BaBA_QC.r")

pronghorn <- read_csv("./data/pronghorn_2h_pts.csv") %>% mutate(date = ymd_hms(date, tz = "US/Mountain")) %>% filter(!is.na(date))
pronghorn.sp <- SpatialPointsDataFrame(coords = cbind(pronghorn$Easting, pronghorn$Northing), data = pronghorn[,c("Animal.ID", "date", "Capture.Area")], proj4string = CRS(target.crs))

fence.sp <- readOGR("./data/Fence_july2021_fieldupdated.shp")
fence.sp <- spTransform(fence.sp, CRS(target.crs))

# get brief BaBA (identify Quick Corss only) result.
for (d in c(50,60,70,80,91,100,110,120,130,141,150)) {
  BaBA.d <- BaBA_QC(animal = pronghorn.sp, barrier = fence.sp, d = d, interval =2, units = "hours")  
  writeOGR(BaBA.d$encounters, paste0("./result/BaBA_QC/BaBA_d", d, ".shp"), paste0("BaBA_d",d), driver = "ESRI Shapefile")
  write_csv(BaBA.d$classification, paste0("./result/BaBA_QC/BaBA_d",d, ".csv"))
}

# calculate # of Quick Cross across individuals at each buffer distance.
path <- paste0(getwd(), "/result/BaBA_QC/")
files <- dir(path = path, recursive = FALSE, pattern = "*\\.csv") #get all files in directory
QC_summary <- data.frame(NULL)
for (x in files) {
  d <- (x %>% str_split("_d") %>% unlist() %>% str_split("\\.") %>% unlist())[2]
  n <- paste0(path, x) %>%
    read_csv(.) %>%
    filter(eventTYPE == "Quick_Cross") %>%
    summarise( n = n()) %>% mutate (d = as.numeric(d))
  QC_summary <- rbind(QC_summary, n)
}

d_max <- QC_summary %>% filter(n == max(n)) %>% select(d)
# 110 

BaBA.d <- BaBA(animal = pronghorn.sp, barrier = fence.sp, d = d_max, interval =2, units = "hours")  
writeOGR(BaBA.d$encounters, paste0("./result/BaBA/BaBA_d", d_max, ".shp"), paste0("BaBA_d",d_max), driver = "ESRI Shapefile")
write_csv(BaBA.d$classification, paste0("./result/BaBA/BaBA_d",d_max, ".csv"))