#baba around 2 with all animals with 2h intervals
setwd("/Users/Mushy 1/Google Drive (wenjing.xu@berkeley.edu)/RESEARCH/Pronghorn/BaBA_Season2")
target.crs <- "+init=epsg:32612"

library(tidyverse)
library(lubridate)
library(BaBA)

pronghorn <- read_csv("./data/pronghorn_2h_pts.csv") %>% mutate(date = ymd_hms(date, tz = "US/Mountain")) %>% filter(!is.na(date))
pronghorn.sp <- SpatialPointsDataFrame(coords = cbind(pronghorn$Easting, pronghorn$Northing), data = pronghorn[,c("Animal.ID", "date", "Capture.Area")], proj4string = CRS(target.crs))

fence.sp <- readOGR("./data/Fence_july2021_fieldupdated.shp")
fence.sp <- spTransform(fence.sp, CRS(target.crs))

for (d in seq(50,150,10)) {
  BaBA.d <- BaBA_QC(animal = pronghorn.sp, barrier = fence.sp, d = d, interval =2, units = "hours")  
  writeOGR(BaBA.d$encounters, "./result/BaBA/", paste0("BaBA_d",d), driver = "ESRI Shapefile")
  write_csv(BaBA.d$classification, paste0("./result/BaBA/BaBA_d",d, ".csv"))
}


BaBA.d <- BaBA(animal = animal, barrier = fence.sp, d = d, interval =2, units = "hours")  
BaBA.qc <- BaBA_QC(animal = animal, barrier = fence.sp, d = d, interval =2, units = "hours")  

BaBA.d <- BaBA.d$classification %>% select(burstID, duration, eventTYPE)
BaBA.qc <- BaBA.qc$classification %>% select(burstID, duration, eventTYPE)

sum(BaBA.d$eventTYPE == "Quick_Cross")
sum(BaBA.qc$eventTYPE == "Quick_Cross")
