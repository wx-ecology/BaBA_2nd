#baba around 2 with all animals
setwd("/Users/Mushy 1/Google Drive (wenjing.xu@berkeley.edu)/RESEARCH/Pronghorn/BaBA_Season2")
setwd("C:/Users/wenjing.xu/Google Drive/RESEARCH/Pronghorn/BaBA_Season2")
target.crs <- "+init=epsg:32612"

library(tidyverse)
library(lubridate)
library(BaBA)

## read files and prep for BaBA
pronghorn <- read_csv("./data/Animal_Location_All.csv") %>% mutate(Date = ymd_hms(Date, tz = "US/Mountain")) %>% 
  rename (date = Date, Animal.ID = Location.ID) 
# calculate GPS data interval
Intervals <- tapply(pronghorn$date, pronghorn$Animal.ID, function(x) as.numeric(names(which.max(table(as.numeric(diff(x), units = "hours"))))))
Intervals <- data.frame(Animal.ID = names(Intervals), interval = Intervals)
pronghorn <- pronghorn %>% left_join(Intervals) %>% filter(!is.na(date))
pronghorn.2h <-pronghorn %>% filter(interval ==2) # 63 animals
pronghorn.3h <- pronghorn %>% filter(interval ==3) # 115 animals
pronghorn.3h.1 <- pronghorn.3h %>% filter(Animal.ID %in% unique(pronghorn.3h$Animal.ID)[1:70])
pronghorn.3h.2 <- pronghorn.3h %>% filter(Animal.ID %in% unique(pronghorn.3h$Animal.ID)[71:length(unique(pronghorn.3h$Animal.ID))])

pronghorn.2h.sp <- SpatialPointsDataFrame(coords = cbind(pronghorn.2h$Easting, pronghorn.2h$Northing), data = pronghorn.2h[,1:2], proj4string = CRS(target.crs))
pronghorn.3h.sp.1 <- SpatialPointsDataFrame(coords = cbind(pronghorn.3h.1$Easting, pronghorn.3h.1$Northing), data = pronghorn.3h.1[,1:2], proj4string = CRS(target.crs))
pronghorn.3h.sp.2 <- SpatialPointsDataFrame(coords = cbind(pronghorn.3h.2$Easting, pronghorn.3h.2$Northing), data = pronghorn.3h.2[,1:2], proj4string = CRS(target.crs))

#read fence file
fence.sp <- readOGR("./data/Fence_may2021_single.shp")
fence.sp <- spTransform(fence.sp, CRS(target.crs))
#fence.segs.sp <- readOGR("./data/", 'Fence_may2021_2kmSegs')

## run baba 
pronghorn.2h.baba.110 <- BaBA(animal = pronghorn.2h.sp, barrier = fence.sp, d = 110, interval =2, units = "hours")
write.csv(pronghorn.2h.baba.110$classification, "./result/prong2h_d110_b4.csv")
writeOGR(pronghorn.2h.baba.110$encounters, "./result", "prong2h_d110_b4", driver = "ESRI Shapefile", overwrite_layer = T)

pronghorn.2h.baba.110.2 <- BaBA(animal = pronghorn.2h.sp, barrier = fence.sp, d = 110, interval =2, b_time = 6, units = "hours")
write.csv(pronghorn.2h.baba.110.2$classification, "./result/prong2h_d110_b6.csv")
writeOGR(pronghorn.2h.baba.110.2$encounters, "./result", "prong2h_d110_b6", driver = "ESRI Shapefile", overwrite_layer = T)

pronghorn.3h1.baba.110 <- BaBA(animal = pronghorn.3h.sp.1, barrier = fence.sp, d = 110, interval =3, b_time = 6, units = "hours")
write.csv(pronghorn.3h1.baba.110$classification, "./result/prong3h1_d110_b6.csv")
writeOGR(pronghorn.3h1.baba.110$encounters, "./result", "prong3h1_d110_b6", driver = "ESRI Shapefile", overwrite_layer = T)

pronghorn.3h2.baba.110 <- BaBA(animal = pronghorn.3h.sp.2, barrier = fence.sp, d = 110, interval =3, b_time = 6, units = "hours")
write.csv(pronghorn.3h2.baba.110$classification, "./result/prong3h2_d110_b6.csv")
writeOGR(pronghorn.3h2.baba.110$encounters, "./result", "prong3h2_d110_b6", driver = "ESRI Shapefile", overwrite_layer = T)

## ba ranking
fence.eva.sp <- readOGR("./data/Fence_june2021_2kmSegs.shp")
fence.eva.sp <- spTransform(fence.eva.sp, CRS(target.crs))
fence.eva.sp$ID <- seq(1:nrow(fence.eva.sp))

Classification <- rbind(read.csv("./result/prong2h_d110_b6.csv"),
                         read.csv("./result/prong3h1_d110_b6.csv"),
                         read.csv("./result/prong3h2_d110_b6.csv"))

Fence.ranks.1 <- BaRanking(Classification, barrier = fence.eva.sp, d = 110, Barrier_ID = "ID", min_total_enc = 0)
Fence.ranks.1 [is.na(Fence.ranks.1)] = 0
st_write(Fence.ranks.1, "./result/Fence_ranks_d110_b6.shp")

