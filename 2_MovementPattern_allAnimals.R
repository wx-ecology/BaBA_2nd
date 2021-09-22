## this step calculate movement metrics, local fence density and produce data table to be put into the multivariate models


################################
########### set up  ############
################################

setwd("G:/My Drive/RESEARCH/Pronghorn/BaBA_Season2/")
setwd("/Users/Mushy 1/Google Drive (wenjing.xu@berkeley.edu)/RESEARCH/Pronghorn/BaBA_Season2/")

library(tidyverse)
library(amt)
library(hrbrthemes)
library(lubridate)

## read data
# raw movement data
pronghorn <- read_csv("./data/pronghorn_2h_pts.csv") %>% 
  mutate(date = ymd_hms(date, tz = "US/Mountain"),
         mo = month(date),
         yr = year(date),
         dy = day(date),
         id_yr_mo = paste0(Animal.ID, "-", yr, "-", mo)) %>% 
  filter(!is.na(date)) %>%
  filter(Animal.ID != "PAPO_138") %>% 
  filter (!(Anmal.ID %in% c("PAPO_163-2016-1", "PAPO_163-2016-2", 
                            "PAPO_164-2016-1", "PAPO_164-2016-2"))) #bad gps data

# remove individual month that is not complete (at least have 28-day data in a month)
pronghorn1 <- pronghorn %>% dplyr::select(id_yr_mo, dy) %>% 
  group_by(id_yr_mo) %>% 
  summarise(n = length(unique(dy))) %>% filter(n >=28)  #filtered out 114
id_mo_complete <- pronghorn1$id_yr_mo #855
pronghorn <- pronghorn %>% filter(id_yr_mo %in% id_mo_complete)

length(unique(pronghorn$Animal.ID)) #62
length(unique(pronghorn$id_yr_mo)) #855

# animal info 
ids <- unique(pronghorn$Animal.ID)
pronghorn.info <- read_csv("./data/01CleanedMovement/Animal_Info_All.csv") %>% filter(Location.ID %in% ids) 
rm(ids, pronghorn1)


################################
####extract movement metrics####
################################

# make movement tracks for each animal_month and calculate monthly step length and max nsd
pronghorn.trk <- pronghorn %>% 
  make_track(Easting, Northing, date, id = id_yr_mo) %>% 
  nest(data = -'id')

pronghorn.step <- pronghorn.trk %>%
  mutate(steps = 
           map(data, function(x) 
             x %>% track_resample(rate = minutes(120), tolerance = minutes(5)) %>% steps_by_burst()),
         nsd = 
           map(data, function(x)   x%>%nsd()))

#calculate monthly accumulated step length
step_sum <- pronghorn.step %>% unnest(cols = steps) %>%
  group_by(id) %>% summarise(total_step_lengths = sum(sl_))

#calculate monthly max displacement
nsd_sum <- pronghorn.step %>% unnest_longer(nsd) %>% 
  group_by(id) %>% summarise(max_displacement = sqrt(max(nsd)))

# merge with animal info
pronghorn.sum <- step_sum %>% left_join(nsd_sum) %>% separate (id, c("id", "yr", "mo"), sep = "-")
pronghorn.sum <- pronghorn.info %>% dplyr::select(Location.ID, Capture.Area) %>% 
  rename (id = Location.ID) %>% left_join(pronghorn.sum) %>% 
  mutate(id_yr_mo = paste0(id, "-", yr, "-", mo)) 

# pronghorn.sum <- pronghorn.sum %>% mutate(id_yr_mo = paste0(id, "-", yr, "-", mo)) %>% 
#   left_join(pronghorn %>% dplyr::select(id_yr_mo))
rm(step_sum, nsd_sum)

################################
########  add baba info ########
################################

## add baba info (baba at d = 110)
pronghorn.baba <- read_csv("./result/BaBA/BaBA_d110max1.csv") %>% 
  dplyr::select(AnimalID, start_time, eventTYPE) %>%
  mutate(mo = month(start_time),
         yr = year(start_time)) %>% 
  rename(id = AnimalID) %>% 
  mutate(id_yr_mo = paste0(id,"-",yr, "-", mo)) %>%
  group_by(id_yr_mo, eventTYPE) %>% 
  summarise( n = n()) %>% #summarize # of event each month
  pivot_wider(names_from = eventTYPE, values_from = n, values_fill = 0) %>% 
  mutate (total_encounter = 
            (Average_Movement + Bounce + Quick_Cross + unknown + Back_n_forth + Trace + Trapped),
          unknown_rate =
            unknown/total_encounter, 
          total_encounter_known = 
            (Average_Movement + Bounce + Quick_Cross + Back_n_forth + Trace + Trapped),
          crossing_rate =
            Quick_Cross/total_encounter_known, 
          normal_rate =
            (Average_Movement + Quick_Cross)/total_encounter_known)  # n = 688

# filter out animals that do not have complete montly data
pronghorn.baba <- pronghorn.baba %>% filter(id_yr_mo %in% id_mo_complete) # n = 670

summary(pronghorn.baba$unknown_rate) # low unknown rate. should be fine.
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.00000 0.00000 0.00000 0.02674 0.04348 0.33333 

# combine dataframes
pronghorn.sum <- pronghorn.baba %>%
  dplyr::select(id_yr_mo, total_encounter, crossing_rate, normal_rate) %>% 
  right_join(pronghorn.sum, by = c("id_yr_mo")) %>% 
  mutate(mo = as.double(mo))  %>%
  arrange(id, mo) # 855

###########################
####Local fence density####
###########################
library(sf)
library(BBMM)
library(raster)
target.crs <- "+init=epsg:32612"
fence <- read_sf("./data/Fence_july2021_fieldupdated.shp")

# funtion that extract line density in a polygon
get_density <- function(polygon, line) {
  
  require(tidyverse)
  require(lubridate)
  require(sf)
  
  options(warn=-1)
  
  polygon <- st_make_valid(polygon)
  subline <- line %>% sf::st_intersection(., polygon) %>%
    dplyr::mutate(length_line = st_length(.),
                  length_line = ifelse(is.na(length_line), 0, length_line))
  tol.length <- sum(subline$length_line)
  
  density <-  tol.length/st_area(polygon)
  return(density)
}

# calculate time differences - prep data for BBMM
pronghorn <- pronghorn %>% group_by(id_yr_mo) %>% 
  mutate(time.lag = (date - lag(date, default = date[1]))/60) %>%
  filter(time.lag != 0)

# for each animal-month, run bbmm and get line density within the 99% BBMM
hr_fence_density <- data.frame() # create an empty list for density
for (i in unique(pronghorn$id_yr_mo)[835: length(unique(pronghorn$id_yr_mo))]){
  animal.i <- pronghorn %>% filter(id_yr_mo == i)
  
  BBMM = brownian.bridge(x=animal.i$Easting, y=animal.i$Northing, 
                         time.lag=as.numeric(animal.i$time.lag),location.error=30, cell.size = 100)  
  ### --------- ask HALL whether 30 is good ----- ##################
  
  # get 99% contour and turn it into polygon
  contours <- bbmm.contour(BBMM, levels=c(99), locations=locations, plot=FALSE)
  BBMM.df <-  data.frame(x=BBMM$x,y=BBMM$y,z=BBMM$probability)
  BBMM.raster <- rasterFromXYZ(BBMM.df, crs=target.crs, digits=2)
  raster.contour <- rasterToContour(BBMM.raster,levels=contours$Z)
  contour <- st_as_sf(raster.contour)
  if (st_geometry_type(st_as_sf(raster.contour)) == "MULTILINESTRING") {
    contour <- st_cast(contour, "MULTIPOLYGON") %>% dplyr::select(-level) %>% mutate (id_mo = i)
  } else {
    contour <- st_cast(contour, "POLYGON") %>%  dplyr::select(-level) %>% mutate (id_mo = i)
  }
  
  # st_buffer(., 110) %>%   #### need to think more about this buffer distance #################
  
  #get density 
  density <- get_density(contour, fence)
  
  hr_fence_density <- rbind(hr_fence_density, data.frame(id_yr_mo = i, fence_density = density))
  print(i)
}

pronghorn.sum <- hr_fence_density %>% right_join(pronghorn.sum) %>%
  filter(!is.na(fence_density), !is.na(total_encounter)) # 822.

#write_csv(pronghorn.sum, "G:/My Drive/RESEARCH/Pronghorn/BaBA_Season2/result/prong_df_glmm.csv")

# ##############################################
# ############## intial visualization ##########
# ##############################################
# 
# pronghorn.sum %>% 
#   ggplot (aes(x = max_displacement, y = normal_rate)) +
#   geom_point() + 
#   geom_smooth(method = "lm") +
#   theme_minimal() 
# 
# pronghorn.sum %>% 
#   ggplot (aes(x = normal_rate, y = total_step_lengths)) +
#   geom_point() + 
#   geom_smooth(method = "lm") +
#   theme_minimal() 
