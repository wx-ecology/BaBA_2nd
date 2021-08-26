setwd("C:/Users/wenjing.xu/Google Drive/RESEARCH/Pronghorn/Analysis/BaBA_Season2")
setwd("/Users/Mushy 1/Google Drive (wenjing.xu@berkeley.edu)/RESEARCH/Pronghorn/BaBA_Season2")

library(tidyverse)
library(lubridate)
library(patchwork) # for plotting multiple 
library(plotly) # interactive plot
library(htmlwidgets)
library(hrbrthemes)

##################################################
############### pronghorn data clean #############
############# and availability summary############
pronghorn1 <- read_csv("./data/00RawMovement/PAPOlocationTable.csv") %>% 
  mutate(Date = mdy_hm(Date, tz = "US/Mountain")  + hours(Hour)) %>% 
  select(Location.ID, Date, Latitude, Longitude, Easting, Northing, Altitute) %>% 
  mutate(Location.ID = paste0("PAPO_", Location.ID)) %>% 
  filter(!is.na(Date)) %>% 
  unique()

# now see which animal has repetitive datetime (indicate wried data situation)
bad.ID <- (pronghorn1 %>% group_by(Location.ID, Date) %>% summarise( n = n()) %>% select(-Date) %>% unique()  %>% filter (n > 1))$Location.ID
# PAPO_75 is bad
pronghorn1 <- pronghorn1 %>% filter (Location.ID != bad.ID)

pronghorn2 <- read_csv("./data/00RawMovement/JMHlocationTable.csv") %>% 
  mutate(Date = mdy_hm(Date, tz = "US/Mountain")  + hours(Hour)) %>% 
  select(Location.ID, Date, Latitude, Longitude, Easting, Northing, Altitute) %>% 
  mutate(Location.ID = paste0("JMH_", Location.ID)) %>%
  filter(!is.na(Date)) %>% 
  unique() # some records have repetitive rows (same time, same locations).
bad.ID <- (pronghorn2 %>% group_by(Location.ID, Date) %>% summarise( n = n()) %>% select(-Date) %>% unique()  %>% filter (n > 1))$Location.ID # ALL GOOD

pronghorn <- rbind(pronghorn1, pronghorn2)
length(unique(pronghorn$Location.ID)) #177 individuals
rm(pronghorn1, pronghorn2)

## data duration summary ##
pronghorn.summary <- pronghorn %>% group_by(Location.ID) %>% 
  summarise(begin = min(Date, na.rm = TRUE), end = max(Date, na.rm = TRUE), center = median(Date, na.rm = TRUE),  intervals = as.numeric(names(sort(table(diff(Date)),decreasing=TRUE)[1]))) %>% 
  mutate(duration = as.duration(begin %--% end),
         days = seconds_to_period(duration)) 
# figure showing start and end of all data available
pronghorn.summary %>% arrange(center) %>% mutate(Location.ID=factor(Location.ID, Location.ID)) %>% ggplot +
  geom_segment( aes(x=Location.ID, xend=Location.ID, y=as.Date(begin), yend=as.Date(end)), color="grey") +
  geom_point( aes(x=Location.ID, y=as.Date(begin)), color=rgb(0.2,0.7,0.1,0.5), size=3 ) +
  geom_point( aes(x=Location.ID, y=as.Date(end)), color=rgb(0.7,0.2,0.1,0.5), size=3 ) +
  scale_y_date(date_labels = "%Y", date_breaks = "1 year") + 
  coord_flip() +
  theme_ipsum() +
  theme(
    legend.position = "none",
  ) +
  xlab("ID") +
  ylab("Date")

write_csv(pronghorn, "./data/01CleanedMovement/Animal_Location_All_clean.csv")

##################################################
############### organize animal info #############
##################################################
jmh_info <- read_csv("./data/01CleanedMovement/FINAL_JMHanimalTableWX.csv") %>% 
  select(Location.ID,Capture.Area,Death.Date,Comments) %>% 
  mutate(Location.ID = paste0("JMH_", Location.ID))
papo_info <- read_csv("./data/01CleanedMovement/FINAL_PAPOanimalTableWX.csv") %>% 
  select(Location.ID,Capture.Area,Death.Date,Comments) %>% 
  mutate(Location.ID = paste0("PAPO_", Location.ID))

pronghorn_info <- rbind(jmh_info, papo_info)
pronghorn.summary <- pronghorn.summary %>% left_join(pronghorn_info, by = "Location.ID")

write_csv(pronghorn.summary %>% select(-center), "./data/01CleanedMovement/Animal_Info_All.csv")

##################################################
########## create tracks for visualization #######
##################################################
library(adehabitatLT)
pronghorn <- read_csv("./data/01CleanedMovement/Animal_Location_All_clean.csv") %>% mutate(Date = ymd_hms(Date, tz = "US/Mountain")) %>% 
  rename (date = Date, Animal.ID = Location.ID) %>% unique()
pronghorn.info <- read_csv("./data/01CleanedMovement/Animal_Info_All.csv") %>% dplyr::select(Location.ID, Capture.Area, intervals) %>%
  rename (Animal.ID = Location.ID) 
pronghorn <- pronghorn %>% left_join(pronghorn.info) %>% filter(!is.na(date))

pronghorn.2h <- pronghorn %>% filter(intervals == 2)
pronghorn.2h.tr <- as.ltraj(xy = as.data.frame(pronghorn.2h[, c("Easting", "Northing")]), 
                            date = pronghorn.2h$date, id = pronghorn.2h$Animal.ID, infolocs = as.data.frame(pronghorn.2h[, "Capture.Area"])) # 63 animals
pronghorn.2h.sp <- ltraj2sldf(pronghorn.2h.tr)
pronghorn.2h.sp@data <- pronghorn.2h.sp@data %>% left_join(.,(pronghorn.2h %>% select(Animal.ID, Capture.Area) %>% rename (id = Animal.ID) %>% unique()), by = "id")
write_csv(pronghorn.2h, "./data/pronghorn_2h_pts.csv")
writeOGR(pronghorn.2h.sp, "./data/pronghorn_2h_tracks.shp", "pronghorn_2h_tracks",driver="ESRI Shapefile")

pronghorn.3h <- pronghorn %>% filter(intervals == 3)
pronghorn.3h.tr <- as.ltraj(xy = as.data.frame(pronghorn.3h[, c("Easting", "Northing")]), 
                            date = pronghorn.3h$date, id = pronghorn.3h$Animal.ID) # 114 animals
pronghorn.3h.sp <- ltraj2sldf(pronghorn.3h.tr)
pronghorn.3h.sp@data <- pronghorn.3h.sp@data %>% left_join(., (pronghorn.3h %>% select(Animal.ID, Capture.Area) %>% rename (id = Animal.ID) %>% unique()), by = "id")
writeOGR(pronghorn.3h.sp, "./data/pronghorn_3h_tracks.shp", "pronghorn_3h_tracks",driver="ESRI Shapefile")

##################################################
############### previous baba result #############
##################################################

# load previous results
data <- read.csv("./data/old/I2_PRON_FB110_B4_P36_FinalCls.csv")
data$date <- ymd_hm (data$burstID, tz = "US/Mountain") 

# total encounter monthly trend
data1 <- data %>% mutate(month = month(date)) %>% 
  group_by(AnimalID, month) %>% summarize(freq = n())

p1 <- ggplot(data = data1, aes(x = month, y = freq, color = AnimalID)) +
  geom_line() +
  theme_bw() + 
  theme(legend.position = "none") 


# normal BaBA monthly trend
data2 <- data %>% mutate(month = month(date),
                         eventCAT = ifelse((eventTYPE %in% c("Bounce", "Trace", "Back-n-forth", "Trapped")), "altered", "normal")) %>% 
  group_by(AnimalID, month, eventCAT) %>% summarize(freq = n())

p2 <- ggplot(data = data2 %>% filter(eventCAT == "normal"), aes(x = month, y = freq, color = AnimalID)) +
  geom_line() +
  theme_bw() + 
  theme(legend.position = "none") 

# altered BaBA monthly trend
data3 <- data %>% mutate(month = month(date),
                         eventCAT = ifelse((eventTYPE %in% c("Bounce", "Trace", "Back-n-forth", "Trapped")), "altered", "normal")) %>% 
  group_by(AnimalID, month, eventCAT) %>% summarize(freq = n())

p3 <- ggplot(data = data3 %>% filter(eventCAT == "altered"), aes(x = month, y = freq, color = AnimalID)) +
  geom_line() +
  theme_bw() + 
  theme(legend.position = "none") 

p1 + p2 + p3
ggplotly(p1)
ggplotly(p3)
