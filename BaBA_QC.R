# # for debugging
# animal = pronghorn.sp[pronghorn.sp$Animal.ID == "PAPO_100", ]
# barrier = fence.sp
# d = 110
# interval = 2
# b_time = 6
# units = "hours"
# max_cross = 0
# tolerance = 0
# exclude_buffer = F
# export_images = F
# 

BaBA_QC <-
  function(animal, barrier, d, 
           interval = NULL, b_time = 4, tolerance = 0, units = "hours", 
           max_cross = 0, exclude_buffer = F, 
           export_images = F, img_path = "event_imgs", img_suffix = NULL) {
    
    #############################################
    ############### initial checks ##############
    #############################################
    if(export_images) {
      if(!dir.exists(img_path)) dir.create(img_path)
    }
    
    # prepare parameters and check input ------
    if (!(class(animal)[1] == "SpatialPointsDataFrame")) stop("animal needs to be a SptialPointsDataFrame")
    if (!(class(barrier)[1] == "SpatialLinesDataFrame")) stop("barrier needs to be a SptialLinesDataFrame")
    if (!"date" %in% names(animal)) stop("please rename the date column to 'date'")
    if (!"Animal.ID" %in% names(animal)) stop("please rename the individual ID column to 'Animal.ID'")
    if (!(inherits(animal$date, "POSIXct"))) stop("date needs to be 'POSIXct' format")
    if (sum(is.na(animal$date)) > 0 ) stop("please exclude rows where date is NA")
    
    interval_per_individual <- tapply(animal$date, animal$Animal.ID, function(x) names(which.max(table( as.numeric(diff(x), units = units)))))
    if(is.null(interval)) { # figure out interval (as the most frequent difference in timestamp) if not provided but give an error if not the same for all individuals
      if(all(interval_per_individual == interval_per_individual[1])) interval <- as.numeric(interval_per_individual[1]) else stop("Not all individuals have been sampled at the same frequency. Run individuals with different intervals seperately, or double-check whether your date column is cleaned.")
    } else {
      if (any(as.numeric(interval_per_individual) > interval, na.rm = T)) stop("BaBA interval needs to be no smaller than the actual data interval. Also double-check whether your date column is cleaned.") 
    }
    
    b <- b_time / interval
    if(b < 1) stop("interval needs to be set no bigger than b_time")
    if (round(b) != b) stop("b_time must be divisible by interval")
    
    #############################################
    ########## detect encounter events ##########
    #############################################
    
    # create point ID by individual -------
    animal <- animal[order(animal$"Animal.ID", animal$date), ]
    animal$ptsID <- NA
    
    for (i in unique(animal$Animal.ID)) {
      mov.seg.i <- animal[animal$Animal.ID == i, ]
      animal@data$ptsID[animal$Animal.ID == i] <-
        seq(nrow(mov.seg.i))
    }
    
    # create buffer around barrier ----
    print("locating encounter events...")
    barrier_buffer <- raster::buffer(barrier, width = d)
    
    # ---- classification step 1: generate encountering event dataframe ---- ####
    
    ## extract points that fall inside the buffer ----
    encounter <- raster::intersect(animal, barrier_buffer)
    
    ## create a burstID ----
    
    for(i in unique(encounter$Animal.ID)){
      
      encounter_i <- encounter[encounter$Animal.ID == i,]
      
      if (nrow(encounter_i) == 0) {
        warning(paste0 ("Individual ", i, " has no locations overlapped with the barrier buffer and is eliminated from analysis." ))
        next()
      }
      
      ## first get time difference between all points in the buffer
      encounter_i$timediff <- c(interval, as.numeric(diff(encounter_i$date), units = units))
      ## then remove the interval from that so when there is no missing point, timediff2 should be 0. If <0, replicated timestamp; if >0, missing timestamp
      encounter_i$timediff2 <- round(encounter_i$timediff - interval, digits = 1)
      
      ## then, if any timediff2 is > interval but <= tolerance, we need to bring in the missing points from outside the buffer.
      if(any(encounter_i$timediff2 > interval & encounter_i$timediff2 <= tolerance, na.rm = T )) {
        
        idx_pts_of_interest <- which(encounter_i$timediff2 > interval & encounter_i$timediff2 <= tolerance)
        
        for(pt in idx_pts_of_interest) {
          # find out what pts to fetch
          ptsID_of_interest_B <- encounter_i$ptsID[pt]
          ptsID_of_interest_A <- encounter_i$ptsID[pt-1]
          
          # fetch the points outside of the buffer and placehold timediff as NA and timediff2 as 0
          fetched_pt <- animal[animal$Animal.ID == i & 
                                 animal$ptsID > ptsID_of_interest_A & 
                                 animal$ptsID < ptsID_of_interest_B, ]
          
          if (nrow(fetched_pt) == 0) {  # if there's no point outside of the buffer between the timestamp that means there's missing data
            # since the missing data is still within the tolerence, we consider timediff2=0 so the points before and after will be in the same event
            encounter_i$timediff2[pt] <- 0
            next() } 
          else {
            fetched_pt$timediff <- NA
            fetched_pt$timediff2 <- 0 
            # replace timediff2 of pts_of_interests to 0
            encounter_i$timediff2[pt] <- 0 
            # append fetched points to each other 
            if(pt == idx_pts_of_interest[1]) fetched_pts <- fetched_pt else fetched_pts <- rbind(fetched_pts, fetched_pt)
          }
        }
        
        # append fetched pts
        encounter_i <- rbind(encounter_i, fetched_pts)
        #recorder animal i encounter event dataframe
        encounter_i <- encounter_i[order(encounter_i$ptsID), ]
      }
      
      ## then do the cum sum of the new dataframe based on timediff2, using that as the unique burst ID (with animalID) 
      encounter_i$burstID <- paste(i, cumsum(encounter_i$timediff2), sep = "_")
      
      # save into encounter_complete ####
      if(i == unique(encounter$Animal.ID[1])) encounter_complete <- encounter_i else encounter_complete <- rbind(encounter_complete, encounter_i)
    }
    
    encounter <- encounter_complete # save back as encounter (encoutner_complete is bigger as it includes extra points that are within tolerance)
    
    #############################################
    ########## classify short events ############
    #############################################
    print("locating Quick Cross...") 
    ### open progress bar ----
    pb <- txtProgressBar( style = 3)
    
    ### create empty object that will hold results ----
    event_df <- NULL
    
    ## run classification procedure for each encounter ####
    for(i in unique(encounter$burstID)) {
      
      # update progress bar
      setTxtProgressBar(pb, which(unique(encounter$burstID) == i)/length(unique(encounter$burstID)))
      
      # get what we need from the encounter ####
      encounter_i <- encounter[encounter$burstID == i, ]
      animal_i <- animal[animal$Animal.ID == encounter_i$Animal.ID[1],]
      start_time <- encounter_i$date[1]
      end_time <- encounter_i$date[nrow(encounter_i)]
      duration <-  difftime (end_time, start_time, units = units)
      
      # calculating straightness of the encounter event ###
      ## this will be used for median duration event but is output for reference for other event ####
      straightness_i <- strtns(encounter_i)
      
      # classify short encounters (bounce and quick cross) ####
      # if no more than b*interval, only spend small amount of time in this burst
      if (duration <= b_time) {
        pt.first <- encounter_i$ptsID[1]#first point in the burst
        pt.last <- encounter_i$ptsID[nrow(encounter_i)]
        
        # extract movement segment with one point before and one point after the segmentation ####
        mov_seg_i <- movement.segment.b(animal_i, pt.first, pt.last)
        
        # count the number of crossing ####
        int.num <- length(rgeos::gIntersection(mov_seg_i, barrier))
        
        # if no crossing and we didn't have both points (before and after), then we can't tell if it crossed
        if (int.num == 0 & nrow(coordinates(mov_seg_i)[[1]][[1]]) != (nrow(encounter_i)+2)) {
          # means that no points were before or after the encounter and we can't tell if the animal crossed
          classification <- "unknown"
        } else {
          classification <- ifelse(int.num == 0, "Not_Quick_Cross", "Quick_Cross")
        }
      } else { classification <- "Not_Quick_Cross" }
      
      event_df <- rbind(event_df, data.frame(
        AnimalID = encounter_i$Animal.ID[1],
        burstID = i,
        easting = coordinates(encounter_i)[1, 1],
        northing = coordinates(encounter_i)[1, 2],
        start_time,
        end_time,
        duration,
        cross = int.num,
        eventTYPE = classification,
        stringsAsFactors = F
      ))
    }
    
    ### close progress bar ----
    close(pb)
    
    print("creating dataframe...")
    ## clean the encounter sp dataframe ##
    encounter <- encounter[!duplicated(encounter@data$burstID),]
    encounter@data <- encounter@data[,c("Animal.ID","burstID","date")]
    encounter@data <- merge(encounter@data, event_df[,c("burstID","eventTYPE")])
    
    ## return output as a lits ####
    return(list(encounters = encounter,
                classification = event_df))
  }


#increase movement segment by one points before and one point after the focused encounter ####
movement.segment.b <- function(animal, pt1, pt2) {
  segments <- animal[animal$ptsID >= pt1 - 1 &
                       animal$ptsID <= pt2 + 1, ]
  seg.line <- Lines(Line(coordinates(segments)),
                    ID = segments$date[1])
  
  segments.sp <- SpatialLines(list(seg.line), proj4string = animal@proj4string)
  
  return(segments.sp)
}


# calculate straigness of movement segment ####
strtns <- function(mov_seg) {
  
  if (sum(duplicated(mov_seg$date)) > 0 ) {
    straightness = NA
    # warning("There are duplicated timestamps")
  } else {
    
    # calculate trajectory
    traj <- adehabitatLT::as.ltraj(xy = coordinates(mov_seg), date = mov_seg$date, id = as.character(mov_seg$Animal.ID))
    
    #moving distance from first pt to last pt in the burst
    traj.dist <- sqrt(
      (traj[[1]]$x[1] - traj[[1]]$x[nrow(traj[[1]])]) * (traj[[1]]$x[1] - traj[[1]]$x[nrow(traj[[1]])]) +
        (traj[[1]]$y[1] - traj[[1]]$y[nrow(traj[[1]])]) * (traj[[1]]$y[1] - traj[[1]]$y[nrow(traj[[1]])])
    )
    
    #sum of all step lengths
    traj.lgth <- sum(traj[[1]]$dist, na.rm = TRUE)
    
    #straightness ranges from 0 to 1. More close to 0 more sinuous it is.
    straightness <- traj.dist/traj.lgth }
  
  return(straightness)
}