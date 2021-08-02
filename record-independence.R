## Explore different methods for thinning data to independent records


# KG method ---------------------------------------------------------------

# sort records by station, species, then time
records <- records[order(records$Camera, records$species, records$datetime),]

# format dates
records$datetime <- as.POSIXct(records$datetime)

# run loop to calculate delta time
records$delta.time.secs <- NA # create column for delta time
records$delta.time.secs[[1]] <- 0 # set first row to 0
for (i in 2:nrow(records)) {
    
    if(records$Camera[[i]] == records$Camera[[i-1]] &
       records$species[[i]] == records$species[[i-1]]) {
        
        # calculate difference 
        records$delta.time.secs[[i]] <- difftime(records$datetime[[i]], 
                                                 records$datetime[[i-1]], 
                                                 units = "secs")
        
    } else {
        
        records$delta.time.secs[[i]] <- 0
        
    }
    
}

# MP method ---------------------------------------------------------------

# import data
fulldat <- read.csv("wildcam_fulldata_2019.csv") # data
fullse <- read.csv("wildcam_fulleffort_2019.csv") # search effort 

# fix date-times
fulldat$Date <- strptime(fulldat$date, "%Y-%m-%d")
fulldat$Date <- as.Date(fulldat$Date); fulldat$date <- NULL
fullse$Date <- strptime(fullse$date, "%Y-%m-%d")
fullse$Date <- as.Date(fullse$Date); fullse$date <- NULL

# remove duplicate sightings (15 mins independence interval)
fulldat$datetime <- strptime(fulldat$datetime, "%Y-%m-%d %H:%M:%S")
fulldat$tag <- paste(fulldat$site, fulldat$species, fulldat$datetime)
fulldat <- fulldat[!duplicated(fulldat$tag),]

fulldat <- fulldat[order(fulldat$site, fulldat$species, fulldat$datetime),]
fulldat$index <- paste(fulldat$site, fulldat$species) #index column is a tag for each unique species/site combo
fulldat$delta.time.mins <- unlist(tapply(fulldat$datetime, INDEX = fulldat$index,
                                         FUN = function(x) c(0, `units<-`(diff(x), "mins"))))
fulldat$datetime <- as.character(fulldat$datetime)
fulldat <- fulldat %>% filter(delta.time.mins == 0 | delta.time.mins >= 15)

