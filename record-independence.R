## Explore different methods for thinning data to independent records

library(dplyr)
library(readr)

# import data
wildcam_fulldata_2019 <- read_csv("2019-data/wildcam_fulldata_2019.csv") # data

# KG method ---------------------------------------------------------------

records <- wildcam_fulldata_2019

# sort records by station, species, then time
records <- records[order(records$site, records$species, records$datetime),]

# format dates
records$datetime <- as.POSIXct(records$datetime)

# remove identical rows
records$tag <- paste(records$site, records$species, records$datetime)
records <- records[!duplicated(records$tag),]
nrow(records) # 299764

# run loop to calculate delta time
records$delta.time.secs <- NA # create column for delta time
records$delta.time.secs[[1]] <- 0 # set first row to 0
for (i in 2:nrow(records)) {
    
    if(records$site[[i]] == records$site[[i-1]] &
       records$species[[i]] == records$species[[i-1]]) {
        
        # calculate difference 
        records$delta.time.secs[[i]] <- difftime(records$datetime[[i]], 
                                                 records$datetime[[i-1]], 
                                                 units = "secs")
        
    } else {
        
        records$delta.time.secs[[i]] <- 0
        
    }
    
}

test1 <- records %>%
    filter(delta.time.secs == 0 | delta.time.secs >= (60 * 15))

nrow(test1) # 152450


# MP method ---------------------------------------------------------------

fulldat <- wildcam_fulldata_2019

# remove duplicate sightings (15 mins independence interval)
fulldat$datetime <- strptime(fulldat$datetime, "%Y-%m-%d %H:%M:%S")
fulldat$tag <- paste(fulldat$site, fulldat$species, fulldat$datetime)
fulldat <- fulldat[!duplicated(fulldat$tag),]
nrow(fulldat) # 299764

fulldat <- fulldat[order(fulldat$site, fulldat$species, fulldat$datetime),]
fulldat$index <- paste(fulldat$site, fulldat$species) #index column is a tag for each unique species/site combo
fulldat$delta.time.mins <- unlist(tapply(fulldat$datetime, INDEX = fulldat$index,
                                         FUN = function(x) c(0, `units<-`(diff(x), "mins"))))

test2 <- fulldat %>% filter(delta.time.mins == 0 | delta.time.mins >= 15)

nrow(test2) # 152450
