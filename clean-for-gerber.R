# Prep batch 1 & 2 data (2016-2018) for Brian Gerber et al. collaboration

library(dplyr)
library(readr)

all_records <- read_csv("~/Documents/github-repos/gorongosa/gorongosa-shiny/wildcam_fulldata_2019.csv") 

all_records_clean <- all_records %>% 
    # filter to just batches 1 and 2 (based on classifiers)
    filter(grepl('KG/Undergrads', Classifier)) %>% 
    # remove columns that they aren't interested in
    select(-c(Classifier, juvenile, moving, eating, resting, standing, interacting, male, directory,
              filename, datetime, juvenile_count, male_count)) %>% 
    # add columns with new information about project
    mutate("General Project" = "Gorongosa",
           "Contact Person's Last Name" = "Gaynor",
           "Country" = "Mozambique",
           "Timezone" = "Africa/Maputo",
           "UTM zone" = NA,
           "General Sampling Period Name" = "2016-2018",
           "Camera Make" = "Bushnell",
           "Camera Model" = "TrophyCam",
           "Flash (Yes or no)" = "No",
           "Bait used" = "No",
           "Bait type (if applicable)" = NA,
           "Photos per trigger" = 2,
           "Class" = "Mammalia") %>% 
    # rename columns as desired
    rename("Site Name" = "site",
           "Photo Date" = "date",
           "Photo Time" = "time",
           "Number of Animals" = "count")

# bring in camera locations
locations <- read.csv("~/Documents/github-repos/gorongosa/gorongosa-camera-traps/data/cam_metadata_fromfield.csv") %>% 
    select(StudySite, Latitude, Longitude) %>% 
    rename("Site Name" = StudySite)

# bring in species traits
species <- read.csv("~/Documents/github-repos/gorongosa/gorongosa-camera-traps/data/2018spp_kingdon.csv") %>%
    mutate(species = tolower(CommName)) %>%  # make lowercase to match MP format
    select(species, Order, Family, GenusLatin, SpeciesLatin, Diet) %>% 
    rename("Genus" = "GenusLatin",
           "Species" = "SpeciesLatin")

# join em together
all_records_clean2 <- all_records_clean %>% 
    left_join(species) %>% 
    left_join(locations) %>% 
    select(-species) # remove common name column

# bring in camera operation
camop <- read.csv("operation-files/Camera_operation_years1and2.csv") %>% 
    rename("Site Name" = Camera) %>%
    mutate_at(c("Start", "End", "Problem1_from", "Problem1_to",
                "Problem2_from", "Problem2_to",
                "Problem3_from", "Problem3_to"),
              ~as.Date(., format = "%m/%d/%y"))







# merge camera start, end, and problem dates with the record table
all_records_clean3 <- left_join(all_records_clean2, camop) 

# label records to drop if outside of operation date (either before start, after end, or during problem window)
# ALSO drop any that were taken ON the day that the camera started/ended, or problem started/ended, so we can
# cleanly start days at midnight
# this loop takes a while to run
all_records_clean3$drop <- NA 
for (i in 1:nrow(all_records_clean3)) {
    if (all_records_clean3$`Photo Date`[i] < all_records_clean3$Start[i]) {
        all_records_clean3$drop[i] <- TRUE}
    else if (all_records_clean3$`Photo Date`[i] > all_records_clean3$End[i]) {
        all_records_clean3$drop[i] <- TRUE}
    else if ((is.na(all_records_clean3$Problem1_from[i]) == FALSE) & (all_records_clean3$`Photo Date`[i] >= all_records_clean3$Problem1_from[i]) & (all_records_clean3$`Photo Date`[i] <= all_records_clean3$Problem1_to[i])) {
        all_records_clean3$drop[i] <- TRUE}
    else if ((is.na(all_records_clean3$Problem2_from[i]) == FALSE) & (all_records_clean3$`Photo Date`[i] >= all_records_clean3$Problem2_from[i]) & (all_records_clean3$`Photo Date`[i] <= all_records_clean3$Problem2_to[i])) {
        all_records_clean3$drop[i] <- TRUE}
    else if ((is.na(all_records_clean3$Problem3_from[i]) == FALSE) & (all_records_clean3$`Photo Date`[i] >= all_records_clean3$Problem3_from[i]) & (all_records_clean3$`Photo Date`[i] <= all_records_clean3$Problem3_to[i])) {
        all_records_clean3$drop[i] <- TRUE}
    else {
        all_records_clean3$drop[i] <- FALSE}
}

# see where the issues are
summary(all_records_clean3$drop)
to_drop <- all_records_clean3[all_records_clean3$drop == TRUE,] %>% 
    select("Site Name", "Photo Date", "Photo Time", "Start", "End", "Problem1_from", 
           "Problem1_to", "Problem2_from", "Problem2_to", "Problem3_from", "Problem3_to")

# exclude records outside of operation dates
all_records_clean4 <- all_records_clean3[all_records_clean3$drop == FALSE,]
