# Prep batch 1 & 2 data (2016-2018) for Erin Phillips & Catherine Keim collaboration

library(dplyr)
library(readr)
library(tidyr)
library(forcats)

all_records <- read_csv("~/Documents/github-repos/gorongosa/gorongosa-shiny/wildcam_fulldata_2019.csv") 

all_records_clean <- all_records %>% 
    # filter to just batches 1 and 2 (based on classifiers)
    filter(grepl('KG/Undergrads', Classifier)) %>% 
    # remove columns that they aren't interested in
    select(site, species, datetime, date, time, count, filename) %>% 
    # filter to only civet & genet
    filter(species %in% c("civet", "genet"))

# bring in camera locations
locations <- read.csv("~/Documents/github-repos/gorongosa/gorongosa-camera-traps/data/cam_metadata_fromfield.csv") %>% 
    select(StudySite, Latitude, Longitude) %>% 
    rename(site = StudySite)

# join em together
all_records_clean2 <- all_records_clean %>% 
    left_join(locations) 

# bring in conservative camera operation (first and last complete day of each sampling period)
camop <- read_csv("operation-files/operation-years1and2-gerber.csv") %>% 
    mutate_at(c("Start", "End"), ~as.Date(., format = "%m/%d/%y")) %>% 
    rename(site = "Site Name",
           OperationPeriod = "Operation Period")

# create copy of dataframe
all_records_clean3 <- all_records_clean2

# create Period column, call NA
all_records_clean3$OperationPeriod <- NA

# loop through rows and, where relevant, assign period according to project treatment/control dates
for (i in seq_len(nrow(camop))) {
    all_records_clean3$OperationPeriod <- ifelse(all_records_clean3$site == camop$site[i] &
                              as.Date(all_records_clean3$date) >= as.Date(camop$Start[i]) &
                              as.Date(all_records_clean3$date) <= as.Date(camop$End[i]),
                          yes = camop$OperationPeriod[i], # assign period from camop
                          no = all_records_clean3$OperationPeriod) # just keep NA
}

# remove any records that fall outside of period
all_records_clean4 <- all_records_clean3 %>% 
    drop_na(OperationPeriod)

# see what was dropped
dropped <- anti_join(all_records_clean3, all_records_clean4)

# join with operation periods, add time to operation start/end
camop_periods <- camop %>% select(OperationPeriod, Start, End)
all_records_clean5 <- left_join(all_records_clean4, camop_periods) %>% 
    mutate("Camera Start Date and Time" = paste0(Start, " 00:00:00"),
           "Camera End Date and Time" = paste0(End, " 23:59:59")) %>% 
    rename("General Sampling Period Name" = "OperationPeriod") %>% 
    select(-c("Start", "End"))

# export!
write.csv(all_records_clean5, "Gorongosa_genet_civet_data_for_Phillips_Keim.csv", row.names = F)
