# Prep batch 1 & 2 data (2016-2018) for Brian Gerber et al. collaboration

library(dplyr)
library(readr)
library(tidyr)
library(forcats)
`%notin%` <- Negate(`%in%`)

all_records <- read_csv("~/Documents/github-repos/gorongosa/gorongosa-shiny/wildcam_fulldata_2019.csv") 

all_records_clean <- all_records %>% 
    
    # filter to just batches 1 and 2 (based on classifiers)
    filter(grepl('KG/Undergrads', Classifier)) %>% 
    
    # remove columns that they aren't interested in
    select(-c(Classifier, juvenile, moving, eating, resting, standing, interacting, male, directory,
              filename, datetime, juvenile_count, male_count)) %>% 
    
    # filter out non-mammals
    filter(species %notin% c("bird_other", "guineafowl_helmeted", "guineafowl_crested",
                             "hornbill_ground", "human", "flood", "bat", "insect",
                             "rain", "fire", "nothing there", "reptile_amphibian",
                             "guinea_fowl", "unknown", "unknown_antelope",
                             "ground_hornbill", "rodent", "duiker", "mongoose")) %>% 
    
    # recode species as needed
    mutate(species = fct_recode(species, 
                                "hippo" = "hippopotamus",
                                "vervet" = "vervet_monkey",
                                "samango" = "samango_monkey")) %>% 
    
    # add columns with new information about project
    mutate("General Project" = "Gorongosa",
           "Contact Person's Last Name" = "Gaynor",
           "Country" = "Mozambique",
           "Timezone" = "Africa/Maputo",
           "UTM zone" = NA,
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
           "Species" = "SpeciesLatin",
           "Species Diet Description" = "Diet")

# join em together
all_records_clean2 <- all_records_clean %>% 
    left_join(species) %>% 
    left_join(locations) %>% 
    select(-species) # remove common name column

# bring in conservative camera operation (first and last complete day of each sampling period)
camop <- read_csv("operation-files/operation-years1and2-gerber.csv") %>% 
    mutate_at(c("Start", "End"), ~as.Date(., format = "%m/%d/%y"))

# create copy of dataframe
all_records_clean3 <- all_records_clean2

# create Period column, call NA
all_records_clean3$`Operation Period` <- NA

# loop through rows and, where relevant, assign period according to project treatment/control dates
for (i in seq_len(nrow(camop))) {
    all_records_clean3$`Operation Period` <- ifelse(all_records_clean3$`Site Name` == camop$`Site Name`[i] &
                              as.Date(all_records_clean3$`Photo Date`) >= as.Date(camop$Start[i]) &
                              as.Date(all_records_clean3$`Photo Date`) <= as.Date(camop$End[i]),
                          yes = camop$`Operation Period`[i], # assign period from camop
                          no = all_records_clean3$`Operation Period`) # just keep NA
}

# remove any records that fall outside of period
all_records_clean4 <- all_records_clean3 %>% 
    drop_na(`Operation Period`)

# see what was dropped
dropped <- anti_join(all_records_clean3, all_records_clean4)

# join with operation periods, add time to operation start/end
camop_periods <- camop %>% select(`Operation Period`, Start, End)
all_records_clean5 <- left_join(all_records_clean4, camop_periods) %>% 
    mutate("Camera Start Date and Time" = paste0(Start, " 00:00:00"),
           "Camera End Date and Time" = paste0(End, " 23:59:59")) %>% 
    rename("General Sampling Period Name" = "Operation Period") %>% 
    select(-c("Start", "End"))

# get columns in the right order and export!
all_records_clean6 <- all_records_clean5 %>% 
    select("General Project",
           "Contact Person's Last Name",
           "Site Name",
           "Country",
           "Timezone",
           "Longitude", "Latitude", "UTM zone",
           "General Sampling Period Name",
           "Photo Date", "Photo Time",
           "Class", "Order", "Family", "Genus", "Species",
           "Number of Animals",
           "Camera Start Date and Time", "Camera End Date and Time",
           "Camera Make", "Camera Model", "Flash (Yes or no)",
           "Bait used", "Bait type (if applicable)",
           "Photos per trigger", "Species Diet Description")

# export!
write.csv(all_records_clean6, "Gorongosa_Gaynor_data_for_Gerber.csv", row.names = F)
