# Clean data for biodiversity database
# to send to Piotr Naskrecki, November 2021

library(dplyr)
library(forcats)
library(tidyr)
`%notin%` <- Negate(`%in%`)

# Bring in 2016-2019 grid data --------------------------------------------

all_records <- read.csv("~/Documents/github-repos/gorongosa/gorongosa-shiny/wildcam_fulldata_2019.csv")

# hmm what is up with the rare species? export for double-checking
rare_species <- all_records %>% 
  filter(species %in% c("caracal", "jackal", "suni", "wild_dog", "leopard"))
write.csv(rare_species, "rare_species_to_check_WildCam.csv", row.names = FALSE)

# and re-import the corrected version
rare_species_fixed <- read.csv("rare_spp_urls_kg.csv") %>% 
  select(-c(question__species, updated_species, notes)) %>% 
  rename(species = kg_species,
         Date = capture_date_local,
         Time = capture_time_local) %>% 
  mutate(Date = as.Date(Date, "%m/%d/%y")) %>% 
  select(site, species, Date, Time)

all_records <- all_records %>% 
  filter(species %notin% c("caracal", "jackal", "suni", "wild_dog", "leopard"))

# take only columns of interest, remove unidentified images
all_records <- select(all_records, site, species, datetime) %>% 
  mutate(species = fct_recode(species,
                              "vervet" = "vervet_monkey",
                              "samango" = "samango_monkey",
                              "hornbill_ground" = "ground_hornbill")) %>% 
  separate(col = datetime, 
           into = c("Date", "Time"),
           sep = " ") %>% 
  mutate(Date = as.Date(Date))
unique(all_records$species)

# bind two dataframes together
all_records2 <- bind_rows(all_records, rare_species_fixed) %>% 
  filter(species %notin% c("reptile_amphibian", "bird_other", "insect", "nothing there", "human",
                           "unknown", "unknown_antelope", "rodent", "fire", "birdofprey", "flood",
                           "rain", "bat", "crane", "vulture", "duiker", "mongoose",
                           "guinea_fowl"))

# bring in species traits
species <- read.csv("~/Documents/github-repos/gorongosa/gorongosa-camera-traps/data/2018spp_kingdon.csv") %>%
  rename(Species = CommName) %>%  # rename to match name of column in records
  mutate(species = tolower(Species)) %>%  # make lowercase to match MP format
  select(species, Species, GenusLatin, SpeciesLatin) 
species_extra <- read.csv("additional-species.csv")
species <- bind_rows(species, species_extra)
  
# join with records
all_records3 <- left_join(all_records2, species)

# confirm all species have latin names
unique_species <- all_records3 %>% 
  select(-c(site, Date, Time)) %>% 
  unique

# format accordingly
all_records3 <- all_records3 %>% 
  select(-species) %>% 
  rename(Camera = site) %>% 
  mutate(Survey = paste0("Camera_Grid_", format(Date, "%Y-%m")))

# drop duplicate species w/in same survey
all_records3$drop <- "No"
all_records3 <- all_records3[order(all_records3$Camera, 
                                   all_records3$Survey, 
                                   all_records3$Species),]
for(i in 2:nrow(all_records3)) {
  if((all_records3$Species[i] == all_records3$Species[i-1]) && 
     (all_records3$Survey[i] == all_records3$Survey[i-1]) &&
     (all_records3$Camera[i] == all_records3$Camera[i-1])) {
    all_records3$drop[i] <- "Yes"
  }
}
all_records4 <- all_records3 %>% 
  filter(drop == "No") %>% 
  select(-drop)

# assign collector based on date
all_records4$Collected_By <- "Kaitlyn Gaynor & Meredith Palmer"
for(i in 1:nrow(all_records4)) {
  if(all_records4$Date[i] < as.Date("2019-01-01")) {
    all_records4$Collected_By[i] <- "Kaitlyn Gaynor"
  }
}

# pair with location
locations_grid <- read.csv("~/Documents/github-repos/gorongosa/gorongosa-camera-traps/data/cam_metadata_fromfield.csv") %>% 
  select(StudySite, Latitude, Longitude) %>% 
  rename(Camera = StudySite)

all_records5 <- left_join(all_records4, locations_grid)


# Bring in Muaredzi/Muanza-Baixo data --------------------------------------------

# has already been cleaned to just one per survey
pilot <- read.csv("~/Dropbox/projects/GORONGOSA2/Pilot Studies/2015 Muaredzi-MuanzaBaixo camera trap study/Camera_trap_data_clean_for_database.csv") %>% 
  rename(Camera = camera,
         Date = date,
         Time = time_init) %>% 
  mutate(Date = as.Date(Date, format = "%m/%d/%y"),
         Collected_By = "Kaitlyn Gaynor")

pilot_locations <- read.csv("~/Dropbox/projects/GORONGOSA2/Pilot Studies/2015 Muaredzi-MuanzaBaixo camera trap study/cam_GPS_points.csv") %>% 
  rename(Camera = Name)

pilot <- left_join(pilot, pilot_locations)


# Bring in Levas Flor data --------------------------------------------

levas <- read.csv("~/Documents/github-repos/gorongosa/gorongosa-levas-flor/LF17_clean.csv") %>% 
  select(Station, Species, Date, Time) %>% 
  rename(Camera = Station) %>% 
  mutate(Collected_By = "Tara Easter",
         Date = as.Date(Date, "%m/%d/%Y"),
         Survey = "Levas_Flor_Camera_Grid") %>% 
  filter(Species %notin% c("unknown_mon", "unknown_ung", "us", "dog",
                           "squirrel", "monitor", "guinea_fowl")) %>% 
  mutate(Species = fct_recode(Species,
                              "bushbaby" = "gelago",
                              "hornbill_ground" = "ground_hornbill",
                              "hornbill_ground" = "hornbill",
                              "mongoose_bushy_tailed" = "bushy_tailed_mongoose",
                              "mongoose_white_tailed" = "white_tailed_mongoose",
                              "mongoose_marsh" = "marsh_mongoose",
                              "duiker_common" = "common_duiker",
                              "duiker_red" = "red_duiker",
                              "sable_antelope" = "sable"))

# join with latin names
species_levas <- species %>% 
  select(-Species) %>% 
  rename(Species = species)
levas <- left_join(levas, species_levas)

# confirm all species have latin names
test_levas_species <- levas %>% 
  select(Species, GenusLatin, SpeciesLatin) %>% 
  unique

# drop duplicate species w/in same survey
levas$drop <- "No"
levas <- levas[order(levas$Camera, 
                     levas$Species),]
for(i in 2:nrow(levas)) {
  if((levas$Species[i] == levas$Species[i-1]) && 
     (levas$Camera[i] == levas$Camera[i-1])) {
    levas$drop[i] <- "Yes"
  }
}
levas2 <- levas %>% 
  filter(drop == "No") %>% 
  select(-drop)

# Combine all data --------------------------------------------

all_data_combined <- bind_rows(all_records5, pilot, levas2) %>% 
  rename(Common_Name = Species,
         Species = SpeciesLatin,
         Genus = GenusLatin) %>% 
  select(Survey, Common_Name, Genus, Species, Date, Time, Latitude, Longitude, Camera, Collected_By)

# export
write.csv(all_data_combined, "camera_trap_data_for_database_2021_11_29.csv", row.names = F)