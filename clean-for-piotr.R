# Clean data for biodiversity database
# to send to Piotr Naskrecki, November 2021

library(dplyr)
library(forcats)
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
  rename(species = kg_species)

all_records <- all_records %>% 
  filter(species %notin% c("caracal", "jackal", "suni", "wild_dog", "leopard"))

all_records <- bind_rows(all_records, rare_species_fixed)

# take only columns of interest, remove unidentified images
all_records <- select(all_records, site, species, datetime) %>% 
  filter(species %notin% c("reptile_amphibian", "bird_other", "insect", "nothing there", "human",
                           "unknown", "unknown_antelope", "rodent", "fire", "birdofprey", "flood",
                           "rain", "bat", "crane", "vulture", "duiker", "mongoose",
                           "guinea_fowl")) %>% 
  mutate(species = fct_recode(species,
                              "vervet" = "vervet_monkey",
                              "samango" = "samango_monkey",
                              "hornbill_ground" = "ground_hornbill"))
unique(all_records$species)

# bring in species traits
species <- read.csv("~/Documents/github-repos/gorongosa/gorongosa-camera-traps/data/2018spp_kingdon.csv") %>%
  rename(Species = CommName) %>%  # rename to match name of column in records
  mutate(species = tolower(Species)) %>%  # make lowercase to match MP format
  select(species, Species, GenusLatin, SpeciesLatin) 
  
# join with records
all_records <- left_join(all_records, species)

# confirm all species have latin names
unique_species <- all_records %>% 
  select(-c(site, datetime)) %>% 
  unique



# Bring in Muaredzi/Muanza-Baixo data --------------------------------------------

pilot <- read.csv("")
