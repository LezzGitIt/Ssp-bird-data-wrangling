## PhD birds in silvopastoral landscapes ##
## Data wrangling 06 -- Final wrangling to filter 'Bird_pcs_dist.csv' (from 04b_Range_apply.R) to generate 'Bird_pcs_analysis.csv', which is the subset of observations within the fixed point count radius (50m) and that used the habitat (e.g. excluding flyovers)

## Description: These are the steps we took to prepare the analysis file for our purposes; however, these are only suggestions and should be modified according to the needs of your analysis

# Load libraries & data ---------------------------------------------------
library(tidyverse)
library(conflicted)

conflicts_prefer(dplyr::select)
conflicts_prefer(dplyr::filter)

# guess_max = Inf: Recording is sparse (only ~180 non-NA, near the end) so the default type guess misreads it as logical and drops the values
Bird_pcs_dist <- read_csv(file = "Derived/Excels/Bird_pcs/Bird_pcs_dist.csv", guess_max = Inf)
Taxonomy <- read_csv(file = "Derived/Excels/Taxonomy/Taxonomy.csv")

# General formatting ---------------------------------------------------------
# Format point count data for downstream analyses 
Bird_pcs_analysis1 <- Bird_pcs_dist %>% 
  mutate(Species_ayerbe_ = str_replace_all(Species_ayerbe, " ", "_")) %>%
  # Add 1 individual when # individual = NA 
  mutate(Count = ifelse(is.na(Count), 1, Count))

# Remove recordings ---------------------------------------------------------
# Given the inconsistent effort, remove birds identified from the recording (Y), including those also noted in the field, since continuous recording made their identification possible; field detections (In field, Cf) stay
Bird_pcs_analysis2 <- Bird_pcs_analysis1 %>% 
  filter(is.na(Recording) | Recording != "Y")

# Remove flyovers ---------------------------------------------------------
# NOTE: CIPAV did not record information on flyovers (sobrevuelos). Thus we cannot exclude the possibility that a species flew over the observer and was included in the data set, despite NOT using the habitat. 
Bird_pcs_analysis3 <- Bird_pcs_analysis2 %>% 
  filter(is.na(Obs_type) | Obs_type != "Sobrevuelo")

# Remove distances > 50m --------------------------------------------------
# NOTE:: there are a few rows that have NAs for distance as data collector forgot to enter a distance in the field 
Bird_pcs_analysis3 %>% 
  filter(is.na(Distance_bird)) 

# However, there are a few species (e.g. hummingbirds, manakins) that would be nearly impossible to identify at >50m. Change the distance of these species to <50m so that they are maintained
Fam_join <- Taxonomy %>% 
  distinct(Species_ayerbe, Family)
Bird_pcs_analysis3 %>%
  left_join(Fam_join) %>%
  filter(is.na(Distance_bird)) %>% 
  distinct(Family, Species_ayerbe) #%>% view()
Bird_pcs_analysis4 <- Bird_pcs_analysis3 %>% 
  left_join(Fam_join) %>%
  mutate(Distance_bird = if_else(is.na(Distance_bird) & Family %in% c("Trochilidae", "Pipridae"), "<50", Distance_bird))

# Make distances numeric
Bird_pcs_analysis5 <- Bird_pcs_analysis4 %>%
  mutate(Distance_bird = case_when(
    Distance_bird == "0-15" ~ "15",
    Distance_bird == "15-30" ~ "30",
    Distance_bird == "30-50" ~ "50",
    Distance_bird == "<25" ~ "25",
    Distance_bird == "<50" ~ "50",
    Distance_bird == ">50" ~ "51",
    .default = Distance_bird
  )) %>%
  mutate(Distance_bird = as.numeric(Distance_bird))

# Remove records with distance unknown or > 50m
Bird_pcs_analysis6 <- Bird_pcs_analysis5 %>% 
  filter(Distance_bird < 51) %>% 
  # select only the columns necessary
  select(-c(Recording, Family, Distance_bird, Obs_type))

# Sum counts --------------------------------------------------------------
# Summarize so each species is listed only once in each point count 
Bird_pcs_analysis6 %>% filter(if_any(everything(), is.na)) # No NAs in any rows
Bird_pcs_analysis7 <- Bird_pcs_analysis6 %>% 
  summarize(Count = sum(Count), .by = -Count)

Bird_pcs_analysis6 %>% filter(Date > "2025-12-22") %>% 
  count(Id_survey, Date, Species_ayerbe, sort = T )

# Reduce impact of outliers ---------------------------------------------
# Lessen the magnitude of 4 outliers with counts > 50 individuals
Bird_pcs_analysis <- Bird_pcs_analysis7 %>% 
  mutate(Count = ifelse(Count > 50, 50, Count)) 

Site_covs <- read_csv(file = "Derived/Excels/Site_covs.csv")
Bird_pcs_analysis %>% left_join(Site_covs) %>% 
  summarize(N_ecor = n_distinct(Ecoregion), .by = Species_ayerbe) %>% 
  arrange(desc(N_ecor))

# Export ------------------------------------------------------------------
Bird_pcs_analysis %>%
  select(-Species_ayerbe_) %>% 
  write_csv("Derived/Excels/Bird_pcs/Bird_pcs_analysis.csv")
