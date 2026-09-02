## Data paper Ecology ## 

## Objective: This script filters 'Bird_pcs_all.csv' to generate 'Bird_pcs_analysis.csv', which is the subset of observations of birds within the fixed point count radius that used the habitat in some way (i.e., not a flyover)

## Description: These are the steps we took to prepare the analysis file for our purposes; however, these are only suggestions and should be modified according to the needs of your analysis.

# Load libraries & data ---------------------------------------------------
library(tidyverse)
library(conflicted)

conflicts_prefer(dplyr::select)
conflicts_prefer(dplyr::filter)

Bird_pcs_all <-  read_csv(file = "Data_paper/DataS1/Bird_pcs_all.csv")
Taxonomy <- read_csv(file = "Data_paper/DataS1/Taxonomy.csv")
Event_covs <- read_csv(file = "Data_paper/DataS1/Event_covs.csv")

# General formatting ---------------------------------------------------------
# Bird_pcs_all contains records that were not identified to species, thus we use Taxonomy.csv to filter observations that were identified to species.  
Bird_pcs_analysis1 <- Bird_pcs_all %>% 
  filter(Species_ayerbe %in% Taxonomy$Species_ayerbe) %>%
  # Add 1 individual when # individual = NA 
  mutate(Count = ifelse(is.na(Count), 1, Count))

# Remove recordings ---------------------------------------------------------
# Given the inconsistent effort, remove birds that were only identified in recordings
Bird_pcs_analysis2 <- Bird_pcs_analysis1 %>% 
  filter(is.na(Grabacion) | Grabacion == "Cf")

# Remove flyovers ---------------------------------------------------------
# NOTE: CIPAV did not record information on flyovers (sobrevuelos). Thus we cannot exclude the possibility that a species flew over the observer and was included in the data set, despite NOT using the habitat. 
Bird_pcs_analysis3 <- Bird_pcs_analysis2 %>% 
  filter(is.na(Tipo_registro) | Tipo_registro != "Sobrevuelo")

# Remove distances > 50m --------------------------------------------------
# NOTE:: There are a few rows where Distancia_bird is NA, when a data collector forgot to enter a distance in the field 
Bird_pcs_analysis3 %>% 
  filter(is.na(Distancia_bird)) 

# Generally we remove these species, but there are a few families (e.g. hummingbirds, manakins) that would be nearly impossible to identify at >50m. Change the distance of these species to <50m so that they are maintained
Fam_join <- Taxonomy %>% 
  distinct(Species_ayerbe, Family_gbif)
Bird_pcs_analysis3 %>%
  left_join(Fam_join) %>%
  filter(is.na(Distancia_bird)) %>% 
  distinct(Family_gbif, Species_ayerbe) #%>% view()
Bird_pcs_analysis4 <- Bird_pcs_analysis3 %>% 
  left_join(Fam_join) %>%
  mutate(Distancia_bird = if_else(is.na(Distancia_bird) & Family_gbif %in% c("Trochilidae", "Pipridae"), "<50", Distancia_bird))

# Make distances numeric
Bird_pcs_analysis5 <- Bird_pcs_analysis4 %>%
  mutate(Distancia_bird = case_when(
    Distancia_bird == "0-15" ~ "15",
    Distancia_bird == "15-30" ~ "30",
    Distancia_bird == "30-50" ~ "50",
    Distancia_bird == "< 25" ~ "25",
    Distancia_bird == "<50" ~ "50",
    Distancia_bird == ">50" ~ "51",
    Distancia_bird == "> 50" ~ "51",
    .default = Distancia_bird
  )) %>%
  mutate(Distancia_bird = as.numeric(Distancia_bird))

# Remove records with distance unknown or > 50m
Bird_pcs_analysis6 <- Bird_pcs_analysis5 %>% 
  filter(Distancia_bird < 51) %>% 
  # select only the columns necessary
  select(-c(Grabacion, Family_gbif, Distancia_bird, Tipo_registro))

# Sum counts --------------------------------------------------------------
# Summarize so each species is listed only once in each point count
Bird_pcs_analysis7 <- Bird_pcs_analysis6 %>% 
  summarize(Count = sum(Count), .by = -Count)

# Reduce impact of outliers ---------------------------------------------
# Lessen the magnitude of 4 outliers with counts > 50 individuals
Bird_pcs_analysis <- Bird_pcs_analysis7 %>% 
  mutate(Count = ifelse(Count > 50, 50, Count)) 

# Export ------------------------------------------------------------------
stop()
Bird_pcs_analysis %>% 
  arrange(Id_muestreo, Id_muestreo_no_dc, Fecha, Pc_start) %>%
  write_csv(file = "Data_paper/DataS1/Bird_pcs_analysis.csv")
