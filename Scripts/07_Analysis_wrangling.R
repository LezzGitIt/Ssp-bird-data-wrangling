## PhD birds in silvopastoral landscapes ##
## Data wrangling 07 -- Final wrangling to filter 'Bird_pcs_dist.csv' to generate 'Bird_pcs_analysis.csv', which is the subset of observations within the fixed point count radius (50m) and that used the habitat (e.g. excluding flyovers)

## Description: These are the steps we took to prepare the analysis file for our purposes; however, these are only suggestions and should be modified according to the needs of your analysis

# Load libraries & data ---------------------------------------------------
library(tidyverse)
library(conflicted)

conflicts_prefer(dplyr::select)
conflicts_prefer(dplyr::filter)

Bird_pcs_dist <- read_csv(file = "Data_paper/DataS1/Bird_pcs_dist.csv")
Taxonomy <- read_csv(file = "Data_paper/DataS1/Taxonomy.csv")

# General formatting ---------------------------------------------------------
# Format point count data for downstream analyses 
Bird_pcs_analysis1 <- Bird_pcs_dist %>% 
  mutate(Species_ayerbe_ = str_replace_all(Species_ayerbe, " ", "_")) %>%
  # Add 1 individual when # individual = NA 
  mutate(Count = ifelse(is.na(Count), 1, Count))

# Remove recordings ---------------------------------------------------------
# Given the inconsistent effort, remove birds that were only identified in recording
Bird_pcs_analysis2 <- Bird_pcs_analysis1 %>% 
  filter(is.na(Grabacion) | Grabacion == "Cf")

# Remove flyovers ---------------------------------------------------------
# NOTE: CIPAV did not record information on flyovers (sobrevuelos). Thus we cannot exclude the possibility that a species flew over the observer and was included in the data set, despite NOT using the habitat. 
Bird_pcs_analysis3 <- Bird_pcs_analysis2 %>% 
  filter(is.na(Tipo_registro) | Tipo_registro != "Sobrevuelo")

# Remove distances > 50m --------------------------------------------------
# NOTE:: there are a few rows that have NAs for distance as data collector forgot to enter a distance in the field 
Bird_pcs_analysis3 %>% 
  filter(is.na(Distancia_bird)) 

# However, there are a few species (e.g. hummingbirds, manakins) that would be nearly impossible to identify at >50m. Change the distance of these species to <50m so that they are maintained
Fam_join <- Taxonomy %>% 
  distinct(Species_ayerbe, Family)
Bird_pcs_analysis3 %>%
  left_join(Fam_join) %>%
  filter(is.na(Distancia_bird)) %>% 
  distinct(Family, Species_ayerbe) #%>% view()
Bird_pcs_analysis4 <- Bird_pcs_analysis3 %>% 
  left_join(Fam_join) %>%
  mutate(Distancia_bird = if_else(is.na(Distancia_bird) & Family %in% c("Trochilidae", "Pipridae"), "<50", Distancia_bird))

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
  select(-c(Grabacion, Family, Distancia_bird, Tipo_registro))

# Sum counts --------------------------------------------------------------
# Summarize so each species is listed only once in each point count 
Bird_pcs_analysis6 %>% filter(if_any(everything(), is.na)) # No NAs in any rows
Bird_pcs_analysis7 <- Bird_pcs_analysis6 %>% 
  summarize(Count = sum(Count), .by = -Count)

# Reduce impact of outliers ---------------------------------------------
# Lessen the magnitude of 4 outliers with counts > 50 individuals
Bird_pcs_analysis <- Bird_pcs_analysis7 %>% 
  mutate(Count = ifelse(Count > 50, 50, Count)) 

# Export ------------------------------------------------------------------
stop()
Bird_pcs_analysis %>% 
  select(-Species_ayerbe_) %>% 
  relocate(Registrado_por, .after = Count) #%>% 
  #write_csv("Derived/Excels/Bird_pcs/Bird_pcs_analysis.csv")
