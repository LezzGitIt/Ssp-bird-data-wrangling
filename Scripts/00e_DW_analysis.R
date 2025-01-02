## PhD birds in silvopastoral landscapes##
## Data wrangling 00e -- Setting up occupancy & abundance data, the unit covs, and event covs
## This script generates the data frames that will be used as inputs for hierarchical modeling in future scripts 

# Contents
# 1)
# 2)
# 3)
# 4)
# 5)
# 6)

#TO DO: 
# Bring in the DW steps from the iNEXT script, look for overlap

# Load libraries & data ---------------------------------------------------
library(tidyverse)
library(janitor)
library(chron)
library(unmarked)
library(readxl)
library(spOccupancy)
library(ubms) # function get_stancode() could be used to get code from model that could then be adapted
library(flocker)
library(brms)
library(ggpubr)
library(sf)
library(cowplot)
library(conflicted)
ggplot2::theme_set(theme_cowplot())
conflicts_prefer(dplyr::select)
conflicts_prefer(dplyr::filter)

load("Rdata/the_basics_12.30.24.Rdata")
load("Rdata/Taxonomy_12.29.24.Rdata")

source("/Users/aaronskinner/Library/CloudStorage/OneDrive-UBC/Grad_School/Rcookbook/Themes_funs.R")

# General formatting -------------------------------------------------------------
# Format point count data for downstream analyses 
Birds_analysis <- Bird_pcs %>% 
  mutate(Nombre_ayerbe_ = str_replace_all(Nombre_ayerbe, " ", "_")) %>%
  filter(Nombre_ayerbe %in% Tax_df3$Species_ayerbe) %>% 
  filter(is.na(Grabacion) | Grabacion == "Cf") %>% # Remove birds identified only in recording
  mutate(Count = ifelse(is.na(Count), 1, Count)) #Add 1 individual when # individuals = NA (6 rows in UBC data)
  
#Remove vuelos
#STILL TO DO: INVESTIGATE UNILLANOS, CIPAV, GAICA MBD
Birds_analysis2 <- Birds_analysis %>% filter(Estrato_vertical != "Vuelo" | is.na(Estrato_vertical) & !(tolower(Distancia_bird) %in% c("sobrevuelo", "vuelo")))

# Make distances numeric
Birds_analysis3 <- Birds_analysis2 %>%
  mutate(Distancia_bird = case_when( # Birds_analysis = Birds analysis file
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

# Removing records with distance > 50m
Birds_fin <- Birds_analysis3 %>% filter(Distancia_bird < 51 | is.na(Distancia_bird))
Birds_fin %>% tabyl(Distancia_bird)
## Still to do:: REMOVE VUELOS

# NOTE:: there are some rows that still have NAs for distance
Birds_fin %>%
  filter(is.na(Distancia_bird)) %>%
  select(Uniq_db, Fecha, Departamento, Id_muestreo, Nombre_finca, Nombre_ayerbe) %>%
  count(Id_muestreo, sort = T)

## Simplify exercise by using only most abundant species 
Spp_counts <- Birds_fin %>%
  #filter(Uniq_db == "Ubc mbd") %>%
  summarize(Count = sum(Count), .by = c(Nombre_ayerbe, Nombre_ayerbe_))

# Pull the 30 species that have been observed more than 30 times
Ubc30 <- Spp_counts %>% 
  filter(Count > 30) %>%
  distinct(Nombre_ayerbe, Nombre_ayerbe_)

# The most abundant species in the project
Top_abu <- Spp_counts %>% slice_max(order_by = Count, n = 2) %>% 
  pull(Nombre_ayerbe)


# Biogeographic_clipping --------------------------------------------------
## Working with 3 different approaches to 
# Load in relevant shapefiles 
Ayerbe_mod_spp_l <- map(Top_abu, \(spp){
  st_read(dsn = "../Geospatial_data/Ayerbe_shapefiles_1890spp", layer = spp) %>% st_make_valid()
})
Ayerbe_mod_spp <- do.call(rbind, Ayerbe_mod_spp_l)

# Calculate the distance (km) from each point count location to the species range
Dist_to_pcs <- Pc_locs_sf %>%
  distinct(Id_muestreo, geometry) %>%
  st_distance(Ayerbe_mod_spp) %>% 
  as_tibble() %>% 
  rename_with(~ Top_abu) %>%
  Cap_snake %>%
  mutate(across(everything(), ~ (.x / 1000) %>% units::drop_units())) %>% 
  bind_cols(distinct(Pc_locs_sf, Id_muestreo)) %>% 
  relocate(Id_muestreo, 1)

# Add Pc_ids & their geometries back in
Dist_pcs_sf <- Pc_locs_sf %>% distinct(Id_muestreo, geometry) %>% 
  full_join(Dist_to_pcs)

# Examine rows with Pcs > 0 km
Dist_pcs_sf %>% filter(if_any(where(is.numeric), ~ .x > 0)) %>%
  ggplot() + 
  geom_sf(aes(color = Eupsittula_pertinax)) +
  geom_sf(data = Ayerbe_mod_spp_l[[2]]) #+
  #geom_sf(data = Pc_locs_sf, color = "red") 

# In future will want to use some distance cutoff, ideally as a proportion of the total area via st_area()
st_area(Ayerbe_mod_spp) 

# For each column that is numeric 
Pcs_in_range <- Dist_to_pcs %>%
  select(where(is.numeric)) %>% # Select only numeric columns
  map(~ Dist_to_pcs %>%
        filter(.x == 0) %>% # Filter rows where the column equals 0
        pull(Id_muestreo)) # Extract 'Nombre_ayerbe' for matching rows

# EXPLORE IN FUTURE
## Nested tibble:: Create a data frame with species names and their respective maps
species_maps_df <- tibble(
  species = Top_abu, 
  spp_map = map(Top_abu, \(spp) {
    st_read(dsn = "../Geospatial_data/Ayerbe_shapefiles_1890spp", layer = spp) %>% 
      st_make_valid()
  })
)

species_maps_df %>% unnest()

# Add a nested column with Pc_locs_sf intersections or joins
nested_results <- species_maps_df %>%
  mutate(
    intersected_points = map(spp_map, \(spp_map) {
      Pc_locs_sf %>%
        distinct(Id_muestreo, geometry) %>%
        st_intersection(spp_map)
    })
  )


# Vignette --------------------------------------------------------------
# Examine data structure 
# Each data structure has 237 rows, the number of sites there are 
wt <- read.csv(system.file("csv","widewt.csv", package="unmarked"))
y <- wt[,2:4]
siteCovs <-  wt[,c("elev", "forest", "length")]
obsCovs <- list(date=wt[,c("date.1", "date.2", "date.3")],
                ivel=wt[,c("ivel.1",  "ivel.2", "ivel.3")])
wt <- unmarkedFrameOccu(y = y, siteCovs = siteCovs, obsCovs = obsCovs)
head(wt)
str(wt)

# Event covariates -----------------------------------------------------------
# We have 551 unique Id_muestreos, so all data frames should have 551 rows
# Covariates that vary by visit, where there is a row for each visit to each point count

# Need a wide dataframe, where all point counts have a single row
Obs_covs_df <- Pc_date4 %>%
  mutate(across(everything(), as.character)) %>%
  pivot_longer(
    cols = c(Fecha, Pc_start, Pc_length),      
    names_to = "Variable",          
    values_to = "Value"             
  ) %>%
  arrange(Ano_grp) %>%
  pivot_wider(
    id_cols = c(Id_muestreo, Variable),
    names_from = c(Ano_grp, Rep),
    names_glue = "Ano{Ano_grp}_Rep{Rep}",
    values_from = c(Value)
  ) #%>% #head()
  
Obs_covs_l <- Obs_covs_df %>% group_by(Variable) %>%
  group_split(.keep = FALSE)
names(Obs_covs_l) <- unique(Obs_covs_df$Variable)

# Unit covariates ---------------------------------------------------------
# Covariates that are fixed for a given point count
unit_covs <- Envi_df2 %>%
  arrange(Id_muestreo) %>%
  select(Id_muestreo, Elev, Avg_temp, Tot.prec)

# Occ abu formatting  -----------------------------------------------------
Pc_date_join <- Pc_date4 %>% distinct(Id_muestreo, Ano_grp, Pc_start, Rep)

# Abundances
Abund_long <- Birds_fin %>%
  full_join(Pc_date_join, relationship = "many-to-many") %>%
  summarize(
    Count = sum(Count),
    .by = c(Id_muestreo, Ano_grp, Rep, Nombre_ayerbe)
  )

## nesting by Uniq_db likely makes more sense, maybe by ecoregion? Do 2 species instead of 30
Abund_long %>% nest(.by = Nombre_ayerbe) %>% 
  filter(Nombre_ayerbe %in% Spp30)

# This works but doesn't seem to maintain all reps (if species wasn't observed?)
Abund_long %>% filter(Nombre_ayerbe == "Bubulcus ibis") %>% 
  arrange(Ano_grp, Rep) %>%
  #select(Id_muestreo, Ano_grp, Rep, Bubulcus_ibis, Amazona_ochrocephala) %>% 
  pivot_wider(id_cols = c(Id_muestreo),
              names_from = c(Ano_grp, Rep),
              names_glue = "Ano{Ano_grp}_Rep{Rep}",
              values_from = Count, 
              values_fill = 0)

# For iNEXT
Spp_wide <- Abund_long %>% 
  pivot_wider(
    names_from = Nombre_ayerbe_,
    values_from = Count,
    values_fill = 0
  ) %>% arrange(Id_muestreo, Ano_grp, Rep)


# CHECK:: There are several point counts that have rows in event_covs but are not in Birds_fin
test <- Birds_fin %>% full_join(event_covs) 
Not_in_bf <- test %>% anti_join(Birds_fin) %>% # Not in Birds_fin df
  filter(Spp_obs != 0) %>% # We know these will be in event_covs and not Birds_fin
  distinct(Id_muestreo, Rep, Fecha, Pc_start, Spp_obs) 
# I confirmed that all of these point counts are due to 1) having only birds >50m, 2) having Estrato_vertical as flyover, or 3) Not observing species that are in Tax_df3 (genus, for example)
# For example:
Bird_pcs %>% right_join(Not_in_bf) %>% 
  distinct(Id_muestreo, Rep, Fecha, Pc_start, Spp_obs, Ano_grp, Distancia_bird, Estrato_vertical)

# Occupancy
Ubc_occ <- Ubc_abu %>% select(-c(Id_muestreo, Ano_grp, Rep)) %>% 
  mutate(across(.cols = everything(), .fns = ~ ifelse(. > 0, 1, 0)))

# Flocker -----------------------------------------------------------------
# TRY unmarked or ubms? 
d <- simulate_flocker_data()

# Create occupancy data frame
Ubc_occ <- Ubc_abu %>% select(-c(Id_muestreo, Ano_grp, Rep)) %>% 
  mutate(across(.cols = everything(), .fns = ~ ifelse(. > 0, 1, 0)))

# NOTE:: Abundance data, event & unit covs were arranged in the same order
event_covs2 <- event_covs %>% select(Fecha, Pc_start)

fd_rep_varying <- make_flocker_data(
  obs = Ubc_occ,
  unit_covs = unit_covs,
  event_covs = event_covs2
)


# Misc --------------------------------------------------------------------

#Nest?
Pcs_nest <- Bird_pcs %>% group_by(Uniq_db) %>% # Nombre_institucion
  nest()

# Access the nested data for a specific Uniq_db
Ubc_mbd_data <- Pcs_nest %>% 
  filter(Uniq_db == "UBC MBD") %>% 
  pluck("data", 1)

# Save & Export -----------------------------------------------------------



