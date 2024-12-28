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

# Load libraries & data ---------------------------------------------------
library(tidyverse)
library(unmarked)
library(readxl)
library(spOccupancy)
library(ubms) # function get_stancode() could be used to get code from model that could then be adapted
library(flocker)
library(brms)
library(ggpubr)
library(cowplot)
ggplot2::theme_set(theme_cowplot())
conflicts_prefer(dplyr::select)
conflicts_prefer(dplyr::filter)

load("Rdata/the_basics_12.27.24.Rdata")
load("Rdata/Taxonomy_12.27.24.Rdata")

source("/Users/aaronskinner/Library/CloudStorage/OneDrive-UBC/Grad_School/Rcookbook/Themes_funs.R")

# General formatting -------------------------------------------------------------
# Format point count data for downstream analyses 
Birds_analysis <- Bird_pcs %>% filter(Nombre_ayerbe %in% Tax_df3$Species_ayerbe) %>% 
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

# Event covariates -----------------------------------------------------------
# Covariates that vary by visit, where there is a row for each visit to each point count
event_covs <- Pc_date4 %>% distinct(Id_muestreo, Ano_grp, Rep, Fecha, Pc_start, Spp_obs) %>% 
  arrange(Id_muestreo, Ano_grp, Rep)

# Unit covariates ---------------------------------------------------------
# Covariates that are fixed for a given point count
unit_covs <- Envi_df2 %>%
  arrange(Id_muestreo) %>%
  select(Elev, Avg_temp, Tot.prec)

# Occ abu formatting  -----------------------------------------------------
# Abundances
Ubc_abu <- Birds_fin %>%
  full_join(event_covs) %>%
  summarize(
    Count = sum(Count),
    .by = c(Id_muestreo, Ano_grp, Rep, Nombre_ayerbe)
  ) %>%
  mutate(Nombre_ayerbe_ = str_replace_all(Nombre_ayerbe, " ", "_")) %>% 
  select(-Nombre_ayerbe) %>%
  pivot_wider(
    names_from = Nombre_ayerbe_,
    values_from = Count,
    values_fill = 0
  ) %>% arrange(Id_muestreo, Ano_grp, Rep)

# CHECK:: Should be same number of rows
nrow(Ubc_abu)
nrow(event_covs)

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
# Pull the 30 species that have been observed more than 30 times
spp30 <- Birds_fin %>%
  filter(Uniq_db == "UBC MBD") %>%
  group_by(Nombre_ayerbe) %>%
  summarize(Count = sum(Count)) %>%
  filter(Count > 30) %>%
  pull(Nombre_ayerbe)

#Nest?
Pcs_nest <- Bird_pcs %>% group_by(Uniq_db) %>% # Nombre_institucion
  nest()

# Access the nested data for a specific Uniq_db
Ubc_mbd_data <- Pcs_nest %>% 
  filter(Uniq_db == "UBC MBD") %>% 
  pluck("data", 1)

# Save & Export -----------------------------------------------------------



