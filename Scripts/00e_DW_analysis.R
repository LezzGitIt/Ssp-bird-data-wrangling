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

load("Rdata/the_basics_12.24.24.Rdata")
load("Rdata/Taxonomy_11.14.24.Rdata")

source("/Users/aaronskinner/Library/CloudStorage/OneDrive-UBC/Grad_School/Rcookbook/Themes_funs.R")

# General formatting -------------------------------------------------------------
nrow(Bird_pcs)

# Format point count data for occupancy and abundance modeling
Bird_pcs <- Bird_pcs %>% filter(Nombre_ayerbe %in% Tax_df3$Species_ayerbe) %>% 
  # There are 6 rows in UBC data where # individuals wasn't recorded. We know there was at least 1 individual
  mutate(Numero_individuos = ifelse(is.na(Numero_individuos), 1, Numero_individuos))

#Remove vuelos
#STILL TO DO: INVESTIGATE UNILLANOS, CIPAV, GAICA MBD
BirdsA <- Bird_pcs %>% filter(Estrato_vertical != "Vuelo" | is.na(Estrato_vertical) & !(tolower(Distancia_observacion) %in% c("sobrevuelo", "vuelo")))
nrow(BirdsA)

# Make distances numeric, 
BirdsA2 <- BirdsA %>%
  mutate(Distancia_observacion = case_when( # BirdsA = Birds analysis file
    Distancia_observacion == "0-15" ~ "15",
    Distancia_observacion == "15-30" ~ "30",
    Distancia_observacion == "30-50" ~ "50",
    Distancia_observacion == "< 25" ~ "25",
    Distancia_observacion == "<50" ~ "50",
    Distancia_observacion == ">50" ~ "51",
    Distancia_observacion == "> 50" ~ "51",
    .default = Distancia_observacion
  )) %>%
  mutate(Distancia_observacion = as.numeric(Distancia_observacion))
# Create Birds analysis file by removing records with distance > 50m
BirdsA3 <- BirdsA2 %>% filter(Distancia_observacion < 51 | is.na(Distancia_observacion))
tabyl(BirdsA3$Distancia_observacion)
nrow(BirdsA2)
## Still to do:: REMOVE VUELOS

# Note there are some rows that still have NAs for distance
BirdsA3 %>%
  filter(is.na(Distancia_observacion)) %>%
  select(Uniq_db, Fecha, Departamento, Id_muestreo, Nombre_finca, Nombre_ayerbe) %>%
  count(Id_muestreo, sort = T)

# Export
# write.csv(BirdsA3, "/Users/aaronskinner/Library/CloudStorage/OneDrive-UBC/Grad_School/PhD/SCR_data/AVES_Updated5.11.23/Birds_Analysis6.13.23.csv", row.names = F)

Pcs_nest <- Bird_pcs %>% group_by(Uniq_db) %>% # Nombre_institucion
  nest()

Bird_pcs %>% filter(is.na(Numero_individuos)) %>% 
  distinct(Nombre_ayerbe, Uniq_db, Fecha, Departamento, Id_muestreo)

Bird_pcs %>% filter(Nombre_ayerbe == "Pachysylvia semibrunnea" & Id_muestreo == "G-MB-Q-ECOR_05")

df_birds$Ubc_gaica_Caf %>% filter(Nombre_ayerbe == "Pachysylvia semibrunnea" & Id_muestreo == "G-MB-Q-ECOR_05")

# Access the nested data for a specific Uniq_db
UBC_MBD_data <- Pcs_nest %>% 
  filter(Uniq_db == "UBC MBD") %>% 
  pluck("data", 1)


# Occ abu formatting  -----------------------------------------------------

unit_covs <- envi_df2 %>%
  filter(Uniq_db == "UBC MBD" & Id_group != "UBC-MB-M-LBR") %>%
  arrange(Id_muestreo) %>%
  select(Elev, Avg_temp, Tot.prec)

# Pull the 30 species that have been observed more than 30 times
spp30 <- Bird_pcs2 %>%
  filter(Uniq_db == "UBC MBD") %>%
  group_by(Nombre_ayerbe) %>%
  summarize(Numero_individuos = sum(Numero_individuos)) %>%
  filter(Numero_individuos > 30) %>%
  pull(Nombre_ayerbe)

# Event_covs
event_covs <- Pc_date4 %>% filter(Uniq_db == "UBC MBD" & Id_group != "UBC-MB-M-LBR") %>% 
  select(Id_muestreo, Rep, Fecha, Pc_start) %>% 
  arrange(Id_muestreo, Rep)

# Abundances
Ubc_abu <- Bird_pcs2 %>%
  filter(Uniq_db == "UBC MBD" & Id_group != "UBC-MB-M-LBR") %>% #& Nombre_ayerbe %in% spp30
  left_join(event_covs[, c("Id_muestreo", "Fecha", "Pc_start", "Rep")]) %>%
  summarize(
    Numero_individuos = sum(Numero_individuos),
    .by = c(Id_muestreo, Rep, Nombre_ayerbe)
  ) %>%
  pivot_wider(
    names_from = Nombre_ayerbe,
    values_from = Numero_individuos,
    values_fill = 0
  ) %>% 
  arrange(Id_muestreo, Rep)

# Occupancy
Ubc_occ <- Ubc_abu %>% mutate(across(.cols = everything(), .fns = ~ ifelse(. > 0, 1, 0)))

fd_rep_varying <- make_flocker_data(
  obs = Ubc_abu,
  unit_covs = unit_covs,
  event_covs = event_covs
)


