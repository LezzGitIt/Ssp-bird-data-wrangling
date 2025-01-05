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
# KEY to keeping workflows with map() tractable, keep things in a single df as long as possible, then split into hierarchical lists if needed. Avoid hierarchical map calls.

# Load libraries & data ---------------------------------------------------
library(tidyverse)
library(janitor)
library(chron)
library(readxl)
library(sf)
library(conflicted)
ggplot2::theme_set(theme_cowplot())
conflicts_prefer(dplyr::select)
conflicts_prefer(dplyr::filter)

load("Rdata/the_basics_01.03.25.Rdata")
load("Rdata/Taxonomy_12.29.24.Rdata")
source("/Users/aaronskinner/Library/CloudStorage/OneDrive-UBC/Grad_School/Rcookbook/Themes_funs.R")

# vignette("unmarked") # Used vignette to help understand how data should be formatted

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
#Ubc30 <- Spp_counts %>% 
 # filter(Count > 30) %>%
  #distinct(Nombre_ayerbe, Nombre_ayerbe_)

# The most abundant species in the project
No_maps <- c("Leptotila verreauxi", "Accipiter bicolor", "Sirystes sibilator", "Thripadectes virgaticeps")
Top_abu_df <- Spp_counts %>% slice_max(order_by = Count, n = 30) %>%
  filter(!Nombre_ayerbe %in% No_maps)
Top_abu <- Top_abu_df %>% 
  arrange(Nombre_ayerbe) %>%
  pull(Nombre_ayerbe)

# Biogeographic_clipping --------------------------------------------------
## Idea from Socolar's (2022) paper: Biogeographic multi-species occupancy models for large-scale survey data. We only want to include point count locations that are within the species range (+ some buffer), differentiating a true zero (possible but not observed) vs points that are simply out of range (more like an NA).
## NOTE:: Working with 2 different approaches - a data frame vs nested list

# Load in relevant shapefiles 
Ayerbe_mod_spp_l <- map(Top_abu, \(spp){
  st_read(dsn = "../Geospatial_data/Ayerbe_shapefiles_1890spp", layer = spp) %>% st_make_valid()
})
names(Ayerbe_mod_spp_l) <- Top_abu
Ayerbe_mod_spp <- do.call(rbind, Ayerbe_mod_spp_l)

# Calculate the shortest distance (km) from each point count location to the species range
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

# Plot the rows with Pcs > 0 km
if(FALSE){
  Dist_pcs_sf %>% filter(if_any(where(is.numeric), ~ .x > 0)) %>%
    ggplot() + 
    geom_sf(data = Ayerbe_mod_spp_l$`Ixothraupis guttata`) +
    geom_sf(aes(color = Ixothraupis_guttata)) #+
  #geom_sf(data = Pc_locs_sf, color = "red") 
}

## Extract 'Id_muestreo' where the distance to the spp range == 0 for later subsetting
Pcs_in_range <- Dist_to_pcs %>%
  select(where(is.numeric)) %>% 
  map(~ Dist_to_pcs %>%
        filter(.x == 0) %>% 
        pull(Id_muestreo))

# Only retain species that have at least some observations within its range 
length(spp_rm)
spp_rm <- Pcs_in_range %>%
  imap(~ if (length(.x) == 0) .y) %>% # Retain names of length-0 slots
  compact() 
if(length(spp_rm) > 0){
  spp_rm %>% list_simplify()
  Pcs_in_range <- Pcs_in_range %>% discard_at(spp_rm)
  
  spp_rm <- str_replace(spp_rm, "_", " ")
  Top_abu_df <- Top_abu_df %>% filter(!Nombre_ayerbe %in% spp_rm2)
  Top_abu <- Top_abu[!Top_abu %in% spp_rm2] 
}

# NOTE:: In the future will likely want to use some proportion of the total area via st_area()
st_area(Ayerbe_mod_spp) 

# Observation covariates -----------------------------------------------------------
# We have 551 unique Id_muestreos, so all data frames should have 551 rows
# Generate Obs_covs_l where each row is a point count, each column is a site visit, and each slot in the list is a covariate 

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

# Define functions to convert type back to date & time
convert_type <- list(as.Date, chron::as.times, chron::as.times)

# Split Obs_covs_df into a list, map function
Obs_covs <- map(Pcs_in_range, \(in_range){
  Obs_covs <- Obs_covs_df %>%
    filter(Id_muestreo %in% in_range) %>% 
    arrange(Id_muestreo) %>%
    group_by(Variable) %>%
    group_split(.keep = FALSE)
  names(Obs_covs) <- sort(unique(Obs_covs_df$Variable)) 
  
  # Convert back to dates and times respectively
  map2(Obs_covs, convert_type, \(oc, type){
    oc %>% mutate(across(-1, ~ type(.))) %>% 
      select(-Id_muestreo) %>% 
      #column_to_rownames(var = "Id_muestreo") %>%
      mutate(across(everything(), scale))
  })
})

# NOTE:: These are not all NAs, just the values shown are (early years of CIPAV)
Obs_covs[[1]][[2]] %>% 
  filter(if_any(everything(), ~!is.na(.x))) %>% 
  select(`Ano16-17_Rep1`)

# Site covariates ---------------------------------------------------------
# Covariates that are fixed for a given point count
Site_covs_df <- Envi_df2 %>%
  arrange(Id_muestreo) %>%
  select(Id_muestreo, Elev, Avg_temp, Tot.prec, Habitat_ut2) %>% 
  mutate(Habitat_ut2 = factor(Habitat_ut2, 
                              levels = c("Pastizales", "Cultivos", "Ssp", "Bosque ripario", "Bosque")))

# Does rownames works?
Site_covs <- map(Pcs_in_range, \(in_range){
  Site_covs_df %>% filter(Id_muestreo %in% in_range) %>% 
    select(-Id_muestreo) %>% 
    #column_to_rownames(var = "Id_muestreo") %>%
    mutate(across(where(is.numeric), scale))
}) 

# Occ abu formatting  -----------------------------------------------------
Pc_date_join <- Pc_date4 %>% distinct(Id_muestreo, Ano_grp, Pc_start, Rep)

# Abundances -- join with Pc_date_join 
Abund_long <- Birds_fin %>%
  full_join(Pc_date_join, relationship = "many-to-many") %>%
  summarize(
    Count = sum(Count),
    .by = c(Id_muestreo, Ano_grp, Rep, Nombre_ayerbe)
  )
 
# Create abundance list with just most abundant species 
Abund_l <- Abund_long %>% filter(Nombre_ayerbe %in% Top_abu) %>% 
  group_split(Nombre_ayerbe, .keep = FALSE)
names(Abund_l) <- names(Pcs_in_range)

# Join abundance data (just containing points where species was observed) with all the point counts that the species could have been observed with
# Add Rep & Ano_grp info to allow for a successful join with Abund_l
Abund_in_range <- map2(Pcs_in_range, Abund_l, \(in_range, abund){
  Date_in_range <- tibble(Id_muestreo = in_range) %>% left_join(Pc_date_join)
#NOTE:: right_join effectively does the clip, ensuring that there are not observations outside of buffer
  abund %>% right_join(Date_in_range)
})

# Issue is that Crotophaga_sulcirostris was NOT seen at all on this Ano_rep combo (or any in 2019 I think). So these columns are not created in the abundance
is.na(Obs_covs$Crotophaga_sulcirostris$Fecha$Ano19_Rep3) %>% table()
# It might work to ensure that all of these combos are in Abund_l
colnames(Obs_covs$Crotophaga_sulcirostris$Fecha)
Abund_l$Crotophaga_sulcirostris

# Pivot_wider and fill in with zeros 
# TO DO: Make a function here? Depending on iNEXT needs
Abund_zeros <- map(Abund_in_range, \(abund){
  abund %>% mutate(Count = if_else(is.na(Count), 0, Count)) %>%  
    arrange(Ano_grp, Rep) %>%
    pivot_wider(id_cols = c(Id_muestreo),
                names_from = c(Ano_grp, Rep),
                names_glue = "Ano{Ano_grp}_Rep{Rep}",
                values_from = Count, 
                values_fill = 0) %>% 
    arrange(Id_muestreo) %>% 
    select(-Id_muestreo)
    #column_to_rownames(var = "Id_muestreo") 
})

# Use the Abund_zeros list to select only the relevant columns for the observation covariates
Obs_covs2 <- map2(Obs_covs, Abund_zeros, \(oc, abund){
  map(oc, \(oc){
    cols <- colnames(abund)
    oc %>% select(all_of(cols))
  })
})

# Any of the observation covs (start time, pc length, or date) dataframes have NAs where a given point count wasn't surveyed at a given repetition. Use this data frame to assign NAs to the abundance dataframe
Abund_nas <- map2(Abund_zeros, Obs_covs, \(abund, oc){
  NAs <- oc$Pc_start
  abund %>% mutate(across(everything(), ~ if_else(is.na(NAs[[cur_column()]]), NA, .x)))
})

# Ensure the orders of Id_muestreo are the same in all input values to UMF
# Function to check if all values in a row are identical
is_same <- function(x) {
  all(x == x[1])
}

same_df <- tibble(
  az = Abund_nas$Bubulcus_ibis$Id_muestreo,
  sc = Site_covs$Bubulcus_ibis$Id_muestreo,
  oc = Obs_covs$Bubulcus_ibis$Fecha$Id_muestreo
) %>% rowwise() %>% 
  mutate(is_same = is_same(c_across(everything())))
table(same_df$is_same)

# unmarkedFrame -----------------------------------------------------------
## Create unmarkedFrames
# Abundance
umf_abu_l <- pmap(
  .l = list(Abund_nas, Site_covs, Obs_covs2), 
  .f = \(abund, sc, oc){
    unmarkedFramePCount(y = abund, siteCovs = sc, obsCovs = oc)
  })

# Occupancy
Occ_nas <- map(Abund_nas, \(abund){
  abund %>% mutate(across(.cols = everything(), .fns = ~ ifelse(. > 0, 1, 0)))
})

umf_occ_l <- pmap(.l = list(Occ_nas, Site_covs, Obs_covs2), 
                  .f = \(abund, sc, oc){
                    unmarkedFrameOccu(y = abund, siteCovs = sc, obsCovs = oc)
                  })

# spOccupancy -------------------------------------------------------------
## Prep the data structure for spOccupancy package

# One benefit is that spOccupancy can account for spatial autocorrelation. 
# Project coordinates for Colombia and filter by relevant PCs for each species
Coords <- map(Pcs_in_range,  \(in_range){
  Pc_locs_sf %>% distinct(Id_muestreo, geometry) %>% 
    filter(Id_muestreo %in% in_range) %>% 
    st_transform(crs = st_crs("EPSG:32618")) %>% 
    st_coordinates() # Remove as needed for plotting 
})

# Ensure adequate plotting 
if(FALSE){
  bi_sf <- Ayerbe_mod_spp %>% filter(Nombre == "Bubulcus ibis") %>% 
    st_transform(crs = st_crs("EPSG:32618"))
  ggplot() + 
    geom_sf(data = bi_sf) +
    geom_sf(data = Coords[[1]])
}

# Combine inputs into the list structure needed by spOccupancy. 
# NOTE:: Different names require in the list occ.covs = occurrence (or occupancy) covariates
spOcc_l <- pmap(list(Occ_nas, Site_covs, Obs_covs2, Coords),
                \(occ, sc, oc, coords){
                  list(y = occ, occ.covs = sc, det.covs = oc, coords = coords)
})


# Top_abu_df --------------------------------------------------------------
Top_abu_df2 <- Top_abu_df %>% mutate(Num_pcs = map_dbl(Pcs_in_range, length))

# iNEXT -------------------------------------------------------------------
Spp_wide <- Abund_long %>% 
  pivot_wider(
    names_from = Nombre_ayerbe,
    values_from = Count,
    values_fill = 0
  ) %>% arrange(Id_muestreo, Ano_grp, Rep)

# Save & Export -----------------------------------------------------------
#rm(list = ls()[!(ls() %in% c("umf_abu_l", "umf_occ_l", "spOcc_l", "Top_abu_df2"))]) #"Abund_nas", "Site_covs", "Obs_covs"
#save.image(paste0("Rdata/Occ_abu_inputs_", format(Sys.Date(), "%m.%d.%y"), ".Rdata"))

# >Interpolate Pc_start ----------------------------------------------------
# Also Pc_date for UniLlanos. Ask Natalia for help if needed

# Visualize the problem: The sites where no species were observed (Spp_obs == 0) have varying amounts of NAs. May be a few additional weird NAs, but this is main challenge
Pc_date4 %>% filter(Spp_obs == 0) %>% 
  select(Uniq_db, Fecha, Pc_length, Pc_start) %>% 
  view()
#drop_na()

# Visualize Pc_lengths by Uniq_db
Pc_date4 %>% distinct(Uniq_db, Pc_length) %>% 
  filter(!Uniq_db %in% c("Cipav mbd", "Gaica distancia"))

# Assign Pc_length based on what we know of the data collectors' methodologies. 
# Really, this is only relevant for CIPAV. Pc_length is helpful for data checking, but in models only CIPAV has meaningful variation (within a Uniq_db), and could just blanket assign values for between Uniq_dbs
# NOTE this provides solution to challenge with different types of class times, use as.numeric first
Pc_date5 <- Pc_date4 %>%
  mutate(
    Pc_length = case_when(
      is.na(Pc_length) & Uniq_db %in% c("Gaica mbd", "Ubc mbd", "Unillanos mbd") ~ 
        as.numeric(times("00:00:00")),
      is.na(Pc_length) & Uniq_db == "Gaica distancia" ~ 
        as.numeric(times("00:10:00")),
      .default = as.numeric(Pc_length) # Convert existing Pc_length to numeric for consistency
    ),
    Pc_length = times(Pc_length) # Convert back to times at the end
  )

# Using Obs_covs_l
TF_mat <- is.na(Obs_covs_l$Fecha) == is.na(Obs_covs_l$Pc_start) & is.na(Obs_covs_l$Fecha) == is.na(Obs_covs_l$Pc_length)
discrepancies <- which(!TF_mat, arr.ind = TRUE) %>% as_tibble()
discrepancies


ids_na_diff <- Obs_covs_l$Fecha[discrepancies$row, 1] %>% pull(Id_muestreo)
Obs_covs_df %>% filter(Id_muestreo %in% ids_na_diff) %>% 
  select(Id_muestreo, unique(discrepancies$col)) %>%
  filter(if_any(everything(), is.na))

# Visualize example
df_meta %>% filter(Id_muestreo == "G-AD-M-LCA1_09") %>% 
  select(Id_muestreo, Fecha, Hora, Spp_obs)

# Think about how imputation might work. Would want to create AM_PM variable 
Pc_date4 %>% filter(Fecha == "2019-09-24") %>% 
  distinct(Id_muestreo, Fecha, Pc_start, Spp_obs) %>% 
  arrange(Id_muestreo, Fecha) 

# Example GPT code to fill in times
Pc_date4_filled <- Pc_date4 %>%
  arrange(Id_muestreo, Fecha, Pc_start) %>% # Arrange by Id_muestreo and Pc_start
  group_by(Fecha, Id_group) %>%
  mutate(
    Pc_start = if_else(
      is.na(Pc_start),
      {
        # Find the previous and next Pc_start times
        prev_time <- dplyr::lag(Pc_start)
        next_time <- lead(Pc_start)
        
        # Interpolate or use available neighbors, ensuring the result is of class 'times'
        if (!is.na(prev_time) & !is.na(next_time)) {
          times((as.numeric(prev_time) + as.numeric(next_time)) / 2) # Midpoint
        } else if (!is.na(prev_time)) {
          prev_time # Use previous time if no next
        } else if (!is.na(next_time)) {
          next_time # Use next time if no previous
        } else {
          NA # Fallback if no neighbors
        }
      },
      Pc_start # Keep original values
    )
  ) %>%
  ungroup()