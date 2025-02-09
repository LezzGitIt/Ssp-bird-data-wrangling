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
library(unmarked)
library(conflicted)
library(cowplot)
ggplot2::theme_set(theme_cowplot())
conflicts_prefer(dplyr::select)
conflicts_prefer(dplyr::filter)

load("Rdata/the_basics_01.09.25.Rdata")
load("Rdata/Taxonomy_12.29.24.Rdata")
source("/Users/aaronskinner/Library/CloudStorage/OneDrive-UBC/Grad_School/Rcookbook/Themes_funs.R")

# vignette("unmarked") # Used vignette to help understand how data should be formatted

# UPDATE:: UNIQUE IDENTIFIER NOW ID X YR X SEASON
Pc_date8 %>% distinct(Id_muestreo, Ano_grp, Season) 

# General formatting -------------------------------------------------------------
# Format point count data for downstream analyses 
Birds_analysis1 <- Bird_pcs  %>% 
  mutate(Nombre_ayerbe_ = str_replace_all(Nombre_ayerbe, " ", "_")) %>%
  filter(Nombre_ayerbe %in% Tax_df3$Species_ayerbe) %>% 
  filter(is.na(Grabacion) | Grabacion == "Cf") %>% # Remove birds identified only in recording
  mutate(Count = ifelse(is.na(Count), 1, Count)) #Add 1 individual when # individual = NA (6 rows)
  
## Remove vuelos
#STILL TO DO: INVESTIGATE UNILLANOS, CIPAV, GAICA MBD
Birds_analysis2 <- Birds_analysis1 %>% filter(Estrato_vertical != "Vuelo" | is.na(Estrato_vertical) & !(tolower(Distancia_bird) %in% c("sobrevuelo", "vuelo")))

## Remove distances > 50m
# NOTE:: there are some rows that have NAs for distance. Nearly all are Gaica 2013 
Birds_analysis2 %>% filter(is.na(Distancia_bird)) %>% 
  pull(Nombre_institucion) %>% table()

# GAICA reported birds > 50m that year, so we do not know if the birds with NA should be included or not.  
Birds_analysis2 %>% filter(is.na(Distancia_bird)) %>% 
  distinct(Uniq_db, Departamento, Id_muestreo, Familia, Nombre_ayerbe) %>% 
  distinct(Familia, Nombre_ayerbe) #%>% view()

# There are a few species (e.g. hummingbirds) that would be nearly impossible to identify at >50m.
Birds_analysis3 <- Birds_analysis2 %>%
  mutate(Distancia_bird = if_else(is.na(Distancia_bird) & Familia %in% c("Trochilidae", "Pipridae"), 
                                  "<50", Distancia_bird))

# Make distances numeric
Birds_analysis4 <- Birds_analysis3 %>%
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

# Remove records with distance unknown or > 50m
Birds_analysis5 <- Birds_analysis4 %>% filter(Distancia_bird < 51) %>% 
  # select only the columns necessary
  select(Ecoregion, Departamento, Uniq_db, Id_group, Id_muestreo_no_dc, Id_muestreo, Ano_grp, Fecha, Ano, Mes, Dia, Pc_start, Pc_length, Orden, Familia, Nombre_ayerbe, Nombre_ayerbe_, Count, Nombre_institucion, Nombre_finca_mixed)

## Add Rep to dataframe -- Pc_start & Rep are equivalent in terms of grouping (when combined with Id_muestreo & Ano_grp), but since we are using Rep for formatting of analysis dataframes it makes sense to include Rep as well
date_join_spp_obs <- Pc_date8 %>% filter(Spp_obs == 1) %>% # Only point counts with spp observed
  distinct(Id_muestreo, Ano_grp, Fecha, Pc_start, Rep) 
Birds_analysis6 <- Birds_analysis5 %>% left_join(date_join_spp_obs) 

# Summarize so each species is listed only once in each point count 
Birds_analysis6 %>% filter(if_any(everything(), is.na)) # No NAs in any rows
Birds_analysis <- Birds_analysis6 %>% summarize(Count = sum(Count), .by = -Count) %>% 
  # Lessen the magnitude of 4 outliers 
  mutate(Count = ifelse(Count > 50, 50, Count)) 

## Simplify this exercise by using only most abundant species 
Spp_counts <- Birds_analysis %>%
  summarize(Count = sum(Count), .by = c(Nombre_ayerbe, Nombre_ayerbe_)) %>%
  arrange(desc(Count)) %>%
  slice_max(n = 30, order_by = Count)

# The most abundant species in the project
No_maps <- c("Leptotila verreauxi", "Accipiter bicolor", "Sirystes sibilator", "Thripadectes virgaticeps")
Top_abu_df <- Spp_counts %>% slice_max(order_by = Count, n = 30) %>%
  filter(!Nombre_ayerbe %in% No_maps)
Top_abu <- Top_abu_df %>% 
  arrange(Nombre_ayerbe) %>%
  pull(Nombre_ayerbe)

Pc_locs_sf %>% distinct(geometry)

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
# We have 551 unique Id_muestreos, so the max number of rows a dataframe will have is 551 rows
# Generate Obs_covs_df where each row is a point count, each column is a site visit, and the 'Variable' column identifies which Observation covariate the row corresponds to  
Bird_pcs %>% distinct(Id_muestreo, Ano_grp, Uniq_db)

# Need a wide dataframe, where all point counts have a single row
Obs_covs_df <- Pc_date8 %>%
  mutate(across(everything(), as.character)) %>%
  pivot_longer(
    cols = c(Fecha, Pc_start, Pc_length, Uniq_db),      
    names_to = "Variable",          
    values_to = "Value"             
  ) %>%
  arrange(Ano_grp, Rep) %>%
  pivot_wider(
    id_cols = c(Id_muestreo, Ano_grp, Variable),
    names_from = c(Rep),
    names_glue = "Rep{Rep}",
    values_from = c(Value)
  ) 
head(Obs_covs_df)

# Split Obs_covs_df into a list, where each slot in the list is a covariate 
Obs_covs_spp <- map(Pcs_in_range, \(in_range){
  Obs_covs <- Obs_covs_df %>%
    filter(Id_muestreo %in% in_range) %>% 
    arrange(Id_muestreo, Ano_grp) %>%
    group_by(Variable) %>%
    group_split(.keep = FALSE)
  names(Obs_covs) <- sort(unique(Obs_covs_df$Variable)) 
  Obs_covs
})

# Convert back to dates and times, respectively, and scale
# Define functions to convert type back to date & time
convert_type <- list(as.Date, as_hms, as_hms, as.factor)

Obs_covs <- map(Obs_covs_spp, \(Obs_covs) {
  # Apply transformations on the first 3 elements
  Obs_covs_hold <- map2(Obs_covs, convert_type, \(oc, type) {
    oc %>% mutate(across(-c(Id_muestreo, Ano_grp), ~ type(.))) %>% 
      select(-c(Id_muestreo, Ano_grp)) %>%                    
      mutate(across(where(is.numeric), scale))                    
  })
  return(Obs_covs_hold)
})

# Site covariates ---------------------------------------------------------
# Covariates that are fixed for a given point count
# Uniq_db, Ano will need to lengthen dataframes (will give abundance trend through time),
date_join_ano <- Pc_date8 %>% distinct(Id_muestreo, Id_group, Ano_grp)
Site_covs_df <- Envi_df2 %>%
  full_join(date_join_ano) %>% 
  mutate(across(where(is.character), as.factor)) %>% 
  mutate(
    Ano1 = as.numeric(str_split_i(Ano_grp, "-", 1)), # Take the earlier of the two years 
    Habitat_cons = fct_relevel(Habitat_cons, 
                               c("Pastizales", "Cultivos", "Ssp", "Bosque ripario", "Bosque")),
    Ecoregion = relevel(Ecoregion, ref = "Piedemonte")
  ) %>%
  arrange(Id_muestreo, Ano_grp) %>%
  select(Id_muestreo, Id_group, Ecoregion, Elev, Avg_temp, Tot.prec, Habitat_cons, Ano1)

# Does rownames works?
Site_covs <- map(Pcs_in_range, \(in_range){
  Site_covs_df %>% filter(Id_muestreo %in% in_range) %>% 
    select(-c(Id_muestreo)) %>% 
    mutate(across(where(is.numeric), scale))
}) 

# Occ abu formatting  -----------------------------------------------------
# Include point counts with no spp observed 
date_bind_no_obs <- Pc_date8 %>% filter(Spp_obs == 0) %>% 
  distinct(Id_muestreo, Ano_grp, Rep) %>% 
  mutate(Nombre_ayerbe = NA, Count = NA)

# Abundances -- Use rbind with date_bind_no_obs to add counts where no species were observed
Abund_no_obs <- Birds_analysis %>%
  distinct(Id_muestreo, Ano_grp, Rep, Nombre_ayerbe, Count) %>% 
  rbind(date_bind_no_obs) 
 
# Create abundance list with just most abundant species 
Abund_l <- Abund_no_obs %>% filter(Nombre_ayerbe %in% Top_abu) %>% 
  group_split(Nombre_ayerbe, .keep = FALSE)
names(Abund_l) <- names(Pcs_in_range)

# Join abundance data (just containing points where species was observed) with all the point counts that the species could have been observed with. 
date_join_rep <- Pc_date8 %>% distinct(Id_muestreo, Ano_grp, Rep)
# Add Rep & Ano_grp info to allow for a successful join with Abund_l
Abund_in_range <- map2(Pcs_in_range, Abund_l, \(in_range, abund){
  Date_in_range <- tibble(Id_muestreo = in_range) %>% left_join(date_join_rep)
#NOTE:: right_join effectively adds NAs for Count where species could have been observed but weren't (greatly lengthening the dataframe), and also does a clip, ensuring that there are not observations outside of buffer
  abund %>% right_join(Date_in_range) %>% 
    # Change NAs for 0s
    mutate(Count = if_else(is.na(Count), 0, Count))
})

# Pivot_wider and fill in with zeros 
# TO DO: Make a function here? Depending on iNEXT needs
Abund_zeros <- map(Abund_in_range, \(abund){
  abund %>% arrange(Id_muestreo, Ano_grp, Rep) %>%
    pivot_wider(id_cols = c(Id_muestreo, Ano_grp),
                names_from = c(Rep),
                names_glue = "Rep{Rep}",
                values_from = Count, 
                values_fill = 0) %>% 
    arrange(Id_muestreo, Ano_grp) %>% 
    select(-c(Id_muestreo, Ano_grp))
    #column_to_rownames(var = "Id_muestreo") 
})

# CHECK:: The number of point counts in range x Ano_grp 
nrow(Site_covs_df)
max(map_dbl(Abund_zeros, nrow))  # 664 should be max

# Use the Abund_zeros list to select only the relevant columns for the observation covariates
Obs_covs2 <- map2(Obs_covs, Abund_zeros, \(oc, abund){
  map(oc, \(oc){
    cols <- colnames(abund)
    oc %>% select(all_of(cols))
  })
})

# Any of the observation covs (start time, pc length, or date) dataframes have NAs where a given point count wasn't surveyed at a given repetition. Use this data frame to assign NAs to the abundance dataframe
Abund_nas <- map2(Abund_zeros, Obs_covs2, \(abund, oc){
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

# Multi-species
# NOTE:: Can do multi species occupancy in unmarked , but would only want to do this, for example, with UBC (& UniLlanos data).
?unmarkedFrameOccuMulti # Set maxOrder = 1L to do zero higherorder interactions OR can manually set by putting covariates for the number of species present and 0s for everything else 
# Example with 3 species
stateformulas <- c("~Habitat_cons", "~Habitat_cons", "~Habitat_cons", "0", "0", "0", "0")

# 'For temporary emigration fit Chandler (2011) model with gpcount(), Don't think this is necessary
# ?unmarked::gpcount()

# Abundance
umf_abu_l <- pmap(
  .l = list(Abund_nas, Site_covs, Obs_covs2), 
  \(abund, sc, oc){
    unmarkedFramePCount(y = abund, siteCovs = sc, obsCovs = oc)
  })

# Occupancy
Occ_nas <- map(Abund_nas, \(abund){
  abund %>% mutate(across(.cols = everything(), .fns = ~ ifelse(. > 0, 1, 0)))
})

umf_occ_l <- pmap(.l = list(Occ_nas, Site_covs, Obs_covs2), 
                  \(abund, sc, oc){
                    unmarkedFrameOccu(y = abund, siteCovs = sc, obsCovs = oc)
                  })

## NOTE:: The UMF will throw an error if any of the dimensions don't match up, which is great. If the code runs this means that dimensions are correct 
# Error due to SiteCov
Bad_sites <- Site_covs$Amazona_ochrocephala %>% filter(Habitat_cons != "Bosque")
unmarkedFramePCount(Abund_nas$Amazona_ochrocephala, Bad_sites, Obs_covs2$Amazona_ochrocephala)

# Error due to obsCov
Obs_covs3$Amazona_ochrocephala <- Obs_covs2$Amazona_ochrocephala
Obs_covs3$Amazona_ochrocephala[[3]] <- Obs_covs2$Amazona_ochrocephala[[3]] %>% select(-1)
unmarkedFramePCount(Abund_nas$Amazona_ochrocephala, Site_covs$Amazona_ochrocephala, Obs_covs3$Amazona_ochrocephala)

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
Top_abu_df2 <- Top_abu_df %>% mutate(Num_pcs = map_dbl(Pcs_in_range, length)) %>% 
  rename(Tot_count = Count)

# iNEXT -------------------------------------------------------------------
Spp_wide <- Abund_long %>% 
  pivot_wider(
    names_from = Nombre_ayerbe,
    values_from = Count,
    values_fill = 0
  ) %>% arrange(Id_muestreo, Ano_grp, Rep) %>% 
  Cap_snake()

# Save & Export -----------------------------------------------------------
# Export R data object for future analyses 
rm(list = ls()[!(ls() %in% c("Birds_analysis", "umf_abu_l", "umf_occ_l", "spOcc_l", "Top_abu_df2"))]) #"Abund_nas", "Site_covs", "Obs_covs"
#save.image(paste0("Rdata/Occ_abu_inputs_", format(Sys.Date(), "%m.%d.%y"), ".Rdata"))

# Export R data object for BIOL314
#rm(list = ls()[!(ls() %in% c("Bird_pcs", "Birds_analysis", "Pc_hab", "Site_covs_df"))])
#save.image(paste0("Rdata/Biol314_inputs_", format(Sys.Date(), "%m.%d.%y"), ".Rdata"))

# Checks ----------------------------------------------------
# There should be no NAs in the relevant columns at this point 
Pc_date8 %>% select(Uniq_db, Fecha, Pc_length, Pc_start) %>% 
  Na_rows_cols(distinct = TRUE)

# Visualize Pc_lengths by Uniq_db
Pc_date8 %>% distinct(Uniq_db, Pc_length) %>% 
  filter(!Uniq_db %in% c("Cipav mbd", "Gaica distancia"))

# Using the nested list Obs_covs 
map_depth(Obs_covs, 2, dim) # Troglodytes_aedon has all 551 rows possible, so we'll use it

# Add Id_muestreo back to the 3 dataframes
Example_l <- map(Obs_covs$Troglodytes_aedon, \(df){
    cbind(Id_muestreo = Site_covs_df$Id_muestreo, df) %>% 
      tibble()
  })

# Could replace the nan for NA , but the coding is more complex than it should be 
Example_l$Pc_length[is.nan(Example_l$Pc_length)] <- NA

# This returns the proportion of values that are TRUE (NA in same loc) vs FALSE (NA in different loc)
Na_locs_equal(Example_l$Fecha, Example_l$Pc_start)
Na_locs_equal(Example_l$Fecha, Example_l$Pc_length)
Na_locs_equal(Example_l$Pc_start, Example_l$Pc_length)

# I think this must be causing the difference.. Likely due to the Pc_lengths that are 0. Fortunately I believe R interprets NA and NaN the same way in 99% of cases 
table(is.nan(Example_l$Pc_length[[2]]))
table(is.nan(Example_l$Pc_start[[2]]))

pcl <- Obs_covs_df %>% filter(Variable == "Pc_length")
pcs <- Obs_covs_df %>% filter(Variable == "Pc_start")
fecha <- Obs_covs_df %>% filter(Variable == "Fecha")

Na_locs_equal(pcl, pcs)
Na_locs_equal(fecha, pcs)
Na_locs_equal(fecha, pcl)

# DELETE 
TF_mat <- is.na(Example_l$Fecha) == is.na(Example_l$Pc_start) & is.na(Example_l$Fecha) == is.na(Example_l$Pc_length)
discrepancies <- which(!TF_mat, arr.ind = TRUE) %>% as_tibble()
discrepancies

ids_na_diff <- Example_l$Fecha[discrepancies$row, 1] %>% pull(Id_muestreo) %>% 
  unique()
Obs_covs_df %>% filter(Id_muestreo %in% ids_na_diff) %>% 
  select(Id_muestreo, unique(discrepancies$col)) %>%
  filter(if_any(everything(), is.na))

# Visualize example
df_meta %>% filter(Id_muestreo == "G-AD-M-LCA1_09") %>% 
  select(Id_muestreo, Fecha, Hora, Spp_obs)

