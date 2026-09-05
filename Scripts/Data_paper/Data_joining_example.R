## Data exploration and example joins ## 

## Objectives: 
# 1) Explore: Basic exploration of the data
# 2) Joins: Provide examples joining the different datasets together using join keys
# 3) Missing values: Identify and visualize key missing data in each dataset 

# Libraries ---------------------------------------------------------------
library(tidyverse)
library(janitor)
library(naniar)

# Bring in data -----------------------------------------------------------
# Read in the data and store in a named list
path <- "DataS1"
File_names <- list.files(path)

Data <- map(File_names, \(file){
  read_csv(file = paste0(path, "/", file))
})
names(Data) <- str_remove(File_names, ".csv")

# Explore -----------------------------------------------------------------
# The datasets were split such that information is minimally repeated between datasets. This helps understand the structure of the datasets, such that the number of rows represent something meaningful. E.g.
map_dbl(Data, nrow)
# There are 20,959 bird observations before filtering, 17,155 bird observations after removing flyovers, recording-only IDs, or species observed outside of the point count radius (see 'Scripts/06_Analysis_wrangling.R'), 2,996 point count surveys, 587 species, and 504 unique survey locations.

# The taxonomy dataframe doesn't have as clear of an interpretation because of taxonomic lumps and splits. However, if you take the unique species names according to Ayerbe's taxonomy, you see again that there are 587 unique species observed.
nrow(distinct(Data$Taxonomy, Species_ayerbe))

# Examine structure of each dataframe
map(Data, \(df){
  str(df)
}) %>% discard(.p = \(df) is.null(df))

# Examine first 15 unique entries of each column 
map(Data, \(df){
  map(df, \(col){
    head(unique(col), n = 15)
  })
})

# Example joins -----------------------------------------------------------
# >Bird, site, and event covs ---------------------------------------------

## The structure of the data (minimal repetition) requires joining data frames by primary keys (see https://r4ds.hadley.nz/joins.html). This is trivial in R, but I provide some examples of things we've commonly done in our work.

## Link the bird observation data with the survey covariates 
Birds_event_covs <- Data$Bird_pcs_all %>% full_join(Data$Event_covs)

# Link the bird observation data with the site information 
Data$Bird_pcs_analysis %>% full_join(Data$Site_covs) 

# Link the survey covariates with the site information 
Data$Event_covs %>% full_join(Data$Site_covs)

# Link all three databases 
Three_way_join <- Data$Bird_pcs_all %>% 
  full_join(Data$Site_covs) %>% 
  full_join(Data$Event_covs)
Three_way_join

# >Bird obs and functional traits ------------------------------------------

Data$Bird_pcs_all %>% 
  left_join(Data$Functional_traits) %>%
  select(Species_ayerbe, Id_muestreo, 14:37)

# Missing data ------------------------------------------------------------
# >No species observed at a point count  ---------------------------------
# There are 56 point counts where no birds were observed, and thus they have no data in Bird_pcs_all
Data$Event_covs %>% filter(Spp_obs == 0) %>% 
  nrow()
# Note that a full_join() was used to create Birds_event_covs, thus this tibble now has data for the point counts where no species were observed
nrow(Birds_event_covs) - nrow(Data$Bird_pcs_all)

# Given that Bird_pcs_analysis is a subset of Bird_pcs_all, there are additional point counts that were surveyed but have no data in Bird_pcs_analysis. For example, the point count "UBC-MB-M-A_02" contains 52 observations, but all of them were either >50m from the observer or were flyovers (Sobrevuelo). 
Data$Bird_pcs_all %>% filter(Id_muestreo == "UBC-MB-M-A_02") %>% 
  distinct(Distancia_bird, Tipo_registro)
# Thus, it has no observations in the Bird_pcs_analysis tibble
Data$Bird_pcs_analysis %>% filter(Id_muestreo == "UBC-MB-M-A_02") 
# There are 101 point counts like this, that are present in Bird_pcs_all but not in Bird_pcs_analysis 
Birds_event_covs_analysis <- Data$Bird_pcs_analysis %>% 
  right_join(Data$Event_covs) %>% 
  filter(Spp_obs == 1)
nrow(Birds_event_covs_analysis) - nrow(Data$Bird_pcs_analysis)

# >Missing values ---------------------------------------------------------
## Bird observation data
miss_var_summary(Data$Bird_pcs_all)
miss_var_summary(Data$Bird_pcs_analysis) # No missing data!

## Plot missingness for bird observations data
# Only UBC GAICA systematically recorded point counts and included that information in data sheets
# Cipav did not record 'Tipo_registro' (about 9% of the observations)
vis_miss(Data$Bird_pcs_all)
# Examine the co-ocurrence of NAs among variables
gg_miss_upset(Data$Bird_pcs_all)

## Event covariates
# Most data sets did not collect detection variables like Noise, weather, and whether there were cows within 50m of the point count
miss_var_summary(Data$Event_covs)

## Site covaraites
# Sub-habitats were deemed not applicable when the primary habitat type was Mosaic or Pasture (Pastizales), and was not recorded for crops (Cultivos)
miss_var_summary(Data$Site_covs)
Data$Site_covs %>% 
  filter(is.na(Habitat_sub)) %>% 
  tabyl(Habitat)
