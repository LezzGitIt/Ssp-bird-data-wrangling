## PhD birds in silvopastoral landscapes ##
# Data wrangling to add the canopy cover / height covariates to Event_covs (from Event_covs_pcs)
# The digitized-polygon land-cover metrics live in Chapter 1 (Ch1-ssp-birds: Scripts/00a_Extract_lcs.R, 00b_LSM.R), not in this deposit pipeline.

## Instructions
# Set 'generate' to TRUE or FALSE, which decides whether to generate the masked and cropped files or whether to import the buffers (already masked and cropped) which are much faster to work with. 

## External files
# Remote sensing .tif files come from the Colombia Woody Vegetation Structure and Change repository: https://zenodo.org/records/18154841.
# NOTE: There are 3 file types for each year 2010 - 2024. 
# 1) Canopy cover - Canopy cover of trees and shrubs > 2m. Will serve as an index of landscape forest cover
# 2) Canopy height - Height (in decimeters) of trees / shrubs > 2m 
# 3) Change - Not using at present

## Script contents
# Bring in .tif files 
# For both the 1) Canopy cover and 2) Canopy height files... 
# -Create buffers of appropriate size (e.g. 1000m)
# -Crop and mask large .tif files using buffers for improved processing speed
# -Extract raster information within each buffer

# To do  ------------------------------------------------------------------

## Conduct scale of effect analysis
# Start with 5, then get more fine scale as needed 
Buffers_soe <- seq(from = 1000, to = 5000, by = 1000)

# Load libraries & data ---------------------------------------------------
## Load libraries
library(tidyverse)
library(janitor)
library(terra)
library(tidyterra)
library(sf)
library(exactextractr)
library(conflicted)
library(cowplot)
ggplot2::theme_set(theme_cowplot())
conflicts_prefer(dplyr::select)
conflicts_prefer(dplyr::filter)
source("Scripts/Pipeline_fns.R") # shared helpers (Cap_snake, standardize_column_contents, Na_rows_cols, ...)

## Load data
Event_covs_pcs <- read_csv("Derived/Excels/Event_covs_pcs.csv")
Pc_locs <- vect("Derived/Geospatial/shp/Pc_locs.gpkg")

# Pc_locs must carry one geometry per point -- a duplicated Id_survey_no_dc (e.g. a stale .gpkg from a run predating the El Hatico Ref_NA fix) makes the canopy joins below go many-to-many. Regenerate the .gpkg from 01 if this trips.
if (anyDuplicated(Pc_locs$Id_survey_no_dc)) {
  stop("Pc_locs.gpkg has duplicate Id_survey_no_dc -- regenerate it from 01_Gen_wrangling.R")
}

# Load in Woody vegetation structure and change (WVSC) file 
Years <- c("2013", "2014", "2016", "2017", "2019", "2022", "2024")

# Generate files? ---------------------------------------------------------
# These are large spatial files, and cropping and masking takes a long time, so I have exported 1km buffers around point counts which are much faster to work with. If loading the buffers then set generate == FALSE, if working with the raw .tif files set generate == TRUE
generate <- FALSE

if(generate){
  Geospatial_path <- "../Geospatial_data/Environmental/"
  
  ## Point to spatial files
  # Canopy cover
  Cover_l <- map(Years, \(year){
    rast(paste0(Geospatial_path, "Colombia_WVCC_", year, ".tif"))
  })
  names(Cover_l) <- Years
  
  # Canopy height
  Height_l <- map(Years, \(year){
    rast(paste0(Geospatial_path, "Colombia_WVCH_", year, ".tif"))
  })
  names(Height_l) <- Years
}

# WVSC 2010-2024 ----------------------------------------------------------
# >Cover ------------------------------------------------------------------
# Buffer of 1000m to serve as an indicator of landscape forest cover
Locs_1k <- Pc_locs %>% buffer(1000) # 5000m
# Example buffers for plotting
Ex_pts <- Pc_locs %>% filter(str_starts(Id_survey_no_dc, "MB-M-LRO1_"))
Ex_buff_1k <- Locs_1k %>% filter(str_starts(Id_survey_no_dc, "MB-M-LRO1_"))

# Crop and mask (SLOW)
if(generate){
  Cover_mask_l <- imap(Cover_l, \(cover_r, year){
    print(year)
    cover_crop <- crop(cover_r, Locs_1k)
    mask(cover_crop, Locs_1k)
  })
}

# Load in data that is already cropped and masked
if(!generate){
  path <- "Derived/Geospatial/tif/wsvc"
  Cover_mask_l <- map(Years, \(year){
    rast(paste0(path, "/Canopy_cover_1k/Cover_buff_", year, ".tif"))
  })
  names(Cover_mask_l) <- Years
}

# Visualize an example buffer
Ex_crop <- crop(Cover_mask_l[[1]], Ex_buff_1k)
Ex_mask <- mask(Ex_crop, Ex_buff_1k)
ggplot() + 
  geom_spatraster(data = Ex_mask) +
  geom_spatvector(data = Ex_pts, color = "red", size = 1) 

# Extract
# exact_extract (C++) replaces a per-year terra::extract loop that ran ~15 min over the 1km buffers -- this is seconds. Keeping only cells at least half inside the buffer (coverage_fraction >= 0.5) reproduces terra's centroid rule, so the values match the previous extraction (height exactly; cover to within 0.04 of a percentage point).
extract_buffers <- function(raster_l, polys, summary_fn) {
  polys_sf <- sf::st_as_sf(polys)
  imap(raster_l, \(r, year) {
    tibble(
      Id_survey_no_dc = polys$Id_survey_no_dc,
      Year = as.numeric(year),
      Value = exact_extract(r, polys_sf, progress = FALSE, fun = \(vals, cov_frac) {
        inside <- cov_frac >= 0.5
        if (any(inside)) summary_fn(vals[inside]) else summary_fn(vals)
      })
    )
  }) %>% list_rbind()
}

Cover_tbl <- extract_buffers(Cover_mask_l, Locs_1k, \(x) mean(x, na.rm = TRUE)) %>%
  mutate(Canopy_cover = Value, Scale_m = 1000) %>%
  select(-Value)

# >Height ------------------------------------------------------------------
# Buffer of 50m to extract the max canopy height of the point count 
Locs_50m <- Pc_locs %>% buffer(50) 
Ex_buff_50m <- Locs_50m %>% filter(str_starts(Id_survey_no_dc, "MB-M-LRO1_"))

# Crop and mask (SLOW)
if(generate){
Height_mask_l <- imap(Height_l, \(height_r, year){
  print(year)
  height_crop <- crop(height_r, Locs_1k)
  mask(height_crop, Locs_1k)
})
}

# Load in data that is already cropped and masked
if(!generate){
  Height_mask_l <- map(Years, \(year){
    rast(paste0(path, "/Canopy_height_50m/Height_buff_", year, ".tif"))
  })
  names(Height_mask_l) <- Years
}

# Visualize an example buffer
Ex_crop <- crop(Height_mask_l[[1]], Ex_buff_50m)
Ex_mask <- mask(Ex_crop, Ex_buff_50m)
ggplot() + geom_spatraster(data = Ex_mask)

# Extract max height within 50m
Height_tbl2 <- extract_buffers(Height_mask_l, Locs_50m, \(x) max(x, na.rm = TRUE)) %>%
  # Convert from decimeters to meters
  mutate(Canopy_height_m = Value / 10) %>%
  select(-Value)

## Join
# WSVC data only goes through 2024, so 2025/2026 surveys are matched to the 2024 canopy layer -- via a temporary join year so the real Year column is never overwritten (an earlier version reset every Recent_pts row to 2025, which also clobbered the other-year visits that share an Id_survey)
Event_covs <- Event_covs_pcs %>%
  mutate(Year_wvsc = ifelse(Year %in% c(2025, 2026), 2024, Year)) %>%
  left_join(Cover_tbl,   by = c("Id_survey_no_dc", "Year_wvsc" = "Year")) %>%
  left_join(Height_tbl2, by = c("Id_survey_no_dc", "Year_wvsc" = "Year")) %>%
  # Pc_obs_span is a QA/QC aid only (see 01_Gen_wrangling.R) -- the deposit keeps Pc_duration
  select(-any_of(c("Year_wvsc", "Scale_m", "Pc_obs_span")))

Event_covs %>% filter(is.na(Canopy_height_m) | is.na(Canopy_cover))

# >Visualize --------------------------------------------------------------
## Visualize distributions 
Event_covs %>% ggplot() + 
  geom_histogram(aes(x = Canopy_height_m))

Event_covs %>% ggplot() + 
  geom_histogram(aes(x = Canopy_cover))

# >Check ------------------------------------------------------------------
Event_covs %>%
  Na_rows_cols(
    id_cols = Id_survey,
    cols_inc = -c(Registered_by, Noise, Weather, Cows_50m, Pc_duration)
  )

# Export ------------------------------------------------------------------
Event_covs %>% write_csv(file = "Derived/Excels/Event_covs.csv")

# Export the masked files to save time in future iterations 
if(FALSE){
  # Canopy cover
  imap(Cover_mask_l, \(mask_r, year){
    print(year)
    writeRaster(mask_r, paste0(path, "/Canopy_cover_1k/Cover_buff_", year, ".tif"))
  })
  
  # Canopy height
  imap(Height_mask_l, \(mask_r, year){
    print(year)
    writeRaster(mask_r, paste0(path, "/Canopy_height_50m/Height_buff_", year, ".tif"))
  })
}