## PhD birds in silvopastoral landscapes ##
# Data wrangling to add the canopy cover / height covariates to Event_covs (from Event_covs_pcs)
# The digitized-polygon land-cover metrics live in Chapter 1 (_ch1_pending/Extract_lcs.R, LSM.R), not in this deposit pipeline.

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
library(conflicted)
library(cowplot)
ggplot2::theme_set(theme_cowplot())
conflicts_prefer(dplyr::select)
conflicts_prefer(dplyr::filter)
source("/Users/aaronskinner/Library/CloudStorage/OneDrive-UBC/Academia/Rcookbook/Themes_funs.R")

## Load data
Event_covs_pcs <- read_csv("Derived/Excels/Event_covs_pcs.csv")
Pc_locs <- vect("Derived/Geospatial/shp/Pc_locs.gpkg")

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
Ex_pts <- Pc_locs %>% filter(Id_group_no_dc == "MB-M-LRO1")
Ex_buff_1k <- Locs_1k %>% filter(Id_group_no_dc == "MB-M-LRO1")

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
Cover_tbl <- imap(Cover_mask_l, \(mask_r, year){
  print(year)
  terra::extract(mask_r, Locs_1k, fun = mean, ID = FALSE) %>% # touches = TRUE,
    tibble() %>% 
    mutate(Id_muestreo_no_dc = Locs_1k$Id_muestreo_no_dc,
           Ano = as.numeric(year), 
           Scale_m = 1000) %>%
    rename(Canopy_cover = paste0("Colombia_WVCC_", year))
}) %>% list_rbind()

# >Height ------------------------------------------------------------------
# Buffer of 50m to extract the max canopy height of the point count 
Locs_50m <- Pc_locs %>% buffer(50) 
Ex_buff_50m <- Locs_50m %>% filter(Id_group_no_dc == "MB-M-LRO1")

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
Height_tbl <- imap(Height_mask_l, \(mask_r, year){
  print(year)
  terra::extract(mask_r, Locs_50m, fun = max, ID = FALSE) %>% # touches = TRUE,
    tibble() %>% 
    mutate(Id_muestreo_no_dc = Locs_50m$Id_muestreo_no_dc,
           Ano = as.numeric(year)) %>%
           #Scale_m = 50 %>%
    rename(Canopy_height_dm = paste0("Colombia_WVCH_", year))
}) %>% list_rbind() 
  
# Convert from decimeters to meters
Height_tbl2 <- Height_tbl %>% 
  mutate(Canopy_height_m = Canopy_height_dm / 10, 
         Ano = as.numeric(Ano)) %>% 
  select(-c(Canopy_height_dm))

## Join
# WSVC data only goes through 2024, so 2025/2026 surveys are matched to the 2024 canopy layer -- via a temporary join year so the real Ano column is never overwritten (an earlier version reset every Recent_pts row to 2025, which also clobbered the other-year visits that share an Id_muestreo)
Event_covs <- Event_covs_pcs %>%
  mutate(Ano_wvsc = ifelse(Ano %in% c(2025, 2026), 2024, Ano)) %>%
  left_join(Cover_tbl,   by = c("Id_muestreo_no_dc", "Ano_wvsc" = "Ano")) %>%
  left_join(Height_tbl2, by = c("Id_muestreo_no_dc", "Ano_wvsc" = "Ano")) %>%
  select(-any_of(c("Ano_wvsc", "Scale_m")))

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
    id_cols = Id_muestreo,
    cols_inc = -c(Registrado_por, Noise, Clima, Cows_50m)
  )

# Export ------------------------------------------------------------------
stop()
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