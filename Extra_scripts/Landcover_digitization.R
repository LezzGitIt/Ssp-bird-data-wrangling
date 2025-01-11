## PhD birds in silvopastoral landscapes##
# The purpose of this script is to generate files that collaborators will need to digitize landcover. This consists of 2 distinct but related efforts:
# Mathilde Manual Digitization & GIS work using satellite imagery in QGIS
# Diana Laura supervised classification using GEE 

# Final digitization files that Mathilde created can be found on Box: SCR Box -> Geodatabase -> Mathilde spatial

# Load libraries & data ---------------------------------------------------
library(tidyverse)
library(terra)
library(tidyterra)
library(sf)
library(cowplot)
library(conflicted)
ggplot2::theme_set(theme_cowplot())
conflicts_prefer(dplyr::select)
conflicts_prefer(dplyr::filter)

source("/Users/aaronskinner/Library/CloudStorage/OneDrive-UBC/Grad_School/Rcookbook/Themes_funs.R")
load("Rdata/the_basics_01.09.25.Rdata")

# Create files to aid in digitization -----------------------------------------------------
# Create spatial file of point counts -- note that there are multiple coordinates for a single point count in a few cases

# Add habitat to point counts and pivot_wider to have single row per PC (there are still multiple rows for some PCs b/c there are multiple coordinates in some cases)
Bird_pcs_hab_sf <- Bird_pcs %>%
  distinct(
    Id_muestreo, Id_group, Departamento, Uniq_db,
    Id_gcs, Nombre_finca_mixed, Nombre_finca, Habitat_og,
    Habitat_ut, Longitud_decimal, Latitud_decimal
  ) %>%
  group_by(Id_muestreo) %>% # Habitat_og, Habitat_ut
  mutate(Habitat = row_number()) %>% # Create numbers to add to columns
  pivot_wider(
    names_from = Habitat,
    values_from = c(Habitat_ut, Habitat_og)
  ) %>%
  ungroup() %>%
  st_as_sf(coords = c("Longitud_decimal", "Latitud_decimal"), crs = 4326)

# Combine Habitat_ut & Habitat_og into a single column with unite()
endings <- 1:7
endings_ <- c(paste0("_", 1:7))
for (i in 1:length(endings)) {
  print(i)
  Pc_locs_sf <- Bird_pcs_hab_sf %>%
    unite(
      col = "Hab_UT_OG",
      ends_with(endings_[i]), sep = "_"
    ) %>%
    rename_with(~ paste0("Hab_UT_OG", endings[i]), Hab_UT_OG)
}

Bird_pcs_hab_sf %>%
  distinct(Id_muestreo, Uniq_db) %>%
  nrow() - 54 # %>% count(Uniq_db)

# Buffers ---------------------------------------------------------------
# Create buffer sizes vector and prepare loop
# NOTE:: Even though in terra::buffer the argument is called 'width' it still refers to the radius
buffer_dists <- c(25, 50, 100, 300, 500, 1000, 5000)
buffers <- union <- v_union <- list()
area_km2 <- as.numeric()

# Create buffers of varying sizes to determine how much area would need to be digitized.. Time is limited so need to pick a reasonable buffer size for Mathilde
sf_use_s2(TRUE) # Otherwise buffer_dists gives weird error about arc degrees

# Function geobuffer_pts to make a geodesic buffer doesn't seem to work anymore in 2024. See github for previous versions of code as I've deleted several small things (including what is needed to make the st_union().

cents_sv <- Pc_locs_sf %>% vect() %>% # sv = spatvector
  project("epsg:4686")
for (i in 1:length(buffer_dists)) {
  print(i)
  buffers[[i]] <- cents_sv %>%
    buffer(width = buffer_dists[i]) %>%
    st_as_sf()
  # 2 options here.. Can combine overlapping polygons st_union -> (direct to, I think) st_cast, or have separate polygons for each Id_group buffers -> group_by -> summarize -> st_cast
  union[[i]] <- buffers[[i]] %>% # st_union() %>% #st_union merges all overlapping polygons
    group_by(Id_group) %>%
    summarize(geometry = st_union(geometry)) %>%
    st_cast("MULTIPOLYGON")
  v_union[[i]] <- st_make_valid(union[[i]])
  area_km2[[i]] <- sum(st_area(v_union[[i]])) / 1000000
}
warnings() # If 'polygon from first part only' this is important means it's only taking a single polygon when not overlapping

# Nested list of buffers #DELETE UNNECESSARY
buffersLL <- map(.x = list(buffers, union), .f = \(buffL)
setNames(buffL, paste0("b", buffer_dists)))

# Name buffers & unioned buffers
names(buffers) <- paste0("b", buffer_dists)
names(union) <- paste0("b", buffer_dists)

# Convert buffers & unioned buffers to vector and set crs to MAGNA-SIRGAS
buffers_ms <- map(buffers, .f = \(x) vect(x) %>% project("epsg:4686"))
union_ms <- map(union, .f = \(x) vect(x) %>% project("epsg:4686"))

# I think numbers have differed in terms of area at least in part b/c initially you did dissolve all polygons (this provides the accurate area)
buffers_df <- data.frame(buffer_dists, area_km2) %>%
  mutate(area3times = area_km2 * 3)
sapply(buffers_df, round, 2)

# >Export files ----------------------------------------------------------
# Create kmz & shapefiles of buffers + points. WILL WANT TO CHANGE PROJECTION
Buffers_cipav <- buffers_ms$b25 %>% filter(Uniq_db == "CIPAV MBD")

# Put items that need to be exported in a list
files.exp <- list(PC_Cents = cents_sv, Buffers_cipav = Buffers_cipav, Buffers_50m = buffers_ms$b50, Buffers_300m = union_ms$b300)
files.exp.sf <- map(files.exp, \(x) st_as_sf(x))

# Rename Centroids file & arrange so they're in order -- if get error may need to tibble and then rename
files.exp.sf$PC_Cents <- files.exp.sf$PC_Cents %>%
  # tibble() %>%
  rename(name = Id_muestreo) %>%
  arrange(name) # %>%
# st_as_sf()

dir <- "Intermediate_products_geospatial/"
filepath <- c("kml", "shp")
filetype <- c("kml", "ESRI Shapefile")

# Create folders if necessary
if (FALSE) {
map(names(files.exp.sf), \(x) dir.create(path = paste0(dir, "shp/", x)))
# Sf
map(names(files.exp.sf), \(name)
map2(
  .x = filepath, .y = filetype,
  .f = \(path, type) {
    path_shp <- paste0(dir, path, "/", name, "/", name, ".", path)
    path_kml <- paste0(dir, path, "/", name, ".", path)
    st_write(
      obj = files.exp.sf[[name]],
      dsn = if (path == "shp") path_shp else path_kml,
      driver = type,
      layer = name
    )
  }
))
}

# >Example farm figure ----------------------------------------------------
Bird_pcs %>%
  filter(Nombre_finca == "La herradura" | Id_gcs == 4121) %>%
  distinct(Id_gcs, Id_group, Nombre_finca, Nombre_finca_mixed, Latitud_decimal, Longitud_decimal)

Ex <- buffers[[4]] %>% filter(Uniq_db == "GAICA MBD" & Id_group == "G-MB-M-LH")
Ex_cents <- Pc_locs_sf %>% filter(Id_group == "G-MB-M-LH")
Ex500 <- Ex %>% st_union()
Ex500 %>% ggplot() +
  geom_sf() +
  geom_sf(data = Ex_cents) +
  ggspatial::annotation_scale(location = "br")


# >Metadata files --------------------------------------------------------
## Create file w/ appropriate dates for extraction of Planet imagery for Seth
Pc_date8 %>%
  distinct(
    Id_muestreo, Nombre_institucion,
    Uniq_db, Departamento, Nombre_finca, Ano, Mes
  ) %>%
  distinct(Departamento, Mes, Ano) %>%
  group_by(Departamento, Ano) %>%
  summarize(Month = round(mean(Mes), 0)) %>%
  mutate(
    Month = ifelse(Departamento == "Meta" & Ano == 2017, 10, Month),
    Month2 = ifelse(Month >= 3 & Month <= 6, 12, 5)
  ) %>%
  as.data.frame() # %>%
# write.xlsx("/Users/aaronskinner/Library/CloudStorage/OneDrive-UBC/Grad_School/PhD/Mentorship/Digitization_Mathilde/Digitization_Dep_month_year2.xlsx", row.names = F)

## Metadata file for Mathilde to fill out
Pc_date8 %>%
  mutate(Id_muestreo = str_split_i(Id_muestreo, "_", i = 1)) %>%
  group_by(Id_muestreo, Ano) %>%
  summarize(across(), Month.min = min(Mes), Month.max = max(Mes)) %>%
  distinct(
    Id_muestreo, Nombre_institucion,
    Uniq_db, Departamento, Nombre_finca, Ano, Month.min, Month.max
  ) %>%
  group_by(Id_muestreo) %>%
  group_modify(~ add_row(.x, Ano = 2023, .before = 0)) %>%
  mutate(Closest_date = NA, Source_closest = NA, N.photos = NA, Notes = NA, Questions = NA) %>%
  as.data.frame() # %>%
# write.xlsx("/Users/aaronskinner/Library/CloudStorage/OneDrive-UBC/Grad_School/PhD/Mentorship/Digitization_Mathilde/Digitization_metadata.xlsx", row.names = F)

# save.image("/Users/aaronskinner/Library/CloudStorage/OneDrive-UBC/Grad_School/R_Files/PhD/ColombiaPhD.Rdata")

# Laura Diana -------------------------------------------------------------
### Export files (points and buffers) for Diana Laura to do landcover classification

## Points
# Define function to pivot_wider based on sets of variables
Pivot_wider_grp <- function(distinct_vars, grp_vars) {
  Pc_date8 %>%
    distinct(across(all_of(distinct_vars))) %>%           
    group_by(across(all_of(grp_vars))) %>%                
    mutate(Ano_rank = row_number(Ano)) %>%                
    pivot_wider(
      id_cols = all_of(grp_vars),                         
      names_from = Ano_rank,                              
      values_from = Ano,                                  
      names_prefix = "Ano"                                
    ) %>%
    ungroup()
}

# Create date_join by Id_muestreo
Date_join_im <- Pivot_wider_grp(distinct_vars = c("Id_group", "Id_muestreo", "Ano", "Ecoregion"), 
                grp_vars = c("Id_group", "Id_muestreo", "Ecoregion"))

Points_export <- Pc_locs_sf %>% full_join(Date_join_im) %>% 
  distinct(Id_group, Id_muestreo, Ano1, Ano2, Ano3, Ecoregion, geometry) %>% 
  rename(Id_point = Id_muestreo, Region = Ecoregion)

## Buffers
# Create date_join by Id_group
Date_join_ig <- Pivot_wider_grp(
  distinct_vars = c("Id_group", "Ano", "Ecoregion"), 
  grp_vars = c("Id_group", "Ecoregion")
  )

# Define function to create buffers 
Buffer_grouped <- function(buff_dist = NULL){
  Pc_locs_sf %>% full_join(Date_join_ig) %>% 
    distinct(Id_muestreo, Id_group, Ecoregion, Ano1, Ano2, Ano3, geometry) %>% 
    st_buffer(buff_dist) %>%
    summarize(geometry = st_union(geometry), .by = -c(Id_muestreo, geometry)) %>%
    st_make_valid() %>% 
    filter(!st_is_empty(geometry)) %>% 
    st_cast("MULTIPOLYGON")
}

# Create 1.5 & 5km buffers
Buffers_group_1500 <- Buffer_grouped(1500)
Buffers_group_5000 <- Buffer_grouped(5000)

Export_files <- list(Points = Points_export,
                      Buffers_group_1500 = Buffers_group_1500, 
                      Buffers_group_5000 = Buffers_group_5000)

## Export points & buffers
if(FALSE){
  imap(Export_files, \(buffers, names){
    buffers %>% st_write(
      driver = "ESRI Shapefile",
      dsn = paste0("Intermediate_products_geospatial/Diana_laura_digitization/Aaron_", names, ".shp"),
      layer = paste0("Aaron_", names)
    ) 
  })
}

# Ensure file exported correctly 
files <- list.files("Intermediate_products_geospatial/Diana_laura_digitization", pattern = ".shp")
Exported_files <- st_read("Intermediate_products_geospatial/Diana_laura_digitization/Aaron_points.shp")

path <- "Intermediate_products_geospatial/Diana_laura_digitization/"
Exported_files <- map(files, \(shp){
  st_read(paste0(path, shp)) 
})
names(Exported_files) <- str_remove_all(files, ".shp")

## Plot examples
load("Rdata/NE_layers_Colombia.Rdata")
# At level of country
Exported_files$Aaron_Buffers_group_5000 %>% ggplot() + #Points_export
  geom_sf(data = neCol) +
  geom_sf(data = neColDepts, alpha = .5) +
  geom_sf(alpha = .2)

# A single 5km buffer 
Points_ex <- Exported_files$Aaron_points2%>% filter(Id_group == "G-MB-Q-PORT") 
Exported_files$Aaron_Buffers_group_5000 %>% filter(Id_group == "G-MB-Q-PORT") %>%
  ggplot() +
  geom_sf() + 
  geom_sf(data = Points_ex)
