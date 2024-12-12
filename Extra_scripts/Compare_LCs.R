## PhD birds in silvopastoral landscapes##
## Compare landcovers to external files
## Purpose:: To compare the files that Mathilde digitized to other publicly available spatial layers

# NOTE:: Have not gone in to & updated this, or reran to ensure code works currently. Am waiting until I have final product from Mathilde's files.

# Tree cover from John Brandt: https://www.sciencedirect.com/science/article/pii/S0034425723001256
# Mathilde Manual Digitization files can be found on Box: SCR Box -> Geodatabase -> Mathilde spatial

## Contents
# 1) Buffering points (individual trees) & lines (live fences)
# 2)
# 3)
# 4)
# 5)
# 6)

# Load libraries & data ---------------------------------------------------
library(terra)
library(tidyterra)
library(sf)
library(tidyverse)
library(janitor)
library(cowplot)
library(gridExtra)
library(conflicted)
ggplot2::theme_set(theme_cowplot())
conflicts_prefer(dplyr::select)
conflicts_prefer(dplyr::filter)

source("/Users/aaronskinner/Library/CloudStorage/OneDrive-UBC/Grad_School/Rcookbook/Themes_funs.R")
load("Rdata/the_basics_11.21.24.Rdata")

# Load files -------------------------------------------------------------
# Bring in files that Mathilde digitized & Brandt's tree cover rasters
files.shp <- list.files(
  path = "../Geospatial/Mathilde",
  pattern = c("2020_shapefile|indivtrees")
)

names(files.shp) <- c("indiv_trees", "LCs2020", "livefences2020")

# Import shapefile landcovers & store in list , project livefences
shp.lc.L <- map(.x = files.shp, \(shp)
                vect(paste0("../Geospatial/Mathilde/", shp)) %>%
                  clean_names())

shp.lc.L$livefences2020 <- project(shp.lc.L$livefences2020, "epsg:4686") # DELETE

files.tif <- list.files(path = "../Geospatial/Mathilde/treecover_clip")
# files.tif <- list.files(path = paste0(path, "/treecover_clip"))

Brandt.treeL <- map(.x = files.tif, .f = \(tif)
                    rast(paste0("../Geospatial/Mathilde/treecover_clip/", tif)))
# Brandt.treeL <- map(.x = files.tif, .f = \(tif)
# rast(paste0(path, "/treecover_clip/", tif)))

names(Brandt.treeL) <- map_chr(str_split(files.tif, "_"), 3)

# Visualize LCs, trees, live fences, & 300m buffers
bbox <- terra::ext(shp.lc.L$LCs2020)
ggplot() +
  geom_spatvector(data = shp.lc.L$LCs2020, aes(fill = lc), alpha = .4) +
  # geom_sf(data = union$b300, alpha = .3) +
  geom_spatvector(data = shp.lc.L$livefences2020, alpha = .3) +
  geom_spatvector(data = shp.lc.L$indiv_trees, size = .5, alpha = .3) +
  coord_sf(xlim = bbox[c(1, 2)], ylim = bbox[c(3, 4)]) +
  theme(legend.position = "none")

# Prep vectors -----------------------------------------------------------------
# Individual trees file is across all 5 ecoregions -- For now we're interested in just the trees in Meta & that will be compared against the tree cover raster
shp.lc.L$indiv_trees2 <- crop(shp.lc.L$indiv_trees, shp.lc.L$LCs2020)

# Buffer trees & live fences (lf) by appropriate distances.
# Discuss buffers with Mathilde - "S" for shrub should be 2m radius (NOT diameter), "T" should be 5m wide, and "F" should be ~Xm radius, indiv trees should be radius 5m at low elevation & 3m at high elevation? See Mathilde's metadata file
trees_lf_buff <- pmap(
  .l = list(shp.lc.L[c(3, 4)], buf.rad = c(5, 7)),
  .f = \(shpL, buf.rad) buffer(shpL, buf.rad)
) # buffer_radii

# There must be dataframe attached for bind_spat_rows() to work!
trees_lf_buff$indiv_trees2$lc_typ <- "distrees"
trees_lf_buff$livefences2020$lc_typ <- "livefence"

# Cut out the buffered trees & live fences from the LC polygons
LCs_holes <- shp.lc.L$LCs2020 %>%
  makeValid() %>%
  terra::erase(trees_lf_buff$indiv_trees2) %>%
  terra::erase(trees_lf_buff$livefences2020)

## PROBLEM SOLVE
## OK, there is an issue here where there are only 461 geometries (and rows), yet all the rows contain 465 entries. This happens with creation of LCs_holes
nrow(LCs_holes)
lapply(LCs_holes, length)
LCs_holes$lc_typ
LCs_holes[468, ]

# Some geometries not valid
table(is.valid(shp.lc.L$LCs2020))

# I don't think this is causing the issue b/c when you rerun with test data frame of valid geometries you get the same issue
TF <- shp.lc.L$LCs2020 %>% is.valid()
test <- shp.lc.L$LCs2020 %>% filter(TF)
test %>%
  terra::erase(trees_lf_buff$indiv_trees2) %>%
  terra::erase(trees_lf_buff$livefences2020)

ggplot() +
  geom_spatvector(data = trees_lf_buff$livefences2020, alpha = .4) +
  # geom_spatvector(data = union_ms$b100, alpha = .3) +
  # geom_sf(data = PC_locsSf, size = .5) +
  coord_sf(xlim = c(-73.683, -73.666), ylim = c(3.323, 3.343))

## END PROBLEM SOLVE

# After buffering there may be overlap between individual trees & live fences, but there shouldn't be. I arbitrarily chose to cut out individual trees using the live fences. Could switch this, likely these will all end up as SSP
trees_lf_buff$indiv_trees3 <- erase(trees_lf_buff$indiv_trees2, trees_lf_buff$livefences2020)

# Combine layers & visualize an example
LCs_comb <- rbind(
  LCs_holes, trees_lf_buff$livefences2020,
  trees_lf_buff$indiv_trees3
) %>%
  tidyterra::select(-id)

# CHECK number of rows
461 + 1066 + 160

# Visualize an example buffer
ggplot() +
  geom_spatvector(data = LCs_comb, aes(fill = lc), alpha = .4) +
  geom_spatvector(data = union_ms$b100, alpha = .3) + # Add buffers
  geom_sf(data = PC_locsSf, size = .5) + # Add centroids
  coord_sf(xlim = c(-73.683, -73.666), ylim = c(3.323, 3.343))

# Id_group ----------------------------------------------------------------
## Append the Id_group of each polygon to the spatvector data frame##
# relate() produces a matrix where column 1 is the Id # of the LC polygons & column 2 is the Id number of the buffer in union b300.
ints.df <- LCs_comb %>%
  relate(union_ms$b300, "intersects", pairs = TRUE) %>%
  data.frame() %>%
  rename_with(~ c("id.lc", "id.buff")) %>%
  tibble()
# NOTE:: There are some digitized polygons that are contained in multiple buffers
ints.df %>% count(id.lc, sort = T)

# Easy solution for now, just take the first Id_group, otherwise will need multiple columns for each Id_group.
# Kind of confusing b/c the 2020 file is formatted differently than the other files will be... Could also figure out which years were sampled when & select Id_group based on year somehow (think about this in more detail with Mathilde).
id.buffs <- ints.df %>%
  slice_head(by = id.lc) %>%
  pull(id.buff)
LCs_comb$Id_group <- union_ms$b300[id.buffs, ]$Id_group
unique(LCs_comb$Id_group)

# Visualize the different Id_groups
# NOTE: The 3 of these overlap , examine with Mathilde
LCs_comb %>%
  filter(Id_group %in% c("G-AD-M-CO", "G-MB-M-EA", "G-AD-M-LP")) %>%
  ggplot() +
  geom_spatvector(aes(fill = Id_group), alpha = .4)

# Prep Brandt tree raster -----------------------------------------------
Brandt.sprc <- sprc(Brandt.treeL)
## mosaic() function creates a single spatial raster where all the cells outside of the tiles are NA. Also see svc() function which is much faster, but all cells outside of tiles are 0.
Brandt.mosaic <- mosaic(Brandt.sprc)
# This is way faster than using the project() function
Brandt.mosaic.ms <- Brandt.mosaic # ms = MAGNA-SIRGAS
crs(Brandt.mosaic.ms) <- "epsg:4686"

# Load spatial layers for plotting
load("/Users/aaronskinner/Library/CloudStorage/OneDrive-UBC/Grad_School/R_Files/PhD/NE_layers_Colombia.Rdata")

# Visualize, Note gray cells are NA
ggplot() +
  geom_sf(data = neCol, fill = NA) +
  geom_spatraster(data = Brandt.mosaic) +
  scale_fill_continuous(na.value = "transparent")
terra::extract(Brandt.mosaic, data.frame(x = -75, y = 7.5))

# Crop & mask Brandt tree cover using Mathilde's digitized files
Brandt.crop <- crop(Brandt.mosaic.ms, LCs_comb, mask = TRUE, touches = FALSE)

# Ensure that the files match up spatially
ggplot() +
  geom_spatraster(data = Brandt.crop) +
  geom_sf(data = LCs_comb, alpha = .4) + # , aes(fill = lc)
  theme(legend.position = "none") +
  coord_sf(xlim = c(-73.683, -73.666), ylim = c(3.323, 3.343))

# Calc tree cover area of LCs  -------------------------------------------------------
# Inspect columns
map(as.data.frame(LCs_comb), unique)

# Calculate the tree cover, where units are 'effective number of hectares', or the total area multiplied by the approximate density of trees (proportion) for that LC type (many self-defined)
LCs_comb_a <- LCs_comb %>%
  mutate(
    area = expanse(LCs_comb, unit = "ha"),
    trees_prop = trees_percent / 100,
    tree_cover_ha = case_when( #' effective number of treed hectares'
      lc == "distrees" & is.na(trees_prop) ~ area * mean(trees_prop, na.rm = TRUE), # .69
      lc == "distrees" & !is.na(trees_prop) ~ area * trees_prop,
      crops_type %in% c("palm", "fruit", "tree") ~ area * .7,
      lc == "forest" ~ area * 0.85,
      lc == "livefence" ~ area * 0.6,
      lc == "sylvopast" ~ area * 0.8,
      lc == "shrub" ~ area * 0.1,
      .default = area * 0
    )
  )

# Note areas are almost identical when crop(touches = FALSE)
expanse(Brandt.crop, unit = "ha")
sum(LCs_comb_a$area)

# NOTE:: Better to aggregate second because aggregate does not weigh the 'trees percent' column by the size of the polygon if you set fun = mean. Instead calculate the 'effective number of treed hectares' for each polygon & then sum them in the aggregate call.
# ALTERNATIVE:: Could probably just avoid aggregating at all for these calculations...
# To add 'crops_type' to aggregate call I think would need to paste lc & cropstype (e.g., crops_fruit). Doesn't really matter too much as there are very few crop polygons that are trees
LCs_comb %>%
  distinct(Id_group, lc, crops_type) %>%
  data.frame()
# The agg_tree_cover_ha is equal to the 'effective number of treed hectares'
LCs_agg_ID_lc <- aggregate(LCs_comb_a,
                           by = c("Id_group", "lc"), # "crops_type"
                           fun = function(x) {
                             sum(x, na.rm = TRUE)
                           }
)

# Visualize just the predominant landcovers to make legend easier to read
LCs_agg_ID_lc %>%
  filter(lc %in% c("intpast", "forest", "crops", "distrees", "livefence", "shrub")) %>%
  ggplot() +
  geom_spatvector(aes(fill = lc), alpha = .4) +
  coord_sf(xlim = c(-73.683, -73.666), ylim = c(3.323, 3.343))

# Inspect results
LCs_comb_a %>%
  data.frame() %>%
  dplyr::select(Id_group, lc, area, trees_prop, tree_cover_ha) %>%
  mutate(across(where(is.numeric), \(x) round(x, digits = 2))) %>%
  # filter(!is.na(crops_type)) %>%
  head(n = 30)

# LC areas by category
LCs_comb_a %>%
  as.data.frame() %>%
  group_by(lc) %>% # Id_group
  summarize(sum.area.ha = sum(area)) %>%
  arrange(desc(sum.area.ha))

# Determine the mean tree cover in each LC category.. Mostly makes sense! But note that water has incorrect high average of tree cover
# Function extract likely more flexible than zonal as it allows for anonymous functions
LCs_agg_lc <- aggregate(LCs_comb_a, by = "lc")
Brandt_tree_cov_lc_prop <- zonal(
  x = Brandt.crop / 100, z = LCs_agg_lc,
  fun = mean, na.rm = TRUE
)[[1]]

# Would be nice to add a measure of variance but I'm not positive that I've calculated this correctly.. I think what's going on is there are some LC types that are highly right-skewed (e.g., lots of values near 0 but a few extreme values near 1) that are inflating sd?
sd <- zonal(
  x = Brandt.crop / 100, z = LCs_agg_lc,
  fun = sd, na.rm = TRUE
)[[1]]


data.frame(LC = LCs_agg_lc$lc, Brandt_tree_cov_lc_prop) %>% # sd
  arrange(desc(Brandt_tree_cov_lc_prop)) %>%
  tibble()

## Calculate summed areas of tree cover from Mathilde's polygons for each ID
Tree_cov_M <- LCs_agg_ID_lc %>%
  as.data.frame() %>%
  group_by(Id_group) %>%
  summarize(
    tree.cover.Mathilde = sum(agg_tree_cover_ha),
    area.ha = sum(agg_area)
  ) %>%
  mutate(prop.tree.cov.Mathilde = tree.cover.Mathilde / area.ha) %>%
  arrange(desc(prop.tree.cov.Mathilde))

# Dissolve polygons in LCs_comb_a so there is a single polygon for each Id_group, which can then be used as the "zones" for zonal() function to calculate the prop of tree cover in each Id_group
LCs_agg_ID <- aggregate(LCs_comb_a, by = "Id_group")
prop.tree.cover.Brandt <- zonal(
  x = Brandt.crop / 100, z = LCs_agg_ID,
  fun = "mean", na.rm = TRUE
)[[1]]

# Create data frame with tree cover estimates from Brandt & Mathilde's digitized polygons
tree_cover_df <- data.frame(Id_group = LCs_agg_ID$Id_group, prop.tree.cover.Brandt) %>%
  full_join(Tree_cov_M[, c("Id_group", "prop.tree.cov.Mathilde", "area.ha")],
            by = "Id_group"
  ) %>%
  tibble() %>%
  arrange(prop.tree.cover.Brandt)
tree_cover_df

# Calculate correlation -------------------------------------------------
# NOTE:: It would be important to do this at additional, and potentially many spatial scales (e.g., landscape scale, 1km). Could apply a 1km grid over the study area
cor(tree_cover_df$prop.tree.cover.Brandt, tree_cover_df$prop.tree.cov.Mathilde, method = "pearson")
# 90% seems high but when inspected visually you can see that they are mostly lock step
tree_cover_df %>% # mutate(across(where(is.numeric), scale)) %>%
  arrange(prop.tree.cover.Brandt) %>%
  mutate(row_n = row_number()) %>%
  pivot_longer(
    cols = c(prop.tree.cover.Brandt, prop.tree.cov.Mathilde),
    values_to = "tree_cover",
    names_to = "Estimate_source"
  ) %>%
  ggplot(aes(x = row_n, y = tree_cover)) +
  geom_line(aes(color = Estimate_source)) #+
# ggrepel::geom_text_repel(aes(label = Id_group)) #+
# theme(legend.position = "none")

# The biggest difference is not too worrisome as it is an artifact of the weird Id_groups & small area (3.13 ha)
tree_cover_df %>% filter(Id_group == "G-AD-M-LCA1")

# Mostly important b/c of mosaic() function which takes forever to run!
# save.image("Rdata/Manual_dig_2.20.24.Rdata")

# Misc --------------------------------------------------------------------
# I don't understand the holes part of all this.. Note that adding the live fences & indiv trees does increase the area by ~40 ha which suggests that this worked as expected
sum(expanse(LCs_holes, unit = "ha"))
sum(expanse(LCs_comb, unit = "ha"))
sum(expanse(LCs_agg_ID_lc, unit = "ha"))
# Yet there are a few hectares that are being identified as holes
sum(expanse(holes_agg, unit = "ha"))
sum(expanse(holes_erase, unit = "ha"))

# Named based on the functions that created the input (aggregate vs erase)
holes_bind <- fillHoles(LCs_comb, inverse = TRUE)
holes_agg <- fillHoles(LCs_agg_ID, inverse = TRUE)
holes_erase <- fillHoles(LCs_holes, inverse = TRUE)

sum(expanse(holes_erase, unit = "ha"))

# Note when you erase from the LC file that it DOES reduce the area, suggesting that these polygons did exist.. Something weird in the fillHoles function but I think the landcover files are good! Would need to talk to someone more knowledgable than I !
sum(expanse(erase(LCs_agg_ID_lc, holes_agg), unit = "ha"))

# Visualize just the holes
holes_erase %>% # filter(Id_group == "G-AD-M-CO") %>%
  ggplot() +
  geom_spatvector(fill = "red") +
  # geom_spatvector(data = LCs_agg) +
  coord_sf(xlim = c(-73.683, -73.666), ylim = c(3.323, 3.343))

# Changing the alpha value you can see that there aren't really holes
# Holes agg - Would not expect there to be holes
map() # Could map through all IDs with a new coord_sf to actually visualize the holes
LCs_agg_ID_lc %>% # filter(Id_group == "G-AD-M-CO") %>%
  ggplot() + # geom_spatvector(data = holes_agg, fill = "red") +
  geom_spatvector(aes(fill = lc), alpha = 1) + # 1
  coord_sf(xlim = c(-73.683, -73.666), ylim = c(3.323, 3.343)) +
  ggtitle("Holes aggregate")
# Holes erase - Would expect there to be holes , but even this map doesn't make sense
ggplot() +
  geom_spatvector(data = holes_erase, fill = "red") +
  geom_spatvector(data = LCs_holes, aes(fill = lc), alpha = .2) + # LC
  coord_sf(xlim = c(-73.683, -73.666), ylim = c(3.323, 3.343)) +
  ggtitle("Holes erase")

# If you want to visualize specific polygons and label them can convert to sf() and use geom_text_repel
LCs_agg_ID_lc %>% filter(Id_group == "G-MB-M-EA") %>% # & lc == "intpast"
  # arrange(desc(area)) %>%
  st_as_sf() %>%
  # mutate(row_n = row_number()) %>% #If don't want to print actual size of polygons..
  mutate_if(is.numeric, round, 2) %>%
  # filter(row_n < 5) %>%
  ggplot() +
  geom_sf(aes(fill = lc)) +
  ggrepel::geom_text_repel(aes(label = agg_tree_cover_ha, geometry = geometry),
                           stat = "sf_coordinates"
  )
