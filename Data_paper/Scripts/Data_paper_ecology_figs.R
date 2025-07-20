## Data paper Ecology figures ## 

# Load data & libraries
# Libraries
library(rnaturalearthdata)
library(rnaturalearth)
library(smoothr)
library(ggspatial)

load("Rdata/NE_layers_Colombia.Rdata")
load("Rdata/the_basics_07.18.25.Rdata")

source("/Users/aaronskinner/Library/CloudStorage/OneDrive-UBC/Grad_School/Rcookbook/Themes_funs.R")

# Fig1: Sampling map ------------------------------------------------------
### Create map showing point count locations on informative background (elevation)

# >Elevation background ---------------------------------------------------
# Elevation - _30s function provides elevation at a 1km resolution, which is fine for plotting but not great for extracting elevation for each record
Elev_1km <- geodata::elevation_30s(country = "Colombia", path = tempdir())
ColElev_df <- terra::as.data.frame(Elev_1km, xy = TRUE) %>% tibble()

# Generate elevation map of Colombia
Col_alt_map <- ggplot() +
  geom_raster(data = ColElev_df, aes(x = x, y = y, fill = COL_elv_msk)) +
  scale_fill_viridis_c(trans = "log") + # Notice log transformation puts more emphasis on lower elevation changes
  labs(
    x = "Longitude",
    y = "Latitude",
    fill = "Elevation"
  ) + guides(fill = "none") +
  theme(axis.title = element_blank())


# >Point formatting -------------------------------------------------------
## Point counts within 0.25 degree grid cells 
# With ~500 point counts in concentrated regions there is too much overlap to clearly visualize what is going on. Instead, I calculate the number of point counts within .25 degrees cells and return the rounded coordinates for plotting
Pc_locs_round <- Pc_locs %>%
  mutate(Latitud_rd = mround(Latitud, .25), 
         Longitud_rd = mround(Longitud, .25)) %>%
  count(Uniq_db, Ecoregion, Latitud_rd, Longitud_rd, sort = T) %>%
  st_as_sf(coords = c("Longitud_rd", "Latitud_rd"), crs = 4326, remove = FALSE)

## Piedemonte
# In Piedemonte there is tons of data and too much overlap, thus I subset Piedemonte points separately, count the number of rows at each set of (rounded) latitude & longitude coords, and jitter the points based on the amount of overlap
Pc_locs_meta <- Pc_locs_round %>% 
  filter(Ecoregion == "Piedemonte") %>% 
  mutate(dup_count = n(), .by = c(Latitud_rd, Longitud_rd))

# Jitter Piedemonte points by the number of duplicates (dup_count) within each 0.25 degree cell 
jitter_amt <- c(.2, .3, .5)
Pc_locs_meta2 <- Pc_locs_meta %>% 
  group_split(dup_count) %>% 
  map2(jitter_amt, \(locs, jam){
    st_jitter(locs, jam)
  }) %>% dplyr::bind_rows()

## Jitter points in all other regions
# In the remaining regions there is far less data, so a small amount of jitter is sufficient. However, the points in Bajo Magdalena and Rio Cesar often end up in the ocean which is problematic 

# Function to jitter points while ensuring all points stay within a boundary polygon
jitter_within_boundary <- function(sf_points, boundary_poly, jitter_factor, max_iter = 10) {
  inside <- rep(FALSE, nrow(sf_points))
  original <- sf_points
  accepted <- st_geometry(sf_points)
  iter <- 1
  
  while (!all(inside) && iter <= max_iter) {
    to_jitter <- which(!inside)
    
    # Propose jittered geometries from original points
    proposed <- st_jitter(sf_points[to_jitter, ], factor = jitter_factor)
    
    # TF vector depending on status inside or outside polygon
    is_inside <- st_within(proposed, boundary_poly, sparse = FALSE)[,1]
    
    # Accept new jittered locations only if they fall inside polygon
    accepted[to_jitter[is_inside]] <- st_geometry(proposed[is_inside, ])
    
    # Update inside status
    inside[to_jitter[is_inside]] <- TRUE
    iter <- iter + 1
  }
  sf_points$geometry <- accepted
  if (!all(inside)) warning("Some points still fell outside the boundary after jittering.")
  sf_points
}

# Apply minor jitter to all points outside of Piedemonte 
Pc_locs_gen <- Pc_locs_round %>% filter(Ecoregion != "Piedemonte") %>% 
  jitter_within_boundary(boundary_poly = neCol, jitter_factor = .02)

## Bind the Piedemonte points with points from all other regions
Pc_locs_jit <- bind_rows(Pc_locs_gen, Pc_locs_meta2)


# >Inset map --------------------------------------------------------------
## Plot inset map for biodiversity data on elevation map background.
# Extract bounding box
bbox_all <- st_bbox(Pc_locs_jit)

# Generate map
Col_alt_map + 
  geom_sf(data = Pc_locs_jit, 
          aes(shape = Uniq_db, size = n, alpha = desc(n))) +
  coord_sf(
    xlim = c(bbox_all[1], bbox_all[3]), ylim = c(bbox_all[2], bbox_all[4]), 
           label_axes = "____", expand = TRUE
    ) + annotation_scale(location = "bl") +
  scale_shape_discrete(
    #name = "Data collector", 
    labels = c(
      "CIPAV", "GAICA\ndistancia", "GAICA", "UBC & GAICA", "UBC", "Universidad de \nlos Llanos"),
    solid = FALSE
    ) +
  scale_size_continuous(range = c(4, 7)) +
  scale_alpha_continuous(range = c(.5, 1)) +
  guides(
    alpha = "none",
    size = guide_legend(title = "Number of \npoint counts"),
    shape = guide_legend(title = "Data collector")
  )
ggsave("Data_paper/Figures/Sampling_map.png", bg = "white")


# >South America map ------------------------------------------------------


