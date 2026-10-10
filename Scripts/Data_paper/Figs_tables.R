# Builds the data-paper figures for Figures/ and the Tables 6-9 column metadata (Rdata/Cols_metadata_l.rds) that the manuscript embeds; source this before rendering the qmd.

# Load libraries -------------------------------------------------
# Libraries
library(readxl)
library(tidyverse)
library(sf)
library(hms)
library(ggpubr)
library(cowplot)
library(ggrepel)
library(rnaturalearthdata)
library(rnaturalearth)
library(geodata)
library(smoothr)
library(terra)
library(tidyterra)
library(ggspatial)
library(conflicted)
library(patchwork)
ggplot2::theme_set(theme_cowplot())
conflicts_prefer(dplyr::lag)
conflicts_prefer(hms::hms)
conflicts_prefer(dplyr::filter)

# Load data  --------------------------------------------------------------
load("Rdata/NE_layers_Colombia.Rdata")
#load("Rdata/the_basics_07.18.25.Rdata")

Pc_locs_sf <- st_read("Derived/Geospatial/shp/Pc_locs.gpkg")
Pc_locs_dc_sf <- st_read("Derived/Geospatial/shp/Pc_locs_dc.gpkg")

# Load in raw abundance data
Bird_pcs_all <- read_csv("DataS1/Bird_pcs_all.csv")
Bird_pcs_analysis <- read_csv("DataS1/Bird_pcs_analysis.csv")
Taxonomy <- read_csv(file = "DataS1/Taxonomy.csv")
Fn_traits <- read_csv(file = "DataS1/Functional_traits.csv")
Site_covs <- read_csv(file = "DataS1/Site_covs.csv")
Event_covs <- read_csv(file = "DataS1/Event_covs.csv")
Prec_df <- read_csv(file = "Derived/Excels/Prec_df.csv")

source("/Users/aaronskinner/Library/CloudStorage/OneDrive-UBC/Academia/Rcookbook/Themes_funs.R")
source("Scripts/Data_paper/Data_paper_fns.R")

# Every figure shows the ecoregions by their display names (Eje Cafetero, Río Cesar, ...), so convert them once here; code below filters on display names
Pc_locs_sf <- Pc_locs_sf %>% mutate(Ecoregion = ecoregion_label(Ecoregion))
Pc_locs_dc_sf <- Pc_locs_dc_sf %>% mutate(Ecoregion = ecoregion_label(Ecoregion))
Site_covs <- Site_covs %>% mutate(Ecoregion = ecoregion_label(Ecoregion))
Prec_df <- Prec_df %>% mutate(Ecoregion = ecoregion_label(Ecoregion))

# Fig1: Sampling map ------------------------------------------------------
### Create map showing point count locations on informative background (elevation)
Envi_path <- "../Geospatial_data/Environmental"

# >Elevation background ---------------------------------------------------
# Elevation - _30s function provides elevation at a 1km resolution, which is fine for plotting but not great for extracting elevation for each record
Elev_1km <- geodata::elevation_30s(country = "Colombia", path = Envi_path)
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

# >Precip background -------------------------------------------------------

Prec_col <- worldclim_country(country = "COL", var = "prec", path = Envi_path)
Tot_prec <- sum(Prec_col)

## Colombia precip map 
Col_prec_map <- ggplot() + 
  geom_spatraster(data = Tot_prec) + 
  labs(
    x = "Longitude",
    y = "Latitude"
  ) + guides(fill = guide_legend(title = "Precipitation (mm)")) +
  theme(axis.title = element_blank()) 

# >Rivers -----------------------------------------------------------------
# Download rivers if needed
#ne_rivers <- ne_download(scale = 10, type = "rivers_lake_centerlines", category = "physical", returnclass = "sf")

## Río Cesar is below Natural Earth's scale-10 cutoff, so build it from the OSM HOT Colombia waterways export (Data/hotosm_col_waterways_osm_gpkg/; gitignored local input, re-bundle for the deposit). Downstream of the Ciénaga de Zapatosa OSM maps the river only as a riverbank polygon, so trace that polygon's centreline and join it to the main stem, which carries the river to the Magdalena.
Osm_waterways <- "Data/hotosm_col_waterways_osm_gpkg/waterways.gpkg"
Cesar_wkt <- "POLYGON((-74.1 8.9, -72.9 8.9, -72.9 10.95, -74.1 10.95, -74.1 8.9))"
# OSM leaves two reaches of the main stem unnamed (9.66-9.76 and 9.82-10.00 N); these ways fill them, checked by plotting against the named segments
Cesar_unnamed_ways <- c("way/418073379", "way/418073374", "way/418073361", "way/418073367", "way/871539527", "way/1032651206", "way/1032651205")
Cesar_osm <- st_read(Osm_waterways, query = "SELECT id, name, waterway, geom FROM waterways", wkt_filter = Cesar_wkt, quiet = TRUE) %>%
  filter(stringi::stri_trans_general(name, "Latin-ASCII") %>% tolower() %in% "rio cesar" | id %in% Cesar_unnamed_ways)

# Centreline of a north-south running riverbank polygon: the midpoint of the polygon's extent in each thin latitude band
polygon_centreline <- function(poly, band_deg = 0.005) {
  bb <- st_bbox(poly)
  lats <- seq(bb["ymin"] + band_deg / 2, bb["ymax"] - band_deg / 2, by = band_deg)
  mids <- map(lats, \(lat) {
    cut <- st_intersection(st_geometry(poly), st_linestring(rbind(c(bb["xmin"] - 1, lat), c(bb["xmax"] + 1, lat))) %>% st_sfc(crs = st_crs(poly)))
    if (length(cut) == 0) return(NULL)
    xs <- st_coordinates(cut)[, "X"]
    c(mean(range(xs)), lat)
  }) %>% compact() %>% do.call(rbind, .)
  st_linestring(mids[order(-mids[, 2]), ])
}
sf_use_s2(FALSE) # planar intersection of the latitude bands
Cesar_main <- Cesar_osm %>% filter(waterway %in% "river") %>% st_union() %>% st_line_merge()
Cesar_lower <- Cesar_osm %>% filter(st_geometry_type(.) %in% c("POLYGON", "MULTIPOLYGON")) %>% st_union() %>% polygon_centreline()
sf_use_s2(TRUE)
# Join the main stem's southern end (Ciénaga de Zapatosa) to the head of the lower reach
Main_coords <- st_coordinates(Cesar_main)[, c("X", "Y")]
Main_south <- Main_coords[which.min(Main_coords[, "Y"]), ]
Lower_coords <- st_coordinates(Cesar_lower)[, c("X", "Y")]
Cesar_link <- st_linestring(rbind(Main_south, Lower_coords[1, ]))
Rio_cesar <- st_sf(name = "Río Cesar",
                   geometry = st_sfc(st_union(c(st_geometry(Cesar_main), st_sfc(Cesar_link, Cesar_lower, crs = 4326)))) %>%
                     st_transform(st_crs(rivers_co)))
rivers_co <- bind_rows(rivers_co, Rio_cesar)

# River labels: each sits where its river crosses a chosen latitude (the crossing nearest near_x, since a river can cross a latitude more than once), nudged by dx / dy degrees so the text clears the line. Natural Earth names the Meta tributary "Guainía", but the river drawn there is the Ariari (Ariari + Guayabero form the Guaviare)
label_on_river <- function(rivers, river_name, lat, near_x, dx = 0, dy = 0) {
  # Planar intersection with a short parallel: under spherical geometry (s2) a long east-west line is a great circle that bows away from the latitude
  s2_was_on <- sf_use_s2(FALSE)
  on.exit(suppressMessages(sf_use_s2(s2_was_on)))
  line <- st_geometry(filter(rivers, name == river_name)) %>% st_union()
  parallel <- st_sfc(st_linestring(rbind(c(near_x - 2, lat), c(near_x + 2, lat))), crs = st_crs(rivers))
  xs <- suppressMessages(st_intersection(line, parallel)) %>% st_coordinates() %>% .[, "X"]
  tibble(x = xs[which.min(abs(xs - near_x))] + dx, y = lat + dy)
}
river_labels <- tribble(
  ~river,      ~label,      ~lat,  ~near_x, ~dx,   ~dy,
  "Magdalena", "Magdalena",  5.6,  -74.6,   0.42,  0,
  "Cauca",     "Cauca",      6.9,  -75.4,  -0.28,  0,
  "Guainía",   "Ariari",     3.77, -74.2,   0,    -0.17,
  "Meta",      "Meta",       4.45, -73.9,   0.3,   0
) %>%
  mutate(pos = pmap(list(river, lat, near_x, dx, dy), \(r, l, nx, x, y) label_on_river(rivers_co, r, l, nx, x, y))) %>%
  unnest(pos) %>%
  st_as_sf(coords = c("x", "y"), crs = st_crs(rivers_co)) %>%
  select(name = label)

# The Cesar label is hard to read over the valley's shading, so it sits on the grey of Venezuela with a leader line from the label's left edge to the river
Cesar_callout <- label_on_river(rivers_co, "Río Cesar", lat = 9.7, near_x = -73.6) %>%
  rename(xend = x, yend = y) %>%
  mutate(x = -73.08, y = 9.42, name = "Cesar")

# >Point formatting -------------------------------------------------------
## Point counts within grid cells
# With ~500 point counts in concentrated regions there is too much overlap to clearly visualize what is going on. Instead, count the point count locations of each data collector within grid cells (0.25 degrees on the main map, finer in the Piedemonte zoom panel) and plot each count at its cell centre
count_in_cells <- function(locs, cell_deg) {
  locs %>%
    st_drop_geometry() %>%
    mutate(Latitud_rd = mround(Latitud, cell_deg),
           Longitud_rd = mround(Longitud, cell_deg)) %>%
    count(Uniq_db, Ecoregion, Latitud_rd, Longitud_rd, sort = TRUE)
}
Pc_locs_round <- count_in_cells(Pc_locs_dc_sf, cell_deg = 0.25)

## Separate data collectors that share a grid cell
# Several data collectors often surveyed the same 0.25 degree cell (up to five in the Piedemonte), so their symbols would sit on top of each other. Rather than a random jitter, which in the Piedemonte moved symbols up to ~50 km from where sampling happened, place the collectors sharing a cell evenly on a small ring around the cell centre. The offset is deterministic and never leaves the cell.
dodge_in_cell <- function(cell_points, ring_radius_deg) {
  cell_points %>%
    mutate(
      n_in_cell = n(),
      angle = 2 * pi * (row_number() - 1) / n_in_cell + pi / 2,
      Longitud_plot = Longitud_rd + if_else(n_in_cell > 1, ring_radius_deg * cos(angle), 0),
      Latitud_plot  = Latitud_rd  + if_else(n_in_cell > 1, ring_radius_deg * sin(angle), 0),
      .by = c(Latitud_rd, Longitud_rd)
    )
}
Pc_locs_jit <- Pc_locs_round %>%
  arrange(Latitud_rd, Longitud_rd, Uniq_db) %>%
  dodge_in_cell(ring_radius_deg = 0.1) %>%
  st_as_sf(coords = c("Longitud_plot", "Latitud_plot"), crs = 4326, remove = FALSE)

# >Sampling map -----------------------------------------------------------
## Main map: elevation background, ecoregion outlines, rivers, and point count locations; an inset locates it in northern South America. Built entirely in R (it replaces the figure assembled by hand in PowerPoint).

# Ecoregions are drawn as the convex hull of each ecoregion's plotted point count locations, buffered so the symbols sit inside. Ecoregions are formally groups of departments (Table 2), but whole departments are large and adjacent, so their outlines hid where sampling actually was; the hull matches the per-ecoregion area reported in Table 2
Ecor_buffer_m <- 15000
# The Piedemonte is shown in a zoom panel rather than on the main map, so its true locations stand in for its plotted ones
Pc_locs_pdm <- Pc_locs_dc_sf %>% filter(Ecoregion == "Piedemonte")
# The Piedemonte's outline on the main map is the zoom-panel rectangle, so it gets no hull
Ecor_polys <- bind_rows(
  Pc_locs_jit %>% filter(Ecoregion != "Piedemonte") %>% select(Ecoregion),
  Pc_locs_pdm %>% select(Ecoregion) %>% rename(geometry = geom)
) %>%
  filter(Ecoregion != "Piedemonte") %>%
  group_by(Ecoregion) %>%
  summarise(.groups = "drop") %>%
  st_convex_hull() %>%
  st_transform(32618) %>%
  st_buffer(Ecor_buffer_m) %>%
  st_transform(4326)

# Ecoregion labels, hand-placed just outside each hull
Ecor_labels <- tibble(
  Ecoregion = c("Eje Cafetero", "Cordillera Oriental", "Piedemonte", "Bajo Magdalena", "Río Cesar"),
  x = c(-75.75, -73.0, -73.95, -75.2, -72.45),
  y = c(5.4, 6.62, 4.12, 11.2, 11.05)
)

# Neighbouring countries, for the grey land around Colombia and for the inset
Countries <- rnaturalearth::ne_countries(scale = 50, returnclass = "sf")

# Main map extent: the sampling locations plus a margin, wide enough to reach the Caribbean coast
Main_xlim <- c(-77.6, -71.0)
Main_ylim <- c(2.4, 11.9)

# Elevation (m) on a log scale, which spreads out the colours at low elevations where most sampling is; cells at or below sea level are floored at 1 m so the log is defined
ColElev_df <- ColElev_df %>% mutate(Elev_m = pmax(COL_elv_msk, 1))
Elev_breaks <- c(10, 100, 1000, 3000)

# Point count locations: size = locations per 0.25 degree cell (per data collector); hollow symbol shape = data collector (Uniq_db)
Collector_labels <- c("Cipav mbd" = "CIPAV", "Gaica distancia" = "GAICA distancia", "Gaica mbd" = "GAICA",
                      "Ubc gaica mbd" = "UBC & GAICA", "Ubc mbd" = "UBC", "Unillanos mbd" = "Unillanos")
Collector_shapes <- setNames(c(1, 0, 5, 2, 6, 4), names(Collector_labels)) # circle, square, diamond, triangle up, triangle down, cross
# Named values + limits keep each collector's symbol fixed and list all six in the legend, including those drawn only in the Piedemonte zoom panel
Collector_shape_scale <- function(...) scale_shape_manual(values = Collector_shapes, limits = names(Collector_labels), labels = Collector_labels, ...)

# Piedemonte cells for the zoom panel: 0.05 degrees (~5.5 km), with collectors sharing a cell dodged on a ring well inside the cell
Pdm_cells <- count_in_cells(Pc_locs_pdm, cell_deg = 0.05) %>%
  arrange(Latitud_rd, Longitud_rd, Uniq_db) %>%
  dodge_in_cell(ring_radius_deg = 0.02) %>%
  st_as_sf(coords = c("Longitud_plot", "Latitud_plot"), crs = 4326, remove = FALSE)

# One size scale for the main map and the zoom panel, so a symbol of a given size means the same number of locations in both
Size_max <- max(Pc_locs_jit$n, Pdm_cells$n)
Size_scale <- scale_size_area(max_size = 7, limits = c(0, Size_max), breaks = keep(c(5, 20, 40, 80), \(b) b <= Size_max),
                              name = "Point count\nlocations")

# The extent (coord_sf) comes after every geom_sf layer, since a geom_sf layer added after coord_sf() resets it
Sampling_map <- ggplot() +
  geom_sf(data = Countries, fill = "grey88", colour = "grey60", linewidth = 0.3) +
  geom_raster(data = ColElev_df, aes(x = x, y = y, fill = Elev_m)) +
  scale_fill_viridis_c(trans = "log10", breaks = Elev_breaks, labels = scales::label_comma(), name = "Elevation (m)") +
  geom_sf(data = Ecor_polys, fill = NA, colour = "black", linewidth = 0.5, linetype = "dashed") +
  geom_sf(data = rivers_co, colour = "#1f5fbf", linewidth = 0.45) +
  geom_sf_text(data = river_labels, aes(label = name), colour = "#1f5fbf", size = 3, fontface = "italic") +
  geom_segment(data = Cesar_callout, aes(x = x - 0.04, y = y, xend = xend, yend = yend), colour = "#1f5fbf", linewidth = 0.35) +
  geom_text(data = Cesar_callout, aes(x = x, y = y, label = name), colour = "#1f5fbf", size = 3, fontface = "italic", hjust = 0) +
  geom_sf(data = filter(Pc_locs_jit, Ecoregion != "Piedemonte"), aes(size = n, shape = Uniq_db), colour = "black", stroke = 0.7) +
  # Invisible copy of the Piedemonte locations: ggplot only draws legend keys for values present in a layer, and two collectors appear only in the zoom panel
  geom_sf(data = Pc_locs_pdm, aes(shape = Uniq_db), alpha = 0, show.legend = c(shape = TRUE, size = FALSE)) +
  Collector_shape_scale(name = "Data collector") +
  Size_scale +
  guides(shape = guide_legend(override.aes = list(size = 2.5, alpha = 1, stroke = 0.7)),
         size = guide_legend(override.aes = list(shape = 1))) +
  geom_label(data = Ecor_labels, aes(x = x, y = y, label = Ecoregion), size = 2.6, fontface = "bold",
             fill = "white", label.size = 0.3, label.padding = unit(0.12, "lines")) +
  annotation_scale(location = "bl", width_hint = 0.25, text_cex = 0.7) +
  annotation_north_arrow(location = "tl", height = unit(0.8, "cm"), width = unit(0.6, "cm"),
                         style = north_arrow_fancy_orienteering(text_size = 7)) +
  coord_sf(xlim = Main_xlim, ylim = Main_ylim, expand = FALSE) +
  labs(x = NULL, y = NULL) +
  theme(panel.background = element_rect(fill = "#dceaf5"), # sea
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text = element_text(size = 7),
        legend.title = element_text(size = 8), legend.text = element_text(size = 7),
        legend.key.height = unit(0.5, "cm"),
        legend.key.spacing.y = unit(0.02, "cm"),
        legend.justification = c(0, 0)) # legends at the bottom of the right column, leaving its top for the inset

# Piedemonte zoom panel: point count locations per data collector in 0.05 degree cells, on the main map's size scale, drawn in the main map's empty bottom-right corner and joined to the Piedemonte box
Pdm_bbox <- st_bbox(Pdm_cells)
Zoom_xlim <- c(Pdm_bbox[["xmin"]], Pdm_bbox[["xmax"]]) + c(-0.05, 0.05)
Zoom_ylim <- c(Pdm_bbox[["ymin"]], Pdm_bbox[["ymax"]]) + c(-0.05, 0.05)
Piedemonte_zoom <- ggplot() +
  geom_raster(data = filter(ColElev_df, between(x, Zoom_xlim[1] - 0.1, Zoom_xlim[2] + 0.1), between(y, Zoom_ylim[1] - 0.1, Zoom_ylim[2] + 0.1)),
              aes(x = x, y = y, fill = Elev_m)) +
  scale_fill_viridis_c(trans = "log10", limits = range(ColElev_df$Elev_m), guide = "none") +
  geom_sf(data = rivers_co, colour = "#1f5fbf", linewidth = 0.45) +
  geom_sf(data = Pdm_cells, aes(shape = Uniq_db, size = n), colour = "black", stroke = 0.6) +
  Collector_shape_scale(guide = "none") +
  Size_scale + guides(size = "none") +
  annotation_scale(location = "bl", width_hint = 0.4, text_cex = 0.55, height = unit(0.12, "cm"), pad_x = unit(0.08, "cm"), pad_y = unit(0.08, "cm")) +
  coord_sf(xlim = Zoom_xlim, ylim = Zoom_ylim, expand = FALSE) +
  theme_void() +
  theme(panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.6),
        plot.background = element_blank())
# Panel position in main-map degrees: anchor it to the bottom-right corner and enlarge the zoom extent as far as the empty corner allows (west to Zoom_room_x, north to Zoom_room_y, clear of the Cordillera Oriental outline)
Zoom_room_x <- -73.05
Zoom_room_y <- 5.35
Zoom_box <- c(xmax = Main_xlim[2] - 0.08, ymin = Main_ylim[1] + 0.08)
Zoom_scale <- min((Zoom_box[["xmax"]] - Zoom_room_x) / diff(Zoom_xlim), (Zoom_room_y - Zoom_box[["ymin"]]) / diff(Zoom_ylim))
Zoom_box[c("xmin", "ymax")] <- c(Zoom_box[["xmax"]] - Zoom_scale * diff(Zoom_xlim), Zoom_box[["ymin"]] + Zoom_scale * diff(Zoom_ylim))
# Leader lines from the zoomed area's right-hand corners to the panel's left-hand corners
Zoom_leaders <- tibble(x = Zoom_xlim[2], y = Zoom_ylim, xend = Zoom_box[["xmin"]], yend = c(Zoom_box[["ymin"]], Zoom_box[["ymax"]]))
Sampling_map <- Sampling_map +
  annotate("rect", xmin = Zoom_xlim[1], xmax = Zoom_xlim[2], ymin = Zoom_ylim[1], ymax = Zoom_ylim[2], fill = NA, colour = "black", linewidth = 0.4) +
  geom_segment(data = Zoom_leaders, aes(x = x, y = y, xend = xend, yend = yend), colour = "black", linewidth = 0.3) +
  annotation_custom(ggplotGrob(Piedemonte_zoom), xmin = Zoom_box[["xmin"]], xmax = Zoom_box[["xmax"]], ymin = Zoom_box[["ymin"]], ymax = Zoom_box[["ymax"]]) +
  coord_sf(xlim = Main_xlim, ylim = Main_ylim, expand = FALSE)

# Inset: northern South America with country borders, Colombia shaded, and the main map's extent boxed; no coordinates
Sampling_inset <- ggplot() +
  geom_sf(data = Countries, fill = "grey92", colour = "grey45", linewidth = 0.25) +
  geom_sf(data = filter(Countries, adm0_a3 == "COL"), fill = "grey55", colour = "black", linewidth = 0.4) +
  annotate("rect", xmin = Main_xlim[1], xmax = Main_xlim[2], ymin = Main_ylim[1], ymax = Main_ylim[2],
           fill = NA, colour = "red", linewidth = 0.5) +
  coord_sf(xlim = c(-80, -60), ylim = c(-10, 13), expand = FALSE) +
  theme_void() +
  theme(panel.background = element_rect(fill = "#dceaf5"),
        panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5))

# Combine: the inset sits at the top of the legend column, outside the map panel so it hides no sampling. Saved at the manuscript's text width (6.5 in) so font and symbol sizes are what the reader sees
Sampling_map_full <- ggdraw(Sampling_map) +
  draw_plot(Sampling_inset, x = 0.785, y = 0.75, width = 0.205, height = 0.235)
ggsave("Figures/Sampling_map.png", Sampling_map_full, bg = "white", width = 6.5, height = 7, dpi = 300)
print(Sampling_map_full)

# >inset prec map ------------------------------------------------------
Col_prec_map + 
  geom_sf(data = Pc_locs_jit, color = "white",
          aes(shape = Uniq_db, size = n, alpha = desc(n))) +
  # Rivers
  geom_sf(data = rivers_co, color = "blue") +
  geom_sf_text(data = river_labels, aes(label = name), color = "#1f5fbf", size = 4) +
  coord_sf(
    xlim = Main_xlim, ylim = Main_ylim,
    label_axes = "____", expand = TRUE
  ) + annotation_scale(location = "bl") +
  scale_shape_discrete(
    #name = "Data collector", 
    labels = c(
      "CIPAV", "GAICA\ndistancia", "GAICA", "UBC & GAICA", "UBC", "Universidad de \nlos Llanos"),
    solid = FALSE
  ) +
  scale_size_continuous(range = c(3, 7)) +
  scale_alpha_continuous(range = c(.5, 1)) +
  guides(
    alpha = "none",
    size = guide_legend(title = "Number of \npoint counts"),
    shape = guide_legend(title = "Data collector")
  )

# Fig2: Temporal distribution of sampling plot ------------------------------
# Boxplots showing the temporal distribution of sampling in each ecoregion 

# Create the 'Grp_spat' variable that shows which point counts are surveyed at the same spatial location, which is especially important for Meta
Es_covs <- Event_covs %>% left_join(Site_covs)

Meta_PCs_related <- Es_covs %>%
  filter(Department == "Meta" & Uniq_db == "Gaica mbd") %>%
  distinct(Id_survey, Year) %>% # head()
  group_by(Id_survey) %>%
  mutate(Visit = paste0("Visit", row_number())) %>%
  pivot_wider(names_from = Visit, values_from = Year) %>%
  mutate(Grp_spat = case_when( # Spatial group
    Visit1 == 2016 & Visit2 == 2017 ~ "G1617" # GAICA 2016-2017 is one group
  ))

Pc_date9 <- Es_covs %>%
  left_join(Meta_PCs_related[, c("Id_survey", "Grp_spat")],
            by = "Id_survey"
  ) %>%
  mutate(Grp_spat = case_when( # Spatial group
    Grp_spat == "G1617" ~ "G1617",
    Uniq_db == "Gaica distancia" ~ "Distancia",
    Uniq_db == "Cipav mbd" & Year == 2016 ~ "CIPAV1",
    Uniq_db == "Cipav mbd" & Year == 2017 ~ "CIPAV2",
    Uniq_db == "Unillanos mbd" ~ "UniL_UBC",
    TRUE ~ "Other"
  )) %>%
  # One specific case for CIPAV
  mutate(Grp_spat = ifelse(Uniq_db == "Cipav mbd" & Year == 17 & Ecoregion == "Cordillera Oriental" & Month == 4, "CIPAV1", Grp_spat)) %>%
  # UBC's solo resurveys (2022 Unillanos, 2025 El Hatico, 2026 Meta) inherit the shape of whoever first surveyed that location, so the resurvey series reads as one set of points; locations UBC surveyed for the first time (e.g. the newer El Hatico points) get their own group rather than folding into "Other"
  mutate(Grp_spat = {
    orig <- Grp_spat[Uniq_db != "Ubc mbd" & !is.na(Grp_spat)]
    if (length(orig)) first(orig)
    else if (all(Uniq_db == "Ubc mbd")) "UBC_new"
    else Grp_spat
  }, .by = Id_survey_no_dc)

# Reduce # of rows to increase readability of plot
Pc_date_p <- Pc_date9 %>% distinct( #PC_date_plot
  Institution_name, Grp_spat, Ecoregion, Year, Month, Day, N_samp_periods
) %>% 
  mutate(Year = str_remove(Year, "20")) #%>% 
 #Add a random Ecoregion so it doesn't add a 6th 'NA' panel
 #add_row(Year = as.character(25), Ecoregion = "Eje Cafetero") 

# Plot 
Pc_temporal_plot <- ggplot(data = Pc_date_p, aes(x = factor(Year), y = Month)) +
  geom_boxplot() +
  geom_jitter(
    data = filter(Pc_date_p, Institution_name != "Cipav"), size = 3, width = 0.3, alpha = .5, aes(color = Institution_name, shape = Grp_spat)
  ) +
  # Graph CIPAV on top of other points
  geom_jitter(
    data = filter(Pc_date_p, Institution_name == "Cipav"), size = 3, width = 0.3, alpha = .6, aes(color = Institution_name, shape = Grp_spat)
  ) +
  facet_wrap(~Ecoregion) +
  scale_y_continuous(breaks = seq(0, 12, by = 3)) +
  labs(
    x = "Year", y = "Month",
    size = "Number of distinct \n sampling periods", color = "Data collector",
  ) +
  scale_shape_manual(values = 0:8) +   # one per Grp_spat level (legend hidden below)
  # Keep the viridis hues for the other collectors, but swap Unillanos off the pale-yellow end (invisible on the white panel) for the Okabe-Ito amber -- colourblind-safe and the conventional warm companion to viridis
  scale_color_manual(values = c(Cipav = "#440154", Gaica = "#3B528B", Ubc = "#21908C", `Ubc gaica` = "#5DC863", Unillanos = "#E69F00")) +
  guides(shape = "none") +
  theme(legend.position = c(0.8, 0.2),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 22)
        )
        #legend.key.size = unit(x = c(1,.5), units = "cm")  
Pc_temporal_plot

# Save plot
ggsave("Figures/Pc_month_year_day_ecoregion.png", bg = "white", width = 12)

# Fig3: Environmental vars histogram --------------------------------------
# Boxplots for Elevation, temp, & precipitation
p <- list()
var_names <- c("Elev", "Avg_temp", "Tot_prec")
# Units sit in the panel titles, so the y axes need no label
title <- c("Elevation (m)", "Temperature (°C)", "Precipitation (mm)")

for (i in c(1:3)) {
  print(i)
  p[[i]] <- Site_covs %>%
    group_by(Ecoregion) %>%
    ggplot(aes(
      x = fct_reorder(Ecoregion, Elev, .fun = median),
      y = !!sym(var_names[i])
    )) +
    geom_boxplot(alpha = 1.0, outliers = FALSE, aes(color = Ecoregion)) +
    geom_jitter(alpha = 0.2) +
    labs(y = NULL, title = title[i], color = "Ecoregion") +
    theme(
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank()
    )
  # guides(color = FALSE) +
  # scale_x_discrete(labels = ecoreg_labs)
}
# The boxplots are combined with the rainfall curves (panel D) into one climate figure, built after the rainfall section below


# Fig4a: Rainfall density plota -------------------------------------------
var_names <- c("Elev", "Avg_temp", "Tot_prec")

# Correlation matrix -- Notice temp & elevation perfectly inverse correlated
Site_covs %>%
  select(all_of(var_names)) %>%
  cor() %>%
  data.frame() %>%
  mutate(across(everything(), round, 2))

# Rainfall seasonality plot: 30-year monthly averages at every point count location

Calc_mean_prec <- function(df, group_variable){
  df %>%
    group_by({{ group_variable }}) %>%
    summarize_if(is.numeric, mean) %>%
    pivot_longer(cols = starts_with("prec"), 
                 names_to = "Month", values_to = "Prec") %>%
    mutate(Month = as.numeric(str_remove(Month, "prec_")))
}
#Per Ecoregion
Prec_ecor <- Calc_mean_prec(df = Prec_df, group_variable = Ecoregion)
#Per department
Prec_depts <- Calc_mean_prec(df = Prec_df, group_variable = Department)

## Plot precip for all ecoregions using smoothed GAM
# cc = cyclic cubic regression spline - Use because the function value at month 12 is constrained to join smoothly back to month 1.
# k = the basis dimension, i.e. the maximal degrees of freedom. This allows the smoother to be as wiggly as one wiggle per month.
Prec_smooth_plot <- ggplot(Prec_ecor, aes(x = Month, y = Prec, color = Ecoregion)) +
  stat_smooth(method = "gam", formula = y ~ s(x, bs = "cc", k = 12), se = FALSE) +
  scale_x_continuous(breaks = c(0, 2, 4, 6, 8, 10, 12)) +
  labs(x = "Month", y = NULL, title = "Monthly precipitation (mm)") +
  guides(color = "none") # the ecoregion legend comes from the boxplots, whose colours match (guides(), not theme(), so the figure-wide legend position below cannot re-enable it)

## Climate figure: elevation (A) and annual precipitation (B) by ecoregion on top, monthly rainfall curves (C) full width below; one shared ecoregion legend. Temperature is not shown: across the locations it is almost perfectly determined by elevation (r = -0.995), so the manuscript caption reports it instead
Climate_fig <- (p[[1]] | p[[3]]) / Prec_smooth_plot +
  plot_layout(guides = "collect", heights = c(1, 0.85)) +
  plot_annotation(tag_levels = "A") &
  theme(legend.position = "bottom", plot.tag = element_text(face = "bold"))
ggsave("Figures/Envi_climate.png", Climate_fig, bg = "white", width = 10, height = 8.5)
print(Climate_fig)

# Fig4b: Precipitation with sampling dates ---------------------------------
# PCs_prec are the points that go on the rainfall seasonality plot
Pcs_prec <- Es_covs %>% 
  distinct(Uniq_db, Ecoregion, Year, Month) %>%
  left_join(Prec_ecor, by = c("Ecoregion", "Month")) %>%
  arrange(Ecoregion, Year, Month) %>%
  mutate(min = Month - 2, max = Month + 2, .by = c(Uniq_db, Ecoregion, Year)) %>% 
  # Create variable 'GrpTemp' that is TRUE when a given point count location is sampled in the same year and has the mean fecha julian within the specified tolerance of the other survey dates
  mutate(GrpTemp = case_when( # GrpTemp = Group together temporally?
    lead(Month) >= min & lead(Month) <= max ~ paste0("TRUE", Month),
    lag(Month) >= min & lag(Month) <= max ~ paste0("TRUE", lag(Month)),
    TRUE ~ "FALSE"
  )) %>%
  # Manually change one issue
  mutate(GrpTemp = ifelse(Ecoregion == "Piedemonte" & Year == 19 & GrpTemp == "TRUE9", "TRUE10", GrpTemp
  )) %>% summarize(Prec = mean(Prec), Mes_mod = mean(Month), 
                   .by = c(Uniq_db, Ecoregion, Year, GrpTemp))

## Plot precipitation for ecoregions 

# Create general function that can facet plot by region, and to emphasize different things 
Plot_prec_samp <- function(regions = "All", dyn_occ = FALSE, facet = TRUE){
  if(!identical(regions, "All")){
    Pcs_prec <- Pcs_prec %>% filter(Ecoregion %in% regions)
    Prec_ecor <- Prec_ecor %>% filter(Ecoregion %in% regions)
  }
  
  if(dyn_occ == TRUE){
    Pcs_prec <- Pcs_prec %>%
      filter(!Uniq_db %in% c("Gaica distancia", "Cipav mbd"))
    Plot_prec <- Prec_ecor %>%
      ggplot(aes(color = Ecoregion)) + 
      geom_jitter(data = Pcs_prec, size = 4,
                  aes(x = Mes_mod, y = Prec, shape = factor(Year)))
  } else {
    Plot_prec <- Prec_ecor %>%
      ggplot() +
      geom_jitter(data = Pcs_prec, size = 6, alpha = .5, 
                  aes(x = Mes_mod, y = Prec, 
                      shape = factor(Year), color = Uniq_db)) + 
      guides(color = guide_legend(title = "Data collector"))
  }
  
  Plot_prec2 <- Plot_prec + 
    geom_line(data = Prec_ecor, aes(x = Month, y = Prec)) +
    scale_x_continuous(breaks = c(0, 2, 4, 6, 8, 10, 12, 14)) +
    labs(
      x = "Month", y = "Precipitation (mm)",
      title = "30-year average rainfall by department"
    ) +
    guides(shape = guide_legend(title = "Year")) +
    scale_shape_manual(values = 0:14)   # one shape per survey year -- 9 years as of 2026, headroom for more 
  
  if(facet == TRUE){
    Plot_prec2 <- Plot_prec2 + facet_wrap(~Ecoregion)
  }
  
  return(Plot_prec2)
}


# Plot regions with potential for dynamic occupancy modeling. This plot goes into Powerpoint and then draw arrows to connect sets of points 
# NOTE: Would likely want to remove Bajo Magdalena
Plot_prec_samp(regions = c("Piedemonte", "Bajo Magdalena", "Eje Cafetero"), dyn_occ = TRUE, facet = FALSE)
ggsave("Figures/Rainfall/Prec_sampling.png", bg = "white")

# Faceted plot, with all data collectors and all regions shown 
Plot_prec_samp(regions = "All", dyn_occ = FALSE, facet = TRUE)
ggsave("Figures/Rainfall/Prec_sampling_faceted.png", bg = "white")

# Fig 5 -------------------------------------------------------
## Bar plots of species with the highest counts and observed at the greatest number of unique point count locations (i.e., localities)

# >Data wrangling ---------------------------------------------------------
# Summarize counts and localities across Species & ecoregion
Species_summary <- Bird_pcs_all %>% 
  left_join(Site_covs) %>%
  summarize(Count = sum(as.numeric(Count), na.rm = TRUE), 
            Localities = n_distinct(Id_survey_no_dc),
            .by = c(Species_ayerbe, Ecoregion))

# Custom function to generate the total number of counts or point count locations irrespective of Ecoregion 
select_top_species <- function(df, Summary_var, slice_n) {
  df %>%
    summarize("Total_{{Summary_var}}" := sum({{ Summary_var }}), 
              .by = Species_ayerbe) %>%
    slice_max(across(starts_with("Total_")), n = slice_n, with_ties = FALSE)
}

# Generate tibbles of 30 species with highest total counts or localities to use in semi_join
Sj_count <- Species_summary %>% 
  select_top_species(Summary_var = Count, slice_n = 30) %>% 
  arrange(Total_Count)
Sj_local <- Species_summary %>% 
  select_top_species(Summary_var = Localities, slice_n = 30)

# Examine the overlap between highest counts and number of locations
Loc_10 <- Sj_local %>% semi_join(Sj_count) %>% 
  slice_max(order_by = Total_Localities, n = 10) %>% 
  mutate(order = row_number())
Count_10 <- Sj_count %>% semi_join(Sj_local) %>% 
  slice_max(order_by = Total_Count, n = 10) %>% 
  mutate(order = row_number())

#For reporting in manuscript - which species are both high abundance and widespread? 
full_join(Count_10, Loc_10, by = "Species_ayerbe") %>% 
  mutate(Order_both = order.x + order.y) %>% 
  arrange(Order_both) %>% 
  filter(!is.na(Order_both)) %>% 
  pull(Species_ayerbe)

# >Create plot ------------------------------------------------------------
# left plot: Counts
p1 <- Species_summary %>%
  semi_join(Sj_count, by = "Species_ayerbe") %>%
  mutate(Species_ayerbe = factor(
    Species_ayerbe, levels = Sj_count$Species_ayerbe
  )) %>%
  ggplot(aes(x = Count, y = Species_ayerbe,
             fill = Ecoregion)) +
  geom_col(color = "black", linewidth = 0.2, width = .8) +
  labs(x = "Total count", y = "Species") +
  theme(legend.position = "none")

# right plot: Unique point count locations (localities)
p2 <- Species_summary %>%
  semi_join(Sj_local, by = "Species_ayerbe") %>%
  mutate(Species_ayerbe = factor(
    Species_ayerbe, levels = Sj_local$Species_ayerbe
  )) %>%
  ggplot(aes(x = Localities, y = Species_ayerbe,
             fill = Ecoregion)) +
  geom_col(color = "black", linewidth = 0.2, width = .8) +
  scale_x_reverse() +
  scale_y_discrete(
    labels = Sj_local$Species_ayerbe, # right-side species
    position = "right"       # put them on the right
  ) +
  labs(x = "Localities", y = NULL) +
  theme(axis.text.y.left  = element_blank(),
        axis.ticks.y.left = element_blank(),
        axis.title.y.left = element_blank())

# Combine plots side by side and use a common legend
# Saved at its printed size (full text width, short enough to share a page with the phylogeny), so the font sizes below are what the reader sees
Species_counts_p <- (p1 + p2) +
  plot_layout(guides = "collect") &
  theme(legend.position = "top",
        text = element_text(size = 7), axis.text = element_text(size = 6),
        legend.key.size = unit(0.3, "cm")) &
  guides(fill = guide_legend(nrow = 1))

ggsave("Figures/Species_counts_localities.png", Species_counts_p,
       bg = "white", width = 6.5, height = 3.6, dpi = 300)
print(Species_counts_p)

# Fig: Example landscape + silvopasture illustrations ------------------
## (A) aerial view of a surveyed Piedemonte farm; (B) the two silvopastoral arrangements it shows, drawn by the SCR project (Proyecto Ganadería Colombiana Sostenible). Inputs (both tracked): Figures/Static/Example_landscape/Live_fences_SCR.jpeg (the project's live-fences poster panel, extracted unchanged from its slide deck) and Figures/Static/Example_landscape/Dispersed_trees_adapted.png (the project's live-fences drawing with the fence-line trees and wires removed and green trees scattered over the pasture, edited by hand once; tracked).

# Crop a project poster panel to its farm diorama (the vertical window the slide shows), trimming the frame at the sides
crop_diorama <- function(img, top = 0.39, bottom = 0.645, side = 0.07) {
  info <- magick::image_info(img)
  magick::image_crop(img, magick::geometry_area(round(info$width * (1 - 2 * side)), round(info$height * (bottom - top)),
                                                round(info$width * side), round(info$height * top)))
}

# Replace a poster panel's coloured, leaf-patterned background with white: a pixel is background if it is saturated and close in hue to the panel's corner colour, and it is whitened only if that region touches the image border (so small same-hue details inside the farm survive)
whiten_background <- function(img, hue_tol = 30, min_sat = 0.2) {
  arr <- as.integer(magick::image_data(img, "rgb"))
  dims <- dim(arr)[1:2]
  hsv_px <- rgb2hsv(t(matrix(arr, ncol = 3)))
  hue <- hsv_px["h", ] * 360
  corners <- c(1, dims[1], prod(dims) - dims[1] + 1, prod(dims))
  bg_hue <- median(hue[corners])
  hue_dist <- pmin(abs(hue - bg_hue), 360 - abs(hue - bg_hue))
  is_bg <- matrix(hue_dist < hue_tol & hsv_px["s", ] > min_sat, nrow = dims[1])
  # Flood fill from every border pixel through background pixels
  keep <- matrix(FALSE, nrow = dims[1], ncol = dims[2])
  frontier <- which(is_bg & (row(is_bg) %in% c(1, dims[1]) | col(is_bg) %in% c(1, dims[2])))
  keep[frontier] <- TRUE
  while (length(frontier)) {
    r <- (frontier - 1) %% dims[1] + 1; cl <- (frontier - 1) %/% dims[1] + 1
    nb <- c(frontier[r > 1] - 1, frontier[r < dims[1]] + 1, frontier[cl > 1] - dims[1], frontier[cl < dims[2]] + dims[1])
    nb <- unique(nb[is_bg[nb] & !keep[nb]])
    keep[nb] <- TRUE; frontier <- nb
  }
  for (k in 1:3) arr[, , k][keep] <- 255L
  magick::image_read(arr / 255)
}

Ssp_illus <- list(
  "Live fences"     = crop_diorama(magick::image_read("Figures/Static/Example_landscape/Live_fences_SCR.jpeg")),
  "Dispersed trees" = magick::image_read("Figures/Static/Example_landscape/Dispersed_trees_adapted.png")
)

# Layout: the aerial view on the left; the two illustrations stacked on the right, labels on their outer sides and a dashed divider between them, so each word clearly belongs to one picture
Aerial <- magick::image_read("Figures/Static/Example_landscape/Example_landscape.png")
Aerial_h <- magick::image_info(Aerial)$height
Label_h <- 110; Gap_px <- 30
Panel_h <- (Aerial_h - Gap_px) / 2 - Label_h
Illus <- map(Ssp_illus, \(im) im %>%
  magick::image_scale(paste0("x", round(Panel_h))) %>%
  whiten_background() %>%
  magick::image_trim(fuzz = 2) %>%
  magick::image_border("white", "20x25"))
Col_w <- max(map_int(Illus, \(x) magick::image_info(x)$width))
make_label <- function(lab) {
  magick::image_blank(Col_w, Label_h, "white") %>%
    magick::image_annotate(lab, size = 48, gravity = "center", font = "Helvetica", color = "black")
}
Divider <- magick::image_graph(width = Col_w, height = Gap_px, bg = "white")
grid::grid.lines(x = c(0.03, 0.97), y = 0.5, gp = grid::gpar(lty = "22", lwd = 3, col = "grey35"))
dev.off()
Illus_col <- magick::image_append(c(make_label(names(Illus)[1]), Illus[[1]], Divider, Illus[[2]], make_label(names(Illus)[2])), stack = TRUE)
# Pad the column to the aerial's height, centred
Col_h <- magick::image_info(Illus_col)$height
Pad_top <- (Aerial_h - Col_h) %/% 2
Illus_col <- magick::image_append(c(magick::image_blank(Col_w, Pad_top, "white"), Illus_col,
                                    magick::image_blank(Col_w, Aerial_h - Col_h - Pad_top, "white")), stack = TRUE)
# Panel letters
Aerial <- magick::image_annotate(Aerial, "A", size = 110, gravity = "northwest", location = "+25+10", color = "white", font = "Helvetica-Bold")
Illus_col <- magick::image_annotate(Illus_col, "B", size = 110, gravity = "northwest", location = "+0+0", color = "black", font = "Helvetica-Bold")
Landscape_fig <- magick::image_append(c(Aerial, magick::image_blank(Gap_px, Aerial_h, "white"), Illus_col))
# Flatten to 8-bit RGB on white: xelatex renders a 16-bit RGBA PNG (what image_read() of a numeric array produces) as a blank space
Landscape_fig <- Landscape_fig %>% magick::image_background("white") %>% magick::image_flatten()
magick::image_write(Landscape_fig, "Figures/Example_landscape_ssp.png", format = "png", depth = 8, density = 300)
print(Landscape_fig)

# Supplementary figs ------------------------------------------------------
## Plot showing numer of point counts per farm, the number of farms each data collector surveyed, and the average number of times each point count was repeated within a season (< 80 days)

# Number of pc per farm
Farm_counts <- Event_covs %>% 
  left_join(Site_covs) %>%
  distinct(Id_scr, Uniq_db, Id_survey) %>%
  count(Id_scr, Uniq_db) %>%
  count(Uniq_db, name = "N_farms")

Db_summ <- Event_covs %>% 
  mutate(Max_rep_season = max(Rep_season), 
         .by = Id_survey) %>%
  summarize(Mean_rep_season = mean(Max_rep_season),
            .by = Uniq_db) %>% 
  mutate(Mean_rep_season = as.factor(round(Mean_rep_season, 0))) %>% 
  full_join(Farm_counts)

# Number of point counts per farm and database
Num_pcs_farm_db <- Event_covs %>% 
  left_join(Site_covs) %>%
  distinct(Id_scr, Uniq_db, Id_survey) %>%
  count(Id_scr, Uniq_db, sort = T)
## Generate labels for plot, where eaach label is the number of farms surveyed
# Adjust the location of the label for UniLlanos & UBC 
x_loc <- summarize(Num_pcs_farm_db, x_loc = max(n) + 1, .by = Uniq_db) %>% 
  arrange(desc(x_loc)) %>%
  mutate(x_loc = x_loc - c(rep(7, 2), rep(0,4)))
label_data <- Farm_counts %>% 
  left_join(x_loc)

# Plot
Pc_per_farm_db_p <- Num_pcs_farm_db %>%
  full_join(Db_summ) %>%
  ggplot(aes(x = n, y = Uniq_db, color = Mean_rep_season)) +
  geom_boxplot(outliers = FALSE) +
  geom_jitter(alpha = .4, width = 0.01) +
  labs(x = "Point counts per farm",
       y = "Data set",
       color = "Repeat surveys \nper point count") +
  theme(legend.position = "top") +
  geom_text(
    data = label_data,
    aes(x = x_loc,
        y = Uniq_db,
        label = paste("N =", N_farms)),
    inherit.aes = FALSE,
    hjust = 0
  )
#quants <- quantile(Pc_per_farm$n, probs = c(0, .1, .9, 1))

# Six boxplot rows: save wide and short so the figure sits inline rather than floating onto its own page.
ggsave("Figures/Pc_per_farm_db.png", Pc_per_farm_db_p, bg = "white", width = 9, height = 5)
print(Pc_per_farm_db_p)

# Data sets ---------------------------------------------------------------
# >Metadata tbls -----------------------------------------------------------
### Summarize one column's contents for the metadata tables, as a single string.
## Numeric / date / time columns give "min – max"; logical columns give both values; a short categorical gives its sorted level list; anything longer (including identifier / name columns, e.g. the Taxonomy crosswalk) gives "<n> distinct" plus a missing count when relevant -- for those the full level list lives in the xlsx 'Column_content_definition'.
summarize_values <- function(col_data){
  non_na <- col_data[!is.na(col_data)]
  if (length(non_na) == 0) return(NA_character_)
  if (inherits(col_data, "hms")) {
    # range() drops the hms class, so take the numeric span and rebuild the clock string
    return(paste(as.character(hms::as_hms(range(as.numeric(non_na)))), collapse = " – "))
  }
  if (inherits(col_data, "Date") || inherits(col_data, "POSIXct")) {
    return(paste(as.character(range(non_na)), collapse = " – "))
  }
  if (is.numeric(col_data)) {
    return(paste(round(range(non_na), 2), collapse = " – "))
  }
  if (is.logical(col_data)) return("FALSE, TRUE")
  categories <- sort(unique(as.character(non_na)))
  n_missing <- sum(is.na(col_data))
  if (length(categories) <= 8 && sum(nchar(categories)) <= 60) {
    paste(categories, collapse = ", ")
  } else {
    paste0(length(categories), " distinct",
           if (n_missing > 0) paste0(", ", n_missing, " missing") else "")
  }
}

## Column-by-column metadata for one dataset: field name, data type, and contents.
extract_metadata <- function(df){
  map_dfr(names(df), function(col_name) {
    col_data <- df[[col_name]]
    tibble(
      Field_name = col_name,
      Data_type = class(col_data)[1],
      Values = summarize_values(col_data)
    )
  })
}

### Create metadata tbls
## Primary point-count file
Bird_pcs_all_meta <- Bird_pcs_all %>% extract_metadata()

## Site covariates
Site_covs_meta <- extract_metadata(read_csv("DataS1/Site_covs.csv", show_col_types = FALSE)) # the deposit as written (Site_covs above carries display names for the figures)

## Taxonomy file
Taxonomy_meta <- extract_metadata(Taxonomy)

## Functional traits
Fn_traits_meta <- Fn_traits %>% extract_metadata() 

## Event covariates 
Event_covs_meta <- Event_covs %>% extract_metadata() 

## Create column metadata list
Cols_metadata_l <- list(Bird_pcs_all = Bird_pcs_all_meta, Site_covs = Site_covs_meta, Event_covs = Event_covs_meta, Taxonomy = Taxonomy_meta, Functional_traits = Fn_traits_meta)

# >Export  ------------------------------------------------------

# Save the metadata list for each Excel included in repository
saveRDS(Cols_metadata_l, file = "Rdata/Cols_metadata_l.rds") 

stop()

# EXTRAS ------------------------------------------------------------------
# >Spatial maps -----------------------------------------------------------

## Create map for 2024 field season
Cubarral <- st_as_sf(data.frame(lat = 3.794, long = -73.839),
                     coords = c("long", "lat"),
                     crs = 4326)

# Extract coordinates for Cubarral label 
Cubarral_coords <- st_coordinates(Cubarral)

Pc_locs_sf %>%
  left_join(distinct(Bird_pcs_all, Id_survey, Id_scr)) %>%
  filter(Uniq_db == "UNILLANOS MBD") %>%
  ggplot() +
  geom_sf(aes(color = Id_scr)) + 
  geom_sf(data = Cubarral, shape = 6, size = 3) +  # Add Cubarral point
  annotation_scale(location = "bl") +  # Add scale bar
  geom_text(aes(x = Cubarral_coords[1], y = Cubarral_coords[2]), 
            label = "Cubarral", nudge_x = 0.016, nudge_y = -0.004, 
            size = 4, fontface = "bold") +  # Add label near Cubarral
  theme_min #+
#guides(color = "none")

#In new iteration Pc_locs_sf is just from point counts, so would have to change this out to a different data frame

Pc_locs_jit <- st_jitter(Pc_locs_sf, factor = .06)
# Pc_locs_jit <- Pc_locs_jit %>% filter(Uniq_db != "CIPAV MBD")
bbox_all <- st_bbox(Pc_locs_jit)

## Plot inset map for biodiversity data. The first plot shows unique point count and telemetry locations
bbox <- st_bbox(c(xmin = -73.887678, xmax = -73.463852, ymax = 3.92, ymin = 3.2), crs = st_crs(4326)) # The jitter applied is making things not line up perfectly
neCol %>% ggplot() +
  geom_sf() +
  geom_sf(data = neColDepts) +
  layer_spatial(bbox, color = "red") +
  geom_sf(
    data = Pc_locs_jit, size = 4, alpha = .3,
    aes(color = Institution_name, shape = Protocolo_muestreo)
  ) +
  geom_sf(
    data = filter(Pc_locs_jit, Protocolo_muestreo != "Punto conteo"), size = 4, alpha = .7,
    aes(color = Institution_name, shape = Protocolo_muestreo)
  ) +
  geom_sf(
    data = filter(Pc_locs_jit, Protocolo_muestreo == "Telemetria"), size = 4, alpha = .1,
    aes(color = Institution_name, shape = Protocolo_muestreo)
  ) +
  coord_sf(
    xlim = c(bbox_all[1], bbox_all[3]), ylim = c(bbox_all[2], bbox_all[4]),
    label_axes = "____", expand = TRUE
  ) #+ theme(legend.position = "none") #+ annotation_scale(location = "bl") + geom_sf(data = ne_cities, color = "light blue") + geom_sf(data = ne_rios, color = "light blue") + theme(legend.position = "none") + theme(axis.title = element_blank())


# Plot without Institution names but with color = Sampling protocol for Vanier
ggplot(data = neCol) +
  geom_sf() +
  geom_sf(data = neColDepts) +
  geom_sf(data = Pc_locs_jit, size = 4, alpha = .3, aes(color = Protocolo_muestreo)) +
  geom_sf(data = filter(Pc_locs_jit, Protocolo_muestreo != "Punto conteo"), size = 4, alpha = .7, aes(color = Protocolo_muestreo)) +
  geom_sf(data = filter(Pc_locs_jit, Protocolo_muestreo == "Telemetria"), size = 4, alpha = .1, aes(color = Protocolo_muestreo)) +
  coord_sf(xlim = c(bbox_all[1], bbox_all[3]), ylim = c(bbox_all[2], bbox_all[4]), label_axes = "____", expand = TRUE) +
  annotation_scale(location = "bl") +
  scale_color_discrete(name = "Methodology", labels = c("Mist net", "Point count", "Telemetry"))

## Black and white
ggplot(data = neCol) +
  geom_sf() +
  geom_sf(data = neColDepts) +
  geom_sf(data = filter(Pc_locs_jit, Protocolo_muestreo == "Punto conteo"), size = 3, alpha = .2, shape = 2) +
  geom_sf(data = filter(Pc_locs_jit, Protocolo_muestreo == "Captura con redes de niebla"), size = 4, alpha = .6, shape = 1) +
  geom_sf(data = filter(Pc_locs_jit, Protocolo_muestreo == "Radiotelemetria"), size = 4, alpha = 1, shape = 0) +
  coord_sf(xlim = c(bbox_all[1], bbox_all[3]), ylim = c(bbox_all[2], bbox_all[4]), label_axes = "____", expand = TRUE) +
  annotation_scale(location = "bl") +
  scale_shape_discrete(name = "Methodology", labels = c("Mist net", "Point count", "Telemetry"))

## Black and white with legend
Pc_locs_jit <- Pc_locs_jit %>%
  mutate(Protocolo_muestreo = factor(Protocolo_muestreo, labels = c("Mist net", "Point count", "Telemetry"))) %>%
  rename(Methodology = Protocolo_muestreo)

ggplot(data = neCol) +
  geom_sf() +
  geom_sf(data = neColDepts) +
  geom_sf(data = Pc_locs_jit, size = 3, alpha = .2, aes(shape = Methodology)) +
  scale_shape_manual(values = c(1, 2, 0)) +
  coord_sf(xlim = c(bbox_all[1], bbox_all[3]), ylim = c(bbox_all[2], bbox_all[4]), label_axes = "____", expand = TRUE) +
  annotation_scale(location = "bl")


# >High-resolution rainfall ----------------------------------------------

#Plot daily precipitation for the 4 months before sampling
ggplot(data = Prec_daily, aes(x = day, y = value, color = year)) +
  stat_smooth(method = "gam", se = FALSE) +
  labs(x = "Day", y = "Daily precipitation", 
       title = "Precipitation in the 4 months \nleading up to sampling") + 
  scale_x_continuous(breaks = c(0, 30, 60, 90, 120)) +
  #Add average of sampling period in 2 years 
  geom_vline(xintercept = c(max(Prec_daily$day) - 20, max(Prec_daily$day)), linetype = "dashed")
