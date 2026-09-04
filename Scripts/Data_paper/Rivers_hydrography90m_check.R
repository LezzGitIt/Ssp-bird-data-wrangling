## PhD birds in silvopastoral landscapes ##
## Exploratory -- does the Hydrography90m stream network resolve the small rivers next to our point counts? The point counts classified as riparian forest are the ground truth: if the layer is detailed enough, those points should snap to a mapped stream within a few tens of metres, on low Strahler-order (small) segments.

## Two ways to get the streams (set stream_source below):
##   (A) "api" -- hydrographr OGC API snaps points to the network server-side, no download. Needs hydrographr (remotes::install_github("glowabio/hydrographr")) and a working TLS chain to aqua.igb-berlin.de (this failed with "unable to get local issuer certificate" in the dev sandbox; expected to work on a normal macOS install).
##   (B) "tiles" -- downloads order_vect_segment_h10v06.gpkg (~2.4 GB; the study area is entirely in Hydrography90m regional unit 33, tile h10v06) from the IGB public server, then a local sf::st_nearest_feature distance. Slower first run, no API dependency.

## Not part of the deposit or the manuscript build -- a scratch analysis to decide whether Hydrography90m is detailed enough for a rivers layer on @fig-sampling-map.

# Setup ---------------------------------------------------------------------
library(sf)
library(dplyr)
library(purrr)
library(ggplot2)
library(stringr)

sf::sf_use_s2(FALSE)

stream_source <- "api"          # "api" or "tiles"
hydro90_dir   <- "Derived/Geospatial/Hydrography90m"   # gitignored cache (tile mode)
out_dir       <- "Figures/Rivers_check"                 # gitignored
utm18n        <- "EPSG:32618"                            # metric CRS for the study area
buffer_thresholds_m <- c(15, 30, 50, 100, 200)          # riparian-buffer distances to score against

# Tile mode: order_vect_segment tiles for regional unit 33 (study area is entirely in RU 33)
tile_ids  <- c("h10v06")                                 # add "h10v08" if southern points snap far
tile_url  <- function(t) sprintf(
  "https://public.igb-berlin.de/index.php/s/agciopgzXjWswF4/download?path=%%2Fr.stream.order%%2Forder_vect_tiles20d&files=order_vect_segment_%s.gpkg", t)

dir.create(hydro90_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# Point counts + riparian flag (from the pipeline) ------------------------
## 01 stops at its own guard; Pc_locs_sf (one row per physical location) and Pc_hab (habitat) are built before that
source("Scripts/01_Gen_wrangling.R")

pc_hab_loc <- Pc_hab %>% distinct(Id_muestreo_no_dc, Habitat, Habitat_sub)

points_sf <- Pc_locs_sf %>%
  distinct(Id_muestreo_no_dc, .keep_all = TRUE) %>%
  select(Id_muestreo_no_dc, Ecoregion, Id_gcs) %>%
  left_join(pc_hab_loc, by = "Id_muestreo_no_dc") %>%
  mutate(is_riparian = !is.na(Habitat_sub) & Habitat_sub == "Ripario")

coords <- st_coordinates(points_sf)
points_df <- points_sf %>%
  st_drop_geometry() %>%
  mutate(longitude = coords[, 1], latitude = coords[, 2], site_id = Id_muestreo_no_dc)

cat("Point counts:", nrow(points_df),
    "| riparian:", sum(points_df$is_riparian),
    "| other forest:", sum(points_df$Habitat == "Bosque" & !points_df$is_riparian, na.rm = TRUE), "\n")

# Distance to the nearest Hydrography90m stream --------------------------
if (stream_source == "api") {

  stopifnot(requireNamespace("hydrographr", quietly = TRUE))
  snapped <- hydrographr::api_get_snapped_points(
    data           = points_df[, c("site_id", "longitude", "latitude")],
    colname_lon    = "longitude",
    colname_lat    = "latitude",
    colname_site_id = "site_id",
    min_strahler   = 1,           # snap to any stream, however small
    add_distance   = TRUE
  )
  snapped <- as.data.frame(snapped)
  dist_col  <- grep("dist", tolower(names(snapped)), value = TRUE)[1]
  order_col <- grep("strahler", tolower(names(snapped)), value = TRUE)[1]
  dist_tbl <- snapped %>%
    mutate(dist_stream_m = as.numeric(.data[[dist_col]]),
           stream_order  = suppressWarnings(as.numeric(.data[[order_col]]))) %>%
    select(site_id = any_of(c("site_id", "Site_id", "SITE_ID")), dist_stream_m, stream_order)

} else if (stream_source == "tiles") {

  local_tiles <- file.path(hydro90_dir, paste0("order_vect_segment_", tile_ids, ".gpkg"))
  walk2(tile_ids, local_tiles, \(t, dest) {
    if (!file.exists(dest)) {
      options(timeout = 3600)
      download.file(tile_url(t), dest, mode = "wb")
    }
  })
  streams <- map(local_tiles, st_read, quiet = TRUE) |>
    bind_rows() |>
    st_zm(drop = TRUE) |>
    st_transform(utm18n)
  order_col <- grep("strahler", tolower(names(streams)), value = TRUE)[1]

  points_m    <- st_transform(points_sf, utm18n)
  nearest_idx <- st_nearest_feature(points_m, streams)
  dist_tbl <- tibble(
    site_id       = points_m$Id_muestreo_no_dc,
    dist_stream_m = as.numeric(st_distance(points_m, streams[nearest_idx, ], by_element = TRUE)),
    stream_order  = streams[[order_col]][nearest_idx]
  )
}

pts <- points_df %>% left_join(dist_tbl, by = "site_id")

# Accuracy summary ------------------------------------------------------
pts <- pts %>%
  mutate(group = case_when(
    is_riparian ~ "Riparian forest",
    Habitat == "Bosque" ~ "Other forest",
    .default = "Non-forest"
  ))

by_group <- pts %>%
  summarize(n = n(),
            median_dist_m = median(dist_stream_m, na.rm = TRUE),
            q90_dist_m    = quantile(dist_stream_m, 0.9, na.rm = TRUE),
            median_order  = median(stream_order, na.rm = TRUE),
            .by = group)

within_thresholds <- map(buffer_thresholds_m, \(d) {
  pts %>%
    filter(is_riparian) %>%
    summarize(threshold_m = d, pct_riparian_within = mean(dist_stream_m <= d, na.rm = TRUE) * 100)
}) |> bind_rows()

cat("\n== Nearest Hydrography90m stream, by point-count type ==\n"); print(by_group)
cat("\n== Riparian point counts within a buffer distance of a mapped stream ==\n"); print(within_thresholds)

write.csv(by_group, file.path(out_dir, "hydrography90m_distance_by_group.csv"), row.names = FALSE)
write.csv(within_thresholds, file.path(out_dir, "hydrography90m_riparian_within.csv"), row.names = FALSE)

# Plots ---------------------------------------------------------------------
## ECDF of nearest-stream distance -- riparian points should hug the y-axis if the network reaches the small rivers
p_ecdf <- pts %>%
  mutate(grp = if_else(is_riparian, "Riparian forest", "All other point counts")) %>%
  ggplot(aes(dist_stream_m, colour = grp)) +
  stat_ecdf(linewidth = 0.9) +
  geom_vline(xintercept = 30, linetype = 2, colour = "grey50") +
  scale_x_continuous(trans = "sqrt", breaks = c(0, 30, 100, 300, 1000, 3000)) +
  labs(x = "Distance to nearest Hydrography90m stream (m)",
       y = "Cumulative proportion of point counts",
       colour = NULL, title = "Does Hydrography90m reach the riparian point counts?") +
  theme_minimal()

ggsave(file.path(out_dir, "hydrography90m_distance_ecdf.png"), p_ecdf, width = 8, height = 5, bg = "white")
print(p_ecdf)

## Nearest-stream Strahler order for the riparian points -- low order = small rivers
p_order <- pts %>%
  filter(is_riparian, !is.na(stream_order)) %>%
  count(stream_order) %>%
  ggplot(aes(factor(stream_order), n)) +
  geom_col(fill = "steelblue") +
  labs(x = "Strahler order of nearest stream", y = "Riparian point counts",
       title = "What size stream do the riparian points sit on?") +
  theme_minimal()

ggsave(file.path(out_dir, "hydrography90m_riparian_stream_order.png"), p_order, width = 6, height = 4, bg = "white")
print(p_order)

# Export ------------------------------------------------------------------
stop()

pts %>%
  select(site_id, Ecoregion, Id_gcs, Habitat, Habitat_sub, is_riparian, dist_stream_m, stream_order) %>%
  arrange(desc(is_riparian), dist_stream_m) %>%
  write.csv(file.path(out_dir, "hydrography90m_point_distances.csv"), row.names = FALSE)
