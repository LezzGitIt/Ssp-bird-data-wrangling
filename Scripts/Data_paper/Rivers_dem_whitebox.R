## PhD birds in silvopastoral landscapes ##
## NOTE: This riparian-corridor-delineation line of work belongs to PhD Chapter 1, not the Ecology data paper -- move this script (and its later revisions) to the Chapter 1 repo when that chapter resumes.

## Exploratory -- derive a stream network from a DEM with WhiteboxTools and see whether it places the small rivers close to the Piedemonte point counts. Sweeps the flow-accumulation threshold, measuring how far riparian vs non-riparian points sit from the derived streams, and exports one threshold as KML for Google Earth.

## The DEM must extend well up-slope of the point counts (the Piedemonte rivers drain the Eastern Cordillera to the W/SW) or the main channels get truncated and lose their upstream area. cop30 uses every tile in Derived/Geospatial/DEM/cop30/ -- add N02/N04, W075/W076 tiles as needed.

## Test run first with SRTM 90 m (dem_source = "srtm90") to shake out the workflow, then Copernicus GLO-30 (cop30) for the real 30 m attempt.

## Not part of the deposit or the manuscript build.

# Setup ---------------------------------------------------------------------
library(sf)
library(terra)
library(dplyr)
library(purrr)
library(whitebox)
library(ggplot2)
if (requireNamespace("CopernicusDEM", quietly = TRUE)) library(CopernicusDEM)   # cop30 only

sf::sf_use_s2(FALSE)
wbt_init()

dem_source       <- "cop30"        # "srtm90" | "cop30"
region           <- "Piedemonte"   # ecoregion to test on
srtm_pad_deg      <- 0.9            # DEM padding around the points (srtm90 crop only -- cop30 uses all local tiles)
breach_dist       <- 200           # least-cost breach search radius (cells)
export_threshold  <- 250           # flow-accum cutoff (cells) to vectorise + export as KML
analysis_buffer_m <- 3000          # clip the exported network to this buffer around the point counts
utm18n            <- "EPSG:32618"

## flow-accumulation cutoffs (cells) to sweep -- ~0.008 km2/cell at 90 m, ~0.0009 km2/cell at 30 m
thresholds <- if (dem_source == "srtm90") c(25, 50, 100, 200, 400, 800) else c(100, 250, 500, 1000, 2000, 4000)

wb_dir  <- "Derived/Geospatial/Whitebox"
dem_dir <- "Derived/Geospatial/DEM"
out_dir <- "Figures/Rivers_check"
walk(c(wb_dir, dem_dir, out_dir), dir.create, showWarnings = FALSE, recursive = TRUE)

# Point counts + riparian flag (from the pipeline) ----------------------
tryCatch(source("Scripts/01_Gen_wrangling.R"), error = function(e) message("01 stopped at its guard: ", conditionMessage(e)))
stopifnot(exists("Pc_locs_sf"), exists("Pc_hab"))

pc_hab_loc <- Pc_hab %>%
  summarize(is_riparian = any(Habitat_sub == "Ripario", na.rm = TRUE),
            any_forest  = any(Habitat == "Bosque", na.rm = TRUE),
            .by = Id_muestreo_no_dc)

points_sf <- Pc_locs_sf %>%
  distinct(Id_muestreo_no_dc, .keep_all = TRUE) %>%
  filter(Ecoregion == region) %>%
  select(Id_muestreo_no_dc, Id_gcs) %>%
  left_join(pc_hab_loc, by = "Id_muestreo_no_dc") %>%
  mutate(group = case_when(is_riparian ~ "Riparian forest",
                           any_forest  ~ "Other forest",
                           .default    = "Non-forest"))
points_m <- st_transform(points_sf, utm18n)
aoi_m <- points_m |> st_buffer(analysis_buffer_m) |> st_union()
cat(region, "point counts:", nrow(points_sf), "| riparian:", sum(points_sf$is_riparian), "\n")

# Get the DEM -----------------------------------------------------------
pt_bb <- st_bbox(points_sf)

dem_raw <- switch(dem_source,

  srtm90 = {
    bb <- pt_bb
    bb[c("xmin", "ymin")] <- bb[c("xmin", "ymin")] - srtm_pad_deg
    bb[c("xmax", "ymax")] <- bb[c("xmax", "ymax")] + srtm_pad_deg
    d <- geodata::elevation_3s(lon = mean(bb[c("xmin", "xmax")]), lat = mean(bb[c("ymin", "ymax")]), path = dem_dir)
    terra::crop(d, terra::ext(bb["xmin"], bb["xmax"], bb["ymin"], bb["ymax"]))
  },

  ## Copernicus GLO-30. CopernicusDEM::aoi_geom_save_tif_matches shells out to the AWS CLI; the copernicus-dem-30m bucket is public but the package omits --no-sign-request, so it needs AWS creds (or pre-download: aws s3 cp s3://copernicus-dem-30m/<tile>/<tile>.tif <cop_dir> --no-sign-request)
  cop30 = {
    cop_dir <- file.path(dem_dir, "cop30")
    dir.create(cop_dir, showWarnings = FALSE, recursive = TRUE)
    tifs <- list.files(cop_dir, pattern = "[.]tif$", full.names = TRUE)
    if (length(tifs) == 0) {
      pad <- pt_bb
      pad[c("xmin", "ymin")] <- pad[c("xmin", "ymin")] - 1
      pad[c("xmax", "ymax")] <- pad[c("xmax", "ymax")] + 1
      CopernicusDEM::aoi_geom_save_tif_matches(sf::st_sf(geometry = sf::st_as_sfc(sf::st_bbox(pad, crs = 4326))),
                                               dir_save_tifs = cop_dir, resolution = 30)
      tifs <- list.files(cop_dir, pattern = "[.]tif$", full.names = TRUE)
    }
    if (length(tifs) == 0) stop("No Copernicus 30 m tiles in ", cop_dir)
    ## terra::vrt() gave an empty mosaic for these COGs (terra 1.9.1) -- merge instead
    if (length(tifs) == 1) terra::rast(tifs) else do.call(terra::merge, lapply(tifs, terra::rast))
  }
)

dem_path <- file.path(wb_dir, "dem.tif")
terra::writeRaster(dem_raw, dem_path, overwrite = TRUE)
cat("DEM:", paste(round(res(dem_raw) * 111320), collapse = " x "), "m cells,", ncell(dem_raw), "cells | extent",
    paste(round(as.vector(ext(dem_raw)), 2), collapse = " "), "\n")

# WhiteboxTools: condition -> flow direction -> flow accumulation ------
dem_b  <- file.path(wb_dir, "dem_breached.tif")
d8_ptr <- file.path(wb_dir, "d8_pointer.tif")
facc   <- file.path(wb_dir, "flow_accum.tif")

wbt_breach_depressions_least_cost(dem = dem_path, output = dem_b, dist = breach_dist, fill = TRUE)
wbt_d8_pointer(dem = dem_b, output = d8_ptr)
wbt_d8_flow_accumulation(input = dem_b, output = facc, out_type = "cells")

## Coverage check: high flow accumulation entering at a DEM edge = a channel truncated from its upstream area
fa <- terra::rast(facc)
edge_in <- c(west  = max(fa[, 1][[1]], na.rm = TRUE),
             east  = max(fa[, ncol(fa)][[1]], na.rm = TRUE),
             north = max(fa[1, ][[1]], na.rm = TRUE),
             south = max(fa[nrow(fa), ][[1]], na.rm = TRUE))
cat("max flow accumulation entering at each DEM edge (cells):\n"); print(round(edge_in))
if (any(edge_in > 5000)) warning("A channel is truncated at a DEM edge -- extend the DEM in that direction.")

# Sweep the stream-extraction threshold --------------------------------
streams_at <- function(thr, add_strahler = FALSE) {
  strm_r <- file.path(wb_dir, sprintf("streams_%d.tif", thr))
  strm_v <- file.path(wb_dir, sprintf("streams_%d.shp", thr))
  wbt_extract_streams(flow_accum = facc, output = strm_r, threshold = thr)
  if (add_strahler) {
    ord_r <- file.path(wb_dir, sprintf("strahler_%d.tif", thr))
    wbt_strahler_stream_order(d8_pntr = d8_ptr, streams = strm_r, output = ord_r)
    wbt_raster_streams_to_vector(streams = ord_r, d8_pntr = d8_ptr, output = strm_v)
  } else {
    wbt_raster_streams_to_vector(streams = strm_r, d8_pntr = d8_ptr, output = strm_v)
  }
  s <- st_read(strm_v, quiet = TRUE)
  if (is.na(st_crs(s))) st_crs(s) <- st_crs(dem_raw)
  s
}

score_threshold <- function(thr) {
  streams <- st_transform(streams_at(thr), utm18n)
  d <- as.numeric(st_distance(points_m, streams[st_nearest_feature(points_m, streams), ], by_element = TRUE))
  tibble(threshold        = thr,
         n_segments       = nrow(streams),
         median_rip_m     = median(d[points_m$is_riparian]),
         median_nonrip_m  = median(d[!points_m$is_riparian]),
         pct_rip_within30 = mean(d[points_m$is_riparian] <= 30) * 100,
         pct_rip_within60 = mean(d[points_m$is_riparian] <= 60) * 100)
}

sweep <- map(thresholds, score_threshold) |> list_rbind()
cat("\n== DEM-derived streams (", dem_source, ") vs ", region, " point counts ==\n", sep = "")
print(as.data.frame(sweep))
write.csv(sweep, file.path(out_dir, paste0("dem_whitebox_", dem_source, "_sweep.csv")), row.names = FALSE)

p <- ggplot(sweep, aes(threshold)) +
  geom_line(aes(y = median_rip_m, colour = "Riparian"), linewidth = 1) +
  geom_line(aes(y = median_nonrip_m, colour = "Non-riparian"), linewidth = 1) +
  geom_hline(yintercept = 30, linetype = 2, colour = "grey50") +
  scale_x_log10() +
  labs(x = "Flow-accumulation threshold (cells)", y = "Median distance to nearest stream (m)",
       colour = NULL, title = paste0("Stream-extraction threshold sweep -- ", dem_source)) +
  theme_minimal()
ggsave(file.path(out_dir, paste0("dem_whitebox_", dem_source, "_sweep.png")), p, width = 8, height = 5, bg = "white")
print(p)

# Export the chosen threshold as KML for Google Earth ------------------
## wbt_raster_streams_to_vector names its attribute column STRM_VAL
strahler_col <- \(nm) nm[grepl("strahler|strm_val|value|dn|grid_code", tolower(nm))][1]

streams_exp <- streams_at(export_threshold, add_strahler = TRUE)
ord <- strahler_col(names(streams_exp))

streams_kml <- streams_exp |>
  st_transform(utm18n) |>
  st_intersection(aoi_m) |>
  st_transform(4326) |>
  mutate(strahler = if (!is.na(ord)) .data[[ord]] else NA_real_,
         Name = paste0("stream (Strahler ", strahler, ")")) |>
  select(Name, strahler)

points_kml <- points_sf |>
  transmute(Name = Id_muestreo_no_dc, Id_gcs, group) |>
  st_transform(4326)

kml_streams <- file.path(out_dir, paste0("dem_whitebox_", dem_source, "_streams_thr", export_threshold, ".kml"))
kml_points  <- file.path(out_dir, paste0(tolower(region), "_point_counts.kml"))
st_write(streams_kml, kml_streams, driver = "KML", delete_dsn = TRUE, quiet = TRUE)
st_write(points_kml, kml_points, driver = "KML", delete_dsn = TRUE, quiet = TRUE)
cat("\nKML written:\n  ", kml_streams, "\n  ", kml_points, "\n")

# ------------------------------------------------------------------------
stop()
