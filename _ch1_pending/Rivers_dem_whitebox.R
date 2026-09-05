## PhD birds in silvopastoral landscapes ##
## NOTE: This riparian-corridor-delineation line of work belongs to PhD Chapter 1, not the Ecology data paper -- move this script (and its later revisions) to the Chapter 1 repo when that chapter resumes.

## Contents
# Derives a stream network for the Piedemonte point counts from FABDEM -- Copernicus GLO-30 with forest/building height removed (Hawker et al. 2022) -- via WhiteboxTools flow-routing and stream extraction. Landed here after SRTM 90 m and plain Copernicus GLO-30 (see git history) both under-performed: DEM extent and resolution weren't the only limiting factors, accounting for height of the forest canopy also helped substantially (visual confirmation in Google Earth).
# "Sweeps the flow-accumulation threshold" = runs the stream extraction at a range of flow-accumulation cut-offs (100-4000 cells; a cell only becomes a stream once that many upslope cells drain through it), so a low cut-off draws in every headwater rill and a high one keeps only major channels, then measures at each cut-off how far the point counts sit from the nearest derived stream.
# Also scores every point by a distance/stream-order heuristic ("how likely is this a mislabelled riparian point?") and exports a review CSV + KMLs for manual reclassification in Google Earth.

## Findings: The water accumulation surface was most accurate in higher elevations and struggled to differentiate the true water flow paths in the flood plains (further East). The DEM resolution meant that the projected waterflows were often offset from the actual stream / river, which made it less useful. The riparian likelihood scores were generally not helpful.

## I conducted manual stream reclassification, scoped to points with no direct field water-body record (Water_body_ever NA -- not visited in a year whose metadata form asked Cuerpo_de_agua):
# "Y" = a river/stream is directly visible next to the point in Google Earth imagery.
# "M" = likely riparian or seasonally flooded -- a mapped river runs through the point's forest but the exact channel can't be located (e.g. gallery forest a few hundred metres wide), or the ground shows clear flood signs with no water visible when the image was taken.
# Stream order weighted the judgement (order 2, and especially 3, counted for more) but the reclassification was subjective; detection got harder further from the Andes as floodplains widen and channels braid.
# Planned sensitivity check: rerun key models with "M" coded as riparian, as non-riparian, and as its own third category.

# Setup ---------------------------------------------------------------------
library(sf)
library(terra)
library(dplyr)
library(purrr)
library(whitebox)
library(ggplot2)

sf::sf_use_s2(FALSE)
wbt_init()

region            <- "Piedemonte"   # ecoregion to test on
breach_dist       <- 200           # least-cost breach search radius (cells)
export_threshold  <- 250           # flow-accum cutoff (cells) to vectorise + export as KML
analysis_buffer_m <- 3000          # clip the exported network to this buffer around the point counts
n_top_candidates  <- 50            # export this many non-riparian-labelled points, ranked by riparian_score, as a review KML
utm18n            <- "EPSG:32618"
dem_source        <- "fabdem"      # used only to tag output filenames

## flow-accumulation cutoffs (cells) to sweep -- ~0.0009 km2/cell at 30 m
thresholds <- c(100, 250, 500, 1000, 2000, 4000)

wb_dir  <- "Derived/Geospatial/Whitebox"
dem_dir <- "Derived/Geospatial/DEM"
out_dir <- "Figures/Rivers_check"
walk(c(wb_dir, dem_dir, out_dir), dir.create, showWarnings = FALSE, recursive = TRUE)

# Point counts + riparian flag (from the pipeline) ----------------------
tryCatch(source("Scripts/01_Gen_wrangling.R"), error = function(e) message("01 stopped at its guard: ", conditionMessage(e)))
stopifnot(exists("Pc_locs_sf"), exists("Pc_hab"))

pc_hab_loc <- Pc_hab %>%
  summarize(is_riparian      = any(Habitat_sub == "Ripario", na.rm = TRUE),
            any_forest       = any(Habitat == "Bosque", na.rm = TRUE),
            water_body_ever  = dplyr::first(Water_body_ever),   # NA where no metadata form asked Cuerpo_de_agua
            water_body_types = dplyr::first(Water_body_types),
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

# Get the DEM -------------------------------------------------------------
## FABDEM (Copernicus GLO-30 with forest & building height removed). No registration needed via the `fabdem` Python package (pip install fabdem); public bucket behind it.
pt_bb <- st_bbox(points_sf)

fab_dir <- file.path(dem_dir, "fabdem")
dir.create(fab_dir, showWarnings = FALSE, recursive = TRUE)
fab_path <- file.path(fab_dir, sprintf("fabdem_%s.tif", tolower(region)))
if (!file.exists(fab_path)) {
  pad <- pt_bb
  pad[c("xmin", "ymin")] <- pad[c("xmin", "ymin")] - 1
  pad[c("xmax", "ymax")] <- pad[c("xmax", "ymax")] + 1
  py <- Sys.which("python3")
  cmd <- sprintf(
    "import fabdem; fabdem.download((%f, %f, %f, %f), output_path='%s', cache='%s')",
    pad["xmin"], pad["ymin"], pad["xmax"], pad["ymax"], fab_path, file.path(fab_dir, ".cache"))
  system2(py, c("-c", shQuote(cmd)))
}
if (!file.exists(fab_path)) stop("FABDEM download failed -- see ", fab_path)
dem_raw <- terra::rast(fab_path)

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

## Coverage check: high flow accumulation entering at a DEM edge = a channel truncated from its upstream area (also fires on real outlets -- informational only)
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

## Riparian likelihood score --------------------------------------------
## Heuristic 0-1 score per point count combining distance to the nearest derived stream with that stream's Strahler order.
## Higher-order streams get a longer decay scale (tau) since they carry both a wider natural riparian corridor and a larger DEM/flow-routing positional error, so the same 100 m offset means much less next to a 4th-order river than next to a 1st-order rill.
## This is a ranking heuristic, not a fitted/calibrated probability -- the field riparian labels are too sparse (n=25 in Piedemonte) and, per Aaron, too unreliable to calibrate against. Use it to flag likely under-labeled points for manual review, not as a validated probability.
tau_by_order    <- c(`1` = 40, `2` = 80, `3` = 150, `4` = 250, `5` = 350)   # metres -- decay scale per Strahler order
score_radius_m  <- 500                                                     # ignore streams farther than this from a point

streams_scored <- streams_at(min(thresholds), add_strahler = TRUE) |>
  st_transform(utm18n) |>
  st_intersection(aoi_m)
ord_col <- strahler_col(names(streams_scored))
max_tau_order <- max(as.integer(names(tau_by_order)))
streams_scored <- streams_scored |>
  mutate(order = pmin(pmax(as.integer(.data[[ord_col]]), 1L), max_tau_order))

## For each point, find every stream within score_radius_m and take the highest exp(-distance / tau[order]) across them -- i.e. the single most-persuasive nearby stream, whatever its order.
near <- st_is_within_distance(points_m, streams_scored, dist = score_radius_m)
riparian_score <- map_dbl(seq_len(nrow(points_m)), function(i) {
  idx <- near[[i]]
  if (length(idx) == 0) return(0)
  d <- as.numeric(st_distance(points_m[i, ], streams_scored[idx, ], by_element = FALSE))
  o <- streams_scored$order[idx]
  max(exp(-d / tau_by_order[as.character(o)]))
})
points_sf <- points_sf |> mutate(riparian_score = round(riparian_score, 3))

cat("\n== Riparian likelihood score (", dem_source, ", tau by order) ==\n", sep = "")
cat("Currently-riparian points -- score summary:\n")
print(summary(points_sf$riparian_score[points_sf$is_riparian]))
cat("Currently-non-riparian points -- score summary:\n")
print(summary(points_sf$riparian_score[!points_sf$is_riparian]))

cat("\nTop 15 non-riparian-labelled points by score (candidates for relabelling):\n")
points_sf |>
  st_drop_geometry() |>
  filter(!is_riparian) |>
  arrange(desc(riparian_score)) |>
  select(Id_muestreo_no_dc, Id_gcs, riparian_score) |>
  slice_head(n = 15) |>
  print(n = 15)

write.csv(points_sf |> st_drop_geometry() |> arrange(desc(riparian_score)),
          file.path(out_dir, paste0("dem_whitebox_", dem_source, "_riparian_score.csv")), row.names = FALSE)

## Review sheet for Aaron to manually confirm/correct riparian status on every Piedemonte point. Aaron_rip is pre-filled "PE" (pre-existing) where the field label already says riparian, blank otherwise, for Aaron to fill in.
review_sheet <- points_sf |>
  st_drop_geometry() |>
  left_join(Site_covs |> select(Id_muestreo_no_dc, Nombre_finca, Habitat, Habitat_sub), by = "Id_muestreo_no_dc") |>
  mutate(Aaron_rip = if_else(is_riparian, "PE", NA_character_)) |>
  select(Point_count = Id_muestreo_no_dc, Id_gcs, Farm = Nombre_finca, riparian_score, Habitat, Habitat_sub,
         Water_body_ever = water_body_ever, Water_body_types = water_body_types, Aaron_rip) |>
  arrange(Point_count)

review_csv <- file.path(out_dir, paste0("dem_whitebox_", dem_source, "_riparian_review.csv"))
write.csv(review_sheet, review_csv, row.names = FALSE, na = "")
cat("Riparian review sheet written:\n  ", review_csv, "\n")

# Export the chosen threshold as KML for Google Earth ------------------
points_kml <- points_sf |>
  transmute(Name = Id_muestreo_no_dc, Id_gcs, group, riparian_score, Water_body_ever = water_body_ever) |>
  st_transform(4326)

kml_streams <- file.path(out_dir, paste0("dem_whitebox_", dem_source, "_streams_thr", export_threshold, ".kml"))
kml_points  <- file.path(out_dir, paste0(tolower(region), "_point_counts.kml"))
st_write(streams_kml, kml_streams, driver = "KML", delete_dsn = TRUE, quiet = TRUE)
st_write(points_kml, kml_points, driver = "KML", delete_dsn = TRUE, quiet = TRUE)
cat("\nKML written:\n  ", kml_streams, "\n  ", kml_points, "\n")

## Top N non-riparian-labelled points by score -- relabelling candidates for manual review
top_candidates <- points_sf |>
  filter(!is_riparian) |>
  arrange(desc(riparian_score)) |>
  slice_head(n = n_top_candidates) |>
  transmute(Name = Id_muestreo_no_dc, Id_gcs, riparian_score, group, Water_body_ever = water_body_ever) |>
  st_transform(4326)

kml_top <- file.path(out_dir, paste0("dem_whitebox_", dem_source, "_top", n_top_candidates, "_candidates.kml"))
st_write(top_candidates, kml_top, driver = "KML", delete_dsn = TRUE, quiet = TRUE)
cat("Top", n_top_candidates, "candidate KML written:\n  ", kml_top, "\n")
