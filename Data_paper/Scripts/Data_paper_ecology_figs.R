## Data paper Ecology figures ## 

# Load libraries -------------------------------------------------
# Libraries
library(readxl)
library(xlsx)
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

Pc_locs_sf <- st_read("Derived_geospatial/shp/Pc_locs.gpkg")

# Load in raw abundance data
Bird_pcs_all <- read_csv("Data_paper/DataS1/Bird_pcs_all.csv")
Bird_pcs_analysis <- read_csv("Data_paper/DataS1/Bird_pcs_analysis.csv")
Taxonomy <- read_csv(file = "Data_paper/DataS1/Taxonomy.csv")
Fn_traits <- read_csv(file = "Data_paper/DataS1/Functional_traits.csv") %>% 
  select(-Match_type)
Site_covs <- read_csv(file = "Data_paper/DataS1/Site_covs.csv")
Event_covs <- read_csv(file = "Data_paper/DataS1/Event_covs.csv")
Prec_df <- read_csv(file = "Derived/Excels/Prec_df.csv")

source("/Users/aaronskinner/Library/CloudStorage/OneDrive-UBC/Grad_School/Rcookbook/Themes_funs.R")

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

# Filter to Colombia only
rivers_co2 <- rivers_co %>%
  filter(
    name %in% c("Magdalena", "Cauca", "Guainía", "Meta") | is.na(name)
    ) %>% mutate(name = case_when(
      name == "Guainía" ~ NA,
      name == "Meta" ~ NA,
      .default = name
    ))

# >Point formatting -------------------------------------------------------
## Point counts within 0.25 degree grid cells 
# With ~500 point counts in concentrated regions there is too much overlap to clearly visualize what is going on. Instead, I calculate the number of point counts within .25 degrees cells and return the rounded coordinates for plotting
Pc_locs_round <- Pc_locs_sf %>%
  st_drop_geometry() %>%
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

# >Inset elev map -----------------------------------------------------
## Plot inset map for biodiversity data on elevation map background.
# Extract bounding box
bbox_all <- st_bbox(Pc_locs_jit)

# Generate map
Col_alt_map + 
  geom_sf(data = Pc_locs_jit, 
          aes(shape = Uniq_db, size = n, alpha = desc(n))) +
  # Rivers
  geom_sf(data = rivers_co, color = "blue") +
  geom_text_repel(
    data = rivers_co2,
    aes(label = name, geometry = geometry),
    stat = "sf_coordinates",
    size = 4, color = "red",
    max.overlaps = Inf
  ) +
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
  scale_size_continuous(range = c(3, 7)) +
  scale_alpha_continuous(range = c(.5, 1)) +
  guides(
    alpha = "none",
    size = guide_legend(title = "Number of \npoint counts"),
    shape = guide_legend(title = "Data collector")
  )
ggsave("Data_paper/Figures/Map_sampling/Sampling_map.png", bg = "white", width = 4)

## Delete in final version
explore <- FALSE
if(explore){
  # Formatting
  Rio_cesar_pts <- Pc_locs_sf %>% filter(Ecoregion == "Rio cesar") 
  Envi_rc <- Rio_cesar_pts %>% left_join(Site_covs)
  
  # Plot zoomed in 
  bbox_rc <- st_bbox(Rio_cesar_pts)
  Col_alt_map + geom_sf(data = Envi_rc,
                        aes(color = Tot_prec)) + 
    coord_sf(
      xlim = c(bbox_rc[1], bbox_rc[3]), ylim = c(bbox_rc[2], bbox_rc[4])
    )
}

# >inset prec map ------------------------------------------------------
Col_prec_map + 
  geom_sf(data = Pc_locs_jit, color = "white",
          aes(shape = Uniq_db, size = n, alpha = desc(n))) +
  # Rivers
  geom_sf(data = rivers_co, color = "blue") +
  geom_text_repel(
    data = rivers_co2,
    aes(label = name, geometry = geometry),
    stat = "sf_coordinates",
    size = 4, color = "red",
    max.overlaps = Inf
  ) +
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
  scale_size_continuous(range = c(3, 7)) +
  scale_alpha_continuous(range = c(.5, 1)) +
  guides(
    alpha = "none",
    size = guide_legend(title = "Number of \npoint counts"),
    shape = guide_legend(title = "Data collector")
  )

# >South America map ------------------------------------------------------
# Plot map of Colombia within South America
ggplot(data = SA) +
  geom_sf() +
  geom_sf(data = SA[SA$adm0_a3 == "COL", ], linewidth = 2, color = "black") #+
  #layer_spatial(st_bbox(Pc_locs_jit), color = "red")

ggsave("Data_paper/Figures/Map_sampling/South_america_grayscale.png", bg = "white", dpi = 300)

# Combine inset map + South America map in powerpoint

# Fig2: Temporal distribution of sampling plot ------------------------------
# Boxplots showing the temporal distribution of sampling in each ecoregion 

# Create the 'Grp_spat' variable that shows which point counts are surveyed at the same spatial location, which is especially important for Meta
Es_covs <- Event_covs %>% left_join(Site_covs)

Meta_PCs_related <- Es_covs %>%
  filter(Departamento == "Meta" & Uniq_db == "Gaica mbd") %>%
  distinct(Id_muestreo, Ano) %>% # head()
  group_by(Id_muestreo) %>%
  mutate(Year = paste0("Year", row_number())) %>%
  pivot_wider(names_from = Year, values_from = Ano) %>%
  mutate(Grp_spat = case_when( # Spatial group
    Year1 == 2016 & Year2 == 2017 ~ "G1617" # GAICA 2016-2017 is one group
  ))

Pc_date9 <- Es_covs %>%
  left_join(Meta_PCs_related[, c("Id_muestreo", "Grp_spat")],
            by = "Id_muestreo"
  ) %>%
  mutate(Grp_spat = case_when( # Spatial group
    Grp_spat == "G1617" ~ "G1617",
    Uniq_db == "Gaica distancia" ~ "Distancia",
    Uniq_db == "Cipav mbd" & Ano == 2016 ~ "CIPAV1",
    Uniq_db == "Cipav mbd" & Ano == 2017 ~ "CIPAV2",
    Uniq_db == "Unillanos mbd" | Uniq_db == "Ubc mbd" ~ "UniL_UBC",
    TRUE ~ "Other"
  )) %>%
  # One specific case for CIPAV
  mutate(Grp_spat = ifelse(Uniq_db == "Cipav mbd" & Ano == 17 & Ecoregion == "Boyaca Santander" & Mes == 4, "CIPAV1", Grp_spat))

# Reduce # of rows to increase readability of plot
Pc_date_p <- Pc_date9 %>% distinct( #PC_date_plot
  Nombre_institucion, Grp_spat, Ecoregion, Ano, Mes, Dia, N_samp_periods
) %>% 
  mutate(Ano = str_remove(Ano, "20")) #%>% 
 #Add a random Ecoregion so it doesn't add a 6th 'NA' panel
 #add_row(Ano = as.character(25), Ecoregion = "Cafetera") 

# Plot 
Pc_temporal_plot <- ggplot(data = Pc_date_p, aes(x = factor(Ano), y = Mes)) +
  geom_boxplot() +
  geom_jitter(
    data = filter(Pc_date_p, Nombre_institucion != "Cipav"), size = 3, width = 0.3, alpha = .5, aes(color = Nombre_institucion, shape = Grp_spat)
  ) +
  # Graph CIPAV on top of other points
  geom_jitter(
    data = filter(Pc_date_p, Nombre_institucion == "Cipav"), size = 3, width = 0.3, alpha = .6, aes(color = Nombre_institucion, shape = Grp_spat)
  ) +
  facet_wrap(~Ecoregion) +
  scale_y_continuous(breaks = seq(0, 12, by = 3)) +
  labs(
    x = "Year", y = "Month",
    size = "Number of distinct \n sampling periods", color = "Data collector",
  ) +
  scale_shape_manual(values = c(0:5)) +
  scale_color_viridis_d() +
  guides(shape = "none") +
  theme(legend.position = c(0.8, 0.2),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 22)
        )
        #legend.key.size = unit(x = c(1,.5), units = "cm")  

# Save plot
ggsave("Data_paper/Figures/02_Pc_month_year_day_ecoregion.png", bg = "white", width = 12)

# Fig3: Environmental vars histogram --------------------------------------
# Boxplots for Elevation, temp, & precipitation
p <- list()
var_names <- c("Elev", "Avg_temp", "Tot_prec")
ylab <- c("meters", "Celsius", "millimeters")
title <- c("Elevation", "Temperature", "Precipitation")
ecoreg_labs <- c("Bajo \nMagdalena", "Boyaca \nSantander", "Cafetera", "Piedemonte", "Rio Cesar")

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
    labs(y = ylab[i], title = title[i], color = "Ecoregion") +
    theme(
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank()
    )
  # guides(color = FALSE) +
  # scale_x_discrete(labels = ecoreg_labs)
}
ggarrange(p[[1]], p[[2]], p[[3]], nrow = 1, common.legend = T, labels = "AUTO")
ggsave("Data_paper/Figures/Envi_vars.png", bg = "white", width = 10)


# Fig4a: Rainfall density plota -------------------------------------------
var_names <- c("Elev", "Avg_temp", "Tot_prec")

# Correlation matrix -- Notice temp & elevation perfectly inverse correlated
Site_covs %>%
  select(all_of(var_names)) %>%
  cor() %>%
  data.frame() %>%
  mutate(across(everything(), round, 2))

# Rainfall seasonality plot
# For rainfall seasonality plot for determining ideal repeat survey timing for dynamic occupancy models, best to exclude Distancia & CIPAV b/c they only have 1 repeat survey 
Prec_df2 <- Prec_df %>% 
  filter(Uniq_db != "GAICA Distancia" & Uniq_db != "CIPAV MBD")
table(Prec_df2$Uniq_db)

Calc_mean_prec <- function(df, group_variable){
  df %>%
    group_by({{ group_variable }}) %>%
    summarize_if(is.numeric, mean) %>%
    pivot_longer(cols = starts_with("prec"), 
                 names_to = "Mes", values_to = "Prec") %>%
    mutate(Mes = as.numeric(str_remove(Mes, "prec_")))
}
#Per Ecoregion
Prec_ecor <- Calc_mean_prec(df = Prec_df2, group_variable = Ecoregion)
#Per department
Prec_depts <- Calc_mean_prec(df = Prec_df2, group_variable = Departamento)

## Plot precip for all ecoregions using smoothed GAM
# cc = cyclic cubic regression spline - Use because the function value at month 12 is constrained to join smoothly back to month 1.
# k = the basis dimension, i.e. the maximal degrees of freedom. This allows the smoother to be as wiggly as one wiggle per month.
ggplot(Prec_ecor, aes(x = Mes, y = Prec, color = Ecoregion)) +
  stat_smooth(method = "gam", formula = y ~ s(x, bs = "cc", k = 12), se = FALSE) +
  scale_x_continuous(breaks = c(0, 2, 4, 6, 8, 10, 12)) +
  labs(x = "Month", y = "Precipitation (mm)") + 
  theme(legend.position = "right")
ggsave("Data_paper/Figures/Rainfall/Prec_smoothed.png", bg = "white", 
       width = 8, height = 5)

# Fig4b: Precipitation with sampling dates ---------------------------------
# PCs_prec are the points that go on the rainfall seasonality plot
Pcs_prec <- Es_covs %>% 
  distinct(Uniq_db, Ecoregion, Ano, Mes) %>%
  left_join(Prec_ecor, by = c("Ecoregion", "Mes")) %>%
  arrange(Ecoregion, Ano, Mes) %>%
  mutate(min = Mes - 2, max = Mes + 2, .by = c(Uniq_db, Ecoregion, Ano)) %>% 
  # Create variable 'GrpTemp' that is TRUE when a given point count location is sampled in the same year and has the mean fecha julian within the specified tolerance of the other survey dates
  mutate(GrpTemp = case_when( # GrpTemp = Group together temporally?
    lead(Mes) >= min & lead(Mes) <= max ~ paste0("TRUE", Mes),
    lag(Mes) >= min & lag(Mes) <= max ~ paste0("TRUE", lag(Mes)),
    TRUE ~ "FALSE"
  )) %>%
  # Manually change one issue
  mutate(GrpTemp = ifelse(Ecoregion == "Piedemonte" & Ano == 19 & GrpTemp == "TRUE9", "TRUE10", GrpTemp
  )) %>% summarize(Prec = mean(Prec), Mes_mod = mean(Mes), 
                   .by = c(Uniq_db, Ecoregion, Ano, GrpTemp))

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
                  aes(x = Mes_mod, y = Prec, shape = factor(Ano)))
  } else {
    Plot_prec <- Prec_ecor %>%
      ggplot() +
      geom_jitter(data = Pcs_prec, size = 6, alpha = .5, 
                  aes(x = Mes_mod, y = Prec, 
                      shape = factor(Ano), color = Uniq_db)) + 
      guides(color = guide_legend(title = "Data collector"))
  }
  
  Plot_prec2 <- Plot_prec + 
    geom_line(data = Prec_ecor, aes(x = Mes, y = Prec)) +
    scale_x_continuous(breaks = c(0, 2, 4, 6, 8, 10, 12)) +
    labs(
      x = "Month", y = "Precipitation (mm)",
      title = "30-year average rainfall by department"
    ) +
    guides(shape = guide_legend(title = "Year")) + 
    scale_shape_manual(values = c(0, 1, 2, 3, 4, 5, 6)) 
  
  if(facet == TRUE){
    Plot_prec2 <- Plot_prec2 + facet_wrap(~Ecoregion)
  }
  
  return(Plot_prec2)
}


# Plot regions with potential for dynamic occupancy modeling. This plot goes into Powerpoint and then draw arrows to connect sets of points 
# NOTE: Would likely want to remove Bajo Magdalena
Plot_prec_samp(regions = c("Piedemonte", "Bajo magdalena", "Cafetera"), dyn_occ = TRUE, facet = FALSE)
ggsave("Data_paper/Figures/Rainfall/Prec_sampling.png", bg = "white")

# Faceted plot, with all data collectors and all regions shown 
Plot_prec_samp(regions = "All", dyn_occ = FALSE, facet = TRUE)
ggsave("Data_paper/Figures/Rainfall/Prec_sampling_faceted.png", bg = "white")

# Fig 5 -------------------------------------------------------
## Bar plots of species with the highest counts and observed at the greatest number of unique point count locations (i.e., localities)

# >Data wrangling ---------------------------------------------------------
# Summarize counts and localities across Species & ecoregion
Species_summary <- Bird_pcs_all %>% 
  left_join(Site_covs) %>%
  summarize(Count = sum(as.numeric(Count), na.rm = TRUE), 
            Localities = n_distinct(Id_muestreo),
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
  geom_col(color = "black", width = .8) +
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
  geom_col(color = "black", width = .8) +
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
(p1 + p2) + 
  plot_layout(guides = "collect") & 
  theme(legend.position = "top")

ggsave("Data_paper/Figures/Species_counts_localities_wide.png", 
       bg = "white", width = 15, height = 10)

# Supplementary figs ------------------------------------------------------
## Plot showing numer of point counts per farm, the number of farms each data collector surveyed, and the average number of times each point count was repeated within a season (< 80 days)

# Number of pc per farm
Farm_counts <- Event_covs %>% 
  left_join(Site_covs) %>%
  distinct(Id_gcs, Uniq_db, Id_muestreo) %>%
  count(Id_gcs, Uniq_db) %>%
  count(Uniq_db, name = "N_farms")

Db_summ <- Event_covs %>% 
  mutate(Max_rep_season = max(Rep_season), 
         .by = Id_muestreo) %>%
  summarize(Mean_rep_season = mean(Max_rep_season),
            .by = Uniq_db) %>% 
  mutate(Mean_rep_season = as.factor(round(Mean_rep_season, 0))) %>% 
  full_join(Farm_counts)

# Number of point counts per farm and database
Num_pcs_farm_db <- Event_covs %>% 
  left_join(Site_covs) %>%
  distinct(Id_gcs, Uniq_db, Id_muestreo) %>%
  count(Id_gcs, Uniq_db, sort = T)
## Generate labels for plot, where eaach label is the number of farms surveyed
# Adjust the location of the label for UniLlanos & UBC 
x_loc <- summarize(Num_pcs_farm_db, x_loc = max(n) + 1, .by = Uniq_db) %>% 
  arrange(desc(x_loc)) %>%
  mutate(x_loc = x_loc - c(rep(7, 2), rep(0,4)))
label_data <- Farm_counts %>% 
  left_join(x_loc)

# Plot
Num_pcs_farm_db %>% 
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

#ggsave("Data_paper/Figures/Pc_per_farm_db.png", bg = "white")

# Data sets ---------------------------------------------------------------
# >Metadata tbls -----------------------------------------------------------
## Custom function to extract the metadata for a given dataset 
extract_metadata <- function(df){
  map_dfr(names(df), function(col_name) {
    col_data <- df[[col_name]]
    if (inherits(col_data, "hms")) {
      low_val <- min(as.character(col_data, na.rm = TRUE)) 
      high_val <- max(as.character(col_data, na.rm = TRUE))
    } else {
      low_val <- as.character(min(col_data, na.rm = TRUE))
      high_val <- as.character(max(col_data, na.rm = TRUE))
    }
    tibble(
      Field_name = col_name,
      Data_type = class(col_data)[1],
      Low_range = low_val,
      High_range = high_val
    )
  })
}

### Create metadata tbls
## Primary abundance file 
Bird_abu_meta <- Bird_pcs_all %>% extract_metadata() 

## Site covariates
Site_covs_meta <- extract_metadata(Site_covs)

## Taxonomy file
Taxonomy_meta <- extract_metadata(Taxonomy)

## Functional traits
Fn_traits_meta <- Fn_traits %>% extract_metadata() 

## Event covariates 
Event_covs_meta <- Event_covs %>% extract_metadata() 

## Create column metadata list
Cols_metadata_l <- list(Bird_abu = Bird_abu_meta, Site_covs = Site_covs_meta, Event_covs = Event_covs_meta, Taxonomy = Taxonomy_meta, Functional_traits = Fn_traits_meta)

# >Export  ------------------------------------------------------

# Save the metadata list for each Excel included in repository
saveRDS(Cols_metadata_l, file = "Data_paper/Rdata/Cols_metadata_l.rds") 

stop()
# Export Excel but write the definitions manually in Excel
if(FALSE){
  Bird_abu_meta %>% 
    select(Field_name) %>% 
    mutate(Definition = NA) %>% 
    data.frame() %>%
    xlsx::write.xlsx(file = "Data_paper/Column_definitions.xlsx", showNA = FALSE,
                     row.names = FALSE, sheetName = "Bird_abundances.csv")
}

# EXTRAS ------------------------------------------------------------------
# >Spatial maps -----------------------------------------------------------

## Create map for 2024 field season
Cubarral <- st_as_sf(data.frame(lat = 3.794, long = -73.839),
                     coords = c("long", "lat"),
                     crs = 4326)

# Extract coordinates for Cubarral label 
Cubarral_coords <- st_coordinates(Cubarral)

Pc_locs_sf %>%
  left_join(distinct(Bird_pcs_all, Id_muestreo, Id_gcs)) %>%
  filter(Uniq_db == "UNILLANOS MBD") %>%
  ggplot() +
  geom_sf(aes(color = Id_gcs)) + 
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
    aes(color = Nombre_institucion, shape = Protocolo_muestreo)
  ) +
  geom_sf(
    data = filter(Pc_locs_jit, Protocolo_muestreo != "Punto conteo"), size = 4, alpha = .7,
    aes(color = Nombre_institucion, shape = Protocolo_muestreo)
  ) +
  geom_sf(
    data = filter(Pc_locs_jit, Protocolo_muestreo == "Telemetria"), size = 4, alpha = .1,
    aes(color = Nombre_institucion, shape = Protocolo_muestreo)
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
