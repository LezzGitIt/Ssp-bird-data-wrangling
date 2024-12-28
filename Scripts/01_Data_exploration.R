##PhD birds in silvopastoral landscapes##
##Data exploration 02 -- 
##Code to confirm data accuracy, explore data, look for biases that could influence analysis, & produce figures in the supporting information 

#Contents: Miscellaneous, see table of contents
# ) Number PCs / farm: Analysis to understand the extent of point counts in / out of SCR farms. & also the temporal coverage for each farm. 

#In the old repository 'Pilot_Colombia_V2' there is additional (rough) miscellaneous code like 1) testing the GAICA distancia database specifically

# Load libraries & data --------------------------------------------------------
library(readxl)
library(tidyverse)
library(sf)
library(chron)
library(ggpubr)
library(cowplot)
library(conflicted)
library(ggrepel)
ggplot2::theme_set(theme_cowplot())
conflicts_prefer(dplyr::select)
conflicts_prefer(dplyr::filter)

load("Rdata/the_basics_12.24.24.Rdata")
load("Rdata/Taxonomy_11.14.24.Rdata")
load("Rdata/Traits_elev_11.14.24.Rdata")
source("/Users/aaronskinner/Library/CloudStorage/OneDrive-UBC/Grad_School/Rcookbook/Themes_funs.R")

# Understand data ---------------------------------------------------------
#Lets break things down by the numbers for all databases
Method_Inst <- Birds_all4 %>% distinct(Id_muestreo, Nombre_institucion, Uniq_db, Protocolo_muestreo, Ecoregion, Departamento, Nombre_finca) 
Method_Inst %>% 
  group_by(Uniq_db, Protocolo_muestreo) %>%
  count()
Method_Inst %>%
  filter(Uniq_db == "CIPAV MBD") %>%
  count(Departamento, Nombre_finca, sort = T)

#Number of point counts per ecoregion
(PC_eco <- Bird_pcs %>% distinct(Uniq_db, Id_muestreo, Ecoregion) %>% 
   count(Uniq_db, Ecoregion))

#Number of point counts per Uniq_db
PC_eco %>% 
  group_by(Uniq_db) %>% 
  summarize(Total = sum(n))

#Number of point counts per habitat type
Bird_pcs %>% distinct(Uniq_db, Id_muestreo, Habitat_ut) %>% 
  count(Uniq_db, Habitat_ut)

# Show counts of finalized habitat types
ggplot(data = Pc_hab, aes(x = Habitat_ut)) +
  geom_bar(aes(y = (..count..), color = Habitat_ut)) +
  theme(axis.text.x = element_blank()) +
  ylab("Count")

# Histogram of repeat surveys.
Pc_date4 %>%
  distinct(Id_muestreo, N_reps) %>% 
  ggplot(aes(x = N_reps)) +
  geom_histogram(color = "black", binwidth = 1) +
  xlab("# of repeat surveys per point count location")

# Boxplot of repeat surveys per department
Pc_date4 %>%
  reframe(N = n(), Departamento = Departamento, .by = Id_muestreo) %>%
  distinct(N, Departamento) %>%
  ggplot(aes(x = Departamento, y = N)) +
  geom_boxplot() +
  ylab("# of repeat surveys \nper point count location")

##Time of point counts
#Plot hours for each Uniq_db 
Sys.setenv(TZ='GMT')
ggplot(data = Birds_all4, aes(x= Uniq_db, y = Hora)) + 
  geom_boxplot(coef = 6) + 
  geom_jitter(width = .2, alpha = .1, aes(color = Protocolo_muestreo)) + 
  scale_y_chron(format="%H:%M") + 
  theme(axis.text.x = element_text(size = 12, vjust = .58, angle = 60))
#ggsave("Sampling_times.png", width = 12, bg = "white")

# Number PCs / farm ---------------------------------------------------
# Analysis to understand the extent of point counts in / out of SCR farms. & also the temporal coverage for each farm. 

# Reduce the number of columns and join w/ # of repeat surveys
Dist_farms <- Bird_pcs %>%
  distinct(
    Id_muestreo, Nombre_finca_mixed, Id_gcs, Finca_referencia, Uniq_db, Distancia_farm
  ) %>%
  group_by(Id_muestreo) %>%
  slice_head() %>%
  ungroup() %>%
  left_join(distinct(Pc_date4[,c("Id_muestreo", "N_reps")]), by = "Id_muestreo")

# Calculate the # of point counts associated with each farm at two different spatial scales, 50 & 500m #

# 50m
Num_PCs_farm50 <- Dist_farms %>% 
  filter(Distancia_farm < 50) %>%
  group_by(Id_gcs) %>%
  summarize(
    Num_PCs50 = n(),
    Avg_rep_surveys50 = mean(N_reps)
  ) # Avg number of repeat surveys per point count

#500m
Num_PCs_farm500 <- Dist_farms %>%
  filter(Distancia_farm < 500) %>%
  group_by(Id_gcs) %>%
  summarize(
    Num_PCs500 = n(),
    Avg_rep_surveys500 = mean(N_reps)
  ) # Avg number of repeat surveys per point count

#Join 50 and 500m dataframes, and understand the number of point counts lost with the different buffers #
Num_PCs_farm <- Num_PCs_farm50 %>%
  full_join(Num_PCs_farm500,
            by = c("Id_gcs")
  ) %>%
  mutate(across(where(is.numeric), round, 2)) %>%
  summarize(across(), Dif_Num_PCs = Num_PCs500 - Num_PCs50)

#NOTE::There are 67 farms (by Id_gcs), but there is one farm (Id_gcs = 671) with all points above 500m, and 3 farms with all points above 50m 
all_IDs <- Bird_pcs %>% pull(Id_gcs) %>% unique()
TF <- all_IDs %in% (Num_PCs_farm500 %>% pull(Id_gcs))
all_IDs[!TF]
Dist_farms %>% filter(Id_gcs == "671")

#Add a row for farm 671 & add 0s for all columns when Id_gcs == 671
Num_PCs_farm <- Num_PCs_farm %>% add_row(Id_gcs = "671", ) %>% 
  mutate(across(-Id_gcs, ~ if_else(Id_gcs == "671", 0, .)))

# Add in information about which data collectors surveyed which point counts. Note I used the larger buffered distance (500m)
Surveyor_farms <- Dist_farms %>%
  filter(Distancia_farm < 500) %>%
  distinct(Id_gcs, Uniq_db) %>%
  mutate(Surveyed = "Y") %>%
  group_by(Id_gcs) %>%
  pivot_wider(names_from = Uniq_db, values_from = Surveyed, values_fill = "N") %>%
  ungroup() %>%
  add_row(Id_gcs = "671", 'CIPAV MBD' = "Y") %>% 
  mutate(across(-c(Id_gcs, 'CIPAV MBD'), ~ if_else(Id_gcs == "671", "N", .)))

# Calculate dates each farm was sampled. 
# By Fecha (each fecha is a column)
Farms_fecha <- Dist_farms %>%
  left_join(distinct(Pc_date4, Id_muestreo, Fecha), # add Fecha
            by = "Id_muestreo"
  ) %>%
  group_by(Id_muestreo) %>%
  arrange(Fecha) %>%
  mutate(Samp_Occ = row_number()) %>%
  pivot_wider(
    names_from = Samp_Occ, names_prefix = "Samp_Occ",
    values_from = Fecha, values_fill = NA
  ) %>%
  group_by(Id_gcs) %>%
  summarize(across(starts_with("Samp_Occ"), ~ min(., na.rm = TRUE))) # max_samp_occ = max(c_across(starts_with("Samp_Occ")), na.rm = TRUE)

# By Year_month (each year_month is a column)
Farms_ym <- Dist_farms %>% # Farms year month
  left_join(distinct(Pc_date4, Id_muestreo, Mes, Ano), # add Fecha
            by = "Id_muestreo"
  ) %>%
  mutate(Year_month = paste0(Ano, "_", Mes)) %>%
  group_by(Id_muestreo) %>%
  arrange(Year_month) %>%
  mutate(Samp_Occ = row_number()) %>%
  pivot_wider(
    names_from = Samp_Occ, names_prefix = "Samp_Occ",
    values_from = Year_month, values_fill = NA
  ) %>%
  group_by(Id_gcs) %>%
  summarize(across(starts_with("Samp_Occ"), ~ min(., na.rm = TRUE)))

# Create final files for export #
Num_PCs_farm_S <- Surveyor_farms %>% # S = surveyor
  left_join(Num_PCs_farm, by = c("Id_gcs")) %>%
  arrange(Id_gcs)

Num_PCs_farm_fecha <- Num_PCs_farm_S %>%
  left_join(Farms_fecha, by = c("Id_gcs"))

Num_PCs_farm_ym <- Num_PCs_farm_S %>%
  left_join(Farms_ym, by = c("Id_gcs"))

# Export Excels
Num_PCs_farm_fecha %>%
  as.data.frame() #%>%
#  write.xlsx(
#    file = "/Users/aaronskinner/Library/CloudStorage/OneDrive-UBC/Grad_School/PhD/SCR My products/Num_PCs_farm2.xlsx",
#    sheetName = "Num_PCs_farm_fecha", row.names = F, showNA = F
#  )

Num_PCs_farm_ym %>%
  as.data.frame() #%>%
# write.xlsx(
#  file = "/Users/aaronskinner/Library/CloudStorage/OneDrive-UBC/Grad_School/PhD/SCR My products/Num_PCs_farm2.xlsx",
#  sheetName = "Num_PCs_farm_ym", row.names = F, append = T, showNA = F
#)

# Summary stats
lapply(Num_PCs_farm_S[, c(2:6)], table) #Y is equal to number of farms surveyed
Num_PCs_farm_S %>% summarize(minPCs = min(Num_PCs500), maxPCs = max(Num_PCs500), meanPCs = mean(Num_PCs500), sdPCs = sd(Num_PCs500), minRepSurv = min(Avg_rep_surveys500), maxRepSurv = max(Avg_rep_surveys500), meanRepSurv = mean(Avg_rep_surveys500), sdRepSurv = sd(Avg_rep_surveys500))

# Plot a few summary plots  
# Histogram of the number of point counts that will be lost if we do a 50 vs 500m buffer
hist(Num_PCs_farm_S[Num_PCs_farm_S$Dif_Num_PCs > 0, ]$Dif_Num_PCs, main = "Difference in number of point counts included\n depending on buffer size (50 & 500m)", xlab = "Number of additional point counts", ylab = "# Farms")

# Histogram of point counts with distances from farm > 0
Dist_farms %>%
  filter(Distancia_farm > 0) %>%
  ggplot(aes(x = Distancia_farm)) +
  geom_histogram(color = "black") +
  xlab("Distance to GCS farm (m)") +
  labs(title = "Distances to nearest farm for 126 point counts") #+ 
#scale_x_continuous(trans = "log10") # , breaks = c(0.1, 1,10, 100, 1000), label = c("0.1", "1", "10", "100", "1000")) #+ geom_density(aes(y = after_stat(scaled)))

# NOTES
# Some farms have multiple GCS IDs, and ID 3580 has two farm names
Bird_pcs %>% distinct(Id_gcs, Nombre_finca, Nombre_finca_mixed) %>% 
  head()
Bird_pcs %>%
  distinct(Id_gcs, Nombre_finca_mixed) %>%
  count(Nombre_finca_mixed, sort = T) %>% 
  filter(n > 1)
Pc_date4 %>%
  filter(Nombre_finca == "El Porvenir") %>%
  distinct(Id_gcs, Nombre_finca) %>%
  rename(Name_GCS = Nombre_finca)
Dist_farms %>% filter(Id_gcs == 3580)

# Spatial maps --------------------------------------------------------------
library(rnaturalearthdata)
library(rnaturalearth)
library(smoothr)
library(ggspatial)

load(paste0("/Users/aaronskinner/Library/CloudStorage/OneDrive-UBC/Grad_School/R_Files/PhD/NE_layers_Colombia.Rdata"))

## Create map for 2024 field season
Cubarral <- st_as_sf(data.frame(lat = 3.794, long = -73.839),
                     coords = c("long", "lat"),
                     crs = 4326)

# Extract coordinates for Cubarral label 
Cubarral_coords <- st_coordinates(Cubarral)

Pc_locs_sf %>%
  left_join(distinct(Bird_pcs, Id_muestreo, Id_gcs)) %>%
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

# Plot map of Colombia within South America
ggplot(data = SA) +
  geom_sf() +
  geom_sf(data = SA[SA$adm0_a3 == "COL", ], color = "green") +
  layer_spatial(st_bbox(Pc_locs_jit), color = "red")

#ggsave("/Users/aaronskinner/Library/CloudStorage/OneDrive-UBC/Grad_School/PhD/Figures/Map_TNC_field_work/Bio_Field_Work_All_methodology_bw.png", bg = "white", units = "mm", device = "png", dpi = 300)

# Plot environmental vars ------------------------------------------------
# Boxplots for Elevation, temp, & precipitation
p <- list()
var_names <- names(Envi_df2[, c(4:6)])
ylab <- c("meters", "Celsius", "millimeters")
title <- c("Elevation", "Temperature", "Precipitation")
ecoreg_labs <- c("Bajo \nMagdalena", "Boyaca \nSantander", "Cafetera", "Piedemonte", "Rio Cesar")

for (i in c(1:3)) {
  print(i)
  p[[i]] <- Envi_df2 %>%
    group_by(Ecoregion) %>%
    ggplot(aes(
      x = fct_reorder(Ecoregion, elev, .fun = median),
      y = !!sym(var_names[i])
    )) +
    geom_boxplot(alpha = 1.0, aes(color = Ecoregion)) +
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
#ggsave("/Users/aaronskinner/Library/CloudStorage/OneDrive-UBC/Grad_School/PhD/Figures/Proposal/envi_vars.png", bg = "white", width = 10)

# Correlation matrix -- Notice temp & elevation perfectly inverse correlated
Envi_df2 %>%
  select(-c(1:3)) %>%
  cor() %>%
  data.frame() %>%
  mutate(across(everything(), round, 2))

## Rainfall 
#Plot daily precipitation for the 4 months before sampling
ggplot(data = Prec_daily, aes(x = day, y = value, color = year)) +
  stat_smooth(method = "gam", se = FALSE) +
  labs(x = "Day", y = "Daily precipitation", 
       title = "Precipitation in the 4 months \nleading up to sampling") + 
  scale_x_continuous(breaks = c(0, 30, 60, 90, 120)) +
  #Add average of sampling period in 2 years 
  geom_vline(xintercept = c(max(Prec_daily$day) - 20, max(Prec_daily$day)), linetype = "dashed")

# Rainfall seasonality plot
# For rainfall seasonality plot for determining ideal repeat survey timing for dynamic occupancy models, best to exclude Distancia & CIPAV b/c they only have 1 repeat survey 
Prec_df2 <- Prec_df %>% filter(Uniq_db != "GAICA Distancia" & Uniq_db != "CIPAV MBD")
table(Prec_df2$Uniq_db)

#Per Ecoregion
Prec_ecor <- Prec_df2 %>%
  group_by(Ecoregion) %>%
  summarize_if(is.numeric, mean) %>%
  pivot_longer(cols = starts_with("prec"), names_to = "Mes", values_to = "Prec") %>%
  mutate(Mes = as.numeric(str_split_fixed(Mes, "c", n = 2)[, 2]))

#Per department
Prec_depts <- Prec_df2 %>%
  group_by(Departamento) %>%
  summarize_if(is.numeric, mean) %>%
  pivot_longer(cols = starts_with("prec"), names_to = "Mes", values_to = "Prec") %>%
  mutate(Mes = as.numeric(str_split_fixed(Mes, "c", n = 2)[, 2]))

## Plot precip for all ecoregions
ggplot(data = Prec_ecor) +
  stat_smooth(method = "gam", se = F, aes(x = Mes, y = Prec, color = Ecoregion)) +
  scale_x_continuous(breaks = c(0, 2, 4, 6, 8, 10, 12)) +
  geom_vline(xintercept = c(1, 3), linetype = "dashed") +
  geom_vline(xintercept = c(4, 6)) +
  labs(x = "Month", y = "Precipitation (mm)")
#ggsave("/Users/aaronskinner/Library/CloudStorage/OneDrive-UBC/Grad_School/PhD/Figures/Prec_all_months.png", bg = "white")

# PCs_prec are the points that go on the rainfall seasonality plot
PCs_prec <- Mes_mod %>%
  filter(Uniq_db != "GAICA Distancia" & Uniq_db != "CIPAV MBD") %>%
  distinct(Ecoregion, Ano, Mes) %>%
  left_join(Prec_ecor, by = c("Ecoregion", "Mes")) %>%
  filter(Ecoregion %in% c("Bajo Magdalena", "Cafetera", "Piedemonte")) %>%
  arrange(Ecoregion) %>%
  group_by(Ecoregion, Ano) %>%
  arrange(Ecoregion, Ano, Mes) %>%
  mutate(min = Mes - 2, max = Mes + 2) %>%
  # Create variable 'GrpTemp' that is TRUE when a given point count location is sampled in the same year and has the mean fecha julian within the specified tolerance of the other survey dates
  mutate(GrpTemp = case_when( # GrpTemp = Group together temporally?
    lead(Mes) >= min & lead(Mes) <= max ~ paste0("TRUE", Mes),
    lag(Mes) >= min & lag(Mes) <= max ~ paste0("TRUE", lag(Mes)),
    TRUE ~ "FALSE"
  )) %>%
  # Manually change one issue
  mutate(GrpTemp = ifelse(Ecoregion == "Piedemonte" & Ano == 19 & GrpTemp == "TRUE9",
                          "TRUE10", GrpTemp
  )) %>%
  group_by(Ecoregion, Ano, GrpTemp) %>%
  summarize(Prec = mean(Prec), Mes_mod = mean(Mes))

# Plot precip for relevant departments for considering dynamic occupancy models
Prec_ecor %>%
  filter(Ecoregion %in% c("Bajo Magdalena", "Cafetera", "Piedemonte")) %>%
  ggplot(aes(color = Ecoregion)) +
  geom_line(aes(x = Mes, y = Prec)) +
  geom_jitter(data = PCs_prec, size = 4, alpha = .5, aes(x = Mes_mod, y = Prec, shape = factor(Ano))) +
  scale_x_continuous(breaks = c(0, 2, 4, 6, 8, 10, 12)) +
  labs(
    x = "Month", y = "Precipitation (mm)",
    title = "30-year Average Rainfall by Department \n and Temporal Sampling History"
  ) +
  # Add lines & green shading for approx migration dates
  geom_vline(xintercept = c(10, 4), linetype = "dashed") + 
  annotate("rect", xmin = 4, xmax = 10, ymin = 0, ymax = 480, alpha = .05, fill = "green")
#ggsave("/Users/aaronskinner/Library/CloudStorage/OneDrive-UBC/Grad_School/PhD/Figures/Proposal/Prec_historical_sampling.png", bg = "white")

# Sampling through time ---------------------------------------------------
## CHECK:: Ensure that the 'Rep' column matches with the metadata that Ecotropico made 
diff_df <- df_birds_red$Gaica_dist %>% 
  distinct(Id_muestreo, Pc_start, Fecha, Ocasion_muestreo_repeticion) %>% 
  full_join(Rep_dfs$Gaica_dist) %>% 
  filter(Ocasion_muestreo_repeticion != Rep) %>% 
  arrange(Id_muestreo, Fecha, Pc_start)

# Confirm these differences are due to no observations using the metadata file
ids <- df_metadata$Gaica_dist %>% filter(Spp_obs == 0) %>% 
  pull(Id_muestreo) %>% 
  unique()

diff_df %>% filter(!Id_muestreo %in% ids)

# >Repeat surveys per point count -----------------------------------------
# Boxplot showing the number of times each survey location was sampled by region and Ano
Pc_date4 %>%
  group_by(Id_muestreo, Ano_grp, Period_Num) %>%
  summarize(N = n(), Ecoregion = Ecoregion, Ano_grp = Ano_grp, Period_Num = Period_Num) %>%
  distinct(N, Ecoregion, Ano_grp, Period_Num) %>%
  ggplot(aes(x = Ecoregion, y = N)) +
  geom_boxplot() +
  geom_jitter(width = 0.05, alpha = 0.2, aes(color = Ano_grp)) +
  scale_y_continuous(breaks = seq(0, 8, by = 2)) +
  ylab("# of repeat surveys \nper point count location") #+ geom_jitter(width=0.1,alpha=0.2) #+ geom_point(position=position_jitterdodge(),alpha=0.3)

# Boxplot showing the # of repeat surveys within each closed survey period by department
Pc_date4 %>%
  group_by(Id_muestreo, Nombre_institucion, Ano, Period_Num) %>%
  summarize(N = n(), Ecoregion = Ecoregion, Nombre_institucion = Nombre_institucion, Ano = Ano, Period_Num = Period_Num) %>%
  distinct(N, Ecoregion, Nombre_institucion, Ano, Period_Num) %>%
  ggplot(aes(x = Ecoregion, y = N)) +
  geom_boxplot() +
  geom_jitter(width = 0.05, alpha = 0.2, aes(color = Nombre_institucion)) +
  scale_y_continuous(breaks = seq(0, 8, by = 2)) +
  ylab("# of repeat surveys per point \ncount location (closed period)") #+ geom_jitter(width=0.1,alpha=0.2) #+ geom_point(position=position_jitterdodge(),alpha=0.3)

# >Temporal distribution of sampling plot ---------------------------------
# Boxplots showing the temporal distribution of sampling in each ecoregion 

# Create the 'Grp_spat' variable that shows which point counts are surveyed at the same spatial location, which is especially important for Meta
Meta_PCs_related <- Pc_date4 %>%
  filter(Departamento == "Meta" & Uniq_db == "GAICA MBD") %>%
  distinct(Id_muestreo, Ano) %>% # head()
  group_by(Id_muestreo) %>%
  mutate(Year = paste0("Year", row_number())) %>%
  pivot_wider(names_from = Year, values_from = Ano) %>%
  mutate(Grp_spat = case_when( # Spatial group
    Year1 == 2016 & Year2 == 2017 ~ "G1617" # GAICA 2016-2017 is one group
  ))

PC_date3 <- Pc_date4 %>%
  left_join(Meta_PCs_related[, c("Id_muestreo", "Grp_spat")],
            by = "Id_muestreo"
  ) %>%
  mutate(Grp_spat = case_when( # Spatial group
    Grp_spat == "G1617" ~ "G1617",
    Uniq_db == "GAICA Distancia" ~ "Distancia",
    Uniq_db == "CIPAV MBD" & Ano == 2016 ~ "CIPAV1",
    Uniq_db == "CIPAV MBD" & Ano == 2017 ~ "CIPAV2",
    Uniq_db == "UNILLANOS MBD" | Uniq_db == "UBC MBD" ~ "UniL_UBC",
    TRUE ~ "Other"
  )) %>%
  # One specific case for CIPAV
  mutate(Grp_spat = ifelse(Uniq_db == "CIPAV MBD" & Ano == 17 & Ecoregion == "Boyaca Santander" & Mes == 4, "CIPAV1", Grp_spat))

# Reduce # of rows to increase readability of plot
PC_date_p <- PC_date3 %>% distinct( #PC_date_plot
  Nombre_institucion, Grp_spat, Ecoregion, Ano, Mes, Dia,N_samp_periods
) %>% 
  mutate(
    Ano = str_remove(Ano, "20")) %>% 
  #Add a random Ecoregion so it doesn't add a 6th 'NA' panel
  add_row(Ano = as.character(25), Ecoregion = "Cafetera") 

#Plot 
ggplot(data = PC_date_p, aes(x = factor(Ano), y = Mes)) +
  geom_boxplot() +
  geom_jitter(
    data = filter(PC_date_p, Nombre_institucion != "CIPAV"), size = 3, width = 0.3, alpha = .5,
    aes(color = Nombre_institucion, shape = Grp_spat)
  ) +
  # Graph CIPAV on top of other points
  geom_jitter(
    data = filter(PC_date_p, Nombre_institucion == "CIPAV"), size = 3, width = 0.3, alpha = .6,
    aes(color = Nombre_institucion, shape = Grp_spat)
  ) +
  facet_wrap(~Ecoregion) +
  scale_y_continuous(breaks = seq(0, 12, by = 3)) +
  labs(
    x = "Year", y = "Month",
    size = "Number of distinct \n sampling periods", color = "Data collector",
  ) +
  scale_shape_manual(values = c(0:5)) +
  scale_color_viridis_d() +
  guides(shape = "none")

#Add v2 to not overwrite something important 
ggsave(paste0("/Users/aaronskinner/Library/CloudStorage/OneDrive-UBC/Grad_School/PhD/Figures/Proposal/PC_month_year_day_ecoregion_", format(Sys.Date(), "%m.%d.%y"), ".png"),
       bg = "white", width = 12)

#NOTE:: Using distinct(Id_muestreo, Mes_mod) is not a perfect solution as there are some point counts in different sampling periods that coincidentally happened to average out to the same Mes_mod value (within a single point count ID). For example...
Pc_date4 %>% 
  distinct(Mes_mod, Ano, Id_muestreo) %>% 
  count(Id_muestreo, Mes_mod, sort = T) %>%
  # When Mes_mod == 6.5 this isn't an issue as these are the Meta points sampled in 2016 and 2017, thus they really should be grouped together 
  filter(Mes_mod != 6.5) %>% 
  filter(n>1)
# Problematic point -- I already manually updated this in Pc_date4 so you can see Samp_Periods_n == 2 as it should be 
Pc_date4 %>% filter(Id_group == "G-MB-Q-ECOR") %>% 
  distinct(Mes_mod, Mes, Ano, Samp_Periods_n) %>% 
  arrange(Ano)


# Testing Ecotropico ------------------------------------------------------
## Look for errors
# Are there any point count IDs in multiple data sets?
Birds_all4 %>% filter(Uniq_db != "UBC_GAICA DOM") %>%
  distinct(Uniq_db, Id_muestreo) %>%
  count(Id_muestreo, sort = T) %>% 
  filter(n>1)

#Identify sampling points that have multiple habitat types. This is expected in Distance sampling, but not in others..
mult_habs <- Bird_pcs %>% filter(Pregunta_gsc != "Distancia") %>% 
  group_by(Uniq_db, Id_muestreo) %>% 
  count(Habitat_ut) %>% 
  ungroup() %>% 
  count(Id_muestreo) %>% 
  filter(n>1) %>% 
  pull(Id_muestreo)

Bird_pcs %>% filter(Id_muestreo %in% mult_habs) %>% 
  distinct(Id_muestreo, Ano, Habitat_og, Habitat_ut) %>% 
  arrange(Id_muestreo, Ano) %>% 
  group_split(Id_muestreo)

# >Distance within & between point count IDs  ------------------------------
#Recognize that some points have multiple coordinates associated with each point. NAs are from telemetry
Bird_pcs %>% distinct(Id_muestreo, Latitud_decimal, Longitud_decimal) %>% 
  count(Id_muestreo, sort = T) %>% 
  filter(n > 1)

# >>Same IDs too far --------------------------------------------------------
#Distance in meters WITHIN point counts / redes de niebla of the same ID_punto_muestreo #

Pc_locs_mult_sf <- Pc_locs_mult %>% st_as_sf(coords = c("Longitud_decimal", "Latitud_decimal"), 
                                             crs = 4326, remove = F)

#Can run this analysis for either Id_group or Id_muestreo (see comments to right of the loop to see where to switch these)
uniqIDparc <- unique(Pc_locs_sf$Id_muestreo) #Id_group
parcIDs <- dist.all <- vector("list",length = length(uniqIDparc))
mn.dist <- max.dist <- rep(NA, length = length(uniqIDparc))
for(i in 1:length(uniqIDparc)){
  print(i)
  parcIDs[[i]] <- subset(Pc_locs_mult_sf, Id_muestreo == uniqIDparc[i]) #Id_group
  dist.all[[i]] <- st_distance(parcIDs[[i]])
  diag(dist.all[[i]]) <- NA
  mn.dist[i] <- round(colMeans(dist.all[[i]], na.rm = TRUE),2) #In meters
  max.dist[i] <- round(apply(dist.all[[i]], 2, max, na.rm = T), 2)
}

NumPts <- sapply(parcIDs, nrow) #Number of points per parcel
dists <- data.frame(uniqIDparc, mn.dist, max.dist, NumPts)
names(dists)[1] <- "Id_muestreo"
dists %>% filter(max.dist > 0) %>% arrange(max.dist)

Method_locs_dist <- merge(x = Pc_locs_sf, y = dists, by = "Id_muestreo", all = F)

Method_locs_dist %>% 
  group_by(Id_muestreo) %>% 
  slice_head() %>% 
  arrange(Uniq_db, max.dist) %>% 
  filter(max.dist > 0) %>% 
  st_drop_geometry() #%>% View()
#write.csv("Problematic_Dists.csv")
sort.int(Method_locs_dist$mn.dist, decreasing = T) #All repeat surveys at a unique point count locations are ON AVERAGE (colmeans) <25m from each other.

Method_locs_dist %>% filter(max.dist > 84) %>% 
  st_drop_geometry() %>% 
  arrange(max.dist) %>% 
  #left_join(Pc_hab[,c("Id_muestreo", "Habitat_og")], 
  #          by = "Id_muestreo") %>%
  distinct() #%>% #View()
#filter(Uniq_db == "CIPAV MBD") #%>% #Select only CIPAV?
#write.xlsx(paste0("/Users/aaronskinner/Library/CloudStorage/OneDrive-UBC/Grad_School/PhD/SCR_documents/Data_cleaning/Preguntas/Prob_dists/Problematic_dists_", format(Sys.Date(), "%m.%d.%y"), ".xlsx"), sheetName = "sameID_far_away", row.names = F)

#Manipulation with dates to understand if problematic points were on the same date or different dates. You can see that only 1 point was collected within 90 days!
Method_locs_dist %>% st_drop_geometry() %>%
  filter(max.dist > 10) %>% 
  select(Id_muestreo, max.dist, Latitud_decimal, Longitud_decimal, Fecha) %>% 
  group_by(Id_muestreo, Latitud_decimal, Longitud_decimal) %>% 
  summarize(Same_date = length(unique(Fecha)) == 1, mxF = max(Fecha), mnF = min(Fecha)) %>%
  group_by(Id_muestreo) %>%
  mutate(Coords = row_number()) %>%
  select(Id_muestreo, mxF, mnF, Coords) %>% 
  pivot_wider(values_from = c(mxF, mnF),
              names_from = Coords,
              names_glue = "{.value}{Coords}") %>% 
  mutate(dif.Day1 = mxF1 - mnF2, dif.Day2 = mxF2 - mnF1, 
         dif.Day = min(abs(dif.Day1), abs(dif.Day2))) %>% 
  select(Id_muestreo, dif.Day) %>% View()
#filter(dif.Day < 150)


# >>Diff IDs too close  ---------------------------------------------------------
##Determine the distance BETWEEN distinct point counts to ensure that they are consistent with the written protocol description (e.g. >150m)
#Create data frame and filter 
#I removed my point counts because they are the same as UniLlanos point counts so everything was showing up as 0 when both were present
Locs1coord <- Method_locs_dist %>% 
  filter(Uniq_db != "UBC MBD") %>% 
  group_by(Id_muestreo) %>% 
  slice_head()

# Find the nearest feature and calculate distances
nearest1 <- Locs1coord %>% 
  st_nearest_feature()
dist_nearest <- round(st_distance(Locs1coord, Locs1coord[nearest1,], by_element=TRUE),1)

# Identify points < 150m
TF <- as.numeric(dist_nearest) < 150
ID_problem_dist <- Locs1coord[TF,] %>% 
  pull(Id_muestreo)
Nearest_pt <- Locs1coord[nearest1,][TF,] %>% 
  pull(Id_muestreo)

#Points that are <150m
dists2close <- data.frame(ID_problem_dist, Nearest_pt, dist_nearest_m = as.numeric(dist_nearest[TF])) %>% 
  left_join(st_drop_geometry(Locs1coord[,c("Id_muestreo", "Uniq_db", "Departamento")]), join_by("ID_problem_dist" == "Id_muestreo")) %>% 
  arrange(Uniq_db, dist_nearest_m) %>% 
  filter(dist_nearest_m < 100)


#This ensures that the first letter of the ID (data collector) and the second letter (question of interest, e.g., "MB") are the same. Ultimately, the distances are only problematic if they are from the same Uniq_db
dists2close_sameUniq_db <- dists2close %>% 
  filter(sapply(ID_problem_dist, function(x){
    str_split_1(x, "-")[1]}) == sapply(Nearest_pt, function(x){str_split_1(x, "-")[1]
    })) %>% 
  filter(sapply(ID_problem_dist, function(x){
    str_split_1(x, "-")[2]}) == sapply(Nearest_pt, function(x){str_split_1(x, "-")[2]
    }))

dists2close_sameUniq_db %>% filter(dist_nearest_m < 100)

#Merge with habitat & export as Excel for CIPAV
dists2close_sameUniq_db %>% 
  #left_join(Pc_hab[,c("Id_muestreo", "Habitat_og")], 
  #         join_by("ID_problem_dist" == "Id_muestreo")) %>% 
  #rename(Original_Habitat_ID_problem_dist = Habitat_og) %>%
  distinct() #%>%
#filter(Uniq_db == "CIPAV MBD" ) #%>%
#write.xlsx(paste0("/Users/aaronskinner/Library/CloudStorage/OneDrive-UBC/Grad_School/PhD/SCR_documents/Data_cleaning/Preguntas/Prob_dists/Problematic_dists_", format(format(Sys.Date(), "%m.%d.%y"), "%m.%d.%y"), ".xlsx"), sheetName = "diffID_too_close", row.names = F, append = T)

#Export as KML for Lina
IDs_too_close <- dists2close %>% filter(Uniq_db == "CIPAV MBD") %>% 
  pull(ID_problem_dist) %>% unique()
IDs_too_far <- Method_locs_dist %>% filter(max.dist > 40 & Uniq_db == "CIPAV MBD") %>% 
  pull(Id_muestreo) %>% unique()

Prob_IDs <- c(IDs_too_close) # , IDs_too_far

Pc_hab %>% st_as_sf(coords = c("Longitud_decimal", "Latitud_decimal"),
                    crs = 4326) %>% 
  filter(Id_muestreo %in% Prob_IDs) %>% 
  select(-Habitat_ut) %>% 
  rename(name = Id_muestreo) #%>%
#st_write(driver='kml', dsn="/Users/aaronskinner/Library/CloudStorage/OneDrive-UBC/Grad_School/PhD/SCR_documents/Preguntas/Prob_dists/Dists_too_close_CIPAV.kml", layer = "Dists_too_close_CIPAV")

# >Metadata ---------------------------------------------------------------
## Examine the consistency between the metadata files & the bird database files 
## Check if there are any IDs that are in bird data but not in metadata, and vice versa
view_mismatch <- function(df){
  df %>% select(Id_muestreo, Fecha, Hora) %>% distinct() %>% 
    filter(!str_detect(Id_muestreo, "LIBRE|RED"))
}

# Create a nested list with points that don't have a match on ID, date, & time
# NOTE:: All UBC_Gaica points have times at start of point count & 5 minutes into point count, thus I remove these points from 
birds_metadata_l <- map2(df_birds_red[-c(1, 4:6)], df_metadata[-c(1, 4:6)], \(bird, meta) {
  meta <- meta %>% filter(!is.na(Hora) & Observacion_especies_por_punto_conteo == 1)
  only_in_bird <- anti_join(bird, meta, by = c("Id_muestreo", "Fecha", "Hora")) %>% view_mismatch()
  only_in_meta <- anti_join(meta, bird, by = c("Id_muestreo", "Fecha", "Hora")) %>% view_mismatch()
  list(only_in_bird = only_in_bird, only_in_meta = only_in_meta)
})
birds_metadata_l$Gaica_dist
map(birds_metadata_l, \(list){
  list %>% bind_rows(.id = "Found") 
}) 

# This can help identify the reason that points from birds_metadata_l appear here... Loop through each row, where each row represents a point count without a match for ID, date, & time
map(1:nrow(birds_metadata_l$Gaica_mbd$only_in_meta), \(row){
  # Select the problematic row that is found only in the metadata
  indexes <- birds_metadata_l$Gaica_mbd$only_in_meta %>% slice(row) %>% 
    distinct(Id_muestreo, Fecha)
  #Join with the bird database to understand where the problem lies
  df_birds_red$Gaica_mbd %>% right_join(indexes, by = c("Id_muestreo", "Fecha")) %>% 
    distinct(Id_muestreo, Fecha, Hora)
})

# Examine if there are point counts with Observacion_especies_por_punto_conteo == 0 that have rows in the bird database
map2(df_birds_red, df_metadata, \(bird, meta) {
  no_registros <- meta %>% filter(Observacion_especies_por_punto_conteo == 0) %>%
    distinct(Id_muestreo, Fecha, Hora)
  bird %>% semi_join(no_registros, by = c("Id_muestreo", "Fecha", "Hora")) %>%
    distinct(Id_muestreo, Fecha, Hora)
})

# Examine if there are point counts with Observacion_especies_por_punto_conteo == 1 that don't have any rows in the bird database
# NOTE:: The Ubc_gaica_Caf points on 5/27 & 5/28 are the 'ensayo' dates, & the LCA07-09 are PCs that we surveyed one time, which were both eliminated in 00a DW 
map2(df_birds_red, df_metadata, \(bird, meta) {
  registros <- meta %>% filter(Observacion_especies_por_punto_conteo == 1) %>%
    distinct(Id_muestreo, Fecha, Hora)
  registros %>% anti_join(bird, by = c("Id_muestreo", "Fecha", "Hora")) %>% 
    distinct(Id_muestreo, Fecha, Hora) %>% 
    filter(!str_detect(Id_muestreo, "LIBRE|RED"))
})

# Inspect individual results 
df_metadata$Gaica_dist %>% filter(Id_muestreo == "G-AD-M-LC_03") %>% 
  select(Id_muestreo, Fecha, Hora, Observacion_especies_por_punto_conteo) %>% 
  distinct()
df_birds_red$Gaica_dist %>% filter(Id_muestreo == "G-AD-M-LC_03") %>% 
  select(Id_muestreo, Fecha, Hora) %>% 
  distinct()


# >Times ------------------------------------------------------------------
## For CIPAV & GAICA distancia, the metadata files include the start and end time for each point.. Examine to see if the the start and end time from metadata matches with the bird database
df_se <- map(df_birds_red[c(1,2)], \(df){ #se = start end
  df %>% mutate(AM_PM = ifelse(Hora > chron::times("14:00:00"), "Afternoon", "Morning")) %>% 
    group_by(Id_muestreo, Fecha, AM_PM) %>% 
    mutate(start = min(Hora), 
           end = max(Hora), 
           Tot_time = end - start) %>% 
    distinct(Id_muestreo, Fecha, start, end, Tot_time)
})

## Join with metadata files to ensure all points have matching Id_muestreo, Fecha, & Hora
# Start times
map2(df_se, df_metadata[c(1,2)], \(se, meta){
  se %>% rename(Hora = start) %>% 
    anti_join(meta)
})

# End times 
map2(df_se, df_metadata[c(1,2)], \(se, meta){
  se %>% rename(Hora = end) %>% 
    anti_join(meta)
})

## Ensure that the metadata 'Total_pc_times' align with the calculated times from the database
map2(df_se, df_metadata[c(1,2)], \(se, meta){
  meta %>% select(Id_muestreo, Fecha, Hora, Total_pc_time) %>% 
    mutate(AM_PM = ifelse(Hora > chron::times("14:00:00"), "Afternoon", "Morning")) %>% 
    filter(!is.na(Total_pc_time)) %>% 
    full_join(se) %>% 
    mutate(Same = near(Tot_time, Total_pc_time)) %>% 
    filter(Same == FALSE)
})

## Examine the difference in times for a given point count on the same day
# Code is old, but still useful as this is for ALL databases (not just CIPAV & GAICA)
Hora_dif_df <- Bird_pcs %>% 
  # NOTE:: Some points are surveyed 2x on the same day
  # GAICA distancia surveyed some points in the AM & PM, whereas UBC & UniLlanos sampled same points between 1.5 & 3 hours apart in some cases
  mutate(AM_PM = ifelse(Hora > chron::times("14:00:00"), "Afternoon", "Morning")) %>% 
  group_by(Id_muestreo, Fecha, AM_PM) %>%
  mutate(Hora_dif = max(Hora) - min(Hora)) %>% 
  distinct(Uniq_db, Id_muestreo, Fecha, Hora_dif, Departamento) 
# NOTE:: Condensed CIPAV rows & calculated PC.length, so CIPAV points show up as 0 minutes in Hora_dif_df

#Examine point counts with times > X minutes. 
Hora_dif_df %>% filter(Hora_dif > chron::times("00:45:00")) #%>% View()

# Testing UBC_GAICA DB ----------------------------------------------------
# >Bird observation data --------------------------------------------------
#Create dataframe with just UBC_GAICA
UBC_gaica_df <- Bird_pcs %>% filter(Nombre_institucion == "UBC_GAICA")
ubc_g_ids <- unique(UBC_gaica_df$Id_muestreo)

##Create dataframe of names to compare column names
# Extract names from each data frame in UBC_gaica
UBC_gaica_l <- df_birds[4:6]
name_list <- lapply(UBC_gaica_l, names)

# Adjust the lengths of each list of names, filling with NA where needed
adjusted_names <- lapply(name_list, function(x) {
  length(x) <- max(sapply(name_list, length)) #Fill with NAs 
  return(x)
})

# Combine them using cbind
combined_names <- do.call(cbind, adjusted_names)

# View result
data.frame(combined_names) %>%
  mutate(TF = apply(., 1, function(row) all(row == row[1])))

#Examine values in each column
lapply(UBC_gaica_df, unique)

## Ensure all point count IDs are specified correctly
# Extract PC IDs not including those by UBC_GAICA to compare against
PCids <- Bird_pcs %>% filter(Nombre_institucion != "UBC_GAICA") %>% 
  pull(Id_muestreo) %>% 
  unique()
# Point counts from Otun Quimbaya have no match
prob_ids <- ubc_g_ids %in% PCids
unique(ubc_g_ids[!prob_ids])

#Determine if there are differences in Lat / long within a single ID
UBC_gaica_df %>% 
  distinct(Id_muestreo, Latitud_decimal, Longitud_decimal) %>% 
  #filter(Id_muestreo %in% c("G-MB-Q-PORT_01", "G-MB-Q-LCA_09"))
  #rename(name = Id_muestreo) %>%
  #st_as_sf(coords = c("Longitud_decimal", "Latitud_decimal"), crs = 4326, remove = F) %>%
  #st_write(driver='kml', dsn = paste0("Intermediate_products_geospatial/kml/GAICA_2024_", format(Sys.Date(), "%m.%d.%y"), ".kml"))
  group_by(Id_muestreo) %>%
  reframe(diff = round(diff(Latitud_decimal), 10))

#Examine species present 
ubc_g_spp <- UBC_gaica_df %>% pull(Nombre_ayerbe) %>% 
  unique()
TF <- ubc_g_spp %in% Ayerbe_all_spp
prob_spp <- ubc_g_spp[!TF]

#Split species name & compare to genus & species 
map(UBC_gaica_l, \(df){
  df %>% mutate(Genus = str_split_i(Nombre_cientifico_FINAL_Ayerbe_2018, " ", i = 1),
           genus_Equal = Genus == Genero,
           Species = str_split_i(Nombre_cientifico_FINAL_Ayerbe_2018, " ", i = 2),
           species_Equal = Species == Epiteto_especifico) %>%
    select(genus_Equal, Nombre_cientifico_FINAL_Ayerbe_2018, Genero, species_Equal, Epiteto_especifico) %>% 
    filter(genus_Equal == FALSE | species_Equal == FALSE) %>% 
    distinct(Nombre_cientifico_FINAL_Ayerbe_2018, Genero, Epiteto_especifico)
})

#Examine difference in times within same day 
Hora_dif_df %>% filter(Uniq_db == "UBC_GAICA DOM") %>% 
  arrange(Hora_dif) %>%
  filter(!Hora_dif %in% c(chron::times("00:05:00"), chron::times("00:00:00")))

#Identify sampling points that have multiple habitat types. 
UBC_gaica_df %>% distinct(Id_muestreo, Habitat_og) %>% 
  count(Id_muestreo) %>% 
  filter(n > 1)

#Identify problematic orders & families
TF_order <- UBC_gaica_df$Orden %in% Tax_df3$order_gbif
TF_family <- UBC_gaica_df$Familia %in% Tax_df3$family_gbif 

UBC_gaica_df[!TF_family,] %>% pull(Familia) %>% unique() #all exist, just not in gbif

#Consistency in Registrado_por 
unique(UBC_gaica_df$Registrado_por)
map(UBC_gaica_l, \(df){
  df %>% distinct(Registrado_por)
})

#Examine Grabaciones -- Natalia can help here. 
UBC_gaica_df %>% select(contains("Grabacion"), Distancia_observacion) %>%
  filter(if_any(contains("Grabacion"), ~ !is.na(.) & . != ""))

UBC_gaica_df %>% select(contains("Grabacion"), Registrado_por) %>% 
  filter(if_any(contains("Grabacion"), ~ !is.na(.)))

# >Metadata ---------------------------------------------------------------
UBC_gaica_metadata_l <- df_metadata[c(4:6)]
name_list_metadata <- lapply(UBC_gaica_metadata_l, names)

# Adjust the lengths of each list of names, filling with NA where needed
adjusted_names <- lapply(name_list_metadata, function(x) {
  length(x) <- max(sapply(name_list_metadata, length)) #Fill with NAs 
  return(x)
})

# Combine them using cbind
combined_names <- do.call(cbind, adjusted_names) %>% 
  data.frame() %>%
  filter(if_any(everything(), ~ . != "Esfuerzo_muestreo(puntos de conteo por finca y conteos por punto)"))

# View result
combined_names %>%
  mutate(TF = apply(., 1, function(row) all(row == row[1]))) #%>% 
  #write.xlsx(file = "Intermediate_products/Excels/Column_names_Ecotropico.xlsx", row.names = F, showNA = F)

# Ensure that all point counts were sampled on 3 different days & that metadata file is consistent with that
UBC_gaica_df %>% distinct(Id_muestreo, Fecha) %>% 
  count(Id_muestreo) %>% 
  filter(n != 3)

#Metadata file
map(UBC_gaica_metadata_l, \(df) {
  df %>% filter(is.na(`Observacion_ especies_por_punto_conteo`))
})


#Do all 'percent' rows sum to 100%?
map(UBC_gaica_metadata_l, \(df) {
  df %>% select(starts_with("percent")) %>%
    mutate(Sum = rowSums(across(everything()), na.rm = T)) %>% 
    filter(Sum != 100)
})

#Look for inconsistencies in habitat types
map(UBC_gaica_metadata_l[1:2], \(df) {
  df %>%
    select(contains("Habitat"), starts_with("percent")) %>% 
    filter(Habitat_predominante == "Potrero")
})

# Ensure that the water features are not changing day to day 
map(UBC_gaica_metadata_l, \(df) {
  df %>% distinct(Id_muestreo, Cuerpo_de_agua) %>% 
    count(Id_muestreo) %>% 
    filter(n > 1)
})

## Ensure all point count IDs are specified correctly in metadata 
map(UBC_gaica_metadata_l, \(metadata){
  prob_ids <- metadata$Id_muestreo %in% PCids
  unique(metadata$Id_muestreo[!prob_ids])
})

## Bring in habitat change files to match up with the metadata files & ensure that the habitat types are the same in both cases
hab_files <- list.files(path = "Data/Habitats")
df_habitats <- map(hab_files, \(file){
  read_xlsx(path = file.path("Data/Habitats", file),
            na = c("Sin informacion", "N/A"))
})

names(df_habitats) <- c("Canopy", "Cafetero", "Meta")

## Examine canopy cover & canopy height
df_habitats$Canopy <- df_habitats$Canopy %>% mutate(Valor = as.numeric(Valor))

# Each ID should have 6 rows
df_habitats$Canopy %>% count(Id_muestreo) %>% 
  filter(n != 6)

# Ensure distribution of Variable & Fuente make sense
df_habitats$Canopy %>% tabyl(Variable, Fuente)

# Should be even split in cardinal directions
df_habitats$Canopy %>% tabyl(Direccion_cardenal)

# Ensure values are reasonable (in meters & % canopy)
ggplot(data = df_habitats$Canopy) + geom_histogram(aes(x = Valor)) +
  facet_wrap(~Variable)

# Merge the two databases & compare.. Not pretty but I did confirm that all habitat types are the same EXCEPT G-MB-Q-LCA_03 which is 'Cultivo de aguacate' in the Habitats sheet & 'Borde de bosque' in the metadata file. This makes sense as in 2024 it is 'Cultivo de aguacate', but we didn't sample the birds at this point in 2024
map2(UBC_gaica_metadata_l[1:2], df_habitats[2:3], 
     \(metadata, habs){
  full_join(metadata, habs[,c("Id_muestreo", "Habitat_ajustado")], 
            by = "Id_muestreo") %>% 
         select(Id_muestreo, Habitat_ajustado, Habitat_predominante) %>% 
         distinct() %>% 
         arrange(Id_muestreo) %>% 
         mutate(Equal = Habitat_ajustado == Habitat_predominante) %>% 
         filter(Equal == FALSE)
})

# Examine changes between years
map(df_habitats[2:3], \(habs){
  habs %>% mutate(Hab_change = Habitat_og == Habitat_ajustado) %>% 
    filter(Hab_change == FALSE) %>% 
    select(Id_muestreo, Ano, Habitat_og, Habitat_ajustado)
}) 

#NOTE:: There is not perfect synchrony between Habitat_ut & Cuerpo_de_agua columns
map2(UBC_gaica_metadata_l[1:2], df_habitats[2:3], 
     \(metadata, habs){
       full_join(metadata[,c("Id_muestreo", "Cuerpo_de_agua")], 
                 habs[,c("Id_muestreo", "Habitat_ut")], 
                 by = "Id_muestreo") %>% 
         select(Id_muestreo, Habitat_ut, "Cuerpo_de_agua") %>% 
         filter(Habitat_ut == "Bosque ripario") %>% 
         filter(Cuerpo_de_agua == "__" | is.na(Cuerpo_de_agua)) %>%
         distinct()
     })

# >Otun Quimbaya comparison ------------------------------------------------
## Comparative analysis of David Monroy & I compared to GAICA 
#NOTE:: Robert & Yuri have motivation to increase their counts in OQ. What can we find out from the data?
Ubc_Monroy <- read_xlsx(path = "Data/Aves/Data_Monroy_Skinner_Otun_Quimbaya2024.xlsx") %>% 
  mutate(Equipo = "David_Aaron", 
         Nombre_ayerbe = paste(Genero, Epiteto_especifico))

# Create new Tax_df3 data frame & rerun 
Ubc_Monroy <- Ubc_Monroy %>% left_join(distinct(Tax_df3[,c("Species_ayerbe", "family_gbif")]), 
                         by = join_by("Nombre_ayerbe" == "Species_ayerbe")) %>% 
  rename(Familia = family_gbif)

#Join data frames
OQ <- df_birds$Ubc_gaica_OQ %>% mutate(Equipo = "GAICA") %>%
  rename(Nombre_ayerbe = Nombre_cientifico_FINAL_Ayerbe_2018) %>% 
  select(names(Ubc_Monroy)[-11], contains("Grabacion")) %>%
  bind_rows(Ubc_Monroy) %>% 
  mutate(Fecha = lubridate::mdy(paste(Mes, Dia, Ano, sep = "/")),
         Grabacion = str_to_sentence(Grabacion, locale = "en")) %>% 
  filter(Id_muestreo != "OQ_Practica")

#Each team surveyed each point 3x
OQ %>% distinct(Equipo, Id_muestreo, Fecha) %>% 
  count(Equipo, Id_muestreo, sort = T)

# GAICA registered 1.46x more than David & I 
nrow(Ubc_Monroy)
nrow(df_birds$Ubc_gaica_OQ) 

#Still registered significantly more even without recordings 
df_birds$Ubc_gaica_OQ %>% filter(Grabacion != "Y" | is.na(Grabacion)) %>% nrow()

#NOTE:: GAICA often had medium confidence on recording ID
OQ %>% tabyl(Confiabilidad_de_grabacion)

#GAICA registered 1 species less than David & I, without recording point counts
OQ_nr <- OQ %>% filter(Grabacion != "Y" | is.na(Grabacion)) #nr = no recording
OQ_nr %>% distinct(Equipo, Nombre_ayerbe) %>% 
  count(Equipo)

#Or 8 more species if considering recordings 
OQ %>% distinct(Equipo, Nombre_ayerbe) %>% 
  count(Equipo)

# Create list with dataframes with & without recordings
OQ_l <- list(Grabando = OQ, Sin_grabar = OQ_nr)

# Compare the families registered by GAICA vs David & I
# Try pie charts
imap(OQ_l, \(df, names) {
  df %>%
    count(Equipo, Familia) %>%
    ggplot(aes(x = "", y = n, fill = Familia)) + 
    geom_bar(stat = "identity", width = 1) +
    coord_polar("y", start = 0) +
    facet_wrap(~ Equipo) +  # Create one pie chart per 'Equipo'
    geom_text_repel(aes(label = Familia), 
                    position = position_stack(vjust = 0.5),  # Center labels inside segments
                    direction = "y") +
    labs(x = NULL, y = NULL) +  # Remove x and y axis labels for cleaner pie chart
    theme_void() +  # Remove other unnecessary plot elements
    guides(fill = "none")
})

# Attempt 2
imap(OQ_l, \(df, names){
  df %>% count(Equipo, Familia) %>% 
    ggplot(aes(x = Equipo, y = n, fill = Familia)) + 
    geom_bar(stat="identity", width=1) 
})

data.frame(Ayerbe_all_spp) %>% write.xlsx(file = "Ayerbe_spp_names2018.xlsx", row.names = FALSE)
OQ_l$Grabando$Id_muestreo
#Plot number of species observed per point count (over 3 days) per team
OQ_comp <- imap(OQ_l, \(df, names){ #OQ comparison 
  df %>% distinct(Equipo, Id_muestreo, Fecha, Nombre_ayerbe) %>% 
    count(Equipo, Id_muestreo, Fecha) %>% 
    ggplot(aes(x = Equipo, y = n)) + geom_boxplot() + 
    geom_jitter(alpha = .3, width = .03, aes(color = Id_muestreo)) + 
    labs(y = "Nro de especies por punto y dia", title = names) + 
    guides(color = "none")
})
ggarrange(OQ_comp[[1]], OQ_comp[[2]])
ggsave("Figures/Otun_comparison.png", bg = "white")

#Maybe they included more individuals towards the edge of the 50m radius? 
OQ_nr %>% filter(!is.na(Distancia_observacion)) %>% 
  ggplot(aes(x = Distancia_observacion)) +
  geom_histogram(stat = "count")


##CONCLUSIONS: 
#I think GAICA was likely generous with distances they included in their point counts. For example, I noted this with a few species in the field (Henicorhina en el punto 8 on the first day, & Quetzal in point 2 on day 2). They also likely tried really hard with recordings, and it is possible that they were texting with David to get information about birds at specific point counts, or sending him recordings. They likely DID learn from David about which birds are in the area. 
#Furthermore, David’s ear is likely very attuned to the species that are important for tourism, and he was definitely not used to focusing on the more common species. Additionally, it’s true that I was surely not as useful as Robert with his camera. When David would be photographing there’s no way he would be able to also concentrate on the songs / calls very much. 
# I did also review a few points & the data entered into the Excel matched the scanned data. There were not too many species written in the margins, indicating not many species were entered after the point counts were over (even those that were could have been from Robert's photographs). 
# I asked GAICA 2x whether there was anything that could have biased the data, and they said there was not. Ultimately, it is impossible to disqualify something more mischievous, but I will never know. Think I have to let it go & trust the data. 

# Miscellaneous -----------------------------------------------------------
# >Cipav_veg ---------------------------------------------------------------
#Create a kml file for exploration in Google Earth
Cipav.veg <- read_xlsx("/Users/aaronskinner/Library/CloudStorage/OneDrive-UBC/Grad_School/PhD/SCR_data/Vegetacion/VEGETACION_CIPAV_MONITOREO-BD_2013-2017_V2.xlsx", sheet = "Base_de_datos")
Cipav.veg %>% rename(Lat = Latitud_decimal, Long = Longitud_decimal) %>%  
  filter(!is.na(Lat) & !is.na(Long)) %>% 
  distinct(Lat, Long, Fecha_evento) %>%
  st_as_sf(coords = c("Long", "Lat"), crs = 4326, remove = F) #%>% 
#st_write(driver='kml', dsn="/Users/aaronskinner/Library/CloudStorage/OneDrive-UBC/Grad_School/PhD/SCR_data/Vegetacion/Cipav_veg.kml", layer = "Cipav_veg")

##Create a list of the column names of each data frame. The 7th data frame is the alphabetized names after combining all data frames using smartbind #
df_names <- lapply(df_birds, function(x){names(x)})
df_names[[7]] <- sort(names(Birds_all))
#Must set the length of each vector to the longest column length in order for data frame to turn out correct. 
vec_length <- sapply(df_names, length)
for(i in 1:length(df_names)){
  length(df_names[[i]]) <- max(vec_length)
}

df_names_final <- data.frame(do.call(cbind, df_names))
write.xlsx(df_names_final, file = "Data_base_names.xlsx", row.names = F)

#Update Edge v Interior for UBC points -- Note would have to go back to old point count names #
Pc_hab %>% filter(Uniq_db == "UBC MBD") %>% 
  distinct(Id_muestreo, Habitat) %>% 
  filter(Habitat == "BR" | Habitat == "B") %>% 
  mutate(Edge_vs_Int = case_when(Id_muestreo == "AN4-BR" ~ "Edge", 
                                 Id_muestreo == "AN16-BR" ~ "Edge",
                                 Id_muestreo == "PO3-BR" ~ "Unk",
                                 TRUE ~ "Interior"))

## Recorridos Libres 2017 ##
# 39 unique IDs + habitat type combination, each one with a unique coordinate.
GaicaRL <- Birds_all %>% filter(Protocolo_muestreo == "Observacion ad hoc en recorridos libres")
GaicaRL %>% distinct(Id_muestreo, Habitat_ut, as.character(Latitud_decimal), as.character(Longitud_decimal))

# Transects were sampled between 1 and 6 (13?) times
GaicaRL %>%
  distinct(Id_muestreo, Habitat_ut, Mes, Dia, as.character(Latitud_decimal), as.character(Longitud_decimal)) %>%
  count(Id_muestreo, sort = T)
