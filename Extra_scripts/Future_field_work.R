## PhD birds in silvopastoral landscapes##
# Future field work -- Identify target point counts / farms for field work in 2024
## This script identifies point counts surveyed in multiple distinct time periods that could be targets for resurvey with the ultimate goal of dynamic occupancy modeling. 

# Contents
# 1) DynOcc Targets -- Use the PC_date object to identify target point counts
# 2) Eje cafetero -- Work specific to the Eje Cafetero field season
# 3) Meta -- Work specific to Meta field season
# MISC
# 4) Sampling periods distinct -- Alternative code to identify target point counts. Fortunately, I arrived at the same set of target point counts with both sets of code. This code is much less efficient
# 5) ANLA permit needed for SELVA


# Libraries & data -------------------------------------------------------------
load("Rdata/the_basics_11.21.24.Rdata")

library(tidyverse)
library(cowplot)
library(sf)
library(readxl)
library(xlsx)
library(conflicted)
ggplot2::theme_set(theme_cowplot())
conflicts_prefer(dplyr::select)
conflicts_prefer(dplyr::filter)

# DynOcc Targets --------------------------------------------------------
# Identify how many distinct sampling periods each point count location has #
Rep_surv <- PC_date2 %>%
  filter(N_samp_periods > 1) %>%
  distinct(Id_muestreo, Uniq_db, Ecoregion, Id_gcs)

# Remember that there are 54 additional points in Meta across the land use gradient
UniLlanos_PC_ids <- BirdPCs %>%
  filter(Uniq_db == "UNILLANOS MBD") %>%
  pull(Id_muestreo) %>%
  unique()
# Generate PC IDs that we will target for resurvey
Rep_surv_ids <- c(Rep_surv$Id_muestreo, UniLlanos_PC_ids)
length(Rep_surv_ids)

# Export as kml file
PC_locsSf %>%
  filter(Id_muestreo %in% Rep_surv_ids) %>%
  distinct(Uniq_db, Id_group, Id_muestreo, .keep_all = TRUE) %>%
  filter(Uniq_db != "UNILLANOS MBD") %>%
  rename(name = Id_muestreo) # %>%
# st_write(driver='kml', dsn = paste0("Intermediate_products_geospatial/kml/Rep_surv_GAICA", format(Sys.Date(), "%m.%d.%y"), ".kml"))

# Summary table
Rep_surv %>%
  summarize(Num_farms = n_distinct(Id_gcs), Num_PCs = n(), .by = c(Ecoregion)) %>% # .by = c(Ecoregion, Id_gcs)
  mutate(Landcover = "Forest fragments") %>%
  arrange(Ecoregion) %>%
  add_row(
    Ecoregion = "Piedemonte", Num_farms = 5,
    Num_PCs = 54, Landcover = "Across gradient"
  )

# NOTE:: Using PC_date2 we arrive at the same 72 points & the same break down by Ecoregion
PC_date2 %>%
  filter(N_samp_periods > 1) %>%
  distinct(Id_muestreo, Ecoregion) %>%
  pull(Ecoregion) %>%
  table()

# Eje cafetero field season -----------------------------------------------
# Note Mes != 11 (remaining points were surveyed in July / August)
# At least in this data set 'Nombre_finca' has all the names we actually saw in the field, nombre_finca_mixed has incorrect names.
EC_PCs_df <- BirdPCs %>%
  filter(Uniq_db == "GAICA MBD" & Ecoregion == "Cafetera") %>%
  mutate(Seas = ifelse(Mes %in% c(7, 8), "Dry", "Wet")) %>%
  distinct(Departamento, Nombre_finca, Id_gcs, Id_muestreo, Ano, Seas, Latitud_decimal, Longitud_decimal, Habitat_og, Habitat_homologado_ut, Habitat_homologado_SUB_UT, Elevacion) %>%
  group_by(Id_muestreo) %>%
  mutate(
    visit_num = row_number(),
    cum_visits = n()
  ) %>% # Cumulative number of visits
  arrange(cum_visits, Id_muestreo) %>%
  ungroup()

EC_PCs <- EC_PCs_df %>% group_split(cum_visits)
names(EC_PCs) <- c("1_visit", "2_visits", "3_visits") # Number of visits in distinct seasons

# Create data frame of final points I hope to resurvey
resurvey_pts <- EC_PCs[[3]] %>%
  select(Id_muestreo, Id_gcs, Habitat_og, Ano, Nombre_finca, Latitud_decimal, Longitud_decimal) %>%
  filter(Ano != 2013) %>%
  # Add in point counts from farm 728
  bind_rows(EC_PCs_df %>% filter(Id_gcs %in% c(728)) %>%
    select(Id_muestreo, Id_gcs, Latitud_decimal, Longitud_decimal)) %>%
  distinct() %>%
  mutate(Top_priority = ifelse(Id_gcs == 728, "N", "Y")) %>%
  as.data.frame()

# Export in Excel format
resurvey_pts # %>% write.xlsx("/Users/aaronskinner/Library/CloudStorage/OneDrive-UBC/Grad_School/PhD/Field_Work/Resurvey_2024_2025/Eje_resurvey_sites.xlsx", row.names = F)

# Export as kml files#
# Just points surveyed 3x
resurvey_pts %>%
  mutate(
    Dueno = case_when(
      Id_gcs == 350 ~ "Juan Carlos Nieto Betancour",
      Id_gcs == 728 ~ "Mauricio Enrique Cano Jaramillo",
      Id_gcs == 1612 ~ "Dario Angel Marulanda Angel",
      Id_gcs == 1629 ~ "Nancy Echeverri de los Rios"
    ),
    PC_ID = sapply(str_split(Id_muestreo, "-"), function(str) {
      str[[4]]
    })
  ) %>%
  rename(name = PC_ID) %>% # Id_gcs
  st_as_sf(coords = c("Longitud_decimal", "Latitud_decimal"), crs = 4326) # %>%
# st_write(driver='kml', dsn="/Users/aaronskinner/Library/CloudStorage/OneDrive-UBC/Grad_School/PhD/Field_Work/Resurvey_2024_2025/Sampling plan/Eje_resurvey_sites_PC_ID.kml", layer = "Eje_resurvey_sites")

# Consider pairing points that were surveyed near each other #
# There are 6 point counts in forest, that were sampled in slightly different locations in 2013 compared to 2017 (Note 1 - 6 was sampled in 2013, 7 - 12 in 2017). I THINK this was due to land use change? Deforestation?##
# Could pair points 3 & 12, and maybe 4 & 11, or maybe 6 & 7
# UPDATE: Visited points in the field & decided against doing this
Rep_diff_loc <- BirdPCs %>%
  filter(Departamento == "Quindio" & str_detect(Id_muestreo, "EC_")) %>%
  distinct(Id_muestreo, Id_gcs, Latitud_decimal, Longitud_decimal, Ano) %>%
  arrange(Id_muestreo) %>%
  st_as_sf(coords = c("Longitud_decimal", "Latitud_decimal"), crs = 4326)

Rd1_2013 <- Rep_diff_loc %>% slice(1:6)
Rd2_2017 <- Rep_diff_loc %>% slice(7:12)

dist_mat <- st_distance(Rd1_2013, Rd2_2017) %>%
  as.data.frame() %>%
  rename_with(~ c(Rd2_2017$Id_muestreo)) %>%
  mutate_if(is.numeric, round, 1)
rownames(dist_mat) <- Rd1_2013$Id_muestreo

# NOTE:: The point G-MB-Q-EC_12-B is closest to nearly all of the 2013 points, so will need to figure out some way to find the pairs of points that minimizes the overall distance between all 6 points.
dist_mat %>%
  rowwise() %>%
  mutate(min_col = which.min(c_across(everything())))

# Meta field season -------------------------------------------------------
# Isolate PCs of GAICA monitoreo de biodiversidad en Meta
# NOTE:: Only 3 points surveyed in both time periods for "G-MB-M-LCR" 
PCs_GMB_M <- BirdPCs %>%
  filter(Uniq_db == "GAICA MBD" & Ecoregion == "Piedemonte") %>%
  filter(Id_group %in% c("G-MB-M-LP1", "G-MB-M-A", "G-MB-M-LCR", "G-MB-M-EPO1"))

# Generate KML
PCs_GMB_M %>%
  distinct(Id_gcs, Id_group, Id_muestreo, Latitud_decimal, Longitud_decimal, Habitat_og) %>%
  st_as_sf(coords = c("Longitud_decimal", "Latitud_decimal"), crs = 4326, remove = T) %>%
  mutate(muestrear_aves = ifelse(Id_muestreo %in% gaica_PC_ids, "Y", "N")) %>%
  select(-Id_group) %>%
  rename(name = Id_muestreo) # %>%
# st_write(driver='kml', dsn = paste0("Intermediate_products_geospatial/kml/Rep_all_pts_GAICA_", format(Sys.Date(), "%m.%d.%y"), ".kml"))

# Generate Excel for changing habitats
PCs_GMB_M %>%
  distinct(Id_gcs, Nombre_finca, Ano, Id_muestreo, Habitat_og, Habitat_homologado_ut) %>%
  mutate(Habitat_ajustado = "", Observacion = "") %>%
  arrange(Nombre_finca, Id_muestreo, Ano) #%>%
  #write.xlsx(file = "Intermediate_products/Excels/Habitats_Meta.xlsx", row.names = F, showNA = F)

# 54 pts land use gradient ------------------------------------------------
# 18 effective days of field work
BirdPCs %>% filter(Uniq_db == "UNILLANOS MBD") %>% 
  distinct(Nombre_finca, Id_muestreo) %>%
  count(Nombre_finca)

# Miscellaneous -----------------------------------------------------------
# >Sampling periods distinct ------------------------------------------------------------
## NOTE:: This is old code that is not nearly as efficient as what is currently in DW_BirdPCs. Fortunately we also arrive at the same answer this way!

# Identify how many distinct sampling periods each point count location has #
# Combine separate months that are really in the same sampling period into a single row.
conflicts_prefer(dplyr::lag)
Mes_Mod <- PC_date2 %>%
  mutate(date.num = as.numeric(PC_date2$Fecha)) %>%
  group_by(Id_muestreo, Ecoregion, Ano, Mes) %>%
  # Create the mean, min, and max date number (DN) within a month
  summarize(meanDN = mean(date.num), minDN = mean(date.num) - 225, maxDN = mean(date.num) + 225) %>%
  group_by(Id_muestreo) %>%
  # Create variable 'GrpTemp' that is TRUE when a given point count location is sampled in the same year and has the mean fecha julian within the specified tolerance of the other survey dates
  mutate(GrpTemp = case_when( # GrpTemp = Group together temporally?
    meanDN < lead(maxDN) & meanDN > lead(minDN) ~ TRUE,
    meanDN < lag(maxDN) & meanDN > lag(minDN) ~ TRUE,
    TRUE ~ F
  )) %>%
  group_by(Id_muestreo, GrpTemp) %>%
  # Average Month (to create a single value) when 'GrpTemp' == TRUE
  mutate(Mes_Mod = case_when(
    GrpTemp == TRUE ~ as.factor(round(mean(Mes), 1)),
    TRUE ~ as.factor(Mes)
  )) %>%
  ungroup() %>%
  left_join(distinct(BirdPCs, Id_muestreo, Departamento, Uniq_db),
    by = "Id_muestreo"
  )

# Create a single row per point count & Mes_mod, then the number of Samp_Periods is equal to the number of rows
Dist_samp_periods <- Mes_Mod %>%
  group_by(Id_muestreo) %>%
  arrange(Ano, Mes, .by_group = TRUE) %>%
  distinct(Id_muestreo, Mes_Mod) %>%
  mutate(Samp_Periods_n = n(), Period_Num = row_number())

# Merge with Mes_Mod
Dist_samp_periods2 <- merge(Mes_Mod[, c("Id_muestreo", "Mes_Mod", "GrpTemp", "Mes", "Ano")], Dist_samp_periods, by = c("Id_muestreo", "Mes_Mod"))
nrow(Dist_samp_periods2)

# Merge with PC_date2
PC_date3 <- merge(PC_date2, select(Dist_samp_periods2, -Ano), by = c("Id_muestreo", "Mes")) %>%
  arrange(Id_muestreo, Ano, Mes)

# Manually adjust a single case where Mes_mod coincidentally happened to average out to the same Mes_mod value (within a single point count ID)
PC_date4 <- PC_date3 %>%
  mutate(Samp_Periods_n = ifelse(Id_group == "G-MB-Q-ECOR", 2, Samp_Periods_n)) %>%
  distinct()

# Compare the code in DW_BirdPCs to the approach here. We get the same answer!
PC_date4 %>%
  left_join(PC_date2[, c("Id_muestreo", "N_samp_periods")]) %>%
  distinct(Id_muestreo, Samp_Periods_n, N_samp_periods) %>%
  mutate(TF = Samp_Periods_n == N_samp_periods) %>%
  pull(TF) %>%
  table()
# >Color band combos -------------------------------------------------------
colors <- c("R", "Y", "N", "B", "K", "W", "G", "A") # 7 colors + aluminum. G = gray, N = green, B = blue, K = black

perms <- data.frame(gtools::permutations(colors, n = 8, r = 3))
combos <- paste0(perms$X1, perms$X2, perms$X3) # , perms$X4
TF <- grepl("A", combos)
permsA <- perms[TF, ]
nrow(permsA)
combosR <- data.frame(paste0(permsA$X1, permsA$X2, "/", permsA$X3))
combosL <- data.frame(paste0(permsA$X1, "/", permsA$X2, permsA$X3))
names(combosR) <- "ColorCombo"
names(combosL) <- "ColorCombo"
combosRL <- rbind(combosR, combosL)
names(combosRL) <- "ColorCombo"
set.seed(123) # So randomization produces same results each time
combos_df2 <- data.frame(combos_df2[sample(1:nrow(combos_df2)), ]) # Randomize order
combos_df2 <- merge(CC, combosRL, by = "ColorCombo", all.y = T)
df <- setNames(data.frame(matrix(ncol = 7, nrow = 252)), c("Date", "Species", "Band#", "BandSize", "Sex", "Location", "Notes"))
combos_df2 <- cbind(combosRL, df)
head(combos_df2)
CC <- read.csv("/Users/aaronskinner/Library/CloudStorage/OneDrive-UBC/Grad_School/PhD/Field_Work/Data_sheets/Color_Banding/ColorCombos.csv")
merge(CC, combos_df2, by = "ColorCombo", all.x = T, all.y = F)
write.csv(combos_df2, file = "/Users/aaronskinner/Library/CloudStorage/OneDrive-UBC/Grad_School/PhD/Field_Work/ColorCombos.csv")

# >ANLA permit -----------------------------------------------------------
## Get SELVA farm coordinates, and polygon encompassing study area
poly <- readOGR(dsn = "/Users/aaronskinner/Library/CloudStorage/OneDrive-UBC/Grad_School/PhD/Field_Work/Spatial_Files/Study_sites_poly_large.kml") # Bring in hand drawn polygon from google earth
writeOGR(obj = poly, dsn = "/Users/aaronskinner/Library/CloudStorage/OneDrive-UBC/Grad_School/PhD/Field_Work/Spatial_Files/Study_sites_polygon_SELVA", layer = "Study_sites_polygon", driver = "ESRI Shapefile") # Convert to a shapefile

## Map for ANLA permit
poly_sf <- st_as_sf(poly)
study_farms <- sf_all[sf_all$name == 3849 | sf_all$name == 268 | sf_all$name == 1522 | sf_all$name == 1025 | sf_all$name == 89 | sf_all$name == 280, c("name", "geometry")] # 1522 is one of Andorra predios (to my understanding)

# Map to ensure that polygon and point coordinates line up as expected
ggplot(data = poly_sf) +
  geom_sf() +
  geom_sf(data = study_farms) +
  ggrepel::geom_text_repel(data = study_farms, aes(label = name, geometry = geometry), stat = "sf_coordinates")

# >Polygons & Telemetry --------------------------------------------------
## Check to see if there is polygon LC data for the farms where telemetry was done,
PgoLB <- merge(PgoLB, deltaDF[, c("ID_2", "Region")], by.y = "ID_2", by.x = "ID_Predio")

TelemPgo <- PgoLB %>% filter(ID_Predio == 731 | ID_Predio == 728 | ID_Predio == 3189)
TelemPgo %>%
  group_by(ID_Predio) %>%
  count() # There is!
TelemPgo %>% print(n = Inf) # PgoCierre does not have the farms in question, damn! I.e. all 2012 & 2013
PgoLB %>%
  filter(Region == "Quindio") %>%
  ggplot() +
  geom_sf() +
  geom_sf(data = TelemPgo, color = "red")
ggsave("/Users/aaronskinner/Library/CloudStorage/OneDrive-UBC/Grad_School/PhD/Field_Work/Spatial_Files/Eje_cafetero_LC_linea_base_Telemetry_Farms.pdf", bg = "white")

deltaDF %>% filter(ID_2 == 731 | ID_2 == 728 | ID_2 == 3189) # Notice 728 did increase substantially in linear features (although farm is small)

# >oSCR spatial capture recap ----------------------------------------------
# Gates Dupont and others have some good vignettes, see this and all the associated links: https://bookdown.org/chrissuthy/oSCRvignettes/
# Also a user group: https://groups.google.com/forum/#!forum/oscr_package
# Idea for setting up study design is pretty cool, generate all possible trap locations using st_sample function, filter trap locations by what is feasible (e.g., elevation, slope, or "no go" polygons), then use the scrdesignGA function stating a specific criteria (e.g. Qp) to select the optimal subset of traps among all available trap locations. Finally, can use simulator() function (Dupont created) to see whether your optimal trap location design will recover the simulated parameter estimates.

library(oSCR) # Can easily incorporate telemetry data. Also see paper, "Improved inferences about landscape connectivity from spatial capture–recapture by integration of a movement model"
# Some key functions:
?telemetry # This brings up a copyable script from Royle et al (2013), using the data("nybears") set. Similar to vignette 1 (integrating telmetry data -  Integrated RSF-SCR models in oSCR)
?e2dist # Distance between trap locations and activity centers
?scrdesignGA # Create optimal trap location design. Would have to specify beta0 and sigma parameters, which seems like you would need a good amount of baseline data. Crit = the 3 Qp values in the Dupont (2021) article?
?data2oscr # These two functions seem key for making the capture array
?make.scrFrame
?oSCR.fit # Fit models
