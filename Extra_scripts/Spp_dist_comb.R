## PhD birds in silvopastoral landscapes##
# Data exploration 01 -- Species distributions & observations outside of range
## This script.

# Contents
# 1) Elev out of range -- Compare each species observation to its elevational range, creating categorical 'Out' column and the continuous 'Amount_out_m' column
# 2) Distribution out of range -- Compare each species observation to its distributional range (shapefile) using for loop
# 3) Fuera rango files -- Generate Excel files to help review the species out of range (for collaborators) based on results of steps 1 & 2
# 4) Distribution maps -- Plot species observations along with species range map (Ayerbe, 2018) on background elevation of Colombia map 

# NOTES:: I have made two versions of this script: 01a_Spp_dist_comb & 01b_Spp_dist_sep. This script (01a_Spp_dist_comb) produces combined outputs from all data collectors (e.g., for me to work on, to review with Nick), whereas 01b_Spp_dist_sep produces outputs specific to each unique database (e.g., if giving products to data collector for their review). Previously there were a few places where code had to be changed (found by searching): "IF data collector" (followed by 'merged' or 'separate').

# Libraries & data --------------------------------------------------------
load("Rdata/the_basics_05.10.25.Rdata")
load("Rdata/Taxonomy_12.29.24.Rdata")
load("Rdata/Traits_elev_12.29.24.Rdata")
source("/Users/aaronskinner/Library/CloudStorage/OneDrive-UBC/Grad_School/Rcookbook/Themes_funs.R")

# Bring in libraries
library(tidyverse)
library(cowplot)
library(raster)
library(sf)
library(sp)
library(terra)
library(tidyterra)
library(gtools)
library(readxl)
library(xlsx)
library(ebirdst)
library(gridExtra)
library(conflicted)
ggplot2::theme_set(theme_cowplot())
conflicts_prefer(dplyr::select)
conflicts_prefer(dplyr::filter)

# Elev out of range ------------------------------------------------------------
# Look for possible species misidentifications or other errors in the data
spp_obs_sf <- Birds_all3 %>%
  st_as_sf(coords = c("Longitud_decimal", "Latitud"), crs = 4326, remove = F) %>%
  left_join(Envi_df2[, c("Id_muestreo", "Elev")])
## CHECK:: The polygon is the same for all 3 previous names of this species
Ayerbe_sf2 %>%
  filter(Nombre_ayerbe == "Spinus psaltria") %>%
  pull(geometry)

# Compare reported elevations (in field) to those from the Colombia DEM
ElevErrors <- Bird_pcs %>%
  left_join(Envi_df2[, c("Id_muestreo", "Elev")]) %>%
  select(Id_muestreo, Latitud, Longitud_decimal, Elevacion, Elev) %>%
  rename(Elevacion_reported = Elevacion) %>%
  mutate(Difference_elev = Elev - Elevacion_reported) %>%
  arrange(desc(Difference_elev)) %>%
  filter(Difference_elev > 150) %>%
  distinct()

#write.xlsx(ElevErrors, "/Users/aaronskinner/Library/CloudStorage/OneDrive-UBC/Grad_School/PhD/Analysis/FueraRango/Elevation_dists_and_errors.xlsx", sheetName = "ElevErrors", row.names = F)

# Elevational ranges from Quintero Jetz et al 2018 & Freeman (2022)
# Add elevation out of range data to spp_obs_sf
# Compare each species observation to its elevational range, creating categorical 'Out' column and the continuous 'Amount_out_m' column
spp_obs_sf_El <- spp_obs_sf %>% # st_drop_geometry()
  left_join(
    Elev_ranges[, c(
      "Species_ayerbe", "Species_bt", "Match.type",
      "Min_elev_comb", "Max_elev_comb", "Year", "Source_elev"
    )],
    by = join_by("Nombre_ayerbe" == "Species_ayerbe")
  ) %>%
  mutate(Out = case_when(
    Elev < Min_elev_comb ~ "Low",
    Elev > Max_elev_comb ~ "High",
    TRUE ~ "FALSE"
  )) %>%
  mutate(Amount_out_m = case_when(
    Out == "Low" ~ Min_elev_comb - Elev,
    Out == "High" ~ Elev - Max_elev_comb,
    TRUE ~ 0
  )) %>%
  rename(Elev_range_year = Year)

# Create ElevOut data base, with a single row for each species
## IF data collectors separate... Change Uniq_db
ElevOut <- spp_obs_sf_El %>%
  st_drop_geometry() %>% # Elevational distributions out of range (ElevOut)
  filter(Out != "FALSE") %>%
  group_by(Nombre_ayerbe, Out) %>% # Uniq_db,
  mutate(Amount_out_m = max(Amount_out_m, na.rm = T)) %>%
  distinct(
    Nombre_ayerbe, Min_elev_comb, Max_elev_comb, # Uniq_db,
    Out, Amount_out_m, Elev_range_year, Source_elev
  ) %>%
  arrange(desc(Amount_out_m)) # Uniq_db,

## NOTE:: It doesn't look like there are any strong patterns in the amount out of range depending on the source
table(ElevOut$Source_elev)
ElevOut <- ElevOut %>% mutate(Source_elev = factor(Source_elev, levels = c("QJ", "Freeman", "Both")))
summary(lm(Amount_out_m ~ Source_elev, data = ElevOut))

# Identify species that are found both above and below the published range limits
sppHL <- spp_obs_sf_El %>%
  st_drop_geometry() %>%
  filter(Out != "FALSE") %>%
  count(Nombre_ayerbe, Out) %>%
  count(Nombre_ayerbe) %>%
  filter(n > 1) %>%
  pull(Nombre_ayerbe)

# Distribution out of range ------------------------------------------------------------
# Compare each species observation to its distributional range (shapefiles) 
# Identify species that are out of elevational range & make maps
sf_use_s2(FALSE) # Will cause errors otherwise

# Create files to store outputs from for loop
range <- aspp_obs_sf <- output <- dist_plots <- TF <- OutRangeObs <- DistsKM <- list()
PCsTF <- maxDist <- InRange <- OutRange <- vector()
# Removes files that don't have a match in the shapefiles
spp_obs_sf2 <- spp_obs_sf_El %>% filter(Nombre_ayerbe %in% Ayerbe_sf2$Nombre_ayerbe)

# IMPORTANT!! Just point counts?
spp_obs_sf3 <- spp_obs_sf2 %>% filter(Protocolo_muestreo == "Punto conteo")
table(spp_obs_sf3$Protocolo_muestreo)

# Create data frame to cycle through each species
# IF data collectors merged.. Run this
SppDb <- data.frame(Nombre_ayerbe = unique(Tax_df3$Species_ayerbe))

# Remove the 3 species with no corresponding file in Ayerbe as this causes error in loop
sciNames <- unique(Tax_df3$Species_ayerbe)
TF_vec <- sciNames %in% Ayerbe_all_spp
missing_spp <- sciNames[!TF_vec]
SppDb <- SppDb %>% filter(!Nombre_ayerbe %in% c(missing_spp, "Accipiter bicolor"))

## IF data collectors separate... Change Uniq_db
# For loop comparing observed distributions to Ayerbe range maps
for (i in 1:nrow(SppDb)) {
  print(i)
  range[[i]] <- Ayerbe_sf2 %>%
    filter(Nombre_ayerbe == SppDb[i, 1]) %>%
    slice_head()
  aspp_obs_sf[[i]] <- spp_obs_sf3 %>% filter(Nombre_ayerbe == SppDb[i, 1]) # & Uniq_db == SppDb[i,2]
  output[[i]] <- st_disjoint(range[[i]], aspp_obs_sf[[i]])
  # This tells us whether all observations are within the range (I think)
  if (!identical(output[[i]][[1]], integer(0))) { # Could likely eliminate this if else if you messed around with any() and all(). Kept for now to not further introduce variation.
    TF[[i]] <- 1:nrow(aspp_obs_sf[[i]]) %in% output[[i]][[1]] # TRUE = Outside range
    PCsTF[i] <- any(TF[[i]])
  } else {
    PCsTF[i] <- F
  }
  if (PCsTF[i] == TRUE) {
    DistsKM[[i]] <- round(as.numeric(c(st_distance(aspp_obs_sf[[i]], range[[i]]) / 1000)), 1)
    maxDist[i] <- max(DistsKM[[i]])
    aspp_obs_sf[[i]] <- cbind(aspp_obs_sf[[i]], DistsKM[[i]])
    OutRangeObs[[i]] <- data.frame(aspp_obs_sf[[i]][TF[[i]], ])
    InRange[i] <- sum(!TF[[i]])
    OutRange[i] <- sum(TF[[i]])
  }
}

names(aspp_obs_sf) <- SppDb$Nombre_ayerbe
names(range) <- SppDb$Nombre_ayerbe

# Fuera rango Excel -----------------------------------------------------
## Create Excels for data collectors
OutRangeObsDf <- OutRangeObs %>%
  bind_rows() %>%
  rename(DistsKM = DistsKM..i..) %>%
  dplyr::select(Uniq_db, Nombre_institucion, Nombre_finca, Registrado_por, Count, Protocolo_muestreo, Departamento, Id_muestreo, Nombre_ayerbe, Fecha, Latitud, Longitud_decimal, DistsKM, Elev) %>%
  filter(!is.na(DistsKM)) # Remove spp that didn't have associated shapefile (eg Leptotila verreauxi)
nrow(OutRangeObsDf)

## IF data collectors separate... Change Uniq_db
FueraRango <- OutRangeObsDf %>%
  group_by(Nombre_ayerbe, Nombre_institucion) %>% # Uniq_db
  slice_max(DistsKM, n = 1, with_ties = FALSE) %>%
  full_join(filter(ElevOut, Amount_out_m > 150), by = c("Nombre_ayerbe")) %>% # , "Uniq_db"
  ungroup() %>%
  arrange(Nombre_ayerbe, desc(DistsKM)) %>% # Uniq_db,
  mutate(across(where(is.numeric), \(x) round(x, 2))) %>%
  select(-Amount_out_m)

FueraRangoCIPAV <- FueraRango %>% filter(Nombre_institucion == "CIPAV")
FueraRangoGAICA <- FueraRango %>% filter(Nombre_institucion == "GAICA")

# These are only 27 additional species not reviewed by GAICA that are 50+ km out of range
FueraRangoCIPAV %>%
  anti_join(FueraRangoGAICA, "Nombre_ayerbe") %>%
  filter(DistsKM > 50)

# NOTE::The numbers of observations is dependent upon whether recorridos libres are removed or not
FueraRango %>%
  group_by(Nombre_institucion) %>%
  count()
FueraRango %>%
  group_by(Nombre_ayerbe) %>%
  summarize(Log_DistsKM = log10(max(DistsKM)), DistsKM = max(DistsKM)) %>%
  ggplot() +
  geom_histogram(aes(x = DistsKM)) +
  scale_x_continuous(trans = "log10", breaks = c(0.1, 1, 10, 100, 1000), label = c("0.1", "1", "10", "100", "1000"))

# Export Excel files
# Create combined file irrespective of data collector
FueraRango %>%
  select(-c(Uniq_db, Nombre_finca, Registrado_por, Id_muestreo)) %>%
  data.frame() %>%
  mutate(
    Nombre_cambiado = NA, Departamentos_afectados = NA,
    Recomendacion = NA, Certeza = NA, Observaciones_adicionales = NA
  ) %>%
  relocate(Nombre_cambiado, .after = Nombre_ayerbe) # %>%
# Combined file for Nick
#write.xlsx(file = "Derived/Fuera_rango/Excels/Combined/Fuera_Rango_Comb12.10.23_v2.xlsx", sheetName = "FueraRango", row.names = F, showNA = F)

# Create data collector specific files
#write.xlsx(data.frame(FueraRangoCIPAV), file = "Derived/Fuera_rango/CIPAV/FueraRangoCIPAV_12.11.23.xlsx", sheetName = "FueraRango", row.names = F)
#write.xlsx(data.frame(FueraRangoGAICA), file = "Derived/Fuera_rango/GAICA/FueraRangoGAICA_12.11.23.xlsx", sheetName = "FueraRango", row.names = F)

# Distribution maps -------------------------------------------------------
# Bring in Colombia spatial layers and then let's plot
load("Rdata/NE_layers_Colombia.Rdata")

#Provides elevation at a 1km resolution, which is fine for plotting but not great for extracting elevation for each record
Elev_1km <- geodata::elevation_30s(country = "Colombia", path = tempdir())
ColElev_df <- terra::as.data.frame(Elev_1km, xy = TRUE) %>% tibble()

# Elevatgeodata# Elevation map of Colombia to use as background
Col_alt_map <- ggplot() +
  geom_raster(data = ColElev_df, aes(x = x, y = y, fill = COL_elv_msk)) +
  scale_fill_viridis_c(trans = "log") + # Notice log transformation puts more emphasis on lower elevation changes
  labs(
    x = "Longitude",
    y = "Latitude",
    fill = "Elevation"
  ) +
  theme(legend.position = "none", axis.title = element_blank())


# Create df to cycle through for loop
i <- which(maxDist > 0)
length(maxDist)

DistInd <- SppDb %>%
  slice(i) %>%
  cbind(data.frame(maxDist = maxDist[which(maxDist > 0)], i = i, InRange[i], OutRange[i])) %>%
  arrange(desc(maxDist)) # Uniq_db
# CHECK:: Ensure that the i's from the DistInd df and the SppDb match up.. Ie these two lines of code should both pull up the same species

DistInd[DistInd$Nombre_ayerbe == "Setophaga cerulea", ]
SppDb[582, ]

# Join with elevational outliers
DistIndEl <- DistInd %>%
  left_join(filter(ElevOut, Amount_out_m > 150), by = c("Nombre_ayerbe")) %>% # Uniq_db
  mutate(
    Out = ifelse(is.na(Out), "FALSE", Out),
    Amount_out_m = ifelse(is.na(Amount_out_m), 0, Amount_out_m)
  )
# Notice there are several species that are in range according to the distribution maps but out of range according to the elevations. For now not going to worry about these but may want to confirm with Nick
ElevOut %>%
  filter(Amount_out_m > 150) %>%
  filter(!Nombre_ayerbe %in% DistInd$Nombre_ayerbe)

# Plotting distribution maps for loop
aspp_obs_sfEl <- list()
for (p in 1:nrow(DistIndEl)) {
  i <- DistIndEl[p, ]$i
  print(paste("p =", p))
  print(paste("i =", i))
  aspp_obs_sf[[i]] <- aspp_obs_sf[[i]] %>% st_jitter(factor = .03)
  aspp_obs_sfEl[[i]] <- aspp_obs_sf[[i]] %>% filter(Amount_out_m > 150)
  dist_plots[[p]] <- Col_alt_map + geom_sf(data = range[[i]], alpha = .5) +
    # geom_sf(data = neCol, fill = NA, col = "green", alpha = .5) +
    geom_sf(data = neColDepts, fill = NA, col = "orange", alpha = .5) +
    # Plot points in distributional range
    geom_sf(
      data = aspp_obs_sf[[i]][!TF[[i]], ], aes(shape = Nombre_institucion),
      size = 2, color = "black", alpha = .3
    ) +
    # Plot points out of distributional range and in elevational range
    geom_sf(
      data = filter(aspp_obs_sf[[i]][TF[[i]], ], Amount_out_m < 150),
      aes(shape = Nombre_institucion), size = 2, color = "red", alpha = .6
    ) +
    scale_shape_manual(values = c(0:4), breaks = c("CIPAV", "GAICA", "UBC_GAICA", "UBC", "UNILLANOS")) +
    # Plot points out of distributional range and out of elevational range
    geom_sf(
      data = aspp_obs_sfEl[[i]], aes(size = Amount_out_m, shape = Nombre_institucion),
      color = "red", alpha = .6
    ) +
    scale_size_continuous(limits = c(150, 1800), range = c(2, 10)) + # breaks = c(#, #2, #3, etc.) doesn't work
    coord_sf(label_axes = "----") +
    ggtitle(paste(
      DistIndEl[DistIndEl$i == i, "Nombre_ayerbe"], "\n",
      paste("maxDist =", maxDist[i], "km"), "\n",
      paste("# In =", InRange[i], " | "), paste("# Out =", OutRange[i])
    )) +
    theme(
      plot.title = element_text(size = 6, face = "plain", hjust = 0.5),
      legend.position = "none" 
    )
  # Annotate if out of elevational range
  if (!is.na(DistIndEl[p, ]$Out) & DistIndEl[p, ]$Out != "FALSE") {
    dist_plots[[p]] <- dist_plots[[p]] +
      labs(caption = paste0(DistIndEl[p, ]$Out, " - ", DistIndEl[p, ]$Amount_out_m, "m")) +
      theme(plot.caption = element_text(color = "red", size = 10, hjust = 0))
  }
  if(!is.na(range[[i]]$Og_name)){
    if(range[[i]]$Og_name != range[[i]]$Nombre_ayerbe) {
      print("Name change")
      # Will change this from synonym to Og_name
      dist_plots[[p]] <- dist_plots[[p]] + labs(caption = paste(range[[i]]$Og_name)) + theme(plot.caption = element_text(color = "blue", size = 6))
  }
  }
  #IF analyzing recorridos libres.. Run this
  # if((DistIndEl$Nombre_ayerbe[p] %in% Spp_Libres) & DistIndEl$Uniq_db[p] == "GAICA MBD"){
  # print("libre")
  # dist_plots[[p]] <- dist_plots[[p]] + labs(caption = "Recorridos libres") + theme(plot.caption = element_text(color = "red", size = 6, hjust = 0))
  # }
}

# Plot spp. observations on top of distributions
dist_plots[sapply(dist_plots, is.null)] <- NULL
## IF data collectors separate... Run this
#Nums <- DistIndEl %>%
  #mutate(row = row_number()) %>%
  #group_by(Uniq_db) %>%
  #summarize(min_i = min(row), max_i = max(row))
#Nums2 <- Nums %>%
  #mutate(Inst = c("CIPAV", rep("GAICA", 2), "Skinner", "UniLlanos")) %>%
  #group_by(Inst) %>%
  #summarize(min_i = min(min_i), max_i = max(max_i)) # Inst = Institution
#length(dist_plots)
#for (i in 1:nrow(Nums2)) { #
#  pdf(file = paste0("/Users/aaronskinner/Library/CloudStorage/OneDrive-UBC/Grad_School/PhD/Analysis/FueraRango/DistributionsPCs/Test/DistributionsPCs_", Nums2$Inst[i], "12.11.23_v2.pdf"), width = 8.5, height = 11, bg = "white")
#  print(marrangeGrob(dist_plots[Nums2[i, ]$min_i:Nums2[i, ]$max_i], ncol = 3, nrow = 3, layout_matrix = matrix(1:9, 3, 3, TRUE)))
#  dev.off()
#}

# IF data collectors merged.. Run this
# Select a species with all 4 data collectors and create a fake plot w/ a legend to extract.
unique(aspp_obs_sf$Nombre)

p_legend <- ggplot(data = aspp_obs_sf$`Amazilia fimbriata`) +
  geom_sf(aes(shape = Nombre_institucion, size = Amount_out_m)) +
  scale_shape_manual(values = c(0:4), breaks = c("CIPAV", "GAICA", "UBC_GAICA", "UBC", "UNILLANOS")) +
  scale_size_continuous(limits = c(150, 1800), range = c(2, 10))
# Extract legend and insert it into the last slot of the list
legend <- ggpubr::get_legend(p_legend)
dist_plots[[length(dist_plots) + 1]] <- legend

#Print PDF file with 9 plots per page
if(FALSE){
  pdf(file = paste0("Derived/Fuera_rango/Maps/Combined/PCs_Comb_Departments2_", format(Sys.Date(), "%m.%d.%y"), ".pdf"), width = 8.5, height = 11, bg = "white")
  print(marrangeGrob(grobs = dist_plots, ncol = 3, nrow = 3, layout_matrix = matrix(1:9, 3, 3, TRUE)))
  dev.off() 
}

# EXTRAS ------------------------------------------------------------------
# I left this code as these were previously important steps in the workflow. The three components are 1) examining Robert & Yuri's recommendations for the species outside of known distribution, 2) confirming the observations / distributions for the three species that had no matches with Ayerbe (2018) taxonomy, 3) assessing other range map options (i.e., in addition to Ayerbe-Quinones 2018)

# >Examine GAICA Fuera Rango ------------------------------------------------
# Bring in Robert & Yuri's recommendations for Species outside of known distribution
FR_Rev_RY <- read_xlsx("/Users/aaronskinner/Library/CloudStorage/OneDrive-UBC/Grad_School/PhD/Analysis/FueraRango/Excels/GAICA/FueraRangoGAICA7.20.23_RevR&Y.xlsx", sheet = "FueraRango") %>%
  rename(Observaciones_adicionales = `Observaciones adicionales`)

# First, determine if there are any species that occurred in multiple data bases, & whether R&Ys recommendations changed in any cases
Ayerb_mult <- FR_Rev_RY %>%
  count(Nombre_ayerbe, sort = T) %>%
  filter(n == 2) %>%
  pull(Nombre_ayerbe)
# Note Mecocerculus minor has 2 different recommendations, & these are supported by species' biology & the habitat type where the species is reported. Thus I made manual change in Excel to Remove distancia & keep MBD results
FR_Rev_RY %>%
  filter(Nombre_ayerbe %in% Ayerb_mult) %>%
  arrange(Nombre_ayerbe) %>%
  select(Nombre_ayerbe, Recomendacion, Certeza)

# General formatting: Add checked.by column, take row w/ max Dist outside of range (@ present there is a row for each Uniq_db), and
FR_Rev_RY2 <- FR_Rev_RY %>%
  mutate(
    Checked.by = "GAICA",
    Observaciones_adicionales = ifelse(Observaciones_adicionales == "Ampliacion de rango", "", Observaciones_adicionales)
  ) %>%
  group_by(Nombre_ayerbe) %>%
  slice_max(DistsKM) %>%
  ungroup()

# Merge with FueraRango so the GAICA suggestions are now incorporated in same df with CIPAV rows
Fuera.rango.RYrecs <- FR_Rev_RY2 %>%
  select(Nombre_ayerbe, Nombre_institucion, Nombre_Cambiado, 15:19) %>%
  right_join(FueraRango, by = c("Nombre_ayerbe", "Nombre_institucion"))

# Take a single common name per Species Ayerbe.. This doesn't need to be perfect as this is just to help find these species in the book
Com.names <- Tax_df3 %>%
  group_by(Species_ayerbe) %>%
  arrange(Common.name, .by_group = TRUE) %>% # Believe this is putting NAs at bottom of each group
  select(Species_ayerbe, Common.name) %>%
  slice_head() %>%
  distinct(Species_ayerbe, Common.name)

# Add common names
Fuera.rango.RYrecs2 <- Fuera.rango.RYrecs %>%
  left_join(Com.names[, c("Species_ayerbe", "Common.name")],
    by = c("Nombre_ayerbe" = "Species_ayerbe")
  ) %>%
  relocate(Common.name, .after = Nombre_ayerbe)

# Organize by distance & species
Fuera.rango.RYrecs3 <- Fuera.rango.RYrecs2 %>%
  mutate(row_order = row_number(desc(DistsKM))) %>%
  group_by(Nombre_ayerbe) %>%
  mutate(Dists.ord = min(row_order)) %>%
  arrange(Dists.ord, Nombre_ayerbe) %>%
  select(-c(Dists.ord, row_order))

nrow(Fuera.rango.RYrecs3) # 267

# Save to FueraRango -> Excels folder
# write.xlsx(data.frame(Fuera.rango.RYrecs3),
#          file = paste0("Fuera_rango_all_RY_", format(Sys.Date(), "%m.%d.%y"), ".xlsx"),
#         sheetName = "FueraRango", row.names = F, showNA = F)

# >Missing species: Confirm within range -----------------------------------------------
# SKIP - Once visualized no need to continue to visualize
# Check guide books & other sources to understand these 2 species..
# From Avibase: For Sirystes sibilator -- 'This taxon is considered a subspecies of Sirystes [sibilator, albocinereus or subcanescens] (sensu lato) by some authors'
TF <- str_detect(Ayerbe_all_spp, "Sirystes")
Ayerbe_all_spp[TF]
Birds_all %>% filter(Nombre_cientifico_FINAL == "Sirystes sibilator")

# Need valid API key
spp_path <- map(missing_spp, \(miss_spp){
  ebirdst_download_status(species = miss_spp, pattern = "range_raw_lr")
})

# OLD
## Visually check a few species whose ranges couldn't be found in Ayerbe 2018 using eBird range maps
View(ebirdst_runs)
# Bring in eBird range maps
bichaw_path <- ebirdst_download(species = "Accipiter bicolor", pattern = "range_raw_lr") # Smallest files, can get smoothed if needed for aesthetics
bichaw_range <- load_ranges(bichaw_path, resolution = "lr", smoothed = F)
whtdov_path <- ebirdst_download(species = "Leptotila verreauxi", pattern = "range_raw_lr") # Smallest files, can get smoothed if needed for aesthetics
whtdov_range <- load_ranges(whtdov_path, resolution = "lr", smoothed = F)

# Plot these and confirm
whtdov_obs <- spp_obs_sf %>% filter(Og_name == "Leptotila verreauxi")
# If this is TRUE then all observations are within the range
all(1:nrow(whtdov_obs) %in% st_contains(whtdov_range, whtdov_obs)[[1]])
ggplot(data = whtdov_range) +
  geom_sf() +
  geom_sf(data = neCol, fill = NA, col = "green", alpha = .5) +
  geom_sf(data = whtdov_obs, color = "black", alpha = .2) +
  coord_sf(label_axes = "----") #+ ggtitle(paste(DistIndEl[DistIndEl$i == i, "Og_name"],"\n", paste("maxDist =", maxDist[i], "km"), "\n", paste("# In =", InRange[i], " | "), paste("# Out =", OutRange[i]))) + theme(plot.title = element_text(size = 6, face = "plain", hjust = 0.5), legend.position = "none")
bichaw_obs <- spp_obs_sf %>% filter(Og_name == "Accipiter bicolor")
all(1:nrow(bichaw_obs) %in% st_contains(bichaw_range, bichaw_obs)[[1]])
ggplot(data = bichaw_range) +
  geom_sf() +
  geom_sf(data = neCol, fill = NA, col = "green", alpha = .5) +
  geom_sf(data = bichaw_obs, color = "black", alpha = .2) +
  coord_sf(label_axes = "----")

# >eBird & BOTW - Other range maps - -------------------------------------
## I spent some time at the beginning assessing range maps options. I went with the Ayerbe (2018) range maps primarily due to the completeness of the taxonomic coverage (1890 species), and for convenience, as it was a smaller file (0.5 GB) and the structure of the file allowed me to bring in only the species that were observed in point counts (instead of all 1890 species).
# See vignette https://cran.r-project.org/web/packages/ebirdst/vignettes/ebirdst.html . Could also consider email Tom Auer for recordings or more information
# Workshop: http://strimas.com/ebirdst-workshop/access.html
# See names needed https://support.ebird.org/en/support/solutions/articles/48000804865-bird-names-in-ebird . eBird/Clements taxonomy
#vignette(package = "ebirdst")
#vignette(package = "ebirdst", topic = "rasters")

View(ebirdst_runs)
ebird <- filter(ebirdst_runs, breeding_range_modeled == TRUE)
table(sciNames %in% ebirdst_runs$scientific_name) # 291 spp. not in ebird
View(ebird %>% filter(breeding_range_modeled = TRUE) %>% dplyr::select(scientific_name))

# BOTW data
#botw_ranges <- st_read("/Users/aaronskinner/Library/CloudStorage/OneDrive-UBC/Grad_School/PhD/Analysis/BOTW_2022_2/BOTW.gdb") # Big file
head(botw_ranges)
st_layers(botw_ranges)
#st_layers(dsn = "/Users/aaronskinner/Library/CloudStorage/OneDrive-UBC/Grad_School/PhD/Analysis/Colombia-SCR/BOTW_2022_2/BOTW.gdb")
botw_spp <- unique(botw_ranges$sci_name)
# Col_botw <- st_intersects(botw_ranges, neCol) #Not recommended takes forever and produces this error: Error in s2_geography_from_wkb(x, oriented = oriented, check = check) : Evaluation error: Found 2206 features with invalid spherical geometry. Along w/ examples like Loop 4 is not valid: Edge 99 has duplicate vertex with edge 104. THIS IS PROBABLY BECAUSE NOT ALL SPP RANGES EXIST IN COLOMBIA. OTHER OPTION IS FILTER DOWN TO NECESSARY SPP (~500), THEN CLIP THEIR RANGES BY COLOMBIA TO MAKE THE FILE EVEN SMALLER
table(sciNames %in% botw_ranges$sci_name) # 33 spp. that aren't in the BOTW
table(gbif_names$species %in% botw_ranges$sci_name) # 40 spp. not in BOTW

# Can use this to search specific spp. names in BOTW data base
data.frame(botw_ranges %>% filter(str_detect(botw_ranges$sci_name, "rutilus")))

# Reassess list and our options
botw_missTF <- Ayerb_miss2 %in% botw_ranges$sci_name # 9 birds that could pull from BOTW
Ayerb_miss2[botw_miss]
botw_miss <- botw_ranges %>% filter(sci_name %in% Ayerb_miss2[botw_missTF])
ebird_miss <- Ayerb_miss2 %in% ebird$scientific_name # 6 T, but the 6 ebird is entirely included in the BOTW
Ayerb_miss2[ebird_miss]

rm(botw_ranges) # Use botw_spp to see how matches are coming along
