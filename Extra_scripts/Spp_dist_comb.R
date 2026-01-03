## PhD birds in silvopastoral landscapes##
# Data exploration 01 -- Species distributions & observations outside of range

# Contents
# 1) Elev out of range -- Compare each species observation to its elevational range, creating categorical 'Out' column and the continuous 'Amount_out_m' column
# 2) Distribution out of range -- Compare each species observation to its distributional range (shapefile) using a for loop
# 3) Fuera rango files -- Generate Excel files to help review the species out of range (for collaborators) based on results of steps 1 & 2
# 4) Distribution maps -- Plot species observations along with species range map (Ayerbe, 2018) on background elevation of Colombia map  

### Notes: 
## Certain species were actually in the original Ayerbe files but named differently.. I updated these manually. I never received clarification why some of these have "_M" or "_1" at the end of the file names.
# "Accipiter bicolor" (didn't have all files), "Crypturellus soui_1", "Parkesia noveboracensis_M" Picumnus olivaceus_M",  "Scytalopus latrans_M", "Nyctibius_grandis", "Machaeropterus regulus" -> "Machaeropterus striolatus"

## NOTES:: I have made two versions of this script: 01a_Spp_dist_comb & 01b_Spp_dist_sep. This script (01a_Spp_dist_comb) produces combined outputs from all data collectors (e.g., for me to work on, to review with Nick), whereas 01b_Spp_dist_sep produces outputs specific to each unique database (e.g., if giving products to data collector for their review). Previously there were a few places where code had to be changed (found by searching): "IF data collector" (followed by 'merged' or 'separate').

# Libraries & data --------------------------------------------------------
## Load data
Bird_pcs_all <-  read_csv(file = "Derived/Excels/Bird_pcs/Bird_pcs_all.csv")
Bird_pcs_in_range <- read_csv("Derived/Excels/Bird_pcs/Bird_pcs_in_range.csv")
Elev_ranges <- read_csv(file = "Derived/Excels/Elev_ranges_all_sources.csv")
Site_covs <- read_csv(file = "Derived/Excels/Site_covs.csv")
Event_covs <- read_csv(file = "Derived/Excels/Event_covs.csv")

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

# Select Bird_df ----------------------------------------------------------
# Data frame
#Bird_df <- Bird_pcs_all
Bird_df <- Bird_pcs_in_range

# File name for pdf if printing
file_name <- "Out_range_after_adjustment"
#file_name <- "Out_range_before_adjustment"

# Elev out of range ------------------------------------------------------------
# Look for possible species misidentifications or other errors in the data
spp_obs_sf <- Bird_df %>%
  mutate(Species_ayerbe = ifelse(Species_ayerbe == "Machaeropterus striolatus", "Machaeropterus regulus", Species_ayerbe)) %>% 
  left_join(Site_covs) %>% 
  left_join(Event_covs) %>%
  st_as_sf(coords = c("Long", "Lat"), crs = 4326, remove = F) %>%
  select(-c(Habitat, Habitat_sub, Tot_prec))

# Elevational ranges from Quintero Jetz et al 2018 & Freeman (2022)
# Add elevation out of range data to spp_obs_sf
# Compare each species observation to its elevational range, creating categorical 'Out' column and the continuous 'Amount_out_m' column
spp_obs_sf_El <- spp_obs_sf %>% # st_drop_geometry()
  left_join(
    Elev_ranges[, c(
      "Species_ayerbe", "Min_elev_comb", "Max_elev_comb", "Year", "Source_comb_elev"
    )]) %>%
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
  group_by(Species_ayerbe, Out) %>% # Uniq_db,
  mutate(Amount_out_m = max(Amount_out_m, na.rm = T)) %>%
  distinct(
    Species_ayerbe, Min_elev_comb, Max_elev_comb, # Uniq_db,
    Out, Amount_out_m, Elev_range_year, Source_comb_elev
  ) %>%
  arrange(desc(Amount_out_m)) # Uniq_db,

# Identify species that are found both above and below the published range limits
sppHL <- spp_obs_sf_El %>%
  st_drop_geometry() %>%
  filter(Out != "FALSE") %>%
  count(Species_ayerbe, Out) %>%
  count(Species_ayerbe) %>%
  filter(n > 1) %>%
  pull(Species_ayerbe)

# Ayerbe shapefiles -------------------------------------------------------
# Use Ayerbe shapefiles 
Ayerbe_path <- "../Geospatial_data/Ayerbe_shapefiles_1890spp/"
Ayerbe_all_spp <- list.files(path = Ayerbe_path, pattern = "\\.dbf$")
Ayerbe_all_spp <- substr(Ayerbe_all_spp, 1, nchar(Ayerbe_all_spp) - 4) # Remove the .dbf extension

Spp_names <- Bird_df %>% 
  filter(Species_ayerbe %in% Ayerbe_all_spp & 
           Species_ayerbe !=  "Accipiter bicolor") %>%
  pull(Species_ayerbe) %>% 
  unique()

Ayerbe_sf <- map_dfr(Spp_names, \(Spp){
  st_read(paste0(Ayerbe_path, Spp, ".shp")) %>% 
    st_make_valid()
}) 
Ayerbe_sf2 <- Ayerbe_sf %>% 
  rename(Species_ayerbe = Nombre) %>% 
  mutate(Species_ayerbe = str_remove_all(Species_ayerbe, "_1|_M"))

# Distribution out of range ----------------------------------------------
# Compare each species observation to its distributional range (shapefiles) 
# Identify species that are out of elevational range & make maps
sf_use_s2(FALSE) # Will cause errors otherwise

# Create files to store outputs from for loop
range <- aspp_obs_sf <- output <- dist_plots <- TF <- OutRangeObs <- DistsKM <- list()
PCsTF <- maxDist <- InRange <- OutRange <- vector()
# Removes files that don't have a match in the shapefiles
spp_obs_sf2 <- spp_obs_sf_El %>% 
  filter(Species_ayerbe %in% Ayerbe_sf2$Species_ayerbe)

## IF data collectors separate... Change Uniq_db
# For loop comparing observed distributions to Ayerbe range maps
for (i in 1:length(Spp_names)) {
  print(i)
  range[[i]] <- Ayerbe_sf2 %>%
    filter(Species_ayerbe == Spp_names[i]) %>%
    slice_head()
  aspp_obs_sf[[i]] <- spp_obs_sf2 %>% 
    filter(Species_ayerbe == Spp_names[i]) # & Uniq_db == SppDb[i,2]
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

names(aspp_obs_sf) <- Spp_names
names(range) <- Spp_names

# Fuera rango Excel -----------------------------------------------------
## Create Excels for data collectors
OutRangeObsDf <- OutRangeObs %>%
  bind_rows() %>%
  rename(DistsKM = DistsKM..i..) %>%
  dplyr::select(Uniq_db, Nombre_institucion, Count, Departamento, Id_muestreo, Species_ayerbe, Fecha, Lat, Long, DistsKM, Elev) %>%
  filter(!is.na(DistsKM)) # Remove spp that didn't have associated shapefile (eg Leptotila verreauxi)
nrow(OutRangeObsDf)

## IF data collectors separate... Change Uniq_db
FueraRango <- OutRangeObsDf %>%
  group_by(Species_ayerbe, Nombre_institucion) %>% # Uniq_db
  slice_max(DistsKM, n = 1, with_ties = FALSE) %>%
  full_join(filter(ElevOut, Amount_out_m > 150), by = c("Species_ayerbe")) %>% # , "Uniq_db"
  ungroup() %>%
  arrange(Species_ayerbe, desc(DistsKM)) %>% # Uniq_db,
  mutate(across(where(is.numeric), \(x) round(x, 2))) %>%
  select(-Amount_out_m)

FueraRangoCIPAV <- FueraRango %>% filter(Nombre_institucion == "Cipav")
FueraRangoGAICA <- FueraRango %>% filter(Nombre_institucion == "Gaica")
FueraRango %>% filter(Nombre_institucion == "Ubc gaica")

# These are only 24 additional species not reviewed by GAICA that are 50+ km out of range
FueraRangoCIPAV %>%
  anti_join(FueraRangoGAICA, "Species_ayerbe") %>%
  filter(DistsKM > 50)

# NOTE::The numbers of observations is dependent upon whether recorridos libres are removed or not
FueraRango %>%
  group_by(Nombre_institucion) %>%
  count()
FueraRango %>%
  group_by(Species_ayerbe) %>%
  summarize(Log_DistsKM = log10(max(DistsKM)), DistsKM = max(DistsKM)) %>%
  ggplot() +
  geom_histogram(aes(x = DistsKM)) +
  scale_x_continuous(trans = "log10", breaks = c(0.1, 1, 10, 100, 1000), label = c("0.1", "1", "10", "100", "1000"))

# Export Excel files
# Create combined file irrespective of data collector
FueraRangoExport <- FueraRango %>%
  select(-c(Uniq_db, Id_muestreo)) %>%
  mutate(
    Nombre_cambiado = NA, Departamentos_afectados = NA,
    Recomendacion = NA, Certeza = NA, Observaciones_adicionales = NA
  ) %>%
  relocate(Nombre_cambiado, .after = Species_ayerbe)
# Export combined file for Nick
#FueraRangoExport %>% data.frame() %>%
# write.xlsx(file = "Derived/Fuera_rango/Excels/Combined/Fuera_Rango_Comb12.26.25.xlsx", sheetName = "FueraRango", row.names = F, showNA = F)

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

DistInd <- tibble(Species_ayerbe = Spp_names[i], maxDist = maxDist[which(maxDist > 0)], i = i, InRange = InRange[i], OutRange = OutRange[i]) %>% 
  arrange(desc(maxDist)) # Uniq_db
# CHECK:: Ensure that the i's from the DistInd df and the SppDb match up.. Ie these two lines of code should both pull up the same species
DistInd %>% filter(Species_ayerbe == "Setophaga cerulea")
Spp_names[409]

# Join with elevational outliers
DistIndEl <- DistInd %>%
  left_join(filter(ElevOut, Amount_out_m > 150), by = c("Species_ayerbe")) %>% # Uniq_db
  mutate(
    Out = ifelse(is.na(Out), "FALSE", Out),
    Amount_out_m = ifelse(is.na(Amount_out_m), 0, Amount_out_m)
  )
# Notice there are several species that are in range according to the distribution maps but out of range according to the elevations. For now not going to worry about these but may want to confirm with Nick
ElevOut %>%
  filter(Amount_out_m > 150) %>%
  filter(!Species_ayerbe %in% DistInd$Species_ayerbe)

## Plotting loop
# Select a species with all 5 data collectors, order them and save object
Instituto_ordered <- aspp_obs_sf$`Amazona ochrocephala` %>% 
  arrange(Nombre_institucion) %>%
  pull(Nombre_institucion) %>% 
  unique()

# Plotting distribution maps for loop
aspp_obs_sfEl <- list()
for (p in 1:nrow(DistIndEl)) {
  i <- DistIndEl[p, ]$i
  print(paste("p =", p))
  print(paste("i =", i))
  aspp_obs_sf[[i]] <- aspp_obs_sf[[i]] %>% st_jitter(factor = .03)
  aspp_obs_sfEl[[i]] <- aspp_obs_sf[[i]] %>% filter(Amount_out_m > 150)
  dist_plots[[p]] <- Col_alt_map + 
    geom_sf(data = range[[i]], alpha = .5) +
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
    scale_shape_manual(values = c(0:4), breaks = Instituto_ordered) +
    # Plot points out of distributional range and out of elevational range
    geom_sf(
      data = aspp_obs_sfEl[[i]], 
      aes(size = Amount_out_m, shape = Nombre_institucion),
      color = "red", alpha = .6
    ) +
    scale_size_continuous(limits = c(150, 1800), range = c(2, 10)) + # breaks = c(#, #2, #3, etc.) doesn't work
    coord_sf(label_axes = "----") +
    ggtitle(paste(
      DistIndEl[DistIndEl$i == i, "Species_ayerbe"], "\n",
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
# Select a species with all 5 data collectors and create a fake plot w/ a legend to extract.
p_legend <- ggplot(data = aspp_obs_sf$`Amazona ochrocephala`) +
  geom_sf(aes(shape = Nombre_institucion, size = Amount_out_m)) +
  scale_shape_manual(values = c(0:4), breaks = Instituto_ordered) +
  scale_size_continuous(limits = c(150, 1800), range = c(2, 10))
# Extract legend and insert it into the last slot of the list
legend <- ggpubr::get_legend(p_legend)
dist_plots[[length(dist_plots) + 1]] <- legend
# Test
#dist_plots[[1]]

# PDF with maps -----------------------------------------------------------
#Print PDF file with 9 plots per page

if(TRUE){
  pdf(file = paste0("Derived/Fuera_rango/Maps/", file_name, format(Sys.Date(), "%m.%d.%y"), ".pdf"), width = 8.5, height = 11, bg = "white")
  print(marrangeGrob(grobs = dist_plots, ncol = 3, nrow = 3, layout_matrix = matrix(1:9, 3, 3, TRUE)))
  dev.off() 
}
stop() 

# Remove / change species -------------------------------------------------
## Using the PDF maps generated in this script, the data collectors (GAICA), Nick and I assessed how likely each species observation was outside of range. We based this on the distinctiveness of the species, the presence of similar species (especially congeners), and the presence or absence of major barriers (e.g., mountains).

# Manually generated list of species to remove / change
Remove_change <- read_csv("Derived/Excels/Spp_remove_change.csv") %>% 
  select(-c(Editor, Observaciones)) %>% 
  # Create 1 row per department
  separate_rows(Departamentos_afectados, sep = ",\\s*") 
Remove_change %>% filter(Species_ayerbe == "Henicorhina leucophrys")

# Add department information for each bird observation
Bird_pcs_all2 <- Bird_pcs_all %>%
  left_join(Site_covs[, c("Id_muestreo_no_dc", "Departamento", "Elev")])

# Separate 1) remove vs change and 2) department-specific vs global
Remove_dept <- Remove_change %>% 
  filter(Recomendacion == "Remove" & !is.na(Departamentos_afectados))
Remove_all <- Remove_change %>% anti_join(Remove_dept) %>% 
  filter(Recomendacion == "Remove")
Change_dept <- Remove_change %>% 
  filter(Recomendacion == "Change" & !is.na(Departamentos_afectados))
Change_all <- Remove_change %>% 
  filter(Recomendacion == "Change" & is.na(Departamentos_afectados))

# Around Santa Marta area Henicorhina leucophrys > 600m are OK 
row_add <- Bird_pcs_all2 %>%
  filter(Species_ayerbe == "Henicorhina leucophrys" & Departamento == "Guajira" & Elev > 600)

# Apply removes first (anti_join)
Bird_pcs_all3 <- Bird_pcs_all2 %>% 
  anti_join(
    Remove_dept, 
    by = c("Species_ayerbe", 
           "Departamento" = "Departamentos_afectados")
  ) %>% anti_join(Remove_all) %>% 
  # Add back in single Henicorhina leucophrys observation
  bind_rows(row_add) %>% 
  select(-Elev)

## Join the 'Change dataframes' which have Species_cambiado, and then apply the changes with mutate

# This requires an extra step because the first join adds columns that then prevent correct matching in the second join. Create custom function to implement changes
implement_changes <- function(df){
  df %>% mutate(Species_ayerbe = ifelse(
    !is.na(Species_cambiado), Species_cambiado, Species_ayerbe
  ))
}

# Swap species using 'Change' data frames
Bird_pcs_all4 <- Bird_pcs_all3 %>% 
  left_join(
    Change_dept, 
    by = c("Species_ayerbe", 
           "Departamento" = "Departamentos_afectados")
  ) %>% implement_changes() %>% 
  # Remove columns before next join
  select(-c(Species_cambiado, Recomendacion)) %>%
  left_join(Change_all) %>%
  implement_changes() 

# Examine / confirm -----------------------------------------------------
## Examine to ensure that code worked as expected 
# Removed 41 observations
nrow(Bird_pcs_all) - nrow(Bird_pcs_all4)
# Remove 12 species 
Spp_og <- Bird_pcs_all %>% pull(Species_ayerbe) %>% unique()
Spp_fin <- Bird_pcs_all4 %>% pull(Species_ayerbe) %>% unique()
length(Spp_og) - length(Spp_fin)

# These are the species that should have been removed
Spp_rm <- Remove_change %>% 
  filter(Recomendacion == "Remove" & is.na(Departamentos_afectados)) %>% 
  pull(Species_ayerbe)
# Should be 0 rows
Bird_pcs_all4 %>% filter(Species_ayerbe %in% Spp_rm)

# Example species - Myiarchus apicalis (changed to ferox)
Bird_pcs_all %>% filter(Species_ayerbe == "Myiarchus ferox") # Originally 12
Bird_pcs_all4 %>% filter(Species_ayerbe == "Myiarchus ferox") # Final of 31
# 19 apicalis changed -> ferox in Meta; 4 removed in Guajira
Bird_pcs_all2 %>% filter(Species_ayerbe == "Myiarchus apicalis") %>% 
  tabyl(Departamento)
Bird_pcs_all4 %>% filter(Species_ayerbe == "Myiarchus apicalis") %>% 
  tabyl(Departamento) # In correct departments

# Export ------------------------------------------------------------------
Bird_pcs_export <- Bird_pcs_all4 %>% 
  select(-c(Species_cambiado, Recomendacion, contains("Departamento")))
#Bird_pcs_export %>% write_csv("Derived/Excels/Bird_pcs/Bird_pcs_in_range.csv")

# EXTRAS ------------------------------------------------------------------
# I left this code as these were previously important steps in the workflow. The three components are 1) examining Robert & Yuri's recommendations for the species outside of known distribution, and 2) confirming the observations / distributions for the three species that had no matches with Ayerbe (2018) taxonomy

# >Examine GAICA Fuera Rango ------------------------------------------------
# Bring in Robert & Yuri's recommendations for Species outside of known distribution
FR_Rev_RY <- read_xlsx("../FueraRango/Excels/GAICA/FueraRangoGAICA7.20.23_RevR&Y.xlsx", sheet = "FueraRango") %>%
  rename(Observaciones_adicionales = `Observaciones adicionales`)

# First, determine if there are any species that occurred in multiple data bases, & whether R&Ys recommendations changed in any cases
Ayerb_mult <- FR_Rev_RY %>%
  count(Species_ayerbe, sort = T) %>%
  filter(n == 2) %>%
  pull(Species_ayerbe)
# Note Mecocerculus minor has 2 different recommendations, & these are supported by species' biology & the habitat type where the species is reported. Thus I made manual change in Excel to Remove distancia & keep MBD results
FR_Rev_RY %>%
  filter(Species_ayerbe %in% Ayerb_mult) %>%
  arrange(Species_ayerbe) %>%
  select(Species_ayerbe, Recomendacion, Certeza)

# General formatting: Add checked.by column, take row w/ max Dist outside of range (@ present there is a row for each Uniq_db), and
FR_Rev_RY2 <- FR_Rev_RY %>%
  mutate(
    Checked.by = "GAICA",
    Observaciones_adicionales = ifelse(Observaciones_adicionales == "Ampliacion de rango", "", Observaciones_adicionales)
  ) %>%
  group_by(Species_ayerbe) %>%
  slice_max(DistsKM) %>%
  ungroup()

# Merge with FueraRango so the GAICA suggestions are now incorporated in same df with CIPAV rows
Fuera.rango.RYrecs <- FR_Rev_RY2 %>%
  select(Species_ayerbe, Nombre_institucion, Nombre_Cambiado, 15:19) %>%
  right_join(FueraRango, by = c("Species_ayerbe", "Nombre_institucion"))

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
            by = c("Species_ayerbe" = "Species_ayerbe")
  ) %>%
  relocate(Common.name, .after = Species_ayerbe)

# Organize by distance & species
Fuera.rango.RYrecs3 <- Fuera.rango.RYrecs2 %>%
  mutate(row_order = row_number(desc(DistsKM))) %>%
  group_by(Species_ayerbe) %>%
  mutate(Dists.ord = min(row_order)) %>%
  arrange(Dists.ord, Species_ayerbe) %>%
  select(-c(Dists.ord, row_order))

nrow(Fuera.rango.RYrecs3) # 267

# Save to FueraRango -> Excels folder
# write.xlsx(data.frame(Fuera.rango.RYrecs3),
#          file = paste0("Fuera_rango_all_RY_", format(Sys.Date(), "%m.%d.%y"), ".xlsx"),
#         sheetName = "FueraRango", row.names = F, showNA = F)

# >Missing species: Confirm within range -------------------------------------------
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