##PhD birds in silvopastoral landscapes##
##Data exploration 02 -- 
##Code to confirm data accuracy, explore data, look for biases that could influence analysis, & produce figures in the supporting information 

#Contents: Miscellaneous, see table of contents
# ) Number PCs / farm: Analysis to understand the extent of point counts in / out of SCR farms. & also the temporal coverage for each farm. 

#In the old repository 'Pilot_Colombia_V2' there is additional (rough) miscellaneous code like 1) testing the GAICA distancia database specifically

# Load libraries & data ---------------------------------------------------
library(readxl)
library(tidyverse)
library(sf)
library(chron)
library(hms)
library(ggpubr)
library(cowplot)
library(conflicted)
library(ggrepel)
library(naniar)
ggplot2::theme_set(theme_cowplot())
conflicts_prefer(dplyr::select)
conflicts_prefer(dplyr::filter)
conflicts_prefer(hms::hms)

load("Rdata/the_basics_09.15.25.Rdata")
load("Rdata/Taxonomy_12.29.24.Rdata")
load("Rdata/Traits_elev_11.14.24.Rdata")
source("/Users/aaronskinner/Library/CloudStorage/OneDrive-UBC/Grad_School/Rcookbook/Themes_funs.R")

# Checking habitat types Natalia ------------------------------------------
df_birds_red$Gaica_dist %>% left_join(Andrea) %>%
  filter(Id_muestreo == "G-AD-M-LD1_08") %>% 
  distinct(Habitat_ut, Habitat_cons)

Bird_pcs %>% filter(Id_muestreo == "G-MB-Q-EC_09") %>% 
  distinct(Habitat_ut, Latitud_decimal, Longitud_decimal)

Pc_locs_mult %>% 
  distinct(Id_muestreo, Latitud_decimal, Longitud_decimal) %>% 
  count(Id_muestreo, sort = T) %>% 
  filter(n > 1) %>% view()

Bird_pcs %>% 
  distinct(Id_muestreo, Latitud_decimal, Longitud_decimal) 

Bird_pcs %>% filter(Id_muestreo == "G-MB-M-EPO1_03")  %>% 
  distinct(Uniq_db, Id_muestreo, Ano, Latitud_decimal, Longitud_decimal) %>% 
  pull(Latitud_decimal) %>% 
  round(5)

# Change G-MB-M-EPO1_03 to 2013 / 2024 coords
# Check into EPO1_03(1) as well


# Understand data ---------------------------------------------------------
# Examine NAs using naniar package 
Bird_pcs %>% miss_var_summary()
Bird_pcs %>% gg_miss_var()
Bird_pcs %>% dplyr::slice_sample() %>%
  vis_miss()

#Lets break things down by the numbers for all databases
Method_Inst <- Birds_comb4 %>% distinct(Id_muestreo, Nombre_institucion, Uniq_db, Protocolo_muestreo, Ecoregion, Departamento, Nombre_finca) 
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
Pc_date8 %>%
  distinct(Id_muestreo, N_reps) %>% 
  ggplot(aes(x = N_reps)) +
  geom_histogram(color = "black", binwidth = 1) +
  xlab("# of repeat surveys per point count location")

# Boxplot of repeat surveys per department
Pc_date8 %>%
  reframe(N = n(), Departamento = Departamento, .by = Id_muestreo) %>%
  distinct(N, Departamento) %>%
  ggplot(aes(x = Departamento, y = N)) +
  geom_boxplot() +
  ylab("# of repeat surveys \nper point count location")

##Time of point counts
#Plot hours for each Uniq_db 
Sys.setenv(TZ='GMT')
ggplot(data = Birds_comb4, aes(x= Uniq_db, y = Hora)) + 
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
  left_join(distinct(Pc_date8[,c("Id_muestreo", "N_reps")]), by = "Id_muestreo")

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
  left_join(distinct(Pc_date8, Id_muestreo, Fecha), # add Fecha
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
  left_join(distinct(Pc_date8, Id_muestreo, Mes, Ano), # add Fecha
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
Pc_date8 %>%
  filter(Nombre_finca == "El Porvenir") %>%
  distinct(Id_gcs, Nombre_finca) %>%
  rename(Name_GCS = Nombre_finca)
Dist_farms %>% filter(Id_gcs == 3580)

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
# Boxplot showing the number of times each survey location was sampled by region and Año

Pc_date8 %>%
  distinct(Rep, Ecoregion, Ano_grp, Rep_season) %>%
  ggplot(aes(x = Ecoregion, y = Rep)) +
  geom_boxplot() +
  geom_jitter(width = 0.05, alpha = 0.2, aes(color = Ano_grp)) +
  scale_y_continuous(breaks = seq(0, 8, by = 2)) +
  ylab("# of repeat surveys \nper point count location") #+ geom_jitter(width=0.1,alpha=0.2) #+ geom_point(position=position_jitterdodge(),alpha=0.3)

# Boxplot showing the # of repeat surveys within each closed survey period by department
Pc_date8 %>%
  distinct(Rep, Ecoregion, Nombre_institucion, Ano_grp, Rep_season) %>%
  ggplot(aes(x = Ecoregion, y = Rep)) +
  geom_boxplot() +
  geom_jitter(width = 0.05, alpha = 0.2, aes(color = Nombre_institucion)) +
  scale_y_continuous(breaks = seq(0, 8, by = 2)) +
  ylab("# of repeat surveys per point \ncount location (closed period)") #+ geom_jitter(width=0.1,alpha=0.2) #+ geom_point(position=position_jitterdodge(),alpha=0.3)

#NOTE:: Using distinct(Id_muestreo, Mes_mod) is not a perfect solution as there are some point counts in different sampling periods that coincidentally happened to average out to the same Mes_mod value (within a single point count ID). For example...
Pc_date8 %>% 
  distinct(Mes_mod, Ano, Id_muestreo) %>% 
  count(Id_muestreo, Mes_mod, sort = T) %>%
  # When Mes_mod == 6.5 this isn't an issue as these are the Meta points sampled in 2016 and 2017, thus they really should be grouped together 
  filter(Mes_mod != 6.5) %>% 
  filter(n>1)
# Problematic point -- I already manually updated this in Pc_date8 so you can see Samp_Periods_n == 2 as it should be 
Pc_date8 %>% filter(Id_group == "G-MB-Q-ECOR") %>% 
  distinct(Mes_mod, Mes, Ano, Samp_Periods_n) %>% 
  arrange(Ano)

# Timeline ----------------------------------------------------------------

library(ggalt)  # For timeline aesthetics

# Data preparation
timeline_data <- data.frame(
  event = c("Silvopasture\nplanting", "GAICA", "CIPAV", "GAICA", "UniLlanos", "UBC", "UBC_Gaica"),
  year = c(2012, 2013, 2016, 2019, 2019, 2022, 2024),
  end = c(2013, 2013, 2016, 2019, 2019, 2022, 2024),
  num_locations = c(NA, 191, 128, 110, 54, 56, 45),
  habitats = c(NA, "Forest", rep(c("Forest, SSP, degraded pasture"), 4), "Forest")
)

timeline_data <- timeline_data %>%
  mutate(label = paste0(event, "\nN = ", num_locations))

# ATTEMPT 2 

# Define positions for labels to avoid overlap
timeline_data <- timeline_data %>%
  mutate(position = rep(c(0.2, -0.2), length.out = n()))  # Alternating text positions

# Define colors for institutions
institution_colors <- c("GAICA" = "#1f78b4", "CIPAV" = "#33a02c", "UniLlanos" = "#e31a1c", "UBC" = "#ff7f00", "UBC_Gaica" = "#6a3d9a")

# Create the timeline plot
ggplot(timeline_data, aes(x = year, y = 0, label = event)) +
  geom_hline(yintercept = 0, color = "black", size = 0.5) +  # Timeline line
  geom_segment(aes(y = position, yend = 0, xend = year), color = "black", size = 0.4) +  # Vertical lines
  geom_point(aes(y = 0, color = event), size = 4) +  # Milestone points
  geom_text(aes(y = position, label = event), size = 4, vjust = ifelse(timeline_data$position > 0, -0.5, 1.5)) +  # Labels
  scale_color_manual(values = institution_colors) +  # Custom colors
  theme_classic() +
  theme(axis.line.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(size = 12, face = "bold"),
        legend.position = "none")  # Hide legend

## GPT attempt 3 

# Data preparation
timeline_data <- data.frame(
  event = c("Silvopasture\nplanting", "GAICA", "CIPAV", "GAICA", "UniLlanos", "UBC", "UBC_Gaica"),
  year = c(2012, 2013, 2016, 2019, 2019, 2022, 2024),
  end = c(2013, 2013, 2016, 2019, 2019, 2022, 2024),
  num_locations = c(NA, 191, 128, 110, 54, 56, 45),
  habitats = c(NA, "Forest", rep(c("Forest, SSP, degraded pasture"), 4), "Forest")
)

timeline_data <- timeline_data %>%
  mutate(label = paste0(event, "\nN = ", num_locations), 
  label = ifelse(event == "Silvopasture\nplanting", "Silvopasture\nplanting", label))

# Define positions for labels to avoid overlap
timeline_data <- timeline_data %>%
  mutate(position = c(rep(c(0.3, -0.3), length.out = n())))  # **Increased spacing to extend y-axis further**

# Define colors for institutions
institution_colors <- c("GAICA" = "#1f78b4", "CIPAV" = "#33a02c", "UniLlanos" = "#e31a1c", 
                        "UBC" = "#ff7f00", "UBC_Gaica" = "#6a3d9a", "Silvopasture\nplanting" = "#a6cee3")

# Create the timeline plot
ggplot(timeline_data, aes(x = year, y = 0, label = event)) +
  
  # **Ensure the x-axis shows specific years**
  scale_x_continuous(breaks = seq(2012, 2024, by = 2)) +  
  
  geom_hline(yintercept = 0, color = "black", size = 0.5) +  # Timeline line
  
  # **Increase the gap between vertical lines and text by adjusting `y` values**
  geom_segment(aes(y = position * 0.8, yend = 0, xend = year), color = "black", size = 0.4) +  
  
  geom_point(aes(y = 0, color = event), size = 4) +  # Milestone points
  
  # **Adjust label spacing by shifting text positions**
  geom_text(aes(y = position, label = label), size = 4, vjust = ifelse(timeline_data$position > 0, -0.75, 1.75)) +  
  
  scale_color_manual(values = institution_colors) +  # Custom colors
  
  theme_classic() +
  theme(axis.line.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        
        # **Ensure x-axis labels are properly displayed**
        axis.text.x = element_text(size = 12, face = "bold"),  
        
        legend.position = "none") + 
  ylim(-.4, .4)
ggsave("Figures/Methods/Timeline.png")

# Testing Ecotropico ------------------------------------------------------
## Look for errors
# Are there any point count IDs in multiple data sets?
Birds_comb4 %>% filter(Uniq_db != "Ubc gaica DOM") %>%
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
# Recognize that some points have multiple coordinates associated with each point. NAs are from telemetry
Bird_pcs %>% distinct(Id_muestreo, Latitud, Longitud) %>% 
  count(Id_muestreo, sort = T) %>% 
  filter(n > 1)

# >>Same IDs too far --------------------------------------------------------
#Distance in meters WITHIN point counts / redes de niebla of the same ID_punto_muestreo #

Pc_locs_mult_sf <- Pc_locs_mult %>% 
  st_as_sf(coords = c("Longitud", "Latitud"), crs = 4326, remove = F)

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
dists %>% filter(max.dist > 0) %>% 
  arrange(max.dist)

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
  select(Id_muestreo, dif.Day)
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

# Identify the nearest point Ids < 150m
TF <- as.numeric(dist_nearest) < 150
ID_problem_dist <- Locs1coord[TF,] %>% 
  pull(Id_muestreo)
Nearest_pt <- Locs1coord[nearest1,][TF,] %>% 
  pull(Id_muestreo)

#Points that are <150m
dists2close <- data.frame(ID_problem_dist, Nearest_pt, dist_nearest_m = as.numeric(dist_nearest[TF])) %>% 
  left_join(st_drop_geometry(Locs1coord[,c("Id_muestreo", "Uniq_db", "Departamento")]), join_by("ID_problem_dist" == "Id_muestreo")) %>% 
  arrange(Uniq_db, dist_nearest_m)

#This ensures that the first letter of the ID (data collector) and the second letter (question of interest, e.g., "MB") are the same. Ultimately, the distances are only problematic if they are from the same Uniq_db
dists2close_sameUniq_db <- dists2close %>% 
  filter(sapply(ID_problem_dist, function(x){
    str_split_1(x, "-")[1]}) == sapply(Nearest_pt, function(x){str_split_1(x, "-")[1]
    })) %>% 
  filter(sapply(ID_problem_dist, function(x){
    str_split_1(x, "-")[2]}) == sapply(Nearest_pt, function(x){str_split_1(x, "-")[2]
    }))


dists2close_sameUniq_db %>% 
  filter(dist_nearest_m < 98.8) %>% 
  tabyl(Uniq_db)

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
birds_metadata_l <- map2(df_birds_red[-c(1, 4:6)], df_metadata[-c(1, 4:6)], 
                         \(bird, meta) {
  meta <- meta %>% filter(!is.na(Hora) & Spp_obs == 1)
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
  no_registros <- meta %>% filter(Spp_obs == 0) %>%
    distinct(Id_muestreo, Fecha, Hora)
  bird %>% semi_join(no_registros, by = c("Id_muestreo", "Fecha", "Hora")) %>%
    distinct(Id_muestreo, Fecha, Hora)
})

# Examine if there are point counts with Spp_obs == 1 that don't have any rows in the bird database
# NOTE:: The Ubc_gaica_Caf points on 5/27 & 5/28 are the 'ensayo' dates, & the LCA07-09 are PCs that we surveyed one time, which were both eliminated in 00a DW 
map2(df_birds_red, df_metadata, \(bird, meta) {
  registros <- meta %>% filter(Spp_obs == 1) %>%
    distinct(Id_muestreo, Fecha, Hora)
  registros %>% anti_join(bird, by = c("Id_muestreo", "Fecha", "Hora")) %>% 
    distinct(Id_muestreo, Fecha, Hora) %>% 
    filter(!str_detect(Id_muestreo, "LIBRE|RED"))
})

# >Times ------------------------------------------------------------------
## For CIPAV & GAICA distancia, the metadata files include the start and end time for each point.. Examine to see if the the start and end time from metadata matches with the bird database
df_se <- map(df_birds_red[c(1,2)], \(df){ #se = start end
  df %>% mutate(AM_PM = ifelse(Hora > as_hms("14:00:00"), "Afternoon", "Morning")) %>% 
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
    mutate(AM_PM = ifelse(
      Hora > as_hms("14:00:00"), "Afternoon", "Morning")
      ) %>% 
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
  mutate(
    AM_PM = ifelse(Hora > as_hms("14:00:00"), "Afternoon", "Morning")
    ) %>% 
  group_by(Id_muestreo, Fecha, AM_PM) %>%
  mutate(Hora_dif = max(Hora) - min(Hora),
         Hora_dif = hms(seconds = as.numeric(Hora_dif))) %>% 
  distinct(Uniq_db, Id_muestreo, Fecha, Hora_dif, Departamento) 
# NOTE:: Condensed CIPAV rows & calculated PC.length, so CIPAV points show up as 0 minutes in Hora_dif_df
                    
#Examine point counts with times > X minutes. 
Hora_dif_df %>% filter(Hora_dif > as_hms("00:45:00")) #%>% View()

# Testing Ubc gaica DB ----------------------------------------------------
# >Bird observation data --------------------------------------------------
#Create dataframe with just Ubc gaica
UBC_gaica_df <- Bird_pcs_all %>% filter(Nombre_institucion == "Ubc gaica")
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
# Extract PC IDs not including those by Ubc gaica to compare against
PCids <- Bird_pcs %>% filter(Nombre_institucion != "Ubc gaica") %>% 
  pull(Id_muestreo) %>% 
  unique()
# Expected result: Point counts from Otun Quimbaya & "G-MB-M-EPO1_03_(1)" have no match
prob_ids <- ubc_g_ids %in% PCids
unique(ubc_g_ids[!prob_ids])

## Spatial
# Determine if there are differences in Lat / long within a single ID
# Expected result: Should be 0 rows 
UBC_gaica_df %>% 
  distinct(Id_muestreo, Latitud_decimal, Longitud_decimal) %>% 
  #filter(Id_muestreo %in% c("G-MB-Q-PORT_01", "G-MB-Q-LCA_09"))
  #rename(name = Id_muestreo) %>%
  #st_as_sf(coords = c("Longitud_decimal", "Latitud_decimal"), crs = 4326, remove = F) %>%
  #st_write(driver='kml', dsn = paste0("Derived_geospatial/kml/GAICA_2024_", format(Sys.Date(), "%m.%d.%y"), ".kml"))
  group_by(Id_muestreo) %>%
  reframe(diff = round(diff(Latitud_decimal), 10))

## Examine Ubc GAICA to identify any spatial errors
Id_spat_errors <- function(Eco_reg){
Pc_locs_sf %>% 
  filter(Ecoregion == Eco_reg & Uniq_db == "Ubc gaica dom") %>% # Piedemonte
  filter(Id_muestreo != "G-MB-M-EPO1_03_(1)" & Id_group != "OQ") %>%
  ggplot() + 
  geom_sf() + 
  ggrepel::geom_text_repel(aes(label = Id_muestreo, geometry = geometry
  ), stat = "sf_coordinates")
}
Id_spat_errors(Eco_reg = "Cafetera") 
Id_spat_errors(Eco_reg = "Piedemonte")
# For OQ,change Id_group != "OQ" to == "OQ" in function
Id_spat_errors(Eco_reg = "Cafetera") 

## Examine species present 
ubc_g_spp <- UBC_gaica_df %>% pull(Species_ayerbe) %>% 
  unique()
TF <- ubc_g_spp %in% Ayerbe_all_spp
prob_spp <- ubc_g_spp[!TF]
# Expected result: "Leptotila verreauxi" "Thripadectes virgaticeps"
prob_spp 

# Split species name & compare to genus & species 
# Expected result: 0 rows 
map(UBC_gaica_l, \(df){
  df %>% 
    mutate(
      Genus = str_split_i(Nombre_cientifico_final_ayerbe_2018, " ", i = 1),
      Genus_equal = Genus == Genero,
      Species = str_split_i(Nombre_cientifico_final_ayerbe_2018, " ", i = 2),
      Species_equal = Species == Epiteto_especifico) %>%
    select(Genus_equal, Nombre_cientifico_final_ayerbe_2018, Genero, Species_equal, Epiteto_especifico) %>% 
    filter(Genus_equal == FALSE | Species_equal == FALSE) %>% 
    distinct(Nombre_cientifico_final_ayerbe_2018, Genero, Epiteto_especifico)
})

## Examine difference in times within same day 
# Expected result: There are 3 point counts with times > 5 minutes 
Pc_date8 %>% filter(Nombre_institucion == "Ubc gaica") %>% 
  filter(!Pc_length %in% c(as_hms("00:05:00"), as_hms("00:00:00"))) %>% 
  pull(Pc_length)

# Identify sampling points that have multiple habitat types. 
# Expected result: 0 rows
UBC_gaica_df %>% distinct(Id_muestreo, Habitat_og) %>% 
  count(Id_muestreo) %>% 
  filter(n > 1)

#Identify problematic orders & families
TF_order <- UBC_gaica_df$Orden %in% Tax_df3$order_gbif
TF_family <- UBC_gaica_df$Familia %in% Tax_df3$family_gbif 

# Expected result: There are 3 families that exist, just not in gbif
UBC_gaica_df[!TF_family,] %>% pull(Familia) %>% unique() 

#Consistency in Registrado_por 
# Expected result: Should be consistent across all databases
unique(UBC_gaica_df$Registrado_por)
map(UBC_gaica_l, \(df){
  df %>% distinct(Registrado_por)
})

#Examine Grabaciones -- Natalia can help here. 
## Not positive what this code is doing. DELETE once Natalia confirms that grabaciones are in good shape
UBC_gaica_df %>% select(contains("Grabacion"), Distancia_bird) %>%
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
  #write.xlsx(file = "Derived/Excels/Column_names_Ecotropico.xlsx", row.names = F, showNA = F)

# Ensure that all point counts were sampled on 3 different days & that metadata file is consistent with that
# Expected result: Only OQ practica was sampled on a single day
UBC_gaica_df %>% distinct(Id_muestreo, Fecha) %>% 
  count(Id_muestreo) %>% 
  filter(n != 3)

## Identify points where we did not survey 
# Expected result: Should return the points where we did not survey
map(UBC_gaica_metadata_l, \(df) {
  df %>% filter(is.na(Spp_obs)) %>% 
    distinct(Id_muestreo, Spp_obs)
})

#Do all 'percent' rows sum to 100%?
# Expected result: 0 rows (except for OQ practica)
map(UBC_gaica_metadata_l, \(df) {
  df %>% select(starts_with("percent")) %>%
    mutate(Sum = rowSums(across(everything()), na.rm = T)) %>% 
    filter(Sum != 100)
})

# Look for inconsistencies in habitat types
map(UBC_gaica_metadata_l[1:3], \(df) {
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

# Examine the relationship between Natalia's estimates and those provided by Canopeo 
## NOTE:: There are tons of points that 
# All points
cc_all <- df_habitats$Canopy %>% 
  pivot_wider(names_from = Fuente, values_from = Valor) %>% 
  gg_plot(x = Natalia, y = Canopeo)

# Average across the 4 cardinal directions
Mean_cc <- df_habitats$Canopy %>% filter(Variable == "Cobertura") %>% #& Valor > 0 
  summarize(mean_cc = mean(Valor), .by = c(Id_muestreo, Fuente))
Mean_cc %>% pivot_wider(names_from = Fuente, values_from = mean_cc) %>% 
  gg_plot(x = Natalia, y = Canopeo)

# Each ID should have 10 rows
# Expected result: 0 rows
df_habitats$Canopy %>% count(Id_muestreo) %>% 
  filter(n != 10)

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

# Examine changes between Habitat_og & Habitat_ajustado 
map(df_habitats[2:3], \(habs){
  habs %>% mutate(Hab_changed = Habitat_og == Habitat_ajustado) %>% 
    filter(Hab_changed == FALSE) %>% 
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
Ubc_Monroy <- read_xlsx(path = "Data/Aves/David_Monroy/Data_Monroy_Skinner_Otun_Quimbaya2024.xlsx") %>% 
  mutate(Equipo = "David_Aaron", 
         Species_ayerbe = paste(Genero, Epiteto_especifico))

# Create new Tax_df3 data frame & rerun 
Ubc_Monroy <- Ubc_Monroy %>% 
  left_join(distinct(Tax_df3[,c("Species_ayerbe", "family_gbif")]), 
                         by = join_by("Species_ayerbe" == "Species_ayerbe")) %>% 
  rename(Familia = family_gbif)

#Join data frames
names(df_birds$Ubc_gaica_OQ)
OQ <- df_birds$Ubc_gaica_OQ %>% mutate(Equipo = "GAICA") %>%
  rename(Species_ayerbe = Nombre_cientifico_final_ayerbe_2018) %>% 
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
OQ_nr %>% distinct(Equipo, Species_ayerbe) %>% 
  count(Equipo)

#Or 8 more species if considering recordings 
OQ %>% distinct(Equipo, Species_ayerbe) %>% 
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
  df %>% distinct(Equipo, Id_muestreo, Fecha, Species_ayerbe) %>% 
    count(Equipo, Id_muestreo, Fecha) %>% 
    ggplot(aes(x = Equipo, y = n)) + geom_boxplot() + 
    geom_jitter(alpha = .3, width = .03, aes(color = Id_muestreo)) + 
    labs(y = "Nro de especies por punto y dia", title = names) + 
    guides(color = "none")
})
ggarrange(OQ_comp[[1]], OQ_comp[[2]])
ggsave("Figures/Otun_comparison.png", bg = "white")

# Maybe they included more individuals towards the edge of the 50m radius? 
OQ_nr %>% filter(!is.na(Distancia_observacion)) %>% 
  ggplot(aes(x = Distancia_observacion)) +
  geom_histogram(stat = "count")

##CONCLUSIONS: 
#I think GAICA was likely generous with distances they included in their point counts. For example, I noted this with a few species in the field (Henicorhina en el punto 8 on the first day, & Quetzal in point 2 on day 2). They also likely tried really hard with recordings, and it is possible that they were texting with David to get information about birds at specific point counts, or sending him recordings. They likely DID learn from David about which birds are in the area. 
#Furthermore, David’s ear is likely very attuned to the species that are important for tourism, and he was definitely not used to focusing on the more common species. Additionally, it’s true that I was surely not as useful as Robert with his camera. When David would be photographing there’s no way he would be able to also concentrate on the songs / calls very much. 
# I did also review a few points & the data entered into the Excel matched the scanned data. There were not too many species written in the margins, indicating not many species were entered after the point counts were over (even those that were could have been from Robert's photographs). 
# I asked GAICA 2x whether there was anything that could have biased the data, and they said there was not. Ultimately, it is impossible to disqualify something more mischievous, but I will never know. Think I have to let it go & trust the data. 

# That being said, there was a 25% increase in observations made only in the recordings. 
Coffee_24 <- Bird_pcs %>% 
  left_join(Event_covs) %>%
  left_join(Site_covs) %>%
  filter(Uniq_db == "Ubc gaica mbd" & Ecoregion == "Cafetera") 
Recording_obs <- Coffee_24 %>%
  filter(Grabacion == "Y" )
Non_recording_obs <- Coffee_24 %>% 
  filter(Grabacion != "Y" | is.na(Grabacion))
# Percent increase of observations from recording only
nrow(Recording_obs) / nrow(Non_recording_obs)

# Quindio points were done over ~7 days, OQ points done over 3 days, but ultimately they weren't so different
Recording_obs %>% tabyl(Departamento)

# Testing Ubc El Hatico ----------------------------------------------------
# >Bird observation data --------------------------------------------------
#Create dataframe with just Ubc gaica
Ubc_hatico <- Bird_pcs_all %>% 
  filter(Nombre_institucion == "Ubc" & Nombre_finca == "El hatico")
ubc_hatico_ids <- unique(Ubc_hatico$Id_muestreo)

##Create dataframe of names to compare column names
# Extract names from each data frame in UBC_gaica
UBC_hatico <- df_birds$Ubc_hatico
name_list <- names(UBC_hatico)

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
lapply(UBC_hatico, unique)

## Ensure all point count IDs are specified correctly
# Extract PC IDs not including those by Ubc gaica to compare against
PCids <- Bird_pcs_all %>% filter(Nombre_institucion != "Ubc") %>% 
  pull(Id_muestreo) %>% 
  unique()
# Expected result: Point counts from Otun Quimbaya & "G-MB-M-EPO1_03_(1)" have no match
prob_ids <- ubc_hatico_ids %in% PCids
unique(ubc_hatico_ids[!prob_ids])

## Spatial
# Determine if there are differences in Lat / long within a single ID
# Expected result: Should be 0 rows 
UBC_hatico %>%
  distinct(Id_muestreo, Latitud_decimal, Longitud_decimal) %>% 
  #filter(Id_muestreo %in% c("G-MB-Q-PORT_01", "G-MB-Q-LCA_09"))
  #rename(name = Id_muestreo) %>%
  #st_as_sf(coords = c("Longitud_decimal", "Latitud_decimal"), crs = 4326, remove = F) %>%
  #st_write(driver='kml', dsn = paste0("Derived_geospatial/kml/GAICA_2024_", format(Sys.Date(), "%m.%d.%y"), ".kml"))
  group_by(Id_muestreo) %>%
  reframe(diff = round(diff(Latitud_decimal), 10))

## Examine Ubc GAICA to identify any spatial errors
Id_spat_errors <- function(Eco_reg){
  Pc_locs_sf %>% 
    filter(Ecoregion == Eco_reg & Uniq_db == "Ubc gaica dom") %>% # Piedemonte
    filter(Id_muestreo != "G-MB-M-EPO1_03_(1)" & Id_group != "OQ") %>%
    ggplot() + 
    geom_sf() + 
    ggrepel::geom_text_repel(aes(label = Id_muestreo, geometry = geometry
    ), stat = "sf_coordinates")
}
Id_spat_errors(Eco_reg = "Cafetera") 
Id_spat_errors(Eco_reg = "Piedemonte")
# For OQ,change Id_group != "OQ" to == "OQ" in function
Id_spat_errors(Eco_reg = "Cafetera") 

## Examine species present 
Ayerbe_all_spp <- list.files(path = "../Geospatial_data/Ayerbe_shapefiles_1890spp", pattern = "\\.dbf$")
Ayerbe_all_spp <- substr(Ayerbe_all_spp, 1, nchar(Ayerbe_all_spp) - 4)
ubc_hatico_spp <- UBC_hatico %>% pull(Nombre_cientifico_final_ayerbe_2018) %>% 
  unique()
TF <- ubc_hatico_spp %in% Ayerbe_all_spp
prob_spp <- ubc_hatico_spp[!TF]
# Expected result: "Leptotila verreauxi"
Nombre_cientifico_original <- prob_spp[!str_detect(prob_spp, "sp")]

# Split species name & compare to genus & species 
# Expected result: 0 rows 
map(UBC_hatico_l, \(df){
  df %>% 
    mutate(
      Genus = str_split_i(Nombre_cientifico_final_ayerbe_2018, " ", i = 1),
      Genus_equal = Genus == Genero,
      Species = str_split_i(Nombre_cientifico_final_ayerbe_2018, " ", i = 2),
      Species_equal = Species == Epiteto_especifico) %>%
    select(Genus_equal, Nombre_cientifico_final_ayerbe_2018, Genero, Species_equal, Epiteto_especifico) %>% 
    filter(Genus_equal == FALSE | Species_equal == FALSE) %>% 
    distinct(Nombre_cientifico_final_ayerbe_2018, Genero, Epiteto_especifico)
})

## Examine difference in times within same day 
# Expected result: There are 3 point counts with times > 5 minutes 
Pc_date8 %>% filter(Nombre_institucion == "Ubc mbd") %>% 
  filter(!Pc_length %in% c(as_hms("00:05:00"), as_hms("00:00:00"))) %>% 
  pull(Pc_length)

# Identify sampling points that have multiple habitat types. 
# Expected result: 0 rows
UBC_hatico %>% distinct(Id_muestreo, Habitat_og) %>% 
  count(Id_muestreo) %>% 
  filter(n > 1)

#Identify problematic orders & families
TF_order <- UBC_gaica_df$Orden %in% Tax_df3$order_gbif
TF_family <- UBC_gaica_df$Familia %in% Tax_df3$family_gbif 

# Expected result: There are 3 families that exist, just not in gbif
UBC_gaica_df[!TF_family,] %>% pull(Familia) %>% unique() 

#Consistency in Registrado_por 
# Expected result: Should be consistent across all databases
unique(UBC_hatico$Registrado_por)
map(UBC_hatico, \(df){
  df %>% distinct(Registrado_por)
})

# >Metadata ---------------------------------------------------------------
UBC_hatico_metadata <- df_metadata$Ubc_hatico

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
#write.xlsx(file = "Derived/Excels/Column_names_Ecotropico.xlsx", row.names = F, showNA = F)

# Ensure that all point counts were sampled on 3 different days & that metadata file is consistent with that
# Expected result: Only OQ practica was sampled on a single day
UBC_hatico %>% distinct(Id_muestreo, Fecha) %>% 
  count(Id_muestreo) %>% 
  filter(n != 3)

## Identify points where we did not survey 
# Expected result: Should return the points where we did not survey
  UBC_hatico_metadata %>% filter(is.na(Spp_obs)) %>% 
    distinct(Id_muestreo, Spp_obs)

#Do all 'percent' rows sum to 100%?
# Expected result: 0 rows (except for OQ practica)
  UBC_hatico_metadata %>% select(starts_with("percent")) %>%
    mutate(Sum = rowSums(across(everything()), na.rm = T)) %>% 
    filter(Sum != 100)

# Look for inconsistencies in habitat types
  UBC_hatico_metadata %>%
    select(contains("Habitat"), starts_with("percent"))

# Ensure that the water features are not changing day to day 
  UBC_hatico_metadata %>% distinct(Id_muestreo, Cuerpo_de_agua) %>% 
    count(Id_muestreo) %>% 
    filter(n > 1)

# Miscellaneous -----------------------------------------------------------
# Andrea habitat ----------------------------------------------------------
# 127 rows total: 54 UniLlanos, 73 GAICA
Andrea <- read_xlsx(path = "/Users/aaronskinner/Library/CloudStorage/OneDrive-UBC/Grad_School/PhD/SCR_SNAPP/SCR_data/Andrea_Cleaned_ANALISIS_UNILLANOS_GAICA_Distanciamiento_Aves.xlsx",
                    sheet = "Puntos",
                    na = c("Sin informacion", "N/A")) %>% 
  clean_names() %>%
  distinct(punto, ejecutor, sistema) %>% 
  filter(ejecutor == "GAICA") %>%
  mutate(punto2 = str_remove_all(punto, "-PAD|-SSPI|-BS|-PA|-PAD")) %>% 
  select(-c(ejecutor))

Exp_hab_match <- df_birds$Gaica_dist %>% 
  select(Id_muestreo, Id_punto_muestreo_original) %>% 
  distinct() %>% 
  tibble() %>% 
  #filter(Id_punto_muestreo_original == "LC-004")
  full_join(Andrea, by = join_by("Id_punto_muestreo_original" == "punto2")) %>%
  rename(punto_gaica = punto, habitat_unillanos = sistema)
 
# Export
Exp_hab_match %>% 
  as.data.frame() #%>% 
  #write.xlsx("Derived/Excels/Habitat_match_unillanos.xlsx", row.names = F, showNA = F)
#filter(!is.na(sistema)) 
  
## Better understand what's happened here
Prob_points <- Exp_hab_match %>% filter(is.na(Id_muestreo))
Prob_points 

# These are all the relevant points, I need Natalia or Audrey to go in & help verify that the points do indeed have the correct LC type in 2019. 
# For LCA that involves checking whether the UniLlanos LC type corresponds to LC1 or LC2
# For CA points it should just be adding a 0 to the point & confirming that LC type is correct. 
## JUST DO A 2-3 AND SEE HOW LONG IT TAKES
Exp_hab_match %>% 
  #distinct(Id_muestreo, Id_punto_muestreo_original) %>% 
  filter(str_detect(Id_punto_muestreo_original, "CA|LC")) %>% 
  arrange(Id_punto_muestreo_original) %>%
  as.data.frame() %>% 
  write.xlsx("Derived/Excels/Habitat_mismatches_6.5.25.xlsx", 
             row.names = F, showNA = F)

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
