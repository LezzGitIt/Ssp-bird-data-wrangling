## PhD birds in silvopastoral landscapes##
# LC summary & adoption
# The purpose of this script is to generate files that Mathilde will need to digitize landcover from satellite imagery in QGIS, as well as to examine the files that she digitized & compare them to other publicly available spatial layers

## NOTE:: Wrote up this code a long time ago, at present it is not very friendly to read or work with. Examine the 'Extra_scripts/SSPi_adoption_share.R' file as well, that may have better documentation.
# For now not going to mess with as there are several possible sources of landcover and it is not entirely clear that this will be the file used

## KEY NOTES
# Note that when changed to Area_Off the proportions no longer make sense -- if switch back to Area.Total the results should make more sense. NEED TO UNDERSTAND THE DIFFERENT AREA MEASUREMENTS MORE CLEARLY

# Contents
# 1)
# 2)
# 3)
# 4)
# 5)
# 6)

# Load libraries ----------------------------------------------------------
library(tidyverse)
library(cowplot)
library(raster)
library(sf)
library(sp)
library(stringi)
library(gtools)
library(GGally)
library(AICcmodavg)
library(readxl)
library(xlsx)
library(conflicted)
ggplot2::theme_set(theme_cowplot())
conflicts_prefer(dplyr::select)
conflicts_prefer(dplyr::filter)

# Load data ---------------------------------------------------------------
load("Rdata/the_basics_11.21.24.Rdata")
source("/Users/aaronskinner/Library/CloudStorage/OneDrive-UBC/Grad_School/Rcookbook/Themes_funs.R")

# Grab project's official BD_UT3 file
LC <- read_xlsx("/Users/aaronskinner/Library/CloudStorage/OneDrive-UBC/Grad_School/PhD/SCR_data/Base de datos UT_v3.xlsx", sheet = "Base UT", col_names = TRUE)

## Coordinates from 2866 farms associated with project
Coords <- read.csv("/Users/aaronskinner/Library/CloudStorage/OneDrive-UBC/Grad_School/PhD/Field_Work/Site_Selection/Farms_Coords3.csv")

# % forest file within a 1 km buffer of farm, sent by Seth
LSforest <- st_read("/Users/aaronskinner/Library/CloudStorage/OneDrive-UBC/Grad_School/PhD/Field_Work/Spatial_Files/Coordenadas_predios_PercentForest", layer = "Coordenadas_predios_PercentForest") # Landscape forest
names(LSforest)[5] <- "SurrPerFor" # Surrounding Percent Forest
sort(LSforest$SurrPerFor, decreasing = T)
LSforest %>%
  filter(NOMBRE_DPT == "META") %>%
  ggplot() +
  geom_sf()

# Data wrangling ----------------------------------------------------------
# Clean up LC file
LC <- LC %>%
  rename_with(make.names) %>%
  rename(
    Ecoregion = Regional,
    Area_UT = AREA..Ha....LONGITUD..Mt., # This is the area of the specific LC in question (not the total farm area)
    Area_Off = Area.documental..Ha., # Official area
    LC = DESCRIPCIÓN.UT
  )

LC <- LC %>%
  mutate(Ecoregion = str_remove(Ecoregion, "[0-5]. ")) %>%
  mutate(Ecoregion = stri_trans_general(str = Ecoregion, id = "Latin-ASCII")) %>%
  mutate(
    Ecoregion = case_when(
      Ecoregion == "Ecorregion Cafetera" ~ "Cafetera",
      Ecoregion == "Piedemonte Orinocense" ~ "Piedemonte",
      Ecoregion == "Valle del rio Cesar" ~ "Rio Cesar",
      TRUE ~ as.character(Ecoregion)
    ),
    # SSPi has setos in it w/ units metros, so need to make new variable for this
    LC = ifelse(LC == "SSPi" & Unidad.de.Medida == "Metros", "SSPiSetos", LC)
  )
table(LC$LC) # Confirm SSPiSetos was created

# Leave both 'base line' LC and last LC to create data base on CHANGE in LC throughout study period
LC <- LC %>%
  group_by(ID_2) %>%
  arrange(ID_2, Momento_1) %>%
  filter(Momento_1 == last(Momento_1) | Momento_1 == first(Momento_1))

# Pivot_wider so each farm has two rows (base line and close) with each column representing amount of each landcover (in Ha)
sites.wide <- LC %>%
  dplyr::select(ID_2, Ecoregion, LC, Area_UT, Area_Off, Momento_2) %>%
  pivot_wider(names_from = LC, values_from = Area_UT, values_fill = 0, values_fn = sum) %>%
  relocate(c("Cercas vivas y Barreras rompevientos", "SSPiSetos"),
    .after = last_col()
  ) %>%
  as.data.frame()
head(sites.wide)

# STILL TO DO: Understand the different area columns.. Why don't all LC classes sum to 1?
LC %>% select(starts_with("Area")) # 4 different areas.. See directorio in Excel for definitions
Tot.area <- apply(sites.wide[, c(5:13)], MARGIN = 1, FUN = sum) / sites.wide$Area_Off
hist(Tot.area[Tot.area < 3])
# Confirm that columns 5:13 represent all LC classes with units in hectars
LC %>%
  ungroup() %>%
  distinct(LC, Unidad.de.Medida)

# Notes on LC types -------------------------------------------------------
# Notes from the manual de usos de la tierra (10 LC types):
# In classes 1-3, bosque ribereno, paramo, and humedales privados are of the same conservation status as the principal LC (e.g., bosque ribereno in class 1 is primary forest, bosque ribereno class 2 is secondary forest, and bosque ribereno class 3 is early phases of restoration)
# 1. Bosque maduro / humedales: No intervention for last 3 decades. Strictly for conservation
# 2. Bosque secondario / humedales en restoration: Strictly for conservation.
# 3. Arboles dispersos y sucesion vegetal: Both for production (pastoreo) & conservation (start of 2ndary forest / riparian corridors). >25 trees / ha (up to upper limit of SSPi 7). If sucesion vegetal, this refers to the initial phase of restoration.
# 4. Cultivos agroforestales: E.g. shade coffee. NO grazing allowed.
# 5. Cercas vivas: El sistema silvopastoril most widespread in all of Latin America. Linear elements of trees, shrubs / palms that replace traditional fences. Trees <3m apart, or copas should touch. Also can serve as a windbreak barrier.
# 6. Suelos agropecuarios con coberturas mayores al 80%. Agricultural land > 80%. This includes pastureland, perennial crops (e.g., árboles frutales, palma africana, caña de azúcar, cacao, aguacate, café sin sombra), semi-perennial crops (plátano, banano, papaya, piña, lulo, mora, tabaco, yuca forrajera), and natural savanna (only relevant for el Piedemonte Orinocense).
# 7. SSPi. Foraging plants w/ bushes and trees destined for feeding of animals. Can also include crops and lumber trees. Divided into zonas bajas y media (0-2000m) and altas (>2000m), where lower elevations have higher densities (5000 "individuos" / ha) than higher elevations (2000 "individuos" / ha). "individuos" = trees or bushes. Does NOT include monocultures of forage grasses.
# 8. Otras practicas agropecuarios. Short-term crops (maíz, fríjol, arroz, sorgo, soya; hortalizas -- tomate, ahuyama, habichuela; raíces y tubérculos -- yuca, ñame, papa), and monoculture forest plantations (1100 trees / ha).
# 9. Pasto degradado. Cover < 80%, w/ very few trees or shrubs.
# 0. Infraestructura. viviendas, patios, jardines, beneficiaderos para café, establos, galpones, estanques para peces, corrales para el manejo animal, jagüeyes o reservorios de agua

# Questions:
# Division of setos forrajeros / cercas vivas from SSPi? But then silvopasture photos show those same items
# Suelos agropecuarios con coberturas mayores al 80% = Agricultural land > 80%? Natural savanna included in this b/c it's treated as pasture in el Piedmonte Orinocense Ecoregion?
# Leguminosa rastrera? Arvense = maleza?
# 10 LC classifications?


# Group LCs ---------------------------------------------------------------
# Combine classes by amount of anthropogenic impact: 1&2 (low impact - forest); 6,8,9,0 (high impact ag); then silvopasture area (3 & 7); silvopasture linear (5 and setos linear). CURRENTLY LEAVING OUT AGROFOREST AS MEDIAN IS 0 AND MEAN AMOUNT IS ONLY ABOUT 1.5% PER FARM
sites.wide$bosque <- sites.wide$`Bosque secundario` + sites.wide$`Bosque Maduro o humedales privados`
sites.wide$SSPiArb <- sites.wide$`Arboles dispersos en potrero y sucesión vegetal` + sites.wide$SSPi # SSPi and arboles
sites.wide$highI <- sites.wide$Infraestructura + sites.wide$`Suelos agropecuarios con coberturas mayores al 80%` + sites.wide$`Suelos con pasturas degradadas` + sites.wide$`Otras prácticas agropecuarias` # High intensity farming

names(sites.wide)
## Calculate the %s for each LC type
sites.wide[, c(19:21)] <- lapply(sites.wide[, c(16:18)], function(x) {
  x / sites.wide$Area_Off
})
sort(apply(sites.wide[, c(19:21)], MARGIN = 1, FUN = sum)) # Confirm the sum of all proportions add to 1. NO LONGER WORKS W/OUT CULTIVOS AGROFORESTALES
names(sites.wide)[c(19:21)] <- paste0("prop.", names(sites.wide[, c(16:18)]))
head(sites.wide)

sites.wide$linear <- (sites.wide$`Cercas vivas y Barreras rompevientos` + sites.wide$SSPiSetos) / sites.wide$Area_Off # metros/ha of linear features
sites.wide$CVpArea <- sites.wide$`Cercas vivas y Barreras rompevientos` / sites.wide$Area_Off # Cercas vivas per unit area
sites.wide$SetopArea <- sites.wide$SSPiSetos / sites.wide$Area_Off # setos per unit area

# Create columns of delta (change) and raw values of dispersed trees (SSPi) and linear features for each farm
# sites.wide has the raw LC classes, the combined classes (bosque, SSPiArb, highI), and then the proportions of these LC classes for both linea base & cierra. There are 269 farms that only have measurements in 1 time period
deltaDF <- sites.wide %>%
  group_by(ID_2) %>%
  arrange(ID_2, desc(Momento_2)) %>%
  mutate(
    dSSPi = ave(SSPiArb, ID_2, FUN = function(x) c(NA, diff(x))), # Raw change in SSPi
    dLinear = ave(linear, ID_2, FUN = function(x) c(NA, diff(x))), # Raw change linear
    Prop.SSPiCierre = last(SSPiArb) / last(Area_Off), # Prop of farm at cierre
    linearCierreMpHa = last(linear) / last(Area_Off), # Linear amt of farm at cierre
    Prop.ForCierre = last(prop.bosque),
    Prop.dSSPi = ave(SSPiArb, ID_2, FUN = function(x) c(NA, diff(x))) / last(Area_Off),
    dLinearMpHa = ave(linear, ID_2, FUN = function(x) c(NA, diff(x))) / last(Area_Off)
  ) %>%
  dplyr::select(ID_2, Ecoregion, Area_Off, Momento_2, dSSPi, dLinear, Prop.SSPiCierre, Prop.ForCierre, Prop.dSSPi, linearCierreMpHa, dLinearMpHa) %>%
  filter(!is.na(dSSPi)) # Prop = proportion of farm area, dLinearMpHa = change in linear features (m) per unit area (ha). DeltaDF = change in amount of SSPi
deltaDF[, c(3, 5:11)] <- lapply(deltaDF[, c(3, 5:11)], round, 2)
nrow(deltaDF)

# Let's check correlations
GGally::ggcorr(deltaDF[, c(3, 5:11)])
cor(deltaDF[, c(3, 5:11)])

## Delta data sets
deltaDFfor <- merge(
  x = deltaDF, y = as.data.frame(LSforest)[, c("ID_Predio", "SurrPerFor")],
  by.x = "ID_2", by.y = "ID_Predio", all.x = T
) %>%
  mutate(Prop.ForSurr = SurrPerFor * .01)
# Only r = ~.5 so they are conveying different information
cor(deltaDFfor$Prop.ForCierre, deltaDFfor$SurrPerFor, use = "complete.obs")

deltaDFsf <- merge(x = deltaDFfor, y = Coords[, c(1:3)], by.x = "ID_2", by.y = "ID", all.x = T)
# Write shapefile for delta values
deltaDFsf <- deltaDFsf %>% filter(!is.na(Coordenada.X))
deltaDFsf <- st_as_sf(
  x = deltaDFsf,
  coords = c("Coordenada.X", "Coordena.Y"),
  crs = 4326
)
# st_write(obj = deltaDFsf, dsn="/Users/aaronskinner/Library/CloudStorage/OneDrive-UBC/Grad_School/PhD/Field_Work/Spatial_Files/deltaDFsf.shp", layer = "deltaDFsf")

# Subset 62 biodiversity farms
Id_gcs <- BirdPCs %>%
  pull(Id_gcs) %>%
  unique()
deltaBiod62 <- deltaDFsf %>%
  filter(ID_2 %in% as.numeric(Id_gcs)) %>%
  arrange(Ecoregion, Prop.dSSPi)

# Summary tables ----------------------------------------------------------
## Summary of farm distribution and farm size
deltaBiod62 %>%
  group_by(Ecoregion) %>%
  summarize(Number.farms = n(), MeanFarmSize = mean(Area_Off))
# Adoption between 2013-2017
deltaBiod62 %>%
  group_by(Ecoregion) %>%
  summarize(Mean.dSSPi = mean(Prop.dSSPi), Min.dSSPi = min(Prop.dSSPi), Max.dSSPi = max(Prop.dSSPi), Mean.LinearMpHa = mean(dLinearMpHa)) %>%
  arrange(Mean.dSSPi)
# These are raw values of SSPi at the end of the project (2017)
deltaBiod62 %>%
  group_by(Ecoregion) %>%
  summarize(Raw.SSPi = mean(Prop.SSPiCierre), sd.Raw.SSPi = sd(Prop.SSPiCierre), Min.RawSSPi = min(Prop.SSPiCierre), Max.RawSSPi = max(Prop.SSPiCierre), Raw.LinearMpHa = mean(linearCierreMpHa), Min.Raw.LinearMpHa = min(linearCierreMpHa), Max.Raw.LinearMpHa = max(linearCierreMpHa)) %>%
  arrange(Raw.SSPi)
# Reduce table for thesis proposal
deltaBiod62 %>%
  group_by(Ecoregion) %>%
  summarize(Number.farms = n(), MeanFarmSize = mean(Area_Off), Raw.SSPi = mean(Prop.SSPiCierre), Raw.LinearMpHa = mean(linearCierreMpHa)) %>%
  arrange(Raw.SSPi)

# Summary plots --------------------------------------------------------
# Raw Linear features
p1 <- deltaBiod62 %>%
  filter(linearCierreMpHa < 30) %>%
  ggplot(aes(x = linearCierreMpHa, fill = Ecoregion)) +
  geom_density(alpha = 0.5, aes(y = after_stat(scaled))) + # trim = T
  geom_vline(aes(xintercept = mean(linearCierreMpHa)), color = "black", linetype = "dashed", linewidth = 1) +
  labs(x = "Linear features (m/Ha)", y = "Density") +
  guides(fill = guide_legend(title = "Ecoregion")) +
  ggtitle("Linear features in 2017")

# Raw proportion of farm in dispersed trees
p2 <- deltaBiod62 %>% ggplot(aes(x = Prop.SSPiCierre, fill = Ecoregion)) +
  geom_density(alpha = 0.5, aes(y = after_stat(scaled))) +
  geom_vline(aes(xintercept = mean(Prop.SSPiCierre)), color = "black", linetype = "dashed", linewidth = 1) +
  labs(x = "Proportion of farm in dispersed trees", y = "Density") +
  guides(fill = guide_legend(title = "Ecoregion")) +
  ggtitle("Dispersed trees in 2017")

# Adoption linear
p3 <- deltaBiod62 %>%
  filter(dLinearMpHa < 30) %>%
  ggplot(aes(x = dLinearMpHa, fill = Ecoregion)) +
  geom_density(alpha = 0.5, aes(y = after_stat(scaled))) +
  geom_vline(aes(xintercept = mean(dLinearMpHa)), color = "black", linetype = "dashed", linewidth = 1) +
  labs(x = "Change in linear features (m/Ha)", y = "Density") +
  guides(fill = guide_legend(title = "Ecoregion")) +
  ggtitle("Adoption (2013-2017) linear features")

# Adoption dispersed trees
p4 <- deltaBiod62 %>% ggplot(aes(x = Prop.dSSPi, fill = Ecoregion)) +
  geom_density(alpha = 0.5, aes(y = after_stat(scaled))) +
  geom_vline(aes(xintercept = mean(Prop.dSSPi)), color = "black", linetype = "dashed", linewidth = 1) +
  labs(x = "Change in proportion of farm in dispersed trees", y = "Density") +
  guides(fill = guide_legend(title = "Ecoregion")) +
  ggtitle("Adoption (2013-2017) dispersed trees")

library(ggpubr)
ggarrange(p1, p2, p3, p4, common.legend = T, labels = "AUTO")
ggsave("/Users/aaronskinner/Library/CloudStorage/OneDrive-UBC/Grad_School/PhD/Figures/Proposal/Raw_and_adoption_SSPi_linear.png", bg = "white", width = 10)

# Forest on farm and in 1km surround
p5 <- deltaBiod62 %>% ggplot(aes(x = Prop.ForCierre, fill = Ecoregion)) +
  geom_density(alpha = 0.5, aes(y = after_stat(scaled))) +
  geom_vline(aes(xintercept = mean(Prop.ForCierre)), color = "black", linetype = "dashed", linewidth = 1) +
  labs(x = "Forest on farm", y = "Density") +
  guides(fill = guide_legend(title = "Ecoregion")) +
  ggtitle("Forest on farm")

head(deltaBiod62)
# Forest in 1km surrounding landscape
p6 <- deltaBiod62 %>% ggplot(aes(x = Prop.ForSurr, fill = Ecoregion)) +
  geom_density(alpha = 0.5, aes(y = after_stat(scaled))) +
  geom_vline(aes(xintercept = mean(Prop.ForSurr)), color = "black", linetype = "dashed", linewidth = 1) +
  labs(x = "Forest in surrounding 1km", y = "Density") +
  guides(fill = guide_legend(title = "Ecoregion")) +
  ggtitle("Surrounding forest")

# Farm size across all farms and just on 62 biodiversity farms
sites.wide %>%
  filter(Area_Off < 500) %>%
  group_by(ID_2) %>%
  summarize(size_Ha = last(Area_Off)) %>%
  ggplot(aes(x = size_Ha)) +
  geom_histogram()
deltaBiod62 %>%
  filter(Area_Off < 300) %>%
  ggplot(aes(x = Area_Off)) +
  geom_histogram()

ggarrange(p5, p6, common.legend = T, labels = "AUTO")
ggsave("/Users/aaronskinner/Library/CloudStorage/OneDrive-UBC/Grad_School/PhD/Figures/Proposal/Forest_farm_surrounding.png", bg = "white", width = 10)

# Take a peek at telemetry farms
data.frame(deltaBiod %>% filter(Coordenadas_estaciones_telemetria != 0) %>% arrange(Ecoregion, dLinearMpHa))[, c(1:8)]

# Create spatial files -----------------------------------------------------
# All farms

df.LC <- merge(x = sites.wide, y = Coords[, c(1:3)], by.x = "ID_2", by.y = "ID", all.x = T)
# Select only the last rows of Momento_2 for each ID, i.e. most recent landcover data
df.LC <- df.LC %>%
  group_by(ID_2) %>%
  filter(Momento_2 == last(Momento_2))
df.tot <- merge(x = df.LC, y = bio[, c(1, 7, 9:17)], by.x = "ID_2", by.y = "ID_Predio", all.x = T)
df.tot <- relocate(df.tot, NOMBRE.DEL.PREDIO, .after = ID_2)
nrow(df.tot)
head(df.tot)

# CHECK:: for inconsistencies on farm size
df.tot$area.sum <- apply(df.tot[, c(6:14)], MARGIN = 1, FUN = sum)
table(dplyr::near(df.tot$area.sum, df.tot$Area_Off))
df.tot$ID_2[which(!dplyr::near(df.tot$area.sum, df.tot$Area_Off))]
# Problem: Additionally, there are some farms that have multiple “Area Total” numbers within a given “Moment” (i.e., multiple sizes at the same time). For example, ID_2 == 5939 shows that the total area is equal to both 17.43 and 27 when Momento_2 == “Cierre”. This is also causing me issues with ID_2 == 5575. I’m also having issues w/ ID_2 == 271 (area is 2x, as there seems to be another verification (“VE”) after “Cierre”) & 6419 (areas of polygons just don’t add up to the total area).

LCcoords <- df.tot[!is.na(df.tot$Coordenada.X), ]
table(LCcoords$Region) # 481 NAs, Over 200 in each Ecoregion
sitesLCsp <- SpatialPointsDataFrame(cbind(LCcoords$Coordenada.X, LCcoords$Coordena.Y), data = LCcoords, proj4string = CRS("EPSG:4326")) ## 281 farms w/out coordinates!!!!
names(sitesLCsp@data)[1] <- "name"
writeOGR(obj = sitesLCsp, dsn = paste0(bs, "Desktop/Grad_School/PhD/Field_Work/TNCfarms.kml"), layer = "TNCfarms", driver = "KML")

## Biodiversity farms kml (Coffee & Piedemonte)
LC.pot <- df.tot[df.tot$Region == "Quindio" | df.tot$Region == "Meta", ] # pot = potential sites
BirdBio <- LC.pot[LC.pot$AVES_GCS.GAICA_2013.2019 == 1, ]
BioCoords <- BirdBio[!is.na(BirdBio$Coordenada.X), ] # 2 farms w/out coords
sitesBiosp <- SpatialPointsDataFrame(cbind(BioCoords$Coordenada.X, BioCoords$Coordena.Y), data = BioCoords, proj4string = CRS("EPSG:4326"))
names(sitesBiosp@data)[1] <- "name"

writeOGR(obj = sitesBiosp, dsn = paste0(bs, "Desktop/Grad_School/PhD/Field_Work/BioTNCfarms.kml"), layer = "BioTNCfarms", driver = "KML")

# Make shapefile for the predio boundaries
Linderos <- rgdal::readOGR(dsn = "/Users/aaronskinner/Library/CloudStorage/OneDrive-UBC/Grad_School/PhD/Field_Work/Spatial_Files/Linderos_Predios/LDO_Predios_GCS", layer = "LDO_Predios_GCS")
LinderosMeta <- subset(Linderos, Regional == "Piedemonte Orinocense" | Regional == "5. Piedemonte Orinocense" | Regional == "Piedemonte Meta")
## 281 farms w/out coordinates!!!!
names(LinderosMeta@data)[11] <- "name"
LinderosMeta <- spTransform(LinderosMeta, CRS("EPSG:4326"))
writeOGR(obj = LinderosMeta, dsn = "/Users/aaronskinner/Library/CloudStorage/OneDrive-UBC/Grad_School/PhD/Field_Work/Spatial_Files/LinderosMeta.kml", layer = "LinderosMeta", driver = "KML")

# CLEANING COMMENT:: Never found creation of BirdSurveys object, so not positive exactly what this was, but see original comment re: survey locations.
# Create spdf of all survey locations.
BirdSurveysSp <- SpatialPointsDataFrame(cbind(BirdSurveys$long, BirdSurveys$lat), data = BirdSurveys, proj4string = CRS("EPSG:4326"))

## Attempt to add color to kml file.. Why didn't it work!? Tried both "Color", "color" and "IconColor" with no luck for any. Colors were "red", "lime","cyan"
sitesBiosp@data$Color <- rep("", nrow(sitesBiosp@data))
sitesBiosp@data$Color <- ifelse(sitesBiosp@data$AVES_GCS.UNILLANOS_2019 == 1, "red", "lime")
sitesBiosp@data$Color <- ifelse(sitesBiosp@data$Coordenadas_estaciones_telemetria == 1, "cyan", sitesBiosp@data$Color)
sitesBiosp@data$Color <- as.factor(sitesBiosp@data$Color)
sitesBiosp@data <- relocate(sitesBiosp@data, Color, .after = name)
names(sitesBiosp)

# Same approach but with methodology
sitesBiosp@data$Methodology <- rep("", nrow(sitesBiosp@data))
sitesBiosp@data$Methodology <- ifelse(sitesBiosp@data$AVES_GCS.UNILLANOS_2019 == 1, "Unillanos", "GAICA")
sitesBiosp@data$Methodology <- ifelse(sitesBiosp@data$Coordenadas_estaciones_telemetria == 1, "Telemetry", sitesBiosp@data$Methodology)
sitesBiosp@data$Methodology <- as.factor(sitesBiosp@data$Methodology)

# All 62 spatial farms w/ bird biodiversity
spGaica <- sitesLCsp[sitesLCsp$AVES_GCS.GAICA_2013.2019 == 1, ]
spGaica@data$Methodology <- ifelse(spGaica@data$Coordenadas_estaciones_telemetria == 1, "Telemetry", "Point Count")


# Site summaries ----------------------------------------------------------
# Do you need this? Can you delete anything?
cafe <- df.tot[df.tot$Region == "Quindio", ]
llano <- df.tot[df.tot$Region == "Meta", ]
sum(is.na(cafe$Coordenada.X)) # 281 farms w/ no coords

summary(df.tot$Area_Off)
hist(df.tot[df.tot$Area.Total < 250, ]$Area.Total)
hist(df.tot[df.tot$Area.Total > 250, ]$Area.Total)
hist(df.tot[df.tot$linear < 1000, ]$linear)

names(LC.pot)
LC.per <- LC.pot[, c(4, 20:23)]
lapply(LC.per, summary)

for (i in 1:ncol(LC.per)) {
  hist(LC.per[, i], main = colnames(LC.per)[i])
}

summary(llano$Area.Total)
hist(llano[llano$Area.Total < 250, ]$Area.Total)
hist(llano[llano$Area.Total > 500, ]$Area.Total)
hist(llano$CVpArea)

llano.per <- llano[, c(4, 20:23)]
lapply(llano.per, summary)
for (i in 1:ncol(llano.per)) {
  hist(llano.per[, i], main = colnames(llano.per)[i])
}

cafe.per <- cafe[, c(4, 20:23)]
lapply(cafe.per, summary)
for (i in 1:ncol(cafe.per)) {
  hist(cafe.per[, i], main = colnames(cafe.per)[i])
}

# The 6 unillanos farms and 3 telemetry farms are within the 42 farms where GAICA conducted point counts. ID 3189 has all 3 methodologies
lapply(df.tot[, c(28:36)], table)
BirdBio <- LC.pot[LC.pot$AVES_GCS.GAICA_2013.2019 == 1, ]
nrow(BirdBio) # 42 farms in coffee / llanos
table(BirdBio$Region)
df.tot[df.tot$AVES_GCS.GAICA_2013.2019 == 1, ] # 64 farms
df.tot[df.tot$Coordenadas_estaciones_telemetria == 1, ] # 3 farms, 2 cafe 1 in Piedemonte.
df.tot[df.tot$AVES_GCS.UNILLANOS_2019 == 1, ] # 6 farms, Piedemonte


BirdBio.per <- BirdBio[, c(4, 20:23)]
lapply(BirdBio.per, summary)
for (i in 1:ncol(BirdBio.per)) {
  hist(BirdBio.per[, i], main = colnames(BirdBio.per)[i])
}


## Let's filter bird biodiversity farms to get some potential candidates for visiting.
table(BirdBio$Region)
BirdBio[BirdBio$Area.Total < 500 & BirdBio$Area.Total > 10 & BirdBio$SSPi.1 > .01, ]
BirdBio[BirdBio$SSPi.1 > .05, c("Region", "SSPi.1", "linear", "lowI.1", "medI.1", "highI.1", "area.sum")]
BirdBio[BirdBio$linear > 50 & BirdBio$Region == "Quindio", c("ID_2", "NOMBRE.DEL.PREDIO", "Region", "SSPi.1", "linear", "lowI.1", "medI.1", "highI.1", "area.sum")]
BirdBio[BirdBio$linear < 50 & BirdBio$highI.1 > .5 & BirdBio$Region == "Quindio", c("ID_2", "NOMBRE.DEL.PREDIO", "Region", "SSPi.1", "linear", "lowI.1", "medI.1", "highI.1", "area.sum")]
BirdBio[BirdBio$Area.Total > 500, ]

## All potential farms
nrow(LC.pot[LC.pot$linear < 20 & LC.pot$highI.1 > .8 & LC.pot$Region == "Meta", c("ID_2", "NOMBRE.DEL.PREDIO", "Region", "SSPi.1", "linear", "lowI.1", "medI.1", "highI.1", "area.sum")]) # 35 in eje, 54 in Meta
sort(LC.pot[LC.pot$SSPi.1 > .1 & LC.pot$lowI.1 > .05 & LC.pot$Region == "Meta", c("ID_2", "NOMBRE.DEL.PREDIO", "Region", "SSPi.1", "linear", "lowI.1", "medI.1", "highI.1", "area.sum")]$SSPi.1)



# Identify farms nearby to sites where biodiversity data was collected in el eje cafetero#
nearL <- st_is_within_distance(sf_cafe, sf_all, dist = 5000) # On the per farm level, so result is any farm w/in 10km of ANY of biodiversity farms
ind <- unlist(nearL)
near <- sort(unique(sf_all[ind, ]$name))
sort(sf_cafe$name) # Compare "near" object to the biodiversity farms, see there is overlap

# Of non-biodiversity farms, these are the 36 additional farms w/in 10km
fincs_near <- filter(LC.pot, ID_2 %in% near)
nrow(fincs_near) # 37 farms - 13 biodiversity = 24 farms

c("ID_2", "NOMBRE.DEL.PREDIO", "Region", "SSPi.1", "linear", "lowI.1", "medI.1", "highI.1", "area.sum")

## Let's combine some potential sites
sspi_near <- fincs_near[fincs_near$AVES_GCS.GAICA_2013.2019 != 1 & fincs_near$linear > 50 & fincs_near$lowI.1 > .05, ] # 10 additional farms
trad_near <- fincs_near[fincs_near$AVES_GCS.GAICA_2013.2019 != 1 & fincs_near$linear < 20 & fincs_near$highI.1 > .6, ]
cafe_bio <- BirdBio[BirdBio$Region == "Quindio", ]
cafe_sites <- rbind(cafe_bio, sspi_near, trad_near)
# cafe_sites <- relocate(cafe_sites, NOMBRE.DEL.PREDIO, .after = ID_2)
nrow(cafe_sites) # 24 sites

# Create Excel for Pablo, round 2
hvy_sspi$Reason <- "hvy_sspi"
trad$Reason <- "traditional"
# Notice I just manually changed the region above (between ecorregion cafetera and piedemonte orinocense), and didn't do everything twice
cafe_pablo <- st_drop_geometry(rbind(hvy_sspi, trad))
meta_yadi <- st_drop_geometry(rbind(hvy_sspi, trad))
head(meta_yadi)
cafe_pablo[cafe_pablo$name == 2380, c("bosque.1", "SSPiArb.1", "linear", "highI.1")]
names(cafe_pablo)
meta_yadi <- meta_yadi[, c("name", "NOMBRE.DEL.PREDIO", "Area.Total", "Momento_2", "bosque.1", "SSPiArb.1", "linear", "highI.1", "SSPi", "AVES_GCS.GAICA_2013.2019", "Reason")]
meta_yadi[meta_yadi$SSPi > 0, ]
write.csv(meta_yadi, file = "/Users/aaronskinner/Library/CloudStorage/OneDrive-UBC/Grad_School/PhD/Field_Work/Site_Selection/meta_sites2.csv")

write.csv(cafe_sites, file = "/Users/aaronskinner/Library/CloudStorage/OneDrive-UBC/Grad_School/PhD/Field_Work/cafe_sites.csv")
meta_sites <- BirdBio[BirdBio$Region == "Meta", ]
write.csv(meta_sites, file = "/Users/aaronskinner/Library/CloudStorage/OneDrive-UBC/Grad_School/PhD/Field_Work/meta_sites.csv")


# Hone in on the rarer elements (big farms, farms w/ lots of silvopasture, bosque maduro). Map these. Put them into Google Earth. Then think about controlling for elevation, temperature, rainfall, surrounding LC

# Spatial maps --------------------------------------------------------------
library(rnaturalearthdata)
library(smoothr)
library(rnaturalearth)

load(paste0("/Users/aaronskinner/Library/CloudStorage/OneDrive-UBC/Grad_School/R_Files/PhD/NE_layers_Colombia.Rdata"))

ne_scale <- 10 # If having issues, may need to play with scale!
SA <- ne_countries(type = "countries", continent = "South America", scale = 110, returnclass = "sf")

# land polygon
neCol <- ne_countries(scale = 50, type = "countries", country = "Colombia", returnclass = "sf")
# Custom function to subset other features to those  within Colombia
wh_subset <- function(x) {
  in_wh <- as.logical(st_intersects(x, neCol, sparse = FALSE))
  st_transform(x[in_wh], crs = "epsg:4686") # NOTE CRS
}

# states_provinces_lines
ne_Depts_lines <- ne_download(
  scale = ne_scale, category = "cultural",
  type = "admin_1_states_provinces_lines",
  returnclass = "sf"
)
ne_Col_Depts_lines <- ne_Depts_lines %>% st_crop(neCol)

# Create a polygon of Meta for cropping or other applications
Meta_poly <- ne_Col_Depts_lines %>%
  filter(NAME_L == "Meta" | NAME_R == "Meta") %>%
  st_union() %>%
  st_cast("POLYGON") %>%
  st_make_valid() %>%
  vect()

# Return the MULTILINESTRING
neColDepts <- ne_Depts_lines %>%
  st_geometry() %>%
  wh_subset()

# rivers
ne_rios <- ne_download(
  scale = ne_scale, category = "physical",
  type = "rivers_lake_centerlines",
  returnclass = "sf"
) %>%
  st_geometry() %>%
  wh_subset()

if (FALSE) {
  ne_cities <- ne_download(
    scale = ne_scale, category = "cultural",
    type = "populated_places",
    returnclass = "sf"
  ) %>%
    st_geometry() %>%
    wh_subset()
}

# Equidistant cylindrical. If needed
# equidist <- "+proj=eqc +lon_0=-72.421875 +lat_ts=5.5892009 +datum=WGS84 +units=m +no_defs"

sf_all <- st_as_sf(sitesLCsp)
sf_bio <- st_as_sf(sitesBiosp)
sf_BirdSurveys <- st_as_sf(BirdSurveysSp)

sf_cafe <- sf_bio[sf_bio$Region == "Quindio", ]
sf_meta <- sf_bio[sf_bio$Region == "Meta", ]
hvy_sspi <- sf_all[sf_all$Region == "Meta" & sf_all$SSPiArb.1 > .5 & sf_all$bosque.1 > .05, ]
trad <- sf_all[sf_all$Region == "Meta" & sf_all$SSPiArb.1 < .1 & sf_all$bosque.1 > .05 & sf_all$highI.1 > .5, ]
LC.pot[LC.pot$Region == "Meta" & LC.pot$SSPi.1 > .1, ]$SSPi.1 ## 134 farms (in eje), only 90 w/ coordinates.
nrow(hvy_sspi)
ggplot(data = neCol) +
  geom_sf() +
  geom_sf(data = neColDepts) +
  geom_sf(data = sf_all[sf_all$Region == "Meta", ], color = "blue") +
  geom_sf(data = trad, color = "orange") +
  geom_sf(data = hvy_sspi, color = "red") +
  geom_sf(data = sf_cafe, color = "green") +
  coord_sf(xlim = c(st_bbox(hvy_sspi)[1], st_bbox(hvy_sspi)[3]), ylim = c(st_bbox(hvy_sspi)[2], st_bbox(hvy_sspi)[4]), label_axes = "____", expand = TRUE)

ggplot(data = neCol) +
  geom_sf() +
  geom_sf(data = neColDepts) +
  geom_sf(data = trad, color = "orange") +
  geom_sf(data = hvy_sspi, color = "red") +
  coord_sf(xlim = c(st_bbox(hvy_sspi)[1], st_bbox(hvy_sspi)[3]), ylim = c(st_bbox(hvy_sspi)[2], st_bbox(hvy_sspi)[4]), label_axes = "____", expand = TRUE) +
  ggrepel::geom_text_repel(
    data = hvy_sspi, aes(label = name, geometry = geometry),
    stat = "sf_coordinates", max.overlaps = 100
  )


ggplot(data = neCol) +
  geom_sf() +
  geom_sf(data = neColDepts) +
  geom_sf(data = sf_all[sf_all$Region == "Quindio", ], aes(color = prop.SSPiArb)) +
  geom_sf(data = sf_cafe, color = "green") +
  coord_sf(xlim = c(st_bbox(hvy_sspi)[1], st_bbox(hvy_sspi)[3]), ylim = c(st_bbox(hvy_sspi)[2], st_bbox(hvy_sspi)[4]), label_axes = "____", expand = TRUE)

# Meta
ggplot(data = neCol) +
  geom_sf() +
  geom_sf(data = neColDepts) +
  geom_sf(data = sf_meta, aes(color = prop.SSPiArb)) +
  coord_sf(xlim = c(st_bbox(sf_meta)[1], st_bbox(sf_meta)[3]), ylim = c(st_bbox(sf_meta)[2], st_bbox(sf_meta)[4]), label_axes = "____", expand = TRUE) +
  ggrepel::geom_text_repel(
    data = sf_meta, aes(label = name, geometry = geometry),
    stat = "sf_coordinates", max.overlaps = 100
  )

# Cafe
ggplot(data = neCol) +
  geom_sf() +
  geom_sf(data = neColDepts) +
  geom_sf(data = sf_cafe, aes(color = prop.SSPiArb)) +
  coord_sf(xlim = c(st_bbox(sf_cafe)[1], st_bbox(sf_cafe)[3]), ylim = c(st_bbox(sf_cafe)[2], st_bbox(sf_cafe)[4]), label_axes = "____", expand = TRUE) +
  ggrepel::geom_text_repel(
    data = sf_cafe, aes(label = name, geometry = geometry),
    stat = "sf_coordinates", max.overlaps = 100
  )

library(ggspatial)
grays <- gray.colors(n = 15)
ggplot(data = SA) +
  geom_sf() +
  geom_sf(data = SA[SA$adm0_a3 == "COL", ], color = "green") +
  layer_spatial(st_bbox(sf_gaica), color = "red")
# Grey scale
ggplot(data = SA) +
  geom_sf() +
  geom_sf(data = SA[SA$adm0_a3 == "COL", ], size = 2, color = "black") +
  layer_spatial(st_bbox(sf_gaica), color = grays[1])

# Just coffee region & Piedemonte
ggplot(data = neCol) +
  geom_sf() +
  geom_sf(data = neColDepts) +
  geom_sf(data = sf_bio, aes(color = Methodology)) +
  coord_sf(xlim = c(sitesBiosp@bbox[1, 1], sitesBiosp@bbox[1, 2]), ylim = c(sitesBiosp@bbox[2, 1], sitesBiosp@bbox[2, 2]), label_axes = "____", expand = TRUE)

metaBioSurveys <- sitesBiosp[sitesBiosp$Region == "Meta", ]
# Bird survey sampling locations (all methodologies)
ggplot(data = neCol) +
  geom_sf() +
  geom_sf(data = neColDepts) +
  geom_sf(data = sf_BirdSurveys, alpha = .3, aes(shape = institution, color = year)) +
  coord_sf(xlim = c(metaBioSurveys@bbox[1, 1], metaBioSurveys@bbox[1, 2]), ylim = c(metaBioSurveys@bbox[2, 1], metaBioSurveys@bbox[2, 2]), label_axes = "____", expand = TRUE)

ggplot(data = sf_BirdSurveys) +
  geom_sf(data = sf_BirdSurveys, alpha = .3, aes(color = Farm))
table(sf_BirdSurveys$LC)

ggplot(data = neCol) +
  geom_sf() +
  geom_sf(data = neColDepts) +
  geom_sf(data = sf_BirdSurveys, alpha = .3, size = 3, aes(color = Protocol, shape = institution)) +
  coord_sf(xlim = c(metaBioSurveys@bbox[1, 1], metaBioSurveys@bbox[1, 2]), ylim = c(metaBioSurveys@bbox[2, 1], metaBioSurveys@bbox[2, 2]), expand = TRUE)

ggplot(data = neCol) +
  geom_sf() +
  geom_sf(data = neColDepts) +
  geom_sf(data = sf_BirdSurveys[sf_BirdSurveys$Protocol == "PUNTOS" | sf_BirdSurveys$Protocol == "PUNTOS DE MUESTREO", ], alpha = .3, aes(color = institution, shape = year)) +
  coord_sf(xlim = c(metaBioSurveys@bbox[1, 1], metaBioSurveys@bbox[1, 2]), ylim = c(metaBioSurveys@bbox[2, 1], metaBioSurveys@bbox[2, 2]), label_axes = "____", expand = TRUE)

View(BirdSurveys[BirdSurveys$institution == "Universidad de los Llanos (UNILLANOS)", ])
View(BirdSurveys[BirdSurveys$institution == "Asociacion GAICA", ])

# Farm LC polygons -------------------------------------------------------------
# Also polylines (PLY files)
GCS_files_dir <- "/Users/aaronskinner/Library/CloudStorage/OneDrive-UBC/Grad_School/PhD/Field_Work/Spatial_Files/Farm_polygon_shapefiles"
GCS_files <- list.files(GCS_files_dir)
# Bring in files
GCS_LC_sp <- list()
for (i in 1:length(GCS_files)) {
  GCS_LC_sp[[i]] <- st_read(dsn = paste0(GCS_files_dir, "/", GCS_files[i]), layer = GCS_files[i]) # GCS landcover spatial files
}
names(GCS_LC_sp) <- c("PgoC", "PlyC", "PgoLB", "PlyLB") #C = cierre, LB = linea base

sf_use_s2(FALSE)
GCS_LC_sp <- lapply(GCS_LC_sp, function(x) {
  st_transform(x, crs = st_crs(3857))
}) # 4326

# Export files as kmz for Mathilde
for (i in 1:length(GCS_LC_sp)) {
  print(i)
  GCS_LC_sp[[i]] %>%
    rename(Name = TIPO_UT) %>%
    st_write(driver = "kml", dsn = paste0(dir, "KMZs/", names(GCS_LC_sp)[i], ".kml"), layer = names(GCS_LC_sp)[i])
}

# Create buffers and union them to create a single multipolygon sf object
PgoL <- GCS_LC_sp[c(1, 3)] # Polygons list
PgoL <- lapply(PgoL, st_zm)
?st_zm
PgoL_union <- lapply(PgoL, st_union)
PgoL_union <- lapply(PgoL_union, st_make_valid)
areas <- sapply(PgoL_union, st_area)
areas / 1000000

## Intersect / overlap point count buffers and the already digitized LC from 2013 & 2017
intersect13_sf <- lapply(v_union, st_intersection, PgoL_union[[1]])
overlap2013 <- sapply(intersect13_sf, st_area) / 1000000

intersect17_sf <- lapply(v_union, st_intersection, PgoL_union[[2]])
overlap2017 <- sapply(intersect17_sf, st_area) / 1000000

# Let's generate the polygons that still need to be digitized (buffers minus already digitized area)
diff13sf <- lapply(v_union, st_difference, PgoL_union[[1]])
# These area calculations could be DELETED, although they do show amount still left to be digitized which is important. This is just to confirm that everything adds up, which it does
diff13area <- sapply(diff13sf, st_area) / 1000000

diff17sf <- lapply(v_union, st_difference, PgoL_union[[2]])
diff17area <- sapply(diff17sf, st_area) / 1000000

buffers_df2 <- cbind(buffers_df, overlap2013, overlap2017) # , diff17area)
# buffers_df2 %>% mutate(overlap.per13 = overlap2013/area_km2,
# ToBeDig3times = area3times - overlap2013 - overlap2017,
# Check.Total = overlap2017 + diff17area,
# TF = near(Check.Total, area_km2)) #This suggests that st_difference and st_intersection are doing what I think they are
sapply(buffers_df2, round, 2)

# Misc --------------------------------------------------------------------
# >Surrounding Landcover ---------------------------------------------------
# Grabbed the CORINE 2018 LC database for all of Colombia from http://geoservicios.ideam.gov.co/geonetwork/srv/spa/catalog.search;jsessionid=97B6F80606F3D7E735B92FA7456F174E#/metadata/285c4d0a-6924-42c6-b4d4-6aef2c1aceb5

ColLC <- rgdal::readOGR(dsn = "/Users/aaronskinner/Library/CloudStorage/OneDrive-UBC/Grad_School/PhD/Field_Work/Spatial_Files/Coberturas_100K_2018/shape coberturas 2018", layer = "cobertura_tierra_clc_2018")

# raster(paste0(wd, "NEON-DS-Field-Site-Spatial-Data/SJER/DigitalTerrainModel/SJER2013_DTM.tif"))