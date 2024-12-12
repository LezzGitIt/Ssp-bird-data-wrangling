## Create spatial data frame with adoption (delta) values for silvopasture##

LC <- read.csv("/Users/aaronskinner/Library/CloudStorage/OneDrive-UBC/Grad_School/PhD/Field_Work/Site_Selection/Farms_LC3.csv")
nrow(LC) # Should be 94628, if extras this may cause problems down the line
Coords <- read.csv("/Users/aaronskinner/Library/CloudStorage/OneDrive-UBC/Grad_School/PhD/Field_Work/Site_Selection/Farms_Coords3.csv")
bio <- read.csv("/Users/aaronskinner/Library/CloudStorage/OneDrive-UBC/Grad_School/PhD/Field_Work/Site_Selection/Datos_Generales_Predios_v2.csv")

names(LC)[names(LC) == "AREA..Ha....LONGITUD..Mt."] <- "Area"
names(LC)[names(LC) == "DESCRIPCIÓN.UT"] <- "LC"

# SSPi has setos in it w/ units metros, so need to make new variable for this and change those to 0 for the other SSPi
LC <- LC %>%
  rename(Region = Regional) %>%
  mutate(
    Region = case_when(
      Region == "1. Bajo Magdalena" ~ "Atlantico",
      Region == "2. Valle del río Cesar" ~ "Guajira",
      Region == "3. Boyacá & Santander" ~ "Santander",
      Region == "4. Ecorregión Cafetera" ~ "Quindio",
      Region == "5. Piedemonte Orinocense" ~ "Meta",
      TRUE ~ as.character(Region)
    ),
    LC = ifelse(LC == "SSPi" & Unidad.de.Medida == "Metros", "SSPiSetos", LC)
  )
table(LC$LC) # Confirm SSPiSetos was created

LC <- LC %>%
  group_by(ID_2) %>%
  arrange(ID_2, Momento_1) %>%
  filter(Momento_1 == last(Momento_1) | Momento_1 == first(Momento_1))

sites.wide <- as.data.frame(LC %>% dplyr::select(ID_2, Region, LC, Area, Area.Total, Momento_2) %>% pivot_wider(names_from = LC, values_from = Area, values_fill = 0, values_fn = sum) %>% relocate(c("Cercas vivas y Barreras rompevientos ", "SSPiSetos"), .after = last_col()))

# Could combine classes by amount of anthropogenic impact: 1&2 (low impact - forest); 6,8,9,0 (high impact ag); then silvopasture area (3 & 7); silvopasture linear (5 and setos linear). CURRENTLY LEAVING OUT AGROFOREST AS MEDIAN IS 0 AND MEAN AMOUNT IS ONLY ABOUT 1.5% PER FARM
sites.wide$bosque <- sites.wide$`Bosque secundario` + sites.wide$`Bosque Maduro o humedales privados`
sites.wide$SSPiArb <- sites.wide$`Arboles dispersos en potrero y sucesión vegetal` + sites.wide$SSPi # SSPi and arboles
sites.wide$highI <- sites.wide$Infraestructura + sites.wide$`Suelos agropecuarios con coberturas mayores al 80%` + sites.wide$`Suelos con pasturas degradadas` + sites.wide$`Otras prácticas agropecuarias` # High intensity farming
names(sites.wide)
# Same for linear features
sites.wide$linear <- (sites.wide$`Cercas vivas y Barreras rompevientos ` + sites.wide$SSPiSetos) / sites.wide$Area.Total # metros/ha
# sites.wide has the raw LC classes, the combined classes (bosque, SSPiArb, highI), and then the proportions of these LC classes for both linea base & cierra. There are 269 farms that only have measurements in 1 time period


colnames(Coords)[1] <- "ID_2"
deltaDFsf <- merge(x = deltaDF, y = Coords[, c(1:3)], by = c("ID_2"), all.x = T)

## Create shapefile
deltaDFsf <- deltaDFsf %>% filter(!is.na(Coordenada.X))
deltaDFsf <- st_as_sf(
  x = deltaDFsf,
  coords = c("Coordenada.X", "Coordena.Y"),
  crs = 4326
)
st_write(obj = deltaDFsf, dsn = "/Users/aaronskinner/Library/CloudStorage/OneDrive-UBC/Grad_School/PhD/Field_Work/Spatial_Files/deltaDFsf.shp", layer = "deltaDFsf")

## Let's take a look at biodiversity farms
colnames(bio)[1] <- "ID_2"
deltaBiod <- merge(x = deltaDFsf, y = bio[, c(1, 7, 9:11)], by = c("ID_2"), all.x = T)
# Visuallize
names(deltaBiod)
deltaBiod62 <- data.frame(deltaBiod %>% filter(AVES_GCS.GAICA_2013.2019 != 0) %>% arrange(Region, Prop.dSSPi))[, c(1:10)] # 62 farms

# Summary tables ----------------------------------------------------------
## Summary of farm distribution and farm size
deltaBiod62 %>%
  group_by(Region) %>%
  summarize(Number.farms = n(), MeanFarmSize = mean(Area.Total))
# Adoption between 2013-2017
deltaBiod62 %>%
  group_by(Region) %>%
  summarize(Mean.dSSPi = mean(Prop.dSSPi), Min.dSSPi = min(Prop.dSSPi), Max.dSSPi = max(Prop.dSSPi), Mean.LinearMpHa = mean(dLinearMpHa)) %>%
  arrange(Mean.dSSPi)
# These are raw values of SSPi at the end of the project (2017)
deltaBiod62 %>%
  group_by(Region) %>%
  summarize(Raw.SSPi = mean(Prop.SSPiCierre), sd.Raw.SSPi = sd(Prop.SSPiCierre), Min.RawSSPi = min(Prop.SSPiCierre), Max.RawSSPi = max(Prop.SSPiCierre), Raw.LinearMpHa = mean(linearCierreMpHa), Min.Raw.LinearMpHa = min(linearCierreMpHa), Max.Raw.LinearMpHa = max(linearCierreMpHa)) %>%
  arrange(Raw.SSPi)
# Reduce table for thesis proposal
deltaBiod62 %>%
  group_by(Region) %>%
  summarize(Number.farms = n(), MeanFarmSize = mean(Area.Total), Raw.SSPi = mean(Prop.SSPiCierre), Raw.LinearMpHa = mean(linearCierreMpHa)) %>%
  arrange(Raw.SSPi)
# Focus in on farms in Quindio for doing more in-depth telemetry work
deltaBiod62 %>%
  filter(Region == "Quindio") %>%
  arrange(linearCierreMpHa) %>%
  select(ID_2, Area.Total, Prop.SSPiCierre, linearCierreMpHa)


# Summary plots --------------------------------------------------------
# Raw Linear features
p1 <- deltaBiod62 %>%
  filter(linearCierreMpHa < 30) %>%
  ggplot(aes(x = linearCierreMpHa, fill = Region)) +
  geom_density(alpha = 0.5, aes(y = ..scaled..)) +
  geom_vline(aes(xintercept = mean(linearCierreMpHa)), color = "black", linetype = "dashed", size = 1) +
  labs(x = "Raw linear features (m/Ha)", y = "Density") +
  guides(fill = guide_legend(title = "Region")) +
  ggtitle("Raw linear")

# Raw proportion of farm in dispersed trees
p2 <- deltaBiod62 %>% ggplot(aes(x = Prop.SSPiCierre, fill = Region)) +
  geom_density(alpha = 0.5, aes(y = ..scaled..)) +
  geom_vline(aes(xintercept = mean(Prop.SSPiCierre)), color = "black", linetype = "dashed", size = 1) +
  labs(x = "Raw Proportion of farm in dispersed trees", y = "Density") +
  guides(fill = guide_legend(title = "Region")) +
  ggtitle("Raw dispersed trees")

# Adoption linear
p3 <- deltaBiod62 %>%
  filter(dLinearMpHa < 30) %>%
  ggplot(aes(x = dLinearMpHa, fill = Region)) +
  geom_density(alpha = 0.5, aes(y = ..scaled..)) +
  geom_vline(aes(xintercept = mean(dLinearMpHa)), color = "black", linetype = "dashed", size = 1) +
  labs(x = "Increase in linear features (m/Ha)", y = "Density") +
  guides(fill = guide_legend(title = "Region")) +
  ggtitle("Adoption linear")

# Adoption dispersed trees
p4 <- deltaBiod62 %>% ggplot(aes(x = Prop.dSSPi, fill = Region)) +
  geom_density(alpha = 0.5, aes(y = ..scaled..)) +
  geom_vline(aes(xintercept = mean(Prop.dSSPi)), color = "black", linetype = "dashed", size = 1) +
  labs(x = "Increase in proportion of farm in dispersed trees", y = "Density") +
  guides(fill = guide_legend(title = "Region")) +
  ggtitle("Adoption dispersed trees")


library(ggpubr)
ggarrange(p1, p2, p3, p4, common.legend = T, labels = "AUTO")
ggsave("Raw_and_adoption_SSPi_linear.png", bg = "white", width = 10)

# Take a peek at telemetry farms
data.frame(deltaBiod %>% filter(Coordenadas_estaciones_telemetria != 0) %>% arrange(Region, dLinearMpHa))[, c(1:8)]
