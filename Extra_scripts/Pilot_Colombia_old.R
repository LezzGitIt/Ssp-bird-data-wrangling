#####PhD bird movement in agricultural landscapes#######
library(tidyr)
library(dplyr)
library(ggplot2)
library(cowplot)
library(raster)
library(sf)
library(sp)
library(rgdal)
library(gtools)
library(maptools)
ggplot2::theme_set(theme_cowplot())
bs <- "/Users/aaronskinner/Library/Mobile Documents/com~apple~CloudDocs/"

?read.xl



# Color band combos -------------------------------------------------------
colors <- c("R", "Y", "N", "B", "K", "W", "G", "A") #7 colors + aluminum. G = gray, N = green, B = blue, K = black

perms <- data.frame(gtools::permutations(colors, n = 8, r =3))
combos<- paste0(perms$X1, perms$X2, perms$X3) #, perms$X4
TF <- grepl("A",combos)
permsA <- perms[TF,]
nrow(permsA)
combosR <- data.frame(paste0(permsA$X1, permsA$X2, "/", permsA$X3))
combosL <- data.frame(paste0(permsA$X1,"/", permsA$X2, permsA$X3))
names(combosR) <- "ColorCombo"
names(combosL) <- "ColorCombo"
combosRL <- rbind(combosR, combosL)
names(combosRL) <- "ColorCombo"
set.seed(123) #So randomization produces same results each time
combos_df2 <- data.frame(combos_df2[sample(1:nrow(combos_df2)), ]) #Randomize order 
combos_df2 <- merge(CC, combosRL, by= "ColorCombo", all.y = T)
df <- setNames(data.frame(matrix(ncol = 7, nrow = 252)), c("Date", "Species", "Band#", "BandSize",  "Sex", "Location", "Notes"))
combos_df2 <- cbind(combosRL, df)
head(combos_df2)
CC <- read.csv("/Users/aaronskinner/Library/CloudStorage/OneDrive-UBC/Grad_School/PhD/Field_Work/Data_sheets/Color_Banding/ColorCombos.csv")
merge(CC, combos_df2, by= "ColorCombo", all.x = T, all.y = F)
?write.csv
write.csv(combos_df2, file = "/Users/aaronskinner/Library/CloudStorage/OneDrive-UBC/Grad_School/PhD/Field_Work/ColorCombos.csv")


# Bring in data -----------------------------------------------------------
##Data provided by TNC and Christina Kennedy
Uni <- read.csv("/Users/aaronskinner/Library/CloudStorage/OneDrive-UBC/Grad_School/PhD/TNC_Gaica_Unillanos_data/Aves_UNILLANOS_2019_CONSOLIDADA_26042022.csv")
Uni$eventDate <- as.Date(Uni$eventDate, "%m/%d/%y")
names(Uni)
UniDF <- read.csv("/Users/aaronskinner/Library/CloudStorage/OneDrive-UBC/Grad_School/PhD/TNC_Gaica_Unillanos_data/Evelyn_Aves-Dentro&Fuera.csv") #UniDF = UniLlanos Dentro & Fuera 
UniDF <- UniDF[UniDF$Punto != "Libres Pradera" & UniDF$Punto != "Libres Rosania" & UniDF$Punto != "Libres Andorra " & UniDF$Punto != "Libres_Porvenir",]

#Analasis distanciamiento
dist <- read.csv("/Users/aaronskinner/Library/CloudStorage/OneDrive-UBC/Grad_School/PhD/TNC_Gaica_Unillanos_data/2019_AVES_ANALISIS_DISTANCIAMIENTO_CONSOLIDADA.csv")
distPts <- distinct(dist, locality, parcelIDCorregido, decimalLatitude, decimalLongitude)
arrange(distPts, parcelIDCorregido)

gaica <- read.csv("/Users/aaronskinner/Library/CloudStorage/OneDrive-UBC/Grad_School/PhD/TNC_Gaica_Unillanos_data/Aves_GAICA_2013-2019_CONSOLIDADA_05052022.csv")
gaicaMeta <- gaica[gaica$stateProvince == "Meta" & gaica$samplingProtocol == "Puntos de Muestreo",]
gaicaMeta$
gaicaMeta$Id_gcs_Farm <- ifelse(gaicaMeta$Id_gcs_Farm == "0", gaicaMeta$ID_Nearest_GCS_Farm, gaicaMeta$Id_gcs_Farm)
names(gaicaMeta)[c(3:4,20,28,30,31)] <- c("Distance_to_GCS_farm","GCS_Name","habitat", "parcelID", "lat", "long")
gaicaMeta <- gaicaMeta[,-27]


#gaica2 <- read.csv("/Users/aaronskinner/Library/CloudStorage/OneDrive-UBC/Grad_School/PhD/TNC_Unillanos_data/Resultados ParcelIDCorregido-PuntosMuestreo_Meta_11042022.csv")
#gaicaMeta <- gaica2[gaica2$stateProvince == "Meta" & gaica2$samplingProtocol == "Puntos de Muestreo",]
nrow(gaicaMeta)
unique(gaicaMeta$municipality)

##Lets explore the data 
length(unique(birds$PointCountID)) #386 unique point counts
names(birds)
nrow(birds) 
sum(birds$Abundance) #25239 birds seen
table(birds$Region)
sort(birds$Abundance, decreasing=T)
table(birds$Land.Cover)
table(birds$Moment) ##What is "Moment?"
max(birds$Date)
min(birds$Date)
birds$locality
unique(birds$municipality)

# Spatial point count files -----------------------------------------------
#Inclusion of event_date complicates things greatly for visualization in GE, as it increases number of unique points from 58 -> 208, and 613 -> 1563
UniDFpc <- distinct(UniDF, Punto, Hábitat, localidad, Latitud.decimal, Longitud.decimal)
names(UniDFpc) <- c("name", "habitat", "farm", "lat", "long")
table(UniDFpc$habitat) #10 forest, 6 CV, 12 PA (28 silvopasture); 26 traditional
UniDFsp <- SpatialPointsDataFrame(cbind(UniDFpc$long, UniDFpc$lat), data = UniDFpc, proj4string = CRS("EPSG:4326"))
writeOGR(obj = UniDFsp, dsn = "/Users/aaronskinner/Library/CloudStorage/OneDrive-UBC/Grad_School/PhD/Field_Work/Spatial_Files/UniDFsp.kml", layer = "UniDFsp", driver = "KML")

UniPC <- distinct(Uni, decimalLatitude, decimalLongitude, samplingProtocol, year, Habitat, locality, Id_gcs_Farm, Name_Farm_Proyect_GCS,  ID_Nearest_GCSFarm, Distance_meters_Nearest.farm_GCS) #institutionCode
names(UniPC)[c(5,9,10)] <- c("Protocol", "long", "lat")
#UniPC <- UniPC[UniPC$samplingProtocol != "Observación ad hoc",]
nrow(UniPC)
UniPC$locality <- as.factor(UniPC$locality)
levels(UniPC$locality) <- c("Rosania", "Porvenir", "Andorra1", "Andorra2", "Andorra", "Pradera")
UniPC <- as.data.frame(UniPC %>% arrange(locality))
UniPC$name <- paste0(UniPC$locality, "_", 1:nrow(UniPC))

UniPCsp <- SpatialPointsDataFrame(cbind(UniPC$long, UniPC$lat), data = UniPC, proj4string = CRS("EPSG:4326")) ##281 farms w/out coordinates!!!!
writeOGR(obj = UniPCsp, dsn = "/Users/aaronskinner/Library/CloudStorage/OneDrive-UBC/Grad_School/PhD/Field_Work/Spatial_Files/UniPC.kml", layer = "UniPC", driver = "KML")

#170 unique parcel IDs

gaicaPC <- distinct(gaicaMeta, lat, long, year, eventDate, locality, parcelID, Id_gcs_Farm, GCS_Name, Distance_to_GCS_farm) 
gaicaPChab <- distinct(gaicaMeta, lat, long, year, habitat, locality, parcelID, Id_gcs_Farm, GCS_Name, Distance_to_GCS_farm) #eventDate
#gaicaPC$uniq.ID <- paste0(gaicaPC$parcelIDCorregido,"_", 1:nrow(gaicaPC))
nrow(gaicaPC)
gaicaPC$name <- paste0(gaicaPC$parcelID, "_", gaicaPC$eventDate)
gaicaPC <- arrange(gaicaPC, desc(name), year)

gaicaHabcheck <- arrange(gaicaPChab[gaicaPChab$lat > 3.65,], parcelID)
write.csv(gaicaHabcheck, file = "/Users/aaronskinner/Library/CloudStorage/OneDrive-UBC/Grad_School/PhD/Field_Work/Data_sheets/Print/gaicaHabcheck.csv")


#Create spdf and GE file
gaicaPCsp <- SpatialPointsDataFrame(cbind(gaicaPC$long, gaicaPC$lat), data = gaicaPC, proj4string = CRS("EPSG:4326")) 
#names(gaicaPCsp@data)[8] <- "name"
#writeOGR(obj = gaicaPCsp, dsn = "/Users/aaronskinner/Library/CloudStorage/OneDrive-UBC/Grad_School/PhD/Field_Work/Spatial_Files/gaicaPC.kml", layer = "gaicaPC", driver = "KML")

gaicaPCsf <- st_as_sf(gaicaPCsp)
uniqIDparc <- unique(gaicaPC$parcelID)
parcIDs <- dist.all <- vector("list",length = length(uniqIDparc))
all.dist <- rep(NA, length = length(uniqIDparc))
for(i in 1:length(uniqIDparc)){
  parcIDs[[i]] <- subset(gaicaPCsf, parcelID == uniqIDparc[i])
  dist.all[[i]] <- st_distance(parcIDs[[i]])
  diag(dist.all[[i]]) <- NA
  all.dist[i] <- round(colMeans(dist.all[[i]], na.rm = TRUE),2) #In meters
}
NumPts <- sapply(parcIDs, nrow) #Number of points per parcel
dists <- data.frame(uniqIDparc, all.dist, NumPts)
names(dists)[1] <- "parcelID"
gaicaPC <- merge(x = gaicaPC, y = dists, by = "parcelID", all.x = T)
gaicaPC[gaicaPC$parcelID == "CO-007",]
arrange(gaicaPCred[gaicaPCred$all.dist > 100, c("parcelID","year", "all.dist", "lat", "locality")], parcelID) 


gaicaPChab[gaicaPChab$parcelIDCorregido == "M2-03",]
gaicaPChab

gaicaPCred <- distinct(gaicaPC, lat, long, year, all.dist, NumPts, locality, parcelID, Id_gcs_Farm, GCS_Name, Distance_to_GCS_farm) #Remove eventDate
nrow(gaicaPCred)
gaicaPCred$name <- paste0(gaicaPCred$parcelID, "_", gaicaPCred$year)
gaicaPCredSp <- SpatialPointsDataFrame(cbind(gaicaPCred$long, gaicaPCred$lat), data = gaicaPCred, proj4string = CRS("EPSG:4326"))
writeOGR(obj = gaicaPCredSp, dsn = "/Users/aaronskinner/Library/CloudStorage/OneDrive-UBC/Grad_School/PhD/Field_Work/Spatial_Files/gaicaPCred.kml", layer = "gaicaPCred", driver = "KML")


gaicaPCred[gaicaPCred$Distance_meters_Nearest_farm_GCS == 0,]
table(gaicaPCred[gaicaPCred$all.dist > 150,"year"])


ggplot(data = gaicaPCsf[gaicaPCsf$parcelID== "M2-04",]) + geom_sf() 

##TO DO: Try to understand the errors a bit using different distance thresholds. Is there a particular year that's problematic? Do the habitats make sense for a certain Do you need to remove all points w/ distance errors, or just some? Check some habitat types of points w/ the PChab df. Will eventually want to reduce file size (something like gaicaPCred and get back in and start playing around in GE).
#May want to try plotting in GE by year.. Can this be done easily via subsetting spatial file into multiple layers? 
#Come up w/ a plan. Ultimately you need ~7*10 = 70 point counts worth of farms for April 18, so potentially identify those ~25 points to survey and leave it at that for now? 

nrow(gaicaPC)
View(gaicaPC)
tail(gaicaPC)
table(gaicaPC$Habitat.donde.se.observó.el.ave)
names(BirdSurveys) <- c("Protocol", "year", "LC", "Farm", "long", "lat")
BirdSurveys$year <- as.factor(BirdSurveys$year)
table(BirdSurveys$Farm, BirdSurveys$LC)

unique(birds$Lat, birds$Lon)
unique(birds$Lon)

#Make shapefile for the predio boundaries 
Linderos <- rgdal::readOGR(dsn ="/Users/aaronskinner/Library/CloudStorage/OneDrive-UBC/Grad_School/PhD/Field_Work/Spatial_Files/Linderos_Predios/LDO_Predios_GCS", layer = "LDO_Predios_GCS")
LinderosMeta <- subset(Linderos, Regional == "Piedemonte Orinocense" |  Regional == "5. Piedemonte Orinocense" |  Regional == "Piedemonte Meta" )
 ##281 farms w/out coordinates!!!!
names(LinderosMeta@data)[11] <- "name"
LinderosMeta <- spTransform(LinderosMeta, CRS("EPSG:4326"))
writeOGR(obj = LinderosMeta, dsn = "/Users/aaronskinner/Library/CloudStorage/OneDrive-UBC/Grad_School/PhD/Field_Work/Spatial_Files/LinderosMeta.kml", layer = "LinderosMeta", driver = "KML")


# Spp Abundance Plot ------------------------------------------------------
##What species are present in the coffee region?
#Ecoregion cafetera, n = 50
cafe <- birds[birds$Region == "Ecorregion cafetera",]
meta <- birds[birds$Region == "Piedemonte Meta",]

#meta <- birds
nrow(cafe) #1644 observations
nrow(meta)
head(cafe)
length(unique(cafe$PointCountID)) ##50 point count surveys

##All surveys in 2017
summary(cafe$Date)
#Each given ID was surveyed on multiple dates, and 260 has 2 LCs?
cafe[cafe$PointCountID == 260, c("Land.Cover","Date")]
table(data.frame(cafe %>% group_by(PointCountID) %>% summarize(LC = unique(Land.Cover)))$LC)

#Meta department
names(meta)
#GAICA raw abundances, irrespective of LC
abundMeta <-gaicaMeta %>% dplyr::select(scientificName, individualCount) %>% group_by(scientificName) %>% summarize(count = sum(individualCount))
abundUL <- Uni %>% dplyr::select(scientificName ,individualCount) %>% group_by(scientificName) %>% summarize(count = sum(individualCount))

#By Landcover, this only works with Diego Lizcano's erroneous data base
meta.LC <- as.data.frame(gaicaMeta %>% dplyr::select(scientificName, individualCount, Sistema) %>% pivot_wider(names_from= Sistema, values_from = individualCount, values_fill = 0, values_fn = sum))
Uni.LC <- as.data.frame(Uni %>% dplyr::select(scientificName, individualCount, Sistema, order) %>% pivot_wider(names_from= Sistema, values_from = individualCount, values_fill = 0, values_fn = sum))
names(meta.LC) <- c("Species","Ag land", "Forest fragments", "Dispersed trees", "SSPi", "Live fences", "Secondary growth", "Degraded pasture")
names(Uni.LC) <- c("Species", "Order", "Silvopasture", "Forest", "Monoculture Pasture")
meta.LC$count <- rowSums(meta.LC[,c(2:4)])
meta.LC <- meta.LC %>% group_by(scientificName) %>% pivot_longer(-c(scientificName,count))
Uni.LC$count <- rowSums(Uni.LC[,c(3:5)])
Uni.LC <- Uni.LC %>% group_by(Species) %>% pivot_longer(-c(Species,count, Order))

#Coffee department
cafe.LC <- as.data.frame(cafe %>% dplyr::select(Scientific.Name, Abundance, Land.Cover) %>% pivot_wider(names_from= Land.Cover, values_from = Abundance, values_fill = 0, values_fn = sum))
names(cafe.LC) <- c("Species","Dispersed trees", "Forest fragments", "Live fences", "Ag land")
cafe.LC$count <- rowSums(cafe.LC[,c(2:5)])
cafe.LC <- cafe.LC %>% group_by(Species) %>% pivot_longer(-c(Species,count))



unique(gaicaMeta$scientificName)


##Meta department
#GAICA data
ggplot(meta.LC[meta.LC$count > 215 & meta.LC$Species != "Bubulcus ibis" & meta.LC$Species != "Vanellus chilensis"  & meta.LC$Species != "Milvago chimachima" & meta.LC$Species != "Setophaga fusca" & meta.LC$Species != "Vireo flavoviridis" & meta.LC$Species != "Orthopsittaca manilatus",], aes(x=reorder(Species, count), y=value, fill= name)) + geom_col(color = "black", width = .8) + theme(axis.title.x = element_blank(), axis.text.x=element_blank()) + ylab("Count") + scale_fill_discrete(name="Landcover", labels = c("Ag land", "Degraded pasture", "Dispersed trees \nin pasture", "Forest fragments", "Live fences", "Secondary growth", "SSPi"))
ggplot(meta.LC[meta.LC$count > 50,], aes(x=reorder(scientificName, count), y=value, fill= name)) + geom_col(color = "black", width = .8) + theme(axis.title.x = element_blank(), axis.text.x = element_text(size = 10, vjust = .58, angle = 60)) + ylab("Count") + scale_fill_discrete(name="Landcover", labels = c("Forest", "Degraded pasture", "SSP"))
#UniLlanos data
#+ theme(axis.title.x = element_blank(), axis.text.x=element_blank()) Uni.LC$count > 15 & Uni.LC$count < 18 
#Come up w/ possible focal spp. 
UniComm <- Uni.LC[Uni.LC$count > 20  & Uni.LC$Species != "Setophaga striata" & Uni.LC$Species != "Setophaga ruticilla" & Uni.LC$Species != "Sturnella magna" & Uni.LC$Species != "Troglodytes aedon" & Uni.LC$Species != "Thraupis episcopus" & Uni.LC$Order == "Passeriformes",] #Common Spp
UniFor <- Uni.LC %>% filter(Species == "Ceratopipra erythrocephala" | Species == "Myrmophylax atrothorax")
UniSppPlot <- rbind(UniComm, UniFor)
UniSppPlot$Species <- gsub(" ", "\n", UniSppPlot$Species)
#Change out scale_fill_grey and scale_fill_discrete
ggplot(UniSppPlot, aes(x=reorder(Species, count), y=value, fill= name)) + geom_col(color = "black", width = .8) + ylab("Count") + scale_fill_grey(name="Landcover", labels = c("Forest", "Monoculture \npasture", "Silvopasture"), start = .1, end = .9) + theme(axis.title.x = element_blank(), axis.text.x = element_text(size = 10, vjust = .58, angle = 60)) 
labels=function(x){sub("\\s", "\n", x)}
?theme

ggplot(Uni.LC[Uni.LC$count > 50,], aes(x=reorder(Species, count), y=value, fill= name)) + geom_col(color = "black", width = .8) + theme(axis.title.x = element_blank(), axis.text.x = element_text(size = 10, vjust = .58, angle = 60)) + ylab("Count") + scale_fill_discrete(name="Landcover", labels = c("Forest", "Degraded pasture", "SSP"))
#Irrespective of LC 
ggplot(abundMeta[abundMeta$count > 100,], aes(x=reorder(scientificName, count), y=count)) + geom_col(color = "black", width = .8) + theme(axis.title.x = element_blank(), axis.text.x = element_text(size = 10, vjust = .58, angle = 60)) + ylab("Count") 
ggplot(abundUL[abundUL$count > 30,], aes(x=reorder(scientificName, count), y=count)) + geom_col(color = "black", width = .8) + theme(axis.title.x = element_blank(), axis.text.x = element_text(size = 10, vjust = .58, angle = 60)) + ylab("Count") 
#+ theme(axis.text.x = element_text(size = 10, vjust = .58, angle = 60))



ggsave("/Users/aaronskinner/Library/CloudStorage/OneDrive-UBC/Grad_School/PhD/Figures/Unillanos_select_passerines_grey.png", bg = "white", units = "mm",device = "png", dpi = 300)

##Coffee region
ggplot(cafe.LC[cafe.LC$count > 100 & cafe.LC$Species != "Setophaga fusca" & cafe.LC$Species != "Colibri thalassinus" & cafe.LC$Species != "Coeligena coeligena",], aes(x=reorder(Species, count), y=value, fill = name)) + geom_col() + theme(axis.text.x = element_text(size = 10, vjust = .58, angle = 60)) + geom_text(aes(label=count), vjust = .05) 
ggsave("/Users/aaronskinner/Library/CloudStorage/OneDrive-UBC/Grad_School/PhD/sp_counts.png")

###Site selection within coffee region####
load(paste0(bs, "Desktop/Grad_School/R_Files/PhD/ColombiaPhD.Rdata"))

LC <- read.csv("/Users/aaronskinner/Library/CloudStorage/OneDrive-UBC/Grad_School/PhD/Field_Work/Site_Selection/Farms_LC3.csv")
nrow(LC) #Should be 94628, if extras this may cause problems down the line
Coords <- read.csv("/Users/aaronskinner/Library/CloudStorage/OneDrive-UBC/Grad_School/PhD/Field_Work/Site_Selection/Farms_Coords3.csv")
bio <- read.csv("/Users/aaronskinner/Library/CloudStorage/OneDrive-UBC/Grad_School/PhD/Field_Work/Site_Selection/Datos_Generales_Predios_v2.csv")

table(bio$Ultimo.monitoreo...Tipo)

head(Coords)
nrow(Coords)
unique(LC$Regional)
table(LC$Regional)
#LC.pot <- LC.pot[LC.pot$Unidad.de.Medida != "Metros",] ##PROBLEMATIC
#LC.pot <- LC.pot[LC.pot$Momento_2 == "Cierre",] 
head(LC)
names(LC)[names(LC) == 'AREA..Ha....LONGITUD..Mt.'] <- "Area"
names(LC)[names(LC) == 'DESCRIPCIÓN.UT'] <- "LC"
#SSPi has setos in it w/ units metros, so need to make new variable for this and change those to 0 for the other SSPi
LC$LC <- ifelse(LC$LC == "SSPi" & LC$Unidad.de.Medida == "Metros", "SSPiSetos", LC$LC)
table(LC$LC) #Confirm SSPiSetos was created

table(LC$Unidad.de.Medida)

##Fact that the only "Momento_2" represented here is "Linea base" proves that the last(Momento_2) approach worked to select only the most recent dates (at least in coffee region and Meta departments)
#i <- LC.pot[LC.pot$Momento_2 == "Línea Base",]$ID_2
#T <- LC$ID_2 %in% i
#table(LC[T,]$Momento_2)

#Select only the last rows of Momento_2 for each ID 
LC <- LC %>% group_by(ID_2) %>% filter(Momento_2 == last(Momento_2))
nrow(LC)
names(LC)

sites.wide <- as.data.frame(LC %>% dplyr::select(ID_2, Regional, LC, Area, Area.Total, Momento_2) %>% pivot_wider(names_from= LC, values_from = Area, values_fill = 0, values_fn = sum) %>% relocate(c("Cercas vivas y Barreras rompevientos ", "SSPiSetos"), .after = last_col()))
names(sites.wide)
nrow(sites.wide)
head(sites.wide)
str(sites.wide)

##Explore and summarize descriptive data
#Notes on landcover (LC) manual de usos de la tierra (10 LC types): 
#In classes 1-3, bosque ribereno, paramo, and humedales privados are of the same conservation status as the principal LC (e.g., bosque ribereno in class 1 is primary forest, bosque ribereno class 2 is secondary forest, and bosque ribereno class 3 is early phases of restoration)
#1. Bosque maduro / humedales: No intervention for last 3 decades. Strictly for conservation
#2. Bosque secondario / humedales en restoration: Strictly for conservation.
#3. Arboles dispersos y sucesion vegetal: Both for production (pastoreo) & conservation (start of 2ndary forest / riparian corridors). >25 trees / ha (up to upper limit of SSPi 7). If sucesion vegetal, this refers to the initial phase of restoration. 
#4. Cultivos agroforestales: E.g. shade coffee. NO grazing allowed.
#5. Cercas vivas: El sistema silvopastoril most widespread in all of Latin America. Linear elements of trees, shrubs / palms that replace traditional fences. Trees <3m apart, or copas should touch. Also can serve as a windbreak barrier. 
#6. Suelos agropecuarios con coberturas mayores al 80%. Agricultural land > 80%. This includes pastureland, perennial crops (e.g., árboles frutales, palma africana, caña de azúcar, cacao, aguacate, café sin sombra), semi-perennial crops (plátano, banano, papaya, piña, lulo, mora, tabaco, yuca forrajera), and natural savanna (only relevant for el Piedemonte Orinocense).
#7. SSPi. Foraging plants w/ bushes and trees destined for feeding of animals. Can also include crops and lumber trees. Divided into zonas bajas y media (0-2000m) and altas (>2000m), where lower elevations have higher densities (5000 "individuos" / ha) than higher elevations (2000 "individuos" / ha). "individuos" = trees or bushes. Does NOT include monocultures of forage grasses. 
#8. Otras practicas agropecuarios. Short-term crops (maíz, fríjol, arroz, sorgo, soya; hortalizas -- tomate, ahuyama, habichuela; raíces y tubérculos -- yuca, ñame, papa), and monoculture forest plantations (1100 trees / ha).
#9. Pasto degradado. Cover < 80%, w/ very few trees or shrubs. 
#0. Infraestructura. viviendas, patios, jardines, beneficiaderos para café, establos, galpones, estanques para peces, corrales para el manejo animal, jagüeyes o reservorios de agua

#Questions: 
#Division of setos forrajeros / cercas vivas from SSPi? But then silvopasture photos show those same items
#Suelos agropecuarios con coberturas mayores al 80% = Agricultural land > 80%? Natural savanna included in this b/c it's treated as pasture in el Piedmonte Orinocense region?
#Leguminosa rastrera? Arvense = maleza? 
#10 LC classifications?

#Could combine classes by amount of anthropogenic impact: 1&2 (low impact - forest); 6,8,9,0 (high impact ag); then silvopasture area (3 & 7); silvopasture linear (5 and setos linear). CURRENTLY LEAVING OUT AGROFOREST AS MEDIAN IS 0 AND MEAN AMOUNT IS ONLY ABOUT 1.5% PER FARM
sites.wide$bosque <- sites.wide$`Bosque secundario` + sites.wide$`Bosque Maduro o humedales privados`
sites.wide$SSPiArb <- sites.wide$`Arboles dispersos en potrero y sucesión vegetal` + sites.wide$SSPi #SSPi and arboles
sites.wide$highI <- sites.wide$Infraestructura + sites.wide$`Suelos agropecuarios con coberturas mayores al 80%` + sites.wide$`Suelos con pasturas degradadas` + sites.wide$`Otras prácticas agropecuarias` #High intensity farming
names(sites.wide)

##Calculate %s for each LC type
names(sites.wide)
sites.wide[,c(19:21)] <- lapply(sites.wide[,c(16:18)], function(x){x/sites.wide$Area.Total})
table(apply(sites.wide[,c(19:21)], MARGIN = 1, FUN = sum)) #Confirm all percentages add to 1. NO LONGER WORKS W/OUT CULTIVOS AGROFORESTALES
colnames(sites.wide[,c(19:21)]) <- paste0("prop.", colnames(sites.wide[,c(16:18)])) #Unclear to me why this isn't working? 
head(sites.wide)
sites.wide$linear <- (sites.wide$`Cercas vivas y Barreras rompevientos `+ sites.wide$SSPiSetos) / sites.wide$Area.Total
sites.wide$CVpArea <- sites.wide$`Cercas vivas y Barreras rompevientos `/ sites.wide$Area.Total
sites.wide$SetopArea <- sites.wide$SSPiSetos/ sites.wide$Area.Total

colnames(Coords)[1] <- "ID_2"
colnames(bio)[1] <- "ID_2"
sites.wide$ID_2
df.LC <- merge(x = sites.wide, y = Coords[,c(1:3)], by= c( "ID_2"), all.x = T)
df.tot <-  merge(x = df.LC, y = bio[,c(1,7,9:17)], by= c( "ID_2"), all.x = T) 
df.tot <- relocate(df.tot, NOMBRE.DEL.PREDIO, .after = ID_2)
df.tot[df.tot$NOMBRE.DEL.PREDIO == "La Lomita- La Pradera",]
df.tot[df.tot$ID_2 == 3836,]

table(is.na(Coords$Coordenada.X))

nrow(df.tot) #1458 rows 12.19.21 
head(df.tot)
names(df.tot)
table(df.tot$DEPARTAMENTO)
table(df.tot$Regional)

df.tot$area.sum <- apply(df.tot[,c(6:14)], MARGIN = 1, FUN = sum)
table(dplyr::near(df.tot$area.sum, df.tot$Area.Total))
df.tot$ID_2[which(!dplyr::near(df.tot$area.sum, df.tot$Area.Total))]
df.tot[df.tot$ID_2 == 192,]
#Problem: Additionally, there are some farms that have multiple “Area Total” numbers within a given “Moment” (i.e., multiple sizes at the same time). For example, ID_2 == 5939 shows that the total area is equal to both 17.43 and 27 when Momento_2 == “Cierre”. This is also causing me issues with ID_2 == 5575. I’m also having issues w/ ID_2 == 271 (area is 2x, as there seems to be another verification (“VE”) after “Cierre”) & 6419 (areas of polygons just don’t add up to the total area).


# Google Earth files ------------------------------------------------------
#All farms
names(df.tot)
LCcoords <- df.tot[!is.na(df.tot$Coordenada.X),]
table(LCcoords$Regional) #481 NAs, Over 200 in each region 
sitesLCsp <- SpatialPointsDataFrame(cbind(LCcoords$Coordenada.X, LCcoords$Coordena.Y), data = LCcoords, proj4string = CRS("EPSG:4326")) ##281 farms w/out coordinates!!!!
names(sitesLCsp@data)[1] <- "name"
writeOGR(obj = sitesLCsp, dsn = paste0(bs, "Desktop/Grad_School/PhD/Field_Work/TNCfarms.kml"), layer = "TNCfarms", driver = "KML")

##Biodiversity farms kml (Coffee & Piedemonte)
BirdBio <- LC.pot[LC.pot$AVES_GCS.GAICA_2013.2019 == 1,]
BioCoords <- BirdBio[!is.na(BirdBio$Coordenada.X),] #2 farms w/out coords
sitesBiosp <- SpatialPointsDataFrame(cbind(BioCoords$Coordenada.X, BioCoords$Coordena.Y), data = BioCoords, proj4string = CRS("EPSG:4326"))
names(sitesBiosp@data)[1] <- "name"

writeOGR(obj = sitesBiosp, dsn = paste0(bs, "Desktop/Grad_School/PhD/Field_Work/BioTNCfarms.kml"), layer = "BioTNCfarms", driver = "KML")

#Create spdf of all survey locations
BirdSurveysSp <- SpatialPointsDataFrame(cbind(BirdSurveys$long, BirdSurveys$lat), data = BirdSurveys, proj4string = CRS("EPSG:4326"))

##Attempt to add color to kml file.. Why didn't it work!? Tried both "Color", "color" and "IconColor" with no luck for any. Colors were "red", "lime","cyan"
sitesBiosp@data$Color <- rep("", nrow(sitesBiosp@data))
sitesBiosp@data$Color <- ifelse(sitesBiosp@data$AVES_GCS.UNILLANOS_2019 == 1, "red", "lime")
sitesBiosp@data$Color <- ifelse(sitesBiosp@data$Coordenadas_estaciones_telemetria == 1, "cyan", sitesBiosp@data$Color)
sitesBiosp@data$Color <- as.factor(sitesBiosp@data$Color)
sitesBiosp@data <- relocate(sitesBiosp@data, Color, .after = name)
names(sitesBiosp)

#Same approach but with methodology
sitesBiosp@data$Methodology <- rep("", nrow(sitesBiosp@data))
sitesBiosp@data$Methodology <- ifelse(sitesBiosp@data$AVES_GCS.UNILLANOS_2019 == 1, "Unillanos", "GAICA")
sitesBiosp@data$Methodology <- ifelse(sitesBiosp@data$Coordenadas_estaciones_telemetria == 1, "Telemetry", sitesBiosp@data$Methodology)
sitesBiosp@data$Methodology <- as.factor(sitesBiosp@data$Methodology)

#All 62 spatial farms w/ bird biodiversity
spGaica <- sitesLCsp[sitesLCsp$AVES_GCS.GAICA_2013.2019 == 1, ]
spGaica@data$Methodology <- ifelse(spGaica@data$Coordenadas_estaciones_telemetria == 1, "Telemetry", "Point Count")


# Surrounding Landcover ---------------------------------------------------
#Grabbed the CORINE 2018 LC database for all of Colombia from http://geoservicios.ideam.gov.co/geonetwork/srv/spa/catalog.search;jsessionid=97B6F80606F3D7E735B92FA7456F174E#/metadata/285c4d0a-6924-42c6-b4d4-6aef2c1aceb5 

ColLC <- rgdal::readOGR(dsn = "/Users/aaronskinner/Library/CloudStorage/OneDrive-UBC/Grad_School/PhD/Field_Work/Spatial_Files/Coberturas_100K_2018/shape coberturas 2018", layer = "cobertura_tierra_clc_2018")

#raster(paste0(wd, "NEON-DS-Field-Site-Spatial-Data/SJER/DigitalTerrainModel/SJER2013_DTM.tif"))

# Site summaries ----------------------------------------------------------
LC.pot <- df.tot[df.tot$Regional == "4. Ecorregión Cafetera" | df.tot$Regional == "5. Piedemonte Orinocense",] #pot for potential sites
table(LC.pot$Regional) #Confirm

cafe <- df.tot[df.tot$Regional ==  "4. Ecorregión Cafetera", ]
llano <- df.tot[df.tot$Regional == "5. Piedemonte Orinocense",]
sum(is.na(cafe$Coordenada.X)) #281 farms w/ no coords


summary(df.tot$Area.Total)
hist(df.tot[df.tot$Area.Total < 250,]$Area.Total)
hist(df.tot[df.tot$Area.Total > 250,]$Area.Total)
hist(df.tot[df.tot$linear < 1000,]$linear)

names(LC.pot)
LC.per <- LC.pot[,c(4,20:23)]
lapply(LC.per,summary)

for(i in 1:ncol(LC.per)){
  hist(LC.per[,i],main = colnames(LC.per)[i])
}

summary(llano$Area.Total)
hist(llano[llano$Area.Total < 250,]$Area.Total)
hist(llano[llano$Area.Total > 500,]$Area.Total)
hist(llano$CVpArea)

llano.per <- llano[,c(4,20:23)]
lapply(llano.per,summary)
for(i in 1:ncol(llano.per)){
  hist(llano.per[,i], main = colnames(llano.per)[i])
}

cafe.per <- cafe[,c(4,20:23)]
lapply(cafe.per,summary)
for(i in 1:ncol(cafe.per)){
  hist(cafe.per[,i], main = colnames(cafe.per)[i])
}

#The 6 unillanos farms and 3 telemetry farms are within the 42 farms where GAICA conducted point counts. ID 3189 has all 3 methodologies
lapply(df.tot[,c(28:36)], table)
BirdBio <- LC.pot[LC.pot$AVES_GCS.GAICA_2013.2019 == 1,]
nrow(BirdBio) #42 farms in coffee / llanos
table(BirdBio$Regional)
df.tot[df.tot$AVES_GCS.GAICA_2013.2019 == 1,] #64 farms
df.tot[df.tot$Coordenadas_estaciones_telemetria == 1,] #3 farms, 2 cafe 1 in Piedemonte. 
df.tot[df.tot$AVES_GCS.UNILLANOS_2019 == 1,] #6 farms, Piedemonte 


BirdBio.per <- BirdBio[,c(4, 20:23)]
lapply(BirdBio.per,summary)
for(i in 1:ncol(BirdBio.per)){
  hist(BirdBio.per[,i], main = colnames(BirdBio.per)[i])
}

#Let's check correlations
library(GGally)
names(LC.per)
GGally::ggcorr(BirdBio.per)
cor(BirdBio.per)
ggpairs(BirdBio.per)

##Let's filter bird biodiversity farms to get some potential candidates for visiting.
table(BirdBio$Regional)
BirdBio$
BirdBio[BirdBio$Area.Total < 500 & BirdBio$Area.Total > 10 & BirdBio$SSPi.1 > .01,]
BirdBio[BirdBio$SSPi.1 > .05, c("Regional","SSPi.1", "linear", "lowI.1", "medI.1", "highI.1", "area.sum")]
BirdBio[BirdBio$linear > 50 & BirdBio$Regional == "4. Ecorregión Cafetera", c("ID_2","NOMBRE.DEL.PREDIO","Regional","SSPi.1", "linear", "lowI.1", "medI.1", "highI.1", "area.sum")]
BirdBio[BirdBio$linear < 50 & BirdBio$highI.1 > .5 & BirdBio$Regional == "4. Ecorregión Cafetera", c("ID_2","NOMBRE.DEL.PREDIO","Regional","SSPi.1", "linear", "lowI.1", "medI.1", "highI.1", "area.sum")]
BirdBio[BirdBio$Area.Total > 500,]

##All potential farms
nrow(LC.pot[LC.pot$linear < 20 & LC.pot$highI.1 > .8 & LC.pot$Regional == "5. Piedemonte Orinocense", c("ID_2","NOMBRE.DEL.PREDIO","Regional","SSPi.1", "linear", "lowI.1", "medI.1", "highI.1", "area.sum")]) #35 in eje, 54 in Meta
sort(LC.pot[LC.pot$SSPi.1 > .1 & LC.pot$lowI.1 > .05 & LC.pot$Regional == "5. Piedemonte Orinocense", c("ID_2","NOMBRE.DEL.PREDIO","Regional","SSPi.1", "linear", "lowI.1", "medI.1", "highI.1", "area.sum")]$SSPi.1)



sf_cafe <- sf_bio[sf_bio$Regional == "4. Ecorregión Cafetera",]
sf_meta <- sf_bio[sf_bio$Regional == "5. Piedemonte Orinocense",]
hvy_sspi <- sf_all[sf_all$Regional == "5. Piedemonte Orinocense" & sf_all$SSPiArb.1 > .5 & sf_all$bosque.1 > .05,] 
trad <- sf_all[sf_all$Regional == "5. Piedemonte Orinocense" & sf_all$SSPiArb.1 < .1 & sf_all$bosque.1 > .05 & sf_all$highI.1 > .5,] 
LC.pot[LC.pot$Regional == "5. Piedemonte Orinocense" & LC.pot$SSPi.1 > .1,]$SSPi.1 ##134 farms (in eje), only 90 w/ coordinates. 
nrow(hvy_sspi)
ggplot(data = neCol) + geom_sf() + geom_sf(data = neColDepts) + geom_sf(data = sf_all[sf_all$Regional == "5. Piedemonte Orinocense",], color = "blue") + geom_sf(data = trad, color = "orange") + geom_sf(data = hvy_sspi, color = "red") + geom_sf(data = sf_cafe, color = "green") + coord_sf(xlim =c(st_bbox(hvy_sspi)[1], st_bbox(hvy_sspi)[3]),ylim = c(st_bbox(hvy_sspi)[2], st_bbox(hvy_sspi)[4]), label_axes = "____", expand = TRUE)

ggplot(data = neCol) + geom_sf() + geom_sf(data = neColDepts) + geom_sf(data = trad, color = "orange") + geom_sf(data = hvy_sspi, color = "red") + coord_sf(xlim =c(st_bbox(hvy_sspi)[1], st_bbox(hvy_sspi)[3]),ylim = c(st_bbox(hvy_sspi)[2], st_bbox(hvy_sspi)[4]), label_axes = "____", expand = TRUE) + ggrepel::geom_text_repel(
  data = hvy_sspi, aes(label = name, geometry = geometry),
  stat = "sf_coordinates", max.overlaps = 100)


ggplot(data = neCol) + geom_sf() + geom_sf(data = neColDepts) + geom_sf(data = sf_all[sf_all$Regional == "4. Ecorregión Cafetera",], aes(color = SSPiArb.1)) + geom_sf(data = sf_cafe, color = "green") + coord_sf(xlim =c(st_bbox(hvy_sspi)[1], st_bbox(hvy_sspi)[3]),ylim = c(st_bbox(hvy_sspi)[2], st_bbox(hvy_sspi)[4]), label_axes = "____", expand = TRUE)

names(sf_meta)
lapply(sf_meta[,c(5,21:24)], summary)

#Meta
ggplot(data = neCol) + geom_sf() + geom_sf(data = neColDepts) + geom_sf(data = sf_meta, aes(color = SSPiArb.1)) + coord_sf(xlim =c(st_bbox(sf_meta)[1], st_bbox(sf_meta)[3]),ylim = c(st_bbox(sf_meta)[2], st_bbox(sf_meta)[4]), label_axes = "____", expand = TRUE) + ggrepel::geom_text_repel(
  data = sf_meta, aes(label = name, geometry = geometry),
  stat = "sf_coordinates", max.overlaps = 100)

#Cafe
ggplot(data = neCol) + geom_sf() + geom_sf(data = neColDepts) + geom_sf(data = sf_cafe, aes(color = SSPiArb.1)) + coord_sf(xlim =c(st_bbox(sf_cafe)[1], st_bbox(sf_cafe)[3]),ylim = c(st_bbox(sf_cafe)[2], st_bbox(sf_cafe)[4]), label_axes = "____", expand = TRUE) + ggrepel::geom_text_repel(
  data = sf_cafe, aes(label = name, geometry = geometry),
  stat = "sf_coordinates", max.overlaps = 100)

#Identify farms nearby to sites where biodiversity data was collected in el eje cafetero#
nearL <- st_is_within_distance(sf_cafe, sf_all, dist = 5000) #On the per farm level, so result is any farm w/in 10km of ANY of biodiversity farms
ind <- unlist(nearL)
near <- sort(unique(sf_all[ind,]$name))
sort(sf_cafe$name) #Compare "near" object to the biodiversity farms, see there is overlap

#Of non-biodiversity farms, these are the 36 additional farms w/in 10km
fincs_near <- filter(LC.pot, ID_2 %in% near)
nrow(fincs_near) #37 farms - 13 biodiversity = 24 farms

c("ID_2","NOMBRE.DEL.PREDIO","Regional","SSPi.1", "linear", "lowI.1", "medI.1", "highI.1", "area.sum")

##Let's combine some potential sites
sspi_near <- fincs_near[fincs_near$AVES_GCS.GAICA_2013.2019 != 1 & fincs_near$linear > 50 & fincs_near$lowI.1 > .05, ] #10 additional farms
trad_near <- fincs_near[fincs_near$AVES_GCS.GAICA_2013.2019 != 1 & fincs_near$linear < 20 & fincs_near$highI.1 > .6, ]
cafe_bio <- BirdBio[BirdBio$Regional == "4. Ecorregión Cafetera",]
cafe_sites <- rbind(cafe_bio, sspi_near, trad_near)
#cafe_sites <- relocate(cafe_sites, NOMBRE.DEL.PREDIO, .after = ID_2)
nrow(cafe_sites) #24 sites

#Create Excel for Pablo, round 2
hvy_sspi$Reason <- "hvy_sspi"
trad$Reason <- "traditional"
#Notice I just manually changed the region above (between ecorregion cafetera and piedemonte orinocense), and didn't do everything twice 
cafe_pablo <- st_drop_geometry(rbind(hvy_sspi, trad))
meta_yadi <- st_drop_geometry(rbind(hvy_sspi, trad))
head(meta_yadi)
cafe_pablo[cafe_pablo$name == 2380, c("bosque.1", "SSPiArb.1", "linear", "highI.1") ]
names(cafe_pablo)
meta_yadi <- meta_yadi[, c("name","NOMBRE.DEL.PREDIO", "Area.Total", "Momento_2","bosque.1", "SSPiArb.1", "linear", "highI.1", "SSPi", "AVES_GCS.GAICA_2013.2019", "Reason") ]
meta_yadi[meta_yadi$SSPi > 0,]
write.csv(meta_yadi, file = "/Users/aaronskinner/Library/CloudStorage/OneDrive-UBC/Grad_School/PhD/Field_Work/Site_Selection/meta_sites2.csv")

#write.csv(cafe_sites, file = "/Users/aaronskinner/Library/CloudStorage/OneDrive-UBC/Grad_School/PhD/Field_Work/cafe_sites.csv")
write.csv(cafe_sites, file = "/Users/aaronskinner/Library/CloudStorage/OneDrive-UBC/Grad_School/PhD/Field_Work/cafe_sites.csv")
meta_sites <- BirdBio[BirdBio$Regional == "5. Piedemonte Orinocense",]
write.csv(meta_sites, file = "/Users/aaronskinner/Library/CloudStorage/OneDrive-UBC/Grad_School/PhD/Field_Work/meta_sites.csv")


#Hone in on the rarer elements (big farms, farms w/ lots of silvopasture, bosque maduro). Map these. Put them into Google Earth. Then think about controlling for elevation, temperature, rainfall, surrounding LC


##Lets make some maps
# Maps plotting --------------------------------------------------------------
library(rnaturalearthdata)
library(smoothr)
library(rnaturalearth)

load(paste0(bs, "Desktop/Grad_School/MS/EWPW/Data_Whips/Analysis/2020Analyses/NE_layers_Colombia.Rdata"))

ne_scale <- 10 #ne = natural earth. If having issues, may need to play with scale!
SA <- ne_countries(type = 'countries', continent = "South America", scale = 110, returnclass = "sf")

# land polygon
neCol <- ne_countries(scale = 50, type = "countries", country = "Colombia", returnclass = "sf")
# function to subset other features to those  within this land area
wh_subset <- function(x) {
  in_wh <- as.logical(st_intersects(x, neCol, sparse = FALSE))
  st_transform(x[in_wh], crs = crs.Mex) #NOTE CRS
}
#states_provinces_lines
neColDepts <- ne_download(scale = ne_scale, category = "cultural",
                              type = "admin_1_states_provinces_lines",
                              returnclass = "sf") %>%
  st_geometry() %>%
  wh_subset()

# rivers
ne_rios <- ne_download(scale = ne_scale, category = "physical",
                         type = "rivers_lake_centerlines",
                         returnclass = "sf") %>%
  st_geometry() %>%
  wh_subset()

#ne_cities <- ne_download(scale = ne_scale, category = "cultural",
                       type = "populated_places",
                       returnclass = "sf") %>%
  st_geometry() %>%
  wh_subset()

#Equidistant cylindrical. If needed
#equidist <- "+proj=eqc +lon_0=-72.421875 +lat_ts=5.5892009 +datum=WGS84 +units=m +no_defs"

sf_all <- st_as_sf(sitesLCsp)
sf_bio <- st_as_sf(sitesBiosp)
sf_gaica <- st_as_sf(spGaica)
sf_BirdSurveys <- st_as_sf(BirdSurveysSp)
sf_gaica_jit <- st_jitter(sf_gaica, factor = .01)

library(ggspatial)
grays <- gray.colors(n = 15)
ggplot(data = SA) + geom_sf() + geom_sf(data = SA[SA$adm0_a3 == "COL",], color = "green") + layer_spatial(st_bbox(sf_gaica), color = "red") 
#Grey scale
ggplot(data = SA) + geom_sf() + geom_sf(data = SA[SA$adm0_a3 == "COL",], size = 2, color = "black") + layer_spatial(st_bbox(sf_gaica), color = grays[1]) 

#Just coffee region & Piedemonte
ggplot(data = neCol) + geom_sf() + geom_sf(data = neColDepts) + geom_sf(data = sf_bio, aes(color = Methodology)) + coord_sf(xlim =c(sitesBiosp@bbox[1,1],sitesBiosp@bbox[1,2]),ylim = c(sitesBiosp@bbox[2,1],sitesBiosp@bbox[2,2]), label_axes = "____", expand = TRUE)

metaBioSurveys <- sitesBiosp[sitesBiosp$Regional == "5. Piedemonte Orinocense",]
#Bird survey sampling locations (all methodologies)
ggplot(data = neCol) + geom_sf() + geom_sf(data = neColDepts) + geom_sf(data = sf_BirdSurveys, alpha = .3, aes(shape = institution, color = year)) + coord_sf(xlim =c(metaBioSurveys@bbox[1,1],metaBioSurveys@bbox[1,2]),ylim = c(metaBioSurveys@bbox[2,1],metaBioSurveys@bbox[2,2]), label_axes = "____", expand = TRUE)

ggplot(data = sf_BirdSurveys) + geom_sf(data = sf_BirdSurveys, alpha = .3, aes(color = Farm))
table(sf_BirdSurveys$LC)

ggplot(data = neCol) + geom_sf() + geom_sf(data = neColDepts) + geom_sf(data = sf_BirdSurveys, alpha = .3, size = 3, aes(color = Protocol, shape = institution)) + coord_sf(xlim =c(metaBioSurveys@bbox[1,1],metaBioSurveys@bbox[1,2]),ylim = c(metaBioSurveys@bbox[2,1],metaBioSurveys@bbox[2,2]), expand = TRUE)

ggplot(data = neCol) + geom_sf() + geom_sf(data = neColDepts) + geom_sf(data = sf_BirdSurveys[sf_BirdSurveys$Protocol == "PUNTOS" | sf_BirdSurveys$Protocol == "PUNTOS DE MUESTREO",], alpha = .3, aes(color = institution, shape = year)) + coord_sf(xlim =c(metaBioSurveys@bbox[1,1],metaBioSurveys@bbox[1,2]),ylim = c(metaBioSurveys@bbox[2,1],metaBioSurveys@bbox[2,2]), label_axes = "____", expand = TRUE)

View(BirdSurveys[BirdSurveys$institution == "Universidad de los Llanos (UNILLANOS)", ])
View(BirdSurveys[BirdSurveys$institution == "Asociacion GAICA", ])

##All bird biodiversity data
bbox <- st_bbox(c(xmin = -73.887678, xmax = -73.463852, ymax = 3.92, ymin = 3.2), crs = st_crs(4326)) #The jitter applied is making things not line up perfectly
ggplot(data = neCol) + geom_sf() + geom_sf(data = neColDepts) + layer_spatial(bbox, color = "red") + geom_sf(data = sf_gaica_jit, alpha = .5, aes(color = Methodology)) + coord_sf(xlim =c(spGaica@bbox[1,1],spGaica@bbox[1,2]),ylim = c(spGaica@bbox[2,1],spGaica@bbox[2,2]), label_axes = "____", expand = TRUE) #+ annotation_scale(location = "bl") + geom_sf(data = ne_cities, color = "light blue") + geom_sf(data = ne_rios, color = "light blue") + theme(legend.position = "none") + theme(axis.title = element_blank())
st_bbox(sf_meta)

ggsave("/Users/aaronskinner/Library/CloudStorage/OneDrive-UBC/Grad_School/PhD/Figures/Map_TNC_field_work/South_America_grey.png", bg = "white", units = "mm",device = "png", dpi = 300)

ggplot(data = neCol) + geom_sf() + geom_sf(data = neColDepts) + geom_sf(data = sf_all[sf_all$Regional == "5. Piedemonte Orinocense",], color = "blue") + geom_sf(data = hvy_sspi, color = "red") + geom_sf(data = trad, color = "orange") + coord_sf(xlim =c(st_bbox(hvy_sspi)[1], st_bbox(hvy_sspi)[3]),ylim = c(st_bbox(hvy_sspi)[2], st_bbox(hvy_sspi)[4]), label_axes = "____", expand = TRUE)

ggsave("/Users/aaronskinner/Library/CloudStorage/OneDrive-UBC/Grad_School/PhD/Figures/Map_TNC_Field_Work/Bio_Field_Work2.png", bg = "white", units = "mm",device = "png", dpi = 300)

##Get SELVA farm coordinates, and polygon encompassing study area
poly <- readOGR(dsn = "/Users/aaronskinner/Library/CloudStorage/OneDrive-UBC/Grad_School/PhD/Field_Work/Spatial_Files/Study_sites_poly_large.kml") #Bring in hand drawn polygon from google earth
writeOGR(obj = poly, dsn = "/Users/aaronskinner/Library/CloudStorage/OneDrive-UBC/Grad_School/PhD/Field_Work/Spatial_Files/Study_sites_polygon_SELVA", layer = "Study_sites_polygon", driver="ESRI Shapefile") #Convert to a shapefile

##Map for ANLA permit 
poly_sf <- st_as_sf(poly)
study_farms <- sf_all[sf_all$name == 3849 | sf_all$name == 268 | sf_all$name == 1522 | sf_all$name == 1025 | sf_all$name == 89 | sf_all$name == 280, c("name","geometry")] #1522 is one of Andorra predios (to my understanding)

#Map to ensure that polygon and point coordinates line up as expected
ggplot(data = poly_sf) + geom_sf() + geom_sf(data = study_farms) + ggrepel::geom_text_repel(data = study_farms, aes(label = name, geometry = geometry),
  stat = "sf_coordinates")

#save(ne_rios, neCol, neColDepts, SA, file = paste0(bs, "Desktop/Grad_School/MS/EWPW/Data_Whips/Analysis/2020Analyses/NE_layers_Colombia.Rdata"))
#save.image("/Users/aaronskinner/Library/CloudStorage/OneDrive-UBC/Grad_School/R_Files/PhD/ColombiaPhD.Rdata")


# oSCR spatial capture recap ----------------------------------------------
#Gates Dupont and others have some good vignettes, see this and all the associated links: https://bookdown.org/chrissuthy/oSCRvignettes/
#Also a user group: https://groups.google.com/forum/#!forum/oscr_package
#Idea for setting up study design is pretty cool, generate all possible trap locations using st_sample function, filter trap locations by what is feasible (e.g., elevation, slope, or "no go" polygons), then use the scrdesignGA function stating a specific criteria (e.g. Qp) to select the optimal subset of traps among all available trap locations. Finally, can use simulator() function (Dupont created) to see whether your optimal trap location design will recover the simulated parameter estimates.

library(oSCR) #Can easily incorporate telemetry data. Also see paper, "Improved inferences about landscape connectivity from spatial capture–recapture by integration of a movement model"
#Some key functions:
?telemetry #This brings up a copyable script from Royle et al (2013), using the data("nybears") set. Similar to vignette 1 (integrating telmetry data -  Integrated RSF-SCR models in oSCR)
?e2dist #Distance between trap locations and activity centers
?scrdesignGA #Create optimal trap location design. Would have to specify beta0 and sigma parameters, which seems like you would need a good amount of baseline data. Crit = the 3 Qp values in the Dupont (2021) article?
?data2oscr #These two functions seem key for making the capture array 
?make.scrFrame
?oSCR.fit #Fit models


