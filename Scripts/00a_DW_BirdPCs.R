## PhD birds in silvopastoral landscapes##
# Data wrangling 00a -- Create base data frame (BirdPCs)
## This script loads raw data from data providers, combines data into single df, & tidies and filters data to produce the environment necessary for future scripts & downstream analyses

# Contents
# 1) Load & format data that will allow for join into a single df
# 2) Merge into single df & continue to format
# 3) Create various point count (PC) files different files based on inclusion of location, date, and habitat 
# 4) Download environmental data -- Temp, precipitation, & elevation
# 5) Identify how many distinct sampling periods each point count location has
# 6) Export Rdata object

# Libraries ---------------------------------------------------------------
library(janitor) # tabyl function
library(tidyverse)
library(chron)
library(raster)
library(sf)
library(sp)
library(gtools)
library(AICcmodavg)
library(readxl)
library(xlsx)
library(chron)
library(naniar)
library(ggpubr)
library(cowplot)
library(conflicted)
library(ColOpenData)
library(geodata)
library(terra)
library(tidyterra)
ggplot2::theme_set(theme_cowplot())
conflicts_prefer(dplyr::select)
conflicts_prefer(dplyr::filter)
conflicts_prefer(utils::head)

#Load Rdata & useful themes / functions script
#load("Rdata/the_basics_11.14.24.Rdata")
source("/Users/aaronskinner/Library/CloudStorage/OneDrive-UBC/Grad_School/Rcookbook/Themes_funs.R")

# Load bird obs & meta data -----------------------------------------------------------

files <- list.files(path = "Data/Aves", pattern = "^A.*xlsx$") #Add |D after A if you want Monroy/Skinner data too

# At present these are just the files with point counts
df_Birds <- map(files, \(file){
  read_xlsx(path = file.path("Data/Aves", file),
            sheet = "Base_de_datos",
            na = c("Sin informacion", "N/A"))
})

df_metadata <- map(files, \(file){
  read_xlsx(path = file.path("Data/Aves", file),
            sheet = "Metadatos_puntos_conteo",
            na = c("Sin informacion", "N/A"))
})

##Join lists together for initial formatting & then separate again STILL TO DO

# Ensure data frames are in the same order
files
names(df_Birds) <- c("Cipav", "Gaica_dist", "Gaica_mbd", "Ubc_gaica_Caf", "Ubc_gaica_Meta", "Ubc_gaica_OQ",  "Ubc", "UniLlanos") # "Ubc_Monroy"
names(df_metadata) <- c("Cipav", "Gaica_dist", "Gaica_mbd", "Ubc_gaica_Caf", "Ubc_gaica_Meta", "Ubc_gaica_OQ", "Ubc", "UniLlanos")

# Format metadata ---------------------------------------------------------
# Standardize metadata column names
df_metadata <- map(df_metadata, \(df) {
  df <- df %>% clean_names(case = "snake") %>% 
    rename(Id_muestreo = id_punto_muestreo_final)
  names(df) <- str_to_sentence(names(df))
  df
})

df_metadata <- lapply(df_metadata, function(x) {
  cbind(x, Fecha = lubridate::mdy(paste(x$Mes, x$Dia, x$Ano, sep = "/")))
})

df_metadata[c(4:6)] <- map(df_metadata[c(4:6)], \(df){
  df %>% rename(Hora = Hora_inicial)
})

df_metadata[c(1:3)] <- map(df_metadata[c(1:3)], \(df){
  df %>% rename(Total_pc_time = Diferencia_de_minutos_entre_el_registro_con_hora_mas_temprana_y_el_registro_con_hora_mas_tardia) %>% 
    mutate(Total_pc_time = sapply(str_split(Total_pc_time, " "), function(x) {x[2]}),
           Total_pc_time = chron::times(Total_pc_time, format = c(times = "hh:mm:ss")))
})

df_metadata <- map(df_metadata, \(df) 
                   df %>% mutate(Hora = sapply(str_split(Hora, " "), function(x) {x[2]}),
                                 Hora = chron::times(Hora, format = c(times = "hh:mm:ss"))))

#Add repetition number to metadata
df_metadata$Ubc <- df_metadata$Ubc %>% 
  mutate(Rep = row_number(), .by = Id_muestreo)

# Format bird observation data -------------------------------------------
# Standardize column names
df_Birds <- map(df_Birds, \(df) {
  df <- df %>% clean_names(case = "snake") %>% 
    rename(Id_muestreo = id_punto_muestreo_final) 
  names(df) <- str_to_sentence(names(df))
  df
})

# Preprocessing date & time ##DELETE? 
df_Birds <- lapply(df_Birds, function(x) {
  cbind(x, Fecha = lubridate::mdy(paste(x$Mes, x$Dia, x$Ano, sep = "/"))) %>% 
    mutate(Fecha = as.Date(Fecha),
    Hora = sapply(str_split(Hora, " "), function(x) {x[2]}),
    Hora = chron::times(Hora, format = c(times = "hh:mm:ss")))
}) ### GAICA did not record date for 74 Recorridos libres

# Remove irrelevant columns from dataframes
df_Birds_red <- lapply(df_Birds, function(x) {
  dplyr::select(
    x, 1:21, -c(Numero_registro_gsc, Id_registro_biologico_gcs),
    contains(c("Id_muestreo", "Protocolo_muestreo", "Mes", "Hora", "repeticion", "Orden", "Familia", "Especie", "cientifico", "Numero_individuos", "Habitat", "Sistema", "Registrado", "Distancia_observacion", "climatica", "Elevacion", "finca", "grabacion", "Estrato_")), "Ano", "Fecha", "Dia"
  )
})

# Display data collector's name 
lapply(df_Birds_red, function(x) {
  TF <- str_detect(names(x), "revisada|estandarizado|_sin formul|_sin formula|_cipav|_gaica|_ubc|_unillanos|corregido")
  names(x)[TF]
})

# Replace with corrected names
df_Birds_red <- lapply(df_Birds_red, function(df) {
  names <- str_remove(names(df), "revisada|estandarizado|_sin formul|_sin formula|_cipav|_gaica|_ubc|_unillanos|corregido")
  names(df) <- names
  df
})

## Data set specific operations##
# CIPAV data frame has a single individual for each row whereas all other databases have the total number of individuals of a given species per row (Numero_individuos column).
# I don't think it's the right time to do this actually.. It is impossible to preserve all columns and perform this operation correctly, so probably best to do it before modeling
# df_Birds_red$Cipav <- df_Birds_red$Cipav %>%
# group_by(Id_muestreo, Fecha) %>%
# mutate(PC.length = max(Hora) - min(Hora), Hora = min(Hora)) %>%
# group_by(Id_muestreo, Fecha, Hora, Nombre_Ayerbe) %>%
# reframe(across(), Numero_individuos = sum(Numero_individuos)) %>%
# distinct() %>%
# as.data.frame()

df_Birds_red$Cipav <- df_Birds_red$Cipav %>% #38 columns
  group_by(Id_muestreo, Fecha) %>%
  mutate(PC.length = (max(Hora) - min(Hora)) / 60, # Can delete as the metadata file is correct
         Hora = min(Hora)) %>% #convert from seconds to minutes
  group_by(Id_muestreo, Fecha, Hora, Nombre_cientifico_final) %>%
  reframe(across(), Numero_individuos = sum(Numero_individuos)) %>%
  distinct() %>% 
  as.data.frame()

# NOTE:: #GAICA Distancia sort of has this issue.. They recorded the specific habitat where the bird was observed & the distance away, so some species have multiple rows in a single point count, but the Numero_individuos reported is the total for that distance & habitat type
df_Birds_red$Gaica_dist %>%
  distinct(
    Id_muestreo, Ocasion_muestreo_repeticion,
    Hora, Nombre_cientifico_final
  ) %>%
  count(Id_muestreo, Nombre_cientifico_final, sort = TRUE) %>%
  head()

# Remove UBC microhabitat columns
df_Birds_red$Ubc <- df_Birds_red$Ubc %>% select(-c(Habitat1 | Habitat2))

#Change species name column so all data frames match ('Nombre_cientifico_final')
df_Birds_red[4:6] <- map(df_Birds_red[4:6], \(df){
  df %>% rename(Nombre_cientifico_final = Nombre_cientifico_final_ayerbe_2018)
})
# Remove practice day data 
df_Birds_red$Ubc_gaica_Caf <- df_Birds_red$Ubc_gaica_Caf %>% 
  filter(!Fecha %in% as.Date(c("2024-05-27", "2024-05-28"))) %>% # Ensayo dates 
  filter(!Id_muestreo %in% c(paste0("G-MB-Q-LCA_0", 7:9), "OQ_Practica")) # PCs surveyed one time

# Combine dfs -------------------------------------------------------------
## Combine all dfs & remove more irrelevant columns##
Birds_all <- smartbind(list = df_Birds_red) %>%
  dplyr::select(-c(
    Coordenadas_geograficas, Incertidumbre_coordenadas, Precision_coordenadas,
    Autoria_nombre_cientifico, Numero_especie, Municipio
  ))
rownames(Birds_all) <- NULL
names(Birds_all)
dim(Birds_all)

# Shorten names & create Ecoregion column
unique(Birds_all$Pregunta_investigacion_gsc) # Ecotropico will update
Birds_all2 <- Birds_all %>%
  mutate(
    Pregunta_investigacion_gsc = case_when(
      Pregunta_investigacion_gsc == "Monitoreo_biodiversidad" ~ "MBD",
      Pregunta_investigacion_gsc == "Analisis_distanciamiento" ~ "Distancia",
      Pregunta_investigacion_gsc == "Monitoreos_biodiversidad" ~ "MBD",
      Pregunta_investigacion_gsc == "Ocupacion_dinamica" ~ "DOM",
      .default = as.character(Pregunta_investigacion_gsc)
    ),
    Protocolo_muestreo = case_when(
      Protocolo_muestreo == "Observacion en puntos fijos de conteo" ~ "Punto conteo",
      Protocolo_muestreo == "Captura con redes de niebla" ~ "Red de niebla",
      Protocolo_muestreo == "Observacion ad hoc en recorridos libres" ~ "Ad hoc",
      .default = Protocolo_muestreo
    ),
    # Create Ecoregions based on related Departments
    Departamento = ifelse(Departamento == "La Guajira",
      "Guajira", Departamento
    ),
    Ecoregion = case_when(
      Departamento == "Atlantico" | Departamento == "Bolivar" ~ "Bajo Magdalena",
      Departamento == "Cesar" | Departamento == "Guajira" ~ "Rio Cesar", # Valle del rio Cesar
      Departamento == "Boyaca" | Departamento == "Santander" ~ "Boyaca Santander",
      Departamento == "Caldas" | Departamento == "Risaralda" | Departamento == "Tolima" |
        Departamento == "Quindio" | Departamento == "Valle del Cauca" ~ "Cafetera",
      Departamento == "Meta" ~ "Piedemonte",
      .default = Departamento
    )
  )

# Continue formatting columns
Birds_all3 <- Birds_all2 %>%
  rename(Habitat_og = Habitat, Nombre_ayerbe = Nombre_cientifico_final) %>% 
  mutate(
    across(c(Nombre_finca, Nombre_ayerbe, Habitat_og, Observacion_de_grabacion, Grabacion), 
           ~ str_to_sentence(., locale = "en")),
    across(c(Nombre_finca, Nombre_ayerbe, Habitat_og, Observacion_de_grabacion, Grabacion), 
           ~ str_squish(.)),
    Uniq_db = paste(Nombre_institucion, Pregunta_investigacion_gsc),
    Hora = chron::times(Hora, format = c(times = "hh:mm:ss")),
    # Especie = str_to_sentence(Especie, locale = "en"),
    Id_group = sapply(
      str_split(Id_muestreo, "_"),
      function(x) {
        x[1]
      }), 
    Ano_grp = case_when(
      Ano %in% c(2013, 2014) ~ "13-14",
      Ano %in% c(2016, 2017) ~ "16-17",
      Ano == 2019 ~ "19",
      Ano == 2022 ~ "22",
      Ano == 2024 ~ "24"
    )) %>%
  mutate(across(.cols = c(matches("Id_pr|Distancia_pr"), Latitud_decimal, Longitud_decimal, Numero_individuos), as.numeric))

### Distancias FULL y Buffer##
Birds_all4 <- Birds_all3 %>%
  #mutate(across(matches("ID_pr|Distancia_pr"), ~ as.numeric(.))) %>% #Not 
  mutate(across(starts_with("Id_pr"), ~ na_if(., 0))) %>%
  mutate(
    Id_gcs = do.call(coalesce, across(starts_with("Id_pr"))),
    # Some farm names are from GCS project, others (particularly reference farms) are not
    Nombre_finca_mixed = do.call(coalesce, across(matches("Nombre_f|Nombre_pr"))), 
    #CHECK:: There is only one ID_predio for each row in the 4 "ID_pr" columns 
    Row_sum = rowSums(across(starts_with("Id_pr")), na.rm = T),
    Same = Id_gcs == Row_sum,
    Distancia_m = rowSums(across(starts_with("Distancia_pr")))
  ) %>%
  mutate(
    Id_gcs = ifelse(Finca_referencia == "Si" & is.na(Id_gcs),
      paste0("Ref_", Nombre_finca_mixed),
      Id_gcs
    )) %>%
  #Fill in Id_gcs for UBC_GAICA data
  mutate(Id_gcs = if_else(is.na(Id_gcs), first(na.omit(Id_gcs)), Id_gcs), .by = Id_muestreo) %>%
  select(-c(Nombre_predio_gcs_poligono_full, Id_predio_gcs_poligono_full, Id_predio_mas_cercano_gcs_poligono_full, Nombre_predio_gcs_poligono_buffer, Id_predio_gcs_poligono_buffer, Id_predio_mas_cercano_gcs_poligono_buffer, Distancia_predio_mas_cercano_gcs_poligono_full, Distancia_predio_mas_cercano_gcs_poligono_buffer, Row_sum, Same))

# Create data base with just point counts
BirdPCs <- Birds_all4 %>% filter(Protocolo_muestreo == "Punto conteo") 
nrow(BirdPCs)

#Remove vuelos
#STILL TO DO: INVESTIGATE UNILLANOS, CIPAV, GAICA MBD
BirdsA <- BirdPCs %>% filter(Estrato_vertical != "Vuelo" | is.na(Estrato_vertical) & !(tolower(Distancia_observacion) %in% c("sobrevuelo", "vuelo")))
nrow(BirdsA)

# Make distances numeric, 
BirdsA2 <- BirdsA %>%
  mutate(Distancia_observacion = case_when( # BirdsA = Birds analysis file
    Distancia_observacion == "0-15" ~ "15",
    Distancia_observacion == "15-30" ~ "30",
    Distancia_observacion == "30-50" ~ "50",
    Distancia_observacion == "< 25" ~ "25",
    Distancia_observacion == "<50" ~ "50",
    Distancia_observacion == ">50" ~ "51",
    Distancia_observacion == "> 50" ~ "51",
    .default = Distancia_observacion
  )) %>%
  mutate(Distancia_observacion = as.numeric(Distancia_observacion))
# Create Birds analysis file by removing records with distance > 50m
BirdsA3 <- BirdsA2 %>% filter(Distancia_observacion < 51 | is.na(Distancia_observacion))
tabyl(BirdsA3$Distancia_observacion)
nrow(BirdsA2)
## Still to do:: REMOVE VUELOS

# Note there are some rows that still have NAs for distance
BirdsA3 %>%
  filter(is.na(Distancia_observacion)) %>%
  select(Uniq_db, Fecha, Departamento, Id_muestreo, Nombre_finca, Nombre_ayerbe) %>%
  count(Id_muestreo, sort = T)

# Export
# write.csv(BirdsA3, "/Users/aaronskinner/Library/CloudStorage/OneDrive-UBC/Grad_School/PhD/SCR_data/AVES_Updated5.11.23/Birds_Analysis6.13.23.csv", row.names = F)

# Point count (PC) files -----------------------------------------------
## Create different files based on inclusion of location, date, and habitat #
# Create a file where each row is a unique point count x data collector #
PC_uniq <- BirdPCs %>% distinct(Uniq_db, Nombre_institucion, Id_group, Ecoregion, Departamento, Id_muestreo, Id_gcs)

#NOTE: There are 540 unique point count x data collector combinations, but 16 points have multiple coordinates. 556 rows here 
PC_locs_mult <- BirdPCs %>% distinct(Uniq_db, Nombre_institucion, Id_group, Ecoregion, Departamento, Id_gcs, Id_muestreo, Latitud_decimal, Longitud_decimal)
nrow(PC_locs_mult)

# For now, take the average lats & longs of these 16 points.
PC_locs <- PC_locs_mult %>% 
  group_by(Id_muestreo) %>% 
  summarize(Latitud_decimal = round(mean(Latitud_decimal), 4), Longitud_decimal = round(mean(Longitud_decimal), 4)) %>% 
  full_join(PC_uniq, by = "Id_muestreo")

# Inclusion of date, time, and Ano_grp
PC_date <- BirdPCs %>% distinct(Nombre_institucion, Uniq_db, Ecoregion, Departamento, Nombre_finca, Id_gcs, Id_group, Id_muestreo, Ano, Mes, Dia, Fecha, Ano_grp)
nrow(PC_date)

# N_reps is the number of times each point count was sampled, and each row is a unique point count
PC_reps <- PC_date %>% reframe(N_reps = n(), across(.cols = everything()), 
                                .by = Id_muestreo) %>% 
  distinct(Id_muestreo, .keep_all = T) # Rep is for number of repeat surveys

#Calculate the number of samp_periods for each point count -- The idea is that b/c some point counts were surveyed up to 3 times, but always 2 of which were in the same Ano_grp (e.g., 13-14), using distinct() will remove a row in the same Ano_grp. The point count IDs with 2 rows are possible resurvey sites (although see the temporal sampling plot for potential issues of seasonality). 
PC_samp_periods <- PC_date %>%
  distinct(Id_muestreo, Uniq_db, Ano_grp, Ecoregion, Id_gcs) %>%
  reframe(N_samp_periods = n(), across(.cols = everything()), 
          .by = Id_muestreo) %>%
  distinct(Id_muestreo, N_samp_periods, Uniq_db, Ecoregion, Id_gcs)

## NOTE:: 'Hora' is not included in creation of PC_date, so there are some farms that were surveyed multiple times on the same day and these resurveys are currently condensed. For example, El Porvenir was really surveyed 3x but it shows only 2x
PC_reps %>%
  filter(Nombre_institucion == "UBC" & Id_group == "UBC-MB-M-EPO3") 

#Merge with PC_reps & PC_samp_periods
PC_date2 <- PC_date %>% 
  left_join(PC_reps[, c("N_reps", "Id_muestreo")]) %>% 
  left_join(PC_samp_periods[, c("N_samp_periods", "Id_muestreo")])

# Inclusion of habitatOG and Habitat Homologado
PC_hab <- distinct(BirdPCs, Id_muestreo, Uniq_db, Ecoregion, Departamento, Nombre_finca_mixed, Latitud_decimal, Longitud_decimal, Habitat_og, Habitat_homologado_ut)

## Create relevant KMZ files
PC_locsSf <- st_as_sf(PC_locs,
  coords = c("Longitud_decimal", "Latitud_decimal"),
  crs = 4326,
  remove = F
)

# Export reduced set of columns to kml
PC_locsSf %>%
  select(Id_muestreo, Uniq_db, Departamento) %>%
  rename(
    name = Id_muestreo,
    DataBase = Uniq_db #,
    #Farm = Nombre_finca_mixed
  ) # %>%
# st_write(obj = Method_InstKml, driver='kml', dsn="/Users/aaronskinner/Library/CloudStorage/OneDrive-UBC/Grad_School/PhD/Field_Work/Spatial_Files/Method_Inst_TEST.kml", layer = "Method_Inst_TEST")

# Environmental data ---------------------------------------------------
## Download monthly temp & precipitation data 
# May want to include additional vars (e.g., precipitation seasonality): https://www.worldclim.org/data/bioclim.html
Wc_col <- map(c('tavg', 'prec'), \(var){
  worldclim_country(country = "COL", var = var, path = "../Geospatial_data/Environmental")
})


# To match the bioclim variables (1 & 12), take average of temperature & sum of rainfall
avg.temp <- mean(Wc_col[[1]])
tot.prec <- sum(Wc_col[[2]])

## Elevation
coords <- data.frame(st_coordinates(PC_locsSf))
# Ensure every point count has an associated elevation raster. Creates a list with nrow() coords
Elev_90m <- pmap(coords, function(X, Y) {
  elevation_3s(lon = X, lat = Y, path = "../Geospatial_data/Environmental/elevation_90m")
})
?elevation_3s

#Bring in the 6 unique tif files & merge into a single elevation file
files.elev <- list.files(path = "../Geospatial_data/Environmental/elevation_90m/elevation", pattern = ".tif")
Elev_90m <- map(.x = files.elev, .f = \(tif) 
                rast(paste0("../Geospatial_data/Environmental/elevation_90m/elevation/", tif)))
Elev_90m <- do.call(merge, Elev_90m)

## Create list with elevation, temp, & precip
envi.vars <- list(elev.dem = Elev_90m, avg.temp = avg.temp, tot.prec = tot.prec)

# Visualize
imap(envi.vars, \(var, names){
  ggplot() + geom_spatraster(data = var) + 
    labs(title = names)
})

# Extract environmental vars & create df with envi variables at each PC location
envi_df <- cbind(
  PC_locsSf[, c("Id_muestreo","Ecoregion", "Departamento", "Uniq_db", "Id_group")],
  sapply(envi.vars, terra::extract, PC_locsSf, ID = FALSE)) %>% 
  rename(Elev = elev.dem.srtm_21_10, Avg_temp = avg.temp.mean, Tot.prec = tot.prec.sum)
envi_df2 <- envi_df %>%
  st_drop_geometry() %>%
  group_by(Id_muestreo) %>%
  slice_head() %>%
  ungroup()

#Extract data & create df where each row is a point count and there are 12 'prec' columns, one for each month
PrecPCs <- terra::extract(Wc_col[[2]], PC_locsSf, ID = FALSE)
Prec_df <- cbind(PC_locsSf[,c("Id_muestreo","Ecoregion", "Departamento", "Uniq_db")], PrecPCs) %>%
  group_by(Id_muestreo) %>%
  slice_head() %>%
  ungroup() %>%
  st_drop_geometry() %>%
  rename_with(~ str_remove(., "COL_wc2.1_30s_"))

## Download daily precipitation data for Cubarral from IDEAM stations for the 4 months before first date + sampling period 
#Unillanos: sampling period (11/10 - 11/26 of 2019, about 16 days)
Prec_daily19 <- ColOpenData::download_climate(code = "50223", start_date = "2019-07-10", end_date = "2019-11-26", tag = "PTPM_CON") %>% mutate(year = "July - Nov, 2019")
#UBC: Sampling period (5/28 - 6/18 of 2022, about 20 days)
Prec_daily22 <- ColOpenData::download_climate(code = "50223", start_date = "2022-01-28", end_date = "2022-06-18", tag = "PTPM_CON") %>% mutate(year = "Jan - June, 2022")
Prec_daily <- bind_rows(Prec_daily19, Prec_daily22) %>% 
  mutate(date = as.Date(date), 
         md = format(date, "%m-%d")) %>% 
  mutate(day = as.numeric(date - min(date)), .by = year)

#Also see landcover function: the fraction of a landcover class in each cell, at 30-seconds (1km at equator) spatial resolution
#?geodata::landcover

#STILL TO DO: Use divipola codes to extract rainfall data for the appropriate municipalities & dates for each set of point counts. 
#NOTE:: This is for a few points in the given municipality (of variable size), but should know what the appropriate spatial scale is (2x2 kms^2?) & how far away the stations are from sampling points. As a first pass just do what's easy & include it in models, see if it makes a difference. Eventually could explore kriging or something more advanced 
#mpio_sf <- download_geospatial(
  #spatial_level = "mpio",
  #simplified = TRUE,
  #include_geom = TRUE,
  #include_cnpv = FALSE
#)

#Has municipality codes
#mpio_sf %>% filter(codigo_municipio == "50223")

#Plot
#ggplot() + geom_sf(data = mpio_sf)

# Save & export -----------------------------------------------------------
rm(list= ls()[!(ls() %in% c("Birds_all", "Birds_all4", "BirdPCs", "PC_date2", 'PC_hab', "PC_locs", "BirdsA2", "df_Birds", "df_metadata", "df_Birds_red", "Mes_Mod", "PC_locs_mult", "PC_locsSf", "envi_df2", "Prec_df", "Prec_daily"))])
save.image(paste0("Rdata/the_basics_", format(Sys.Date(), "%m.%d.%y"), ".Rdata"))
