## PhD birds in silvopastoral landscapes##
# Data wrangling 00a -- Create base data frame (Bird_pcs)
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
#load("Rdata/the_basics_12.29.24.Rdata")
source("/Users/aaronskinner/Library/CloudStorage/OneDrive-UBC/Grad_School/Rcookbook/Themes_funs.R")

# Load bird obs & meta data -----------------------------------------------------------

files <- list.files(path = "Data/Aves", pattern = "^A.*xlsx$") #Add |D after A if you want Monroy/Skinner data too

# At present these are just the files with point counts
df_birds <- map(files, \(file){
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
names(df_birds) <- c("Cipav", "Gaica_dist", "Gaica_mbd", "Ubc_gaica_Caf", "Ubc_gaica_Meta", "Ubc_gaica_OQ",  "Ubc", "UniLlanos") # "Ubc_Monroy"
names(df_metadata) <- c("Cipav", "Gaica_dist", "Gaica_mbd", "Ubc_gaica_Caf", "Ubc_gaica_Meta", "Ubc_gaica_OQ", "Ubc", "UniLlanos")

# Format bird observation data -------------------------------------------
# Standardize column names
df_birds <- map(df_birds, \(df) {
  df <- df %>% clean_names(case = "snake") %>% 
    rename(Id_muestreo = id_punto_muestreo_final, Count = numero_individuos) 
  names(df) <- str_to_sentence(names(df))
  df
})

# Preprocessing date & time ##DELETE? 
df_birds <- lapply(df_birds, function(x) {
  cbind(x, Fecha = lubridate::mdy(paste(x$Mes, x$Dia, x$Ano, sep = "/"))) %>% 
    mutate(Fecha = as.Date(Fecha),
           Hora = sapply(str_split(Hora, " "), function(x) {x[2]}),
           Hora = chron::times(Hora, format = c(times = "hh:mm:ss")),
           Ano_grp = case_when(
             Ano %in% c(2013, 2014) ~ "13-14",
             Ano %in% c(2016, 2017) ~ "16-17",
             Ano == 2019 ~ "19",
             Ano == 2022 ~ "22",
             Ano == 2024 ~ "24"
           ))
}) ### GAICA did not record date for 74 Recorridos libres

# Remove irrelevant columns from dataframes
df_birds_red <- lapply(df_birds, function(x) {
  dplyr::select(
    x, 1:21,
    contains(c("Id_muestreo", "Protocolo_muestreo", "Ano", "Mes", "Hora", "repeticion", "Orden", "Familia", "Especie", "cientifico", "Count", "Habitat", "Sistema", "Registrado", "Distancia_observacion", "climatica", "Elevacion", "finca", "grabacion", "Cigarras", "Vacas", "Ruido", "Estrato_")), "Fecha", "Dia") %>%
    select(-contains(c("Numero_registro", "Id_registro_biologico", "Observacion_climatica_evento", "Concatenar", "repeticion")))
})

# Display data collector's name 
lapply(df_birds_red, function(x) {
  TF <- str_detect(names(x), "revisada|estandarizado|_sin formul|_sin formula|_cipav|_gaica|_ubc|_unillanos|corregido")
  names(x)[TF]
})

# Replace with corrected names
df_birds_red <- lapply(df_birds_red, function(df) {
  df <- df %>%
    rename_with(~ str_replace_all(
      str_remove_all(., "revisada|estandarizado|_sin formul|_sin formula|_cipav|_gaica|_ubc|_unillanos|corregido|_homologado|_base_infotnc|_investigacion"),
      c("Observacion_climatica" = "Clima", 
        "Distancia_observacion" = "Distancia_bird")
    ))
  df
})

## Group point counts, determine pc_start time & pc_length
# Some data collectors surveyed certain point counts multiple times on the same day, & some data collectors reported the time that each bird was observed instead of a single time at the start of the point count

# Create 'Same_pc' column to indicate the rows where the time is less than the value in the cutoff_time vector. These observations of the same Id_muestreo & Fecha will be grouped as a point count.
# Specify database specific cutoff times, as CIPAV has a single point count that is 90 minutes long (and thus should be grouped together), and UBC & Unillanos has distinct same day point counts that are only separated by 68 and 74 minutes, respectively (and thus should be grouped apart). Otherwise, largest difference in point count times in the same day is 13 minutes (from an 18 minute point count), so I used that for convenience for all other databases. 
cutoff_time <- c(91, rep(18, 5), 68, 74)

df_birds_red <- map2(df_birds_red, cutoff_time, \(df, cutoff){
  df %>% group_by(Id_muestreo, Fecha) %>% 
    arrange(Hora) %>% 
    mutate(Pc_length_day = as.numeric((Hora - first(Hora)) * 1440), #For entire day, not within a PC
           Same_pc = if_else(Pc_length_day < cutoff, "Same", "Diff")) %>% 
    select(-Pc_length_day)
}) 

# Group_by the 'Same_pc' column to generate point count start times & the length of each point count
df_birds_red <- map(df_birds_red, \(df){ 
  df %>% group_by(Id_muestreo, Ano_grp, Fecha, Same_pc) %>% 
    mutate(Pc_start = min(Hora),
           Pc_length = (max(Hora) - min(Hora))) %>% 
    ungroup()
})

# Shorten names, create Ecoregion column, & generate Uniq_db
df_birds_red <- map(df_birds_red, \(df){
  df %>% mutate(
    Pregunta_gsc = case_when(
      Pregunta_gsc == "Monitoreo_biodiversidad" | Pregunta_gsc == "Monitoreos_biodiversidad" ~ "mbd",
      Pregunta_gsc == "Analisis_distanciamiento" ~ "distancia",
      Pregunta_gsc == "Ocupacion_dinamica" ~ "dom", 
      .default = as.character(Pregunta_gsc)
    ),
    Protocolo_muestreo = case_when(
      Protocolo_muestreo == "Observacion en puntos fijos de conteo" ~ "Punto conteo",
      Protocolo_muestreo == "Captura con redes de niebla" ~ "Red de niebla",
      Protocolo_muestreo == "Observacion ad hoc en recorridos libres" ~ "Ad hoc",
      .default = Protocolo_muestreo
    ),
    # Create Ecoregions based on related Departments
    Departamento = ifelse(Departamento == "La Guajira", "Guajira", Departamento),
    Ecoregion = case_when(
      Departamento == "Atlantico" | Departamento == "Bolivar" ~ "Bajo Magdalena",
      Departamento == "Cesar" | Departamento == "Guajira" ~ "Rio Cesar", # Valle del rio Cesar
      Departamento == "Boyaca" | Departamento == "Santander" ~ "Boyaca Santander",
      Departamento == "Caldas" | Departamento == "Risaralda" | Departamento == "Tolima" |
        Departamento == "Quindio" | Departamento == "Valle del Cauca" ~ "Cafetera",
      Departamento == "Meta" ~ "Piedemonte",
      .default = Departamento
    ), 
    Uniq_db = paste(Nombre_institucion, Pregunta_gsc, sep = " ")
  )
})

# Function to convert the entries within columns to title case & remove underscores for consistency & predictability 
standardize_column_contents <- function(col) {
  unique_vals <- unique(col)
  transformed_vals <- setNames(
    str_replace_all(unique_vals, "_", " ") %>% str_to_sentence(),
    unique_vals
  )
  transformed_vals[col]
}

# Apply the function to each data frame in the list
df_birds_red <- map(df_birds_red, \(df) {
  df %>% mutate(
    across(
      .cols = -c(Id_muestreo) & where(is.character), 
      .fns = ~ standardize_column_contents(.)
    )
  )
})

# WORKS, BACKUP CODE
if (FALSE){
  df_birds_red <- map(df_birds_red, \(df){
    df %>% mutate(across(
      .cols = -c(Id_muestreo) & where(is.character), 
      .fns = ~ {unique_vals <- unique(.)
      transformed_vals <- setNames(str_to_sentence(unique_vals), unique_vals)
      return(transformed_vals[.])}
    ))
  })
}

# >Dataset specific operations -------------------------------------------
# CIPAV data frame has a single individual for each row whereas all other databases have the total number of individuals of a given species per row (Count column).
df_birds_red$Cipav <- df_birds_red$Cipav %>% #38 columns
  group_by(Id_muestreo, Fecha, Hora, Nombre_cientifico_final) %>%
  reframe(across(), Count = sum(Count))

# NOTE:: #GAICA Distancia sort of has this issue.. They recorded the specific habitat where the bird was observed & the distance away, so some species have multiple rows in a single point count, but the Count reported is the total for that distance & habitat type
df_birds_red$Gaica_dist %>%
  distinct(
    Id_muestreo, #Ocasion_muestreo_repeticion,
    Hora, Nombre_cientifico_final
  ) %>%
  count(Id_muestreo, Nombre_cientifico_final, sort = TRUE) %>%
  head()

# Remove UBC microhabitat columns
df_birds_red$Ubc <- df_birds_red$Ubc %>% select(-c(Habitat1 | Habitat2))

#Change species name column so all data frames match ('Nombre_cientifico_final')
df_birds_red[4:6] <- map(df_birds_red[4:6], \(df){
  df %>% rename(Nombre_cientifico_final = Nombre_cientifico_final_ayerbe_2018)
})

# Remove practice day data 
df_birds_red$Ubc_gaica_Caf <- df_birds_red$Ubc_gaica_Caf %>% 
  filter(!Fecha %in% as.Date(c("2024-05-27", "2024-05-28"))) %>% # Ensayo dates 
  filter(!Id_muestreo %in% c(paste0("G-MB-Q-LCA_0", 7:9), "OQ_Practica")) # PCs surveyed one time

# Combine dfs -------------------------------------------------------------
## Combine all dfs & remove more irrelevant columns##
# smartbind seems to work well on data frames, not tibbles
df_birds_red <- map(df_birds_red, \(df) {
  df %>% as.data.frame()
})

Birds_all <- smartbind(list = df_birds_red) %>%
  dplyr::select(-c(
    Coordenadas_geograficas, Incertidumbre_coordenadas, Precision_coordenadas,
    Autoria_nombre_cientifico, Numero_especie, Municipio
  ))
rownames(Birds_all) <- NULL
head(Birds_all)
names(Birds_all)
dim(Birds_all)

# Continue formatting columns
Birds_all2 <- Birds_all %>%
  rename(Habitat_og = Habitat, Nombre_ayerbe = Nombre_cientifico_final) %>% 
  mutate(
    across(c(Nombre_finca, Nombre_ayerbe, Habitat_og, Observacion_de_grabacion, Grabacion), 
           ~ str_to_sentence(., locale = "en")),
    across(c(Nombre_finca, Nombre_ayerbe, Habitat_og, Observacion_de_grabacion, Grabacion), 
           ~ str_squish(.)),
    across(c(Hora, Pc_start, Pc_length), 
           ~ chron::times(., format = c(times = "hh:mm:ss"))),
    Fecha = as.Date(Fecha),
    Id_group = sapply(
      str_split(Id_muestreo, "_"),
      function(x) {
        x[1]
      })) %>%
  mutate(across(.cols = c(matches("Id_pr|Distancia_pr"), Latitud_decimal, Longitud_decimal, Count), as.numeric))

### Distancias FULL y Buffer##
Birds_all3 <- Birds_all2 %>%
  #mutate(across(matches("ID_pr|Distancia_pr"), ~ as.numeric(.))) %>% #Not 
  mutate(across(starts_with("Id_pr"), ~ na_if(., 0))) %>%
  mutate(
    Id_gcs = do.call(coalesce, across(starts_with("Id_pr"))),
    # Some farm names are from GCS project, others (particularly reference farms) are not
    Nombre_finca_mixed = do.call(coalesce, across(matches("Nombre_f|Nombre_pr"))), 
    #CHECK:: There is only one ID_predio for each row in the 4 "ID_pr" columns 
    Row_sum = rowSums(across(starts_with("Id_pr")), na.rm = T),
    Same = Id_gcs == Row_sum,
    Distancia_farm = rowSums(across(starts_with("Distancia_pr")))
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
Bird_pcs <- Birds_all3 %>% filter(Protocolo_muestreo == "Punto conteo") 
nrow(Bird_pcs)

# Format metadata ---------------------------------------------------------
# Standardize metadata column names
df_metadata <- map(df_metadata, \(df) {
  df <- df %>% clean_names(case = "snake") %>% 
    rename(Id_muestreo = id_punto_muestreo_final)
  names(df) <- str_to_sentence(names(df))
  return(df)
})

df_metadata <- lapply(df_metadata, function(x) {
  cbind(x, Fecha = lubridate::mdy(paste(x$Mes, x$Dia, x$Ano, sep = "/"))) %>% 
    rename(Spp_obs = Observacion_especies_por_punto_conteo)
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

df_meta <- smartbind(list = df_metadata)
rownames(df_meta) <- NULL

# >Rep_dfs ------------------------------------------------------
## Generate Rep_dfs list, which has a row for each unique point count, including those where no species were observed. Multiple point counts in the same day is accounted for by generation of 'Same_pc' column 

# Create Additional metadata df to join with the No_obs list 
Add_metadata <- map(df_birds_red, \(df){
  df %>% distinct(Ecoregion, Nombre_institucion, Uniq_db, Id_muestreo, Ano, Ano_grp)
})

# PROBLEM:
df_metadata$UniLlanos %>% filter(Spp_obs == 0) %>% 
  select(Id_muestreo, Fecha, Hora, Spp_obs)

# Filter & format so only the point counts where no species were observed remain
No_obs_l <- map2(df_metadata, Add_metadata, 
                 \(meta, add){
  meta %>% filter(Spp_obs == 0) %>% 
    left_join(add) %>%
    select(Ecoregion, Nombre_institucion, Uniq_db, Id_muestreo, Ano_grp, Fecha, Hora, Spp_obs) %>% 
    rename(Pc_start = Hora)
})

No_obs_l$UniLlanos <- No_obs_l$UniLlanos %>% mutate(Fecha = NA)

# Create a 'Rep' column that contains the repetition number for a given survey, add Spp_obs column
Rep_dfs <- map2(df_birds_red, No_obs_l, \(df, No_obs){
  df %>% filter(Protocolo_muestreo == "Punto conteo") %>%
    mutate(Spp_obs = 1) %>% 
    group_by(Id_muestreo, Ano_grp, Fecha, Same_pc) %>% 
    slice_head(n = 1) %>% 
    full_join(No_obs) %>% # Join with the point counts where no species were observed remain
    group_by(Id_muestreo, Ano_grp) %>% 
    arrange(Fecha, Pc_start) %>% 
    mutate(Rep = row_number()) %>% 
    distinct(Ecoregion, Nombre_institucion, Uniq_db, Id_muestreo, Ano_grp, Fecha, Pc_start, Pc_length, Same_pc, Rep, Spp_obs) %>% 
    ungroup()
})

# Point count (PC) files -----------------------------------------------
## Create different files based on inclusion of location, date, and habitat #
# Create a file where each row is a unique point count x data collector #
Pc_uniq <- Bird_pcs %>% distinct(Uniq_db, Nombre_institucion, Id_group, Ecoregion, Departamento, Id_muestreo, Id_gcs)

#NOTE: There are 540 unique point count x data collector combinations, but 16 points have multiple coordinates. 556 rows here 
Pc_locs_mult <- Bird_pcs %>% distinct(Uniq_db, Nombre_institucion, Id_group, Ecoregion, Departamento, Id_gcs, Id_muestreo, Latitud_decimal, Longitud_decimal)
nrow(Pc_locs_mult)

# For now, take the average lats & longs of these 16 points.
Pc_locs <- Pc_locs_mult %>% 
  group_by(Id_muestreo) %>% 
  summarize(Latitud_decimal = round(mean(Latitud_decimal), 4), 
            Longitud_decimal = round(mean(Longitud_decimal), 4)) %>% 
  full_join(Pc_uniq, by = "Id_muestreo")

# Inclusion of date, time, and Ano_grp
Pc_date <- Bird_pcs %>% distinct(Nombre_institucion, Uniq_db, Ecoregion, Departamento, Nombre_finca, Id_gcs, Id_group, Id_muestreo, Ano, Mes, Dia, Fecha, Pc_start, Ano_grp) 
nrow(Pc_date)

# Combine Rep_dfs with additional information
Pc_date2 <- Rep_dfs %>% bind_rows() %>% 
  select(-Same_pc) %>% 
  full_join(Pc_date) 

# Calculate the total number of reps per PC
Pc_date3 <- Pc_date2 %>% reframe(N_reps = n(), across(.cols = everything()), .by = Id_muestreo) 

# Calculate the number of samp_periods for each point count -- The idea is that b/c some point counts were surveyed up to 3 times, but always 2 of which were in the same Ano_grp (e.g., 13-14), using distinct() will remove a row in the same Ano_grp. The point count IDs with 2 rows are possible resurvey sites (although see the temporal sampling plot for potential issues of seasonality). 
# NOTE:: There are challenges given that Id_muestreo is not always the same, even when the point is the same.. For example UBC-MB-M-A_01 & U-MB-M-A_01 are the same point, but they have different Id_muestreos. If you created a new column removing the datacollectors initials this would fix the problem, but it's probably not necessary at present
Pc_samp_periods <- Pc_date2 %>% 
  distinct(Id_muestreo, Uniq_db, Ano_grp, Ecoregion) %>%
  reframe(N_samp_periods = n(), across(.cols = everything()), 
          .by = Id_muestreo) %>%
  distinct(Id_muestreo, Uniq_db, N_samp_periods) %>% 
  tibble() 

# Merge with PC_samp_periods
Pc_date4 <- Pc_date3 %>% 
  left_join(Pc_samp_periods %>% distinct(Id_muestreo, N_samp_periods))

# Inclusion of habitatOG and Habitat_ut 
Pc_hab <- distinct(Bird_pcs, Id_muestreo, Uniq_db, Ecoregion, Departamento, Nombre_finca_mixed, Latitud_decimal, Longitud_decimal, Habitat_og, Habitat_ut)

## Create relevant KMZ files
Pc_locs_sf <- st_as_sf(Pc_locs,
                       coords = c("Longitud_decimal", "Latitud_decimal"),
                       crs = 4326,
                       remove = F
)

# Export reduced set of columns to kml
Pc_locs_sf %>%
  select(Id_muestreo, Uniq_db, Departamento) %>%
  rename(
    name = Id_muestreo,
    DataBase = Uniq_db #,
    #Farm = Nombre_finca_mixed
  ) # %>%
# st_write(obj = Method_InstKml, driver='kml', dsn="/Users/aaronskinner/Library/CloudStorage/OneDrive-UBC/Grad_School/PhD/Field_Work/Spatial_Files/Method_Inst_TEST.kml", layer = "Method_Inst_TEST")

# Event covariates --------------------------------------------------------
# Format the information that varies per visit (event) , e.g. weather 
# distinct(Ano_grp, Rep) would be unique combos of samp_period and repetition
# NOTE:: df_birds

## Standardize weather covariates 
# For UniLlanos 
df_birds_red$UniLlanos <- df_birds_red$UniLlanos %>% mutate(Clima = case_when(
  str_detect(Clima, regex("despejado|sol", ignore_case = TRUE)) ~ "Despejado", 
  str_detect(Clima, regex("lloviz|lluv", ignore_case = TRUE)) ~ "Llovizna",
  str_detect(Clima, regex("brisa", ignore_case = TRUE)) ~ "Brisa",
  str_detect(Clima, regex("nublado", ignore_case = TRUE)) ~ "Nublado",
  Clima %in% c("Nubado", "Nubaldo") ~ "Nublado",
  .default = Clima
))

# Write UniLlanos .xlsx for Ecotropico
# NOTE:: Used df_birds to send Ecotropico full data set, not df_birds_red
df_birds$UniLlanos %>% as.data.frame() #%>% 
# write.xlsx(file = "Intermediate_products/UniLlanos_climate_standardized.xlsx", 
#           showNA = FALSE, row.names = FALSE)

# For Gaica_dist
df_birds_red$Gaica_dist <- df_birds_red$Gaica_dist %>%  mutate(Clima = case_when(
  Clima == "Nublado con llovizna" ~ "Llovizna",
  .default = Clima
)) 

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
coords <- data.frame(st_coordinates(Pc_locs_sf))
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
Envi_df <- cbind(
  Pc_locs_sf[, c("Id_muestreo","Ecoregion", "Departamento", "Uniq_db", "Id_group")],
  sapply(envi.vars, terra::extract, Pc_locs_sf, ID = FALSE)) %>% 
  rename(Elev = elev.dem.srtm_21_10, Avg_temp = avg.temp.mean, Tot.prec = tot.prec.sum)
Envi_df2 <- Envi_df %>%
  st_drop_geometry() %>%
  group_by(Id_muestreo) %>%
  slice_head() %>%
  ungroup()

#Extract data & create df where each row is a point count and there are 12 'prec' columns, one for each month
PrecPCs <- terra::extract(Wc_col[[2]], Pc_locs_sf, ID = FALSE)
Prec_df <- cbind(Pc_locs_sf[,c("Id_muestreo","Ecoregion", "Departamento", "Uniq_db")], PrecPCs) %>%
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
rm(list= ls()[!(ls() %in% c("Birds_all", "Birds_all3", "Bird_pcs", "Pc_date4", 'Pc_hab', "Pc_locs", "df_birds", "df_metadata", "df_birds_red", "Mes_mod", "Pc_locs_mult", "Pc_locs_sf", "Envi_df2", "Prec_df", "Prec_daily"))])
#save.image(paste0("Rdata/the_basics_", format(Sys.Date(), "%m.%d.%y"), ".Rdata"))


# Working -----------------------------------------------------------------
# Use anti_join to see mismatches between metadata & the birds databases 
map2(df_birds_red, df_metadata, \(birds, meta){
  detection <- birds %>% select(Id_muestreo, Fecha, Hora, 
                                contains(c("Clima", "Cigarras", "Ruido", "Vacas_menos_50"))) %>% 
    distinct() 
  meta <- meta %>% distinct(Id_muestreo, Fecha, Hora, Spp_obs)
  comb <- meta %>% full_join(detection)
  comb %>% anti_join(detection) %>% 
    filter(!str_detect(Id_muestreo, "LCR|JB|LCA") & # In metadata file but 0 (or 1) accompanying PC
             !str_detect(Id_muestreo, "ECOR|PORT") & # Ensayo days
             Spp_obs != 0) # Points where no species were observed 
})

# Find the cutoff_times specific to each database
map(df_birds_red, \(df){
  df %>% group_by(Id_muestreo, Fecha) %>% 
    filter(!str_detect(Id_muestreo, "LIBRE")) %>% 
    arrange(Hora) %>% 
    mutate(Pc_length_day = as.numeric((Hora - first(Hora)) * 1440), #For entire day, not within a PC
           Same_pc = if_else(Pc_length_day < 91, "Same", "Diff")) %>% 
    distinct(Pc_length_day) %>% 
    arrange(desc(Pc_length_day)) %>%
    filter(Pc_length_day < 91)
}) 

## FOLLOWUP: 
# Ecotropico:: No observations where Hora == 07:36:00 or 07:37:00
df_birds_red$Gaica_dist %>% filter(Id_muestreo == "G-AD-M-CO_05") %>% 
  distinct(Id_muestreo, Fecha, Hora) %>% 
  arrange(Fecha, Hora)

# GAICA:: Hay una inconsistencia en 6/04, los metadatos y la base de datos principal no tienen la misma hora (7:38 vs 8:38) 
df_birds_red$Ubc_gaica_OQ %>% filter(Id_muestreo == "OQ_03") %>% 
  distinct(Id_muestreo, Fecha, Hora)
df_metadata$Ubc_gaica_OQ %>% filter(Id_muestreo == "OQ_03") %>% 
  distinct(Id_muestreo, Fecha, Hora)


Pc_date4 %>% filter(Spp_obs == 0)

Rep_dfs %>% 
  bind_rows() %>% 
  filter(if_any(everything(), is.na)) %>% 
  #count(Id_muestreo, sort = T) %>%
  left_join(Pc_samp_periods[, c("N_samp_periods", "Uniq_db", "Id_muestreo")]) %>% 
  count(Id_muestreo, sort = T) 

Rep_dfs %>% bind_rows() %>% 
  filter(Id_muestreo == "G-MB-Q-ECOR_03")

df_metadata$Gaica_dist %>% filter(Id_muestreo == "G-AD-M-LCA2_04") %>%
  distinct(Id_muestreo, Fecha, Hora, Comentario)


#TO DO: Generate dataframes of detection covariates, 
Pc_date4 %>% group_split(Uniq_db)

map2(df_birds_red, , \(birds, date){
  detection <- birds %>% select(Id_muestreo, Fecha, Hora, 
                                contains(c("Clima", "Cigarras", "Ruido", "Vacas_menos_50"))) %>% 
    distinct() 
  meta <- meta %>% distinct(Id_muestreo, Fecha, Hora, Spp_obs)
  comb <- meta %>% full_join(detection)
  comb %>% anti_join(detection) %>% 
    filter(!str_detect(Id_muestreo, "LCR|JB|LCA") & # In metadata file but 0 (or 1) accompanying PC
             !str_detect(Id_muestreo, "ECOR|PORT") & # Ensayo days
             Spp_obs != 0) # Points where no species were observed 
})
