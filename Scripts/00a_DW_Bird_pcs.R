## PhD birds in silvopastoral landscapes##
# Data wrangling 00a -- Create base data frame (Bird_pcs_all)
## This script loads raw data from data providers, combines data into single df, & tidies and filters data to produce the environment necessary for future scripts & downstream analyses

# Contents
# 1) Load & format data that will allow for join into a single df
# 2) Merge into single df & continue to format
# 3) Create various point count (PC) files different files based on inclusion of location, date, and habitat
# 4) Download environmental data -- Temp, precipitation, & elevation
# 5) Identify how many distinct sampling periods each point count location has
# 6) Export Rdata object

# To do -------------------------------------------------------------------
# Run 00e with as_hms , but consider using lubridate instead, particularly storing your Pc_length in minutes instead.

## Time removal modeling for CIPAV data? 
if(FALSE){
  Bird_pcs_all %>% 
    filter(Uniq_db == "Cipav mbd" & Id_muestreo == "C-MB-A-ED_01") %>% 
    distinct(Id_muestreo, Fecha, Hora, Species_ayerbe, Pc_length) %>% 
    arrange(Id_muestreo) %>% 
    count(Species_ayerbe, sort = T) # Hora,
}

# Libraries ---------------------------------------------------------------
library(janitor) # tabyl function
library(tidyverse)
library(hms)
library(raster)
library(sf)
library(sp)
library(gtools)
library(AICcmodavg)
library(readxl)
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
conflicts_prefer(hms::hms)

# Load Rdata & useful themes / functions script
# load("Rdata/the_basics_02.27.25.Rdata")
source("/Users/aaronskinner/Library/CloudStorage/OneDrive-UBC/Grad_School/Rcookbook/Themes_funs.R")

# Load bird obs & meta data ---------------------------------------------------

files <- list.files(path = "Data/Aves", pattern = "^A.*xlsx$") # Add |D after A if you want Monroy/Skinner data too

# At present these are just the files with point counts
df_birds <- map(files, \(file){
  read_xlsx(
    path = file.path("Data/Aves", file),
    sheet = "Base_de_datos",
    na = c("Sin informacion", "N/A"), trim_ws = TRUE
  )
})

df_metadata <- map(files, \(file){
  read_xlsx(
    path = file.path("Data/Aves", file),
    sheet = "Metadatos_puntos_conteo",
    na = c("Sin informacion", "N/A"), trim_ws = TRUE
  )
})

## Join lists together for initial formatting & then separate again STILL TO DO
# Ensure data frames are in the same order
files
names(df_birds) <- c("Cipav", "Gaica_dist", "Gaica_mbd", "Ubc_gaica_Caf", "Ubc_gaica_Meta", "Ubc_gaica_OQ", "Ubc_hatico", "Ubc", "UniLlanos") # "Ubc_Monroy"
names(df_metadata) <- c("Cipav", "Gaica_dist", "Gaica_mbd", "Ubc_gaica_Caf", "Ubc_gaica_Meta", "Ubc_gaica_OQ", "Ubc_hatico", "Ubc", "UniLlanos")

# Format bird observation data -------------------------------------------
df_birds <- map(df_birds, \(df) {
  df %>%
    Cap_snake() %>%
    rename(Id_muestreo = Id_punto_muestreo_final, 
           Count = Numero_individuos, 
           Habitat_og = Habitat) %>% 
    mutate(Registrado_por = str_remove(Registrado_por, "-Caicedo"))
})

# Pre-processing date & time
df_birds <- map(df_birds, function(x) {
  bind_cols(
    x, Fecha = lubridate::mdy(paste(x$Mes, x$Dia, x$Ano, sep = "/"))
    ) %>% mutate(
      Fecha = as.Date(Fecha),
      Hora = sapply(str_split(Hora, " "), function(x) {
        x[2]
      }),
      Hora = as_hms(Hora),
      Ano_grp = case_when(
        Ano %in% c(2013, 2014) ~ "13-14",
        Ano %in% c(2016, 2017) ~ "16-17",
        Ano == 2019 ~ "19",
        Ano == 2022 ~ "22",
        Ano == 2024 ~ "24"
      )
    )
}) ### GAICA did not record date for 74 Recorridos libres

# Remove irrelevant columns from dataframes
df_birds_red <- map(df_birds, function(x) {
  dplyr::select(
    x, 1:21,
    contains(c("Id_muestreo", "Id_punto_muestreo_original", "Protocolo_muestreo", "Ano", "Mes", "Hora", "repeticion", "Orden", "Familia", "Especie", "cientifico", "Count", "Tipo_registro", "Habitat", "Sistema", "Registrado", "Distancia_observacion", "climatica", "Elevacion", "finca", "grabacion", "Cigarras", "Vacas", "Ruido", "Estrato_")), "Fecha", "Dia"
  ) %>%
    select(-contains(c("Numero_registro", "Id_registro_biologico", "Observacion_climatica_evento", "Concatenar", "repeticion")))
})

# Display data collector's name
map(df_birds_red, function(x) {
  TF <- str_detect(names(x), "revisada|estandarizado|_sin formul|_sin formula|_cipav|_gaica|_ubc|_unillanos|corregido")
  names(x)[TF]
})

# Replace with corrected names
df_birds_red <- map(df_birds_red, function(df) {
  df <- df %>%
    rename_with(~ str_replace_all(
      str_remove_all(., "revisada|estandarizado|_sin formul|_sin formula|_cipav|_gaica|_ubc|_unillanos|corregido|_homologado|_base_infotnc|_investigacion"),
      c(
        "Observacion_climatica" = "Clima",
        "Distancia_observacion" = "Distancia_bird"
      )
    ))
  df
})

## Group point counts, determine pc_start time & pc_length
# Some data collectors surveyed certain point counts multiple times on the same day, & some data collectors reported the time that each bird was observed instead of a single time at the start of the point count

# Create 'Same_pc' column to indicate the rows where the time is less than the value in the cutoff_time vector. These observations of the same Id_muestreo & Fecha will be grouped as a point count.
# Specify database specific cutoff times, as CIPAV has a single point count that is 90 minutes long (and thus should be grouped together), and UBC & Unillanos has distinct same day point counts that are only separated by 68 and 74 minutes, respectively (and thus should be grouped apart). Otherwise, largest difference in point count times in the same day is 13 minutes (from an 18 minute point count), so I used that for convenience for all other databases.

cutoff_time <- hms::hms(minutes = c(91, rep(18, 6), 61, 73))

df_birds_red <- map2(df_birds_red, cutoff_time, \(df, cutoff){
  df %>%
    group_by(Id_muestreo, Fecha) %>%
    arrange(Hora) %>%
    # For entire day, not within PC
    mutate(
      Pc_length_day = hms::hms(seconds = as.numeric(Hora - first(Hora))),
      Same_pc = case_when(
        Pc_length_day < cutoff ~ "Same",
        # One manual adjustment
        Id_muestreo == "UBC-MB-M-LBR_02" & Hora == as_hms("11:05:00") ~ "Diff2",
        Pc_length_day >= cutoff ~ "Diff"
      )
    ) %>% select(-Pc_length_day)
})

# Group_by the 'Same_pc' column to generate point count start times & the length of each point count
df_birds_red <- map(df_birds_red, \(df){
  df %>%
    group_by(Id_muestreo, Ano_grp, Fecha, Same_pc) %>%
    mutate(
      Pc_start = first(Hora),
      Pc_length = hms::hms(seconds = as.numeric(last(Hora) - Pc_start))
    ) %>% ungroup()
})

# Shorten names, create Ecoregion column
df_birds_red <- map(df_birds_red, \(df){
  df %>% mutate(
    Pregunta_gsc = case_when(
      Pregunta_gsc %in% c("Monitoreo_biodiversidad", "Monitoreos_biodiversidad") ~ "mbd",
      Pregunta_gsc == "Analisis_distanciamiento" ~ "distancia",
      Pregunta_gsc == "Ocupacion_dinamica" ~ "mbd",
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
      Departamento %in% c("Atlantico", "Bolivar") ~ "Bajo Magdalena",
      Departamento %in% c("Cesar", "Guajira") ~ "Rio Cesar",
      Departamento %in% c("Boyaca", "Santander") ~ "Boyaca Santander",
      Departamento %in% c("Caldas", "Risaralda", "Tolima", "Quindio", "Valle del Cauca") ~ "Cafetera",
      Departamento == "Meta" ~ "Piedemonte",
      .default = Departamento
    )
  )
})

# Create Uniq_db and Id_muestreo_no_dc (no data collector)
df_birds_red <- map(df_birds_red, \(df){
  df %>% mutate(
    Uniq_db = paste(Nombre_institucion, Pregunta_gsc, sep = " "),
    Id_muestreo_no_dc = str_remove(Id_muestreo, "^[^-]+-")
  )
})

# Apply the function to each data frame in the list
df_birds_red <- map(df_birds_red, \(df) {
  df %>% mutate(
    across(
      # Do not apply to certain columns.. 
      .cols = -c(Id_muestreo, Id_muestreo_no_dc, Registrado_por, contains("Id_punto_muestreo_original")) & where(is.character),
      .fns = ~ standardize_column_contents(.) # Custom function
    )
  )
})

# >Dataset specific operations -------------------------------------------
## Merge 58 points that have a single predominant habitat type from Andrea's dataset
# Standardize Andrea's dataset according to SCR terminology
Andrea <- read_xlsx("Derived/Excels/Habitat_match_unillanos.xlsx") %>%
  mutate(Habitat = case_when(
    habitat_unillanos %in% c("PA", "CV") ~ "Ssp",
    habitat_unillanos == "Bo" ~ "Bosque",
    habitat_unillanos == "PT" ~ "Pastizales",
    .default = habitat_unillanos
  )) %>%
  rename(Id_punto_muestreo_original = ID_punto_muestreo_ORIGINAL) %>%
  filter(
    if_all(c(Habitat, ID_punto_muestreo_FINAL, habitat_unillanos), ~ !is.na(.))
    ) %>% mutate(habitat_unillanos = case_when(
      habitat_unillanos == "PA" ~ "Arboles dispersos",
      habitat_unillanos == "CV" ~ "Cerca viva",
      habitat_unillanos %in% c("Bosque", "Pastizales") ~ NA
    )) %>%
  distinct(Id_punto_muestreo_original, Habitat, habitat_unillanos)

# Join Gaica distancia with Andrea's dataset 
df_birds_red$Gaica_dist <- df_birds_red$Gaica_dist %>%
  left_join(Andrea) %>% 
  mutate(Habitat_sub_ut = NA_character_,
         Habitat_sub_ut = coalesce(Habitat_sub_ut, habitat_unillanos)) %>% 
  select(-habitat_unillanos)

# Change rows with 'Vuelo' to Tipo_registro so Estrato_vertical is no longer needed
df_birds_red$Gaica_dist <- df_birds_red$Gaica_dist %>% 
  mutate(
    Tipo_registro = if_else(
    Estrato_vertical == "Vuelo", "Vuelo", Tipo_registro
    )) 

## Couldn't reach original (2013) point in 2017, so GAICA surveyed a different location (borde de bosque) 70 meters away in 2017. In 2024 they surveyed both original spot (2013) and the 2017 location.
df_birds_red$Gaica_mbd <- df_birds_red$Gaica_mbd %>% 
  mutate(Id_muestreo = if_else(
    Id_muestreo == "G-MB-M-EPO1_03" & Ano == 2017, "G-MB-M-EPO1_03_(1)", Id_muestreo
  ), Id_muestreo_no_dc = if_else(
    Id_muestreo_no_dc == "MB-M-EPO1_03" & Ano == 2017, "MB-M-EPO1_03_(1)",Id_muestreo_no_dc
  ))

# Remove UBC microhabitat columns
df_birds_red$Ubc <- df_birds_red$Ubc %>% select(-c(Habitat1 | Habitat2))

# Change species name column so all data frames match ('Nombre_cientifico_final')
df_birds_red[4:6] <- map(df_birds_red[4:6], \(df){
  df %>% rename(Nombre_cientifico_final = Nombre_cientifico_final_ayerbe_2018)
})

# Add 'Vuelo' information to Tipo_registro column, and change distance to >50
df_birds_red[4:6] <- map(df_birds_red[4:6], \(df){
  df %>% mutate(Tipo_registro = if_else(
    Distancia_bird %in% c("Vuelo", "Sobrevuelo"), "Sobrevuelo", Tipo_registro
  ), 
  Distancia_bird = if_else(
    Distancia_bird %in% c("Vuelo", "Sobrevuelo"), ">50", Distancia_bird
  ))
})

# NOTE in Unillanos:: Distancia_obs is >50 whenever Comentario == "Fuera"
df_birds$UniLlanos %>% 
  filter(Comentario_registro == "Fuera") %>% 
  distinct(Tipo_registro, Distancia_observacion)

# Remove practice day data
df_birds_red$Ubc_gaica_Caf <- df_birds_red$Ubc_gaica_Caf %>%
  filter(!Fecha %in% as.Date(c("2024-05-27", "2024-05-28"))) %>% # Ensayo dates
  filter(!Id_muestreo %in% c(paste0("G-MB-Q-LCA_0", 7:9))) # PCs surveyed one time

df_birds_red$Ubc_gaica_OQ <- df_birds_red$Ubc_gaica_OQ %>% 
  filter(Id_muestreo != "OQ_Practica")

## Natalia put in lots of work to check and improve upon GAICA's recordings database, particularly she added all of the 
path <- "/Users/aaronskinner/Library/CloudStorage/OneDrive-UBC/Grad_School/PhD/Mentorship/Natalia"
files <- list.files(path, pattern = ".xlsx")
Site_names <- c("Valle_de_cocora", "Otun_quimbaya")
Grabaciones_natalia_l <- map2(files, Site_names, \(file_name, sites){
  read_xlsx(paste0(path, "/", file_name), sheet = "Grabaciones") %>% 
    Cap_snake() %>%
    mutate(Site = sites) %>% 
    rename(Count_nat = Numero_individuos, 
           Distancia_bird_nat = Observacion_de_grabacion,
           Id_muestreo = Id_punto_muestreo_final, 
           Grabacion_nat = Grabacion, 
           Confiabilidad_nat = Confiabilidad_de_grabacion, 
           Nombre_cientifico_final = Nombre_cientifico_final_ayerbe_2018) %>%
    replace_with_na_at(.vars = "Count", condition = ~.x == "N/A") %>%
    # Format for easy join in next step
    mutate(Fecha = lubridate::mdy(paste(Mes, Dia, Ano, sep = "/")),
           Fecha = as.Date(Fecha),
           Hora = sapply(str_split(Hora, " "), function(x) {
             x[2]
           }),
           Hora = as_hms(Hora),
           Count_nat = as.numeric(Count_nat),
           Distancia_bird_nat = if_else(
             is.na(Distancia_bird_nat), "<50", ">50")
    ) %>% 
    relocate(Site, .after = Hora)
})

# Remove sounds only identified to family or not identified at all
Rm_non_spp <- c("Trochilidae", "Tyrannidae", "Desconocido")
Grabaciones_natalia_l2 <- map(Grabaciones_natalia_l, \(df){
  df %>% filter(!Nombre_cientifico_final %in% Rm_non_spp)
})

# Remove duplicate records in Natalia's Excel that are causing problems in the join below. This is OK because I ensured that all the correct information is already in the Grabacion column of the primary database.
Grabaciones_natalia_l3 <- map(Grabaciones_natalia_l2, \(df){
  rm_to_facilitate_join <- df %>% 
    count(Fecha, Id_muestreo, Nombre_cientifico_final, sort = T) %>% 
    filter(n > 1)
  df %>% anti_join(rm_to_facilitate_join)
})

# Format, join with bird database  
df_birds_red[c("Ubc_gaica_Caf", "Ubc_gaica_OQ")] <- map2(
  df_birds_red[c("Ubc_gaica_Caf", "Ubc_gaica_OQ")], Grabaciones_natalia_l3, 
  \(birds, recordings){
    rec_join <- recordings %>% 
      select(Fecha, Id_muestreo, Nombre_cientifico_final, Grabacion_nat, Count_nat, Distancia_bird_nat, Confiabilidad_nat) %>%
      mutate(across(
        .cols = -Id_muestreo & where(is.character), 
        .fns = ~ standardize_column_contents(.))
      )
    birds %>% full_join(rec_join) %>% 
      # Add the new data from Natalia's dataset
      mutate(
        Distancia_bird = coalesce(Distancia_bird, Distancia_bird_nat),
        Grabacion = coalesce(Grabacion, Grabacion_nat),
        Confiabilidad_de_grabacion = coalesce(Confiabilidad_de_grabacion,
                                              Confiabilidad_nat)
      ) %>% 
      # Remove Natalia's rows 
      select(-ends_with("nat"))
  })

## TESTING - HERE
# Anti-join to see where the mismatches are 
# Confirmed 3 birds from Caf are in Word docs
if(FALSE){
  test$Ubc_gaica_Caf %>% anti_join(df_birds_red$Ubc_gaica_Caf) %>% 
    select(-contains("Distancia"))
  # Myiarchus cephalotes is not in the Word doc 
  test$Ubc_gaica_OQ %>% anti_join(df_birds_red$Ubc_gaica_OQ) %>% 
    select(-contains("Distancia"))
  
  test$Ubc_gaica_Caf %>% filter(!is.na(Count_nat) & is.na(Count)) %>% 
    select(-c(contains("Grabacion")))
  test$Ubc_gaica_Caf %>% 
    filter(!is.na(Distancia_bird_nat) & is.na(Distancia_bird)) %>% 
    select(-c(contains("Grabacion")))
  test$Ubc_gaica_Caf %>% filter(!is.na(Grabacion_nat) & is.na(Grabacion))
}

## Standardize weather covariates
# UniLlanos - Ecotropico updated 
df_birds_red$UniLlanos <- df_birds_red$UniLlanos %>% 
  rename(Clima = Clima_final)

# Change Soleado to Despejado to match with UniLlanos
df_birds_red$Gaica_dist <- df_birds_red$Gaica_dist %>% 
  mutate(Clima = case_when(
    Id_muestreo == "G-AD-M-LP_06" & Fecha == as.Date("2019-09-29") & Pc_start ==
    as_hms("16:25:00") ~ "Llovizna",
    Clima == "Soleado" ~ "Despejado",
    .default = Clima))

# NOTE:: Clima has been standardized for databases that have it 
map(df_birds_red, \(df){
  if(!is.null(df$Clima)){
    df %>% distinct(Id_muestreo, Fecha, Clima) %>%
      tabyl(Clima)
  }
})

df_birds_red$Ubc_hatico <- df_birds_red$Ubc_hatico %>% 
  mutate(across(c(Latitud_decimal, Longitud_decimal), as.character))

# Combine dfs -------------------------------------------------------------
## Combine all dfs & remove more irrelevant columns##
# smartbind seems to work well on data frames, not tibbles
df_birds_red <- map(df_birds_red, \(df) {
  df %>% as.data.frame()
})

Birds_comb <- smartbind(list = df_birds_red) %>%
  dplyr::select(-c(
    Coordenadas_geograficas, Incertidumbre_coordenadas, Precision_coordenadas,
    Autoria_nombre_cientifico, Numero_especie, Municipio
  )) %>% as_tibble()
rownames(Birds_comb) <- NULL
head(Birds_comb)
names(Birds_comb)
dim(Birds_comb)

# Create Id_group, plus additional column formatting
Birds_comb2 <- Birds_comb %>%
  rename(Habitat_sub = Habitat_sub_ut,
         Species_ayerbe = Nombre_cientifico_final,
         Latitud = Latitud_decimal,
         Longitud = Longitud_decimal) %>%
  mutate(
    across(
      c(Nombre_finca, Species_ayerbe, Habitat_og, Observacion_de_grabacion, Grabacion), ~ str_to_sentence(., locale = "en")
    ),
    across(
      c(Nombre_finca, Species_ayerbe, Habitat_og, Observacion_de_grabacion, Grabacion), ~ str_squish(.)
    ),
    across(
      c(Hora, Pc_start, Pc_length),
      ~ as_hms(.)
    ),
    Fecha = as.Date(Fecha),
    Id_group = sapply(
      str_split(Id_muestreo, "_"),
      function(x) {
        x[1]
      }
    )
  ) %>%
  mutate(across(
    .cols = c(matches("Id_pr|Distancia_pr"), Latitud, Longitud, Count), as.numeric
  )) %>% mutate(Tipo_registro = case_when(
    Tipo_registro == "Visual-auditivo" ~ "Visual/auditivo",
    Tipo_registro == "Vuelo" ~ "Sobrevuelo",
    .default =  Tipo_registro
  )) %>% select(-Id_punto_muestreo_original)

### Distancias FULL y Buffer, Id_gcs##
Birds_comb3 <- Birds_comb2 %>%
  # mutate(across(matches("ID_pr|Distancia_pr"), ~ as.numeric(.))) %>%
  mutate(across(starts_with("Id_pr"), ~ na_if(., 0))) %>%
  mutate(
    Id_gcs = do.call(coalesce, across(starts_with("Id_pr"))),
    # Some farm names are from GCS project, others (particularly reference farms) are not
    Nombre_finca_mixed = do.call(coalesce, across(matches("Nombre_f|Nombre_pr"))),
    # CHECK:: There is only one ID_predio for each row in the 4 "ID_pr" columns
    Row_sum = rowSums(across(starts_with("Id_pr")), na.rm = T),
    Same = Id_gcs == Row_sum,
    Distancia_farm = rowSums(across(starts_with("Distancia_pr")))
  ) %>%
  mutate(
    Id_gcs = ifelse(Finca_referencia == "Si" & is.na(Id_gcs),
      paste0("Ref_", Nombre_finca_mixed),
      Id_gcs
    )
  ) %>%  
  # Fill in Id_gcs for UBC_GAICA data
  mutate(Id_gcs = if_else(is.na(Id_gcs), first(na.omit(Id_gcs)), Id_gcs), 
         .by = Id_muestreo) %>% 
  select(-c(Nombre_predio_gcs_poligono_full, Id_predio_gcs_poligono_full, Id_predio_mas_cercano_gcs_poligono_full, Nombre_predio_gcs_poligono_buffer, Id_predio_gcs_poligono_buffer, Id_predio_mas_cercano_gcs_poligono_buffer, Distancia_predio_mas_cercano_gcs_poligono_full, Distancia_predio_mas_cercano_gcs_poligono_buffer, Row_sum, Same))

# 2 manual adjustments - In Google Earth the 50m buffers showed the correct location, but the points were changed by GAICA in a revision. Changed points back to the (approximate) centroid of the 50m buffer. 

Birds_comb4 <- Birds_comb3 %>% 
  mutate(Latitud = case_when(
    Id_muestreo == "G-MB-M-EPO1_03" ~ 3.8285,
    Id_muestreo == "G-MB-M-EPO1_03_(1)" ~ 3.8293, 
    .default = Latitud
  ), 
  Longitud = case_when(
    Id_muestreo == "G-MB-M-EPO1_03" ~ -73.842,
    Id_muestreo == "G-MB-M-EPO1_03_(1)" ~ -73.8417, 
    .default = Longitud
  ))
  
# Bird_pcs_all  ---------------------------------------------------------------
# Create data base with just point counts and Id_muestreo_no_dc column
Bird_pcs_all <- Birds_comb4 %>%
  filter(Protocolo_muestreo == "Punto conteo")

# NOTE:: This worked, as the difference in unique Ids is the 54 UniLlanos points
length(unique(Bird_pcs_all$Id_muestreo)) - length(unique(Bird_pcs_all$Id_muestreo_no_dc))

# Format metadata ---------------------------------------------------------
# Standardize metadata column names
df_metadata <- map(df_metadata, \(df) {
  df <- df %>%
    clean_names(case = "snake") %>%
    rename(Id_muestreo = id_punto_muestreo_final)
  names(df) <- str_to_sentence(names(df))
  return(df)
})

df_metadata$Gaica_mbd <- df_metadata$Gaica_mbd %>% 
  mutate(Id_muestreo = if_else(
    Id_muestreo == "G-MB-M-EPO1_03" & Ano == 2017, "G-MB-M-EPO1_03_(1)", Id_muestreo
  ))

df_metadata <- lapply(df_metadata, function(x) {
  bind_cols(
    x, Fecha = lubridate::mdy(paste(x$Mes, x$Dia, x$Ano, sep = "/"))
    ) %>% rename(Spp_obs = Observacion_especies_por_punto_conteo)
})

df_metadata[c(4:6)] <- map(df_metadata[c(4:6)], \(df){
  df %>% rename(Hora = Hora_inicial)
})

df_metadata[c(1:3)] <- map(df_metadata[c(1:3)], \(df){
  df %>%
    rename(Total_pc_time = Diferencia_de_minutos_entre_el_registro_con_hora_mas_temprana_y_el_registro_con_hora_mas_tardia) %>%
    mutate(
      Total_pc_time = sapply(str_split(Total_pc_time, " "), function(x) {
        x[2]
      }),
      Total_pc_time = as_hms(Total_pc_time)
    )
})

df_metadata <- map(df_metadata, \(df)
df %>% mutate(
  Hora = sapply(str_split(Hora, " "), function(x) {
    x[2]
  }),
  Hora = as_hms(Hora))
)

df_meta <- smartbind(list = df_metadata) %>% tibble()
rownames(df_meta) <- NULL

# >Rep_dfs ------------------------------------------------------
## Generate Rep_dfs list, which has a row for each unique point count, including those where no species were observed. Multiple point counts in the same day is accounted for by generation of 'Same_pc' column

# Create Additional metadata df to join with the No_obs list
Add_metadata <- map(df_birds_red, \(df){
  df %>% distinct(Ecoregion, Departamento, Nombre_institucion, Uniq_db, Id_muestreo, Ano, Ano_grp)
})

# PROBLEM: Unillanos did not record time or date when they observed no species? 
df_metadata$UniLlanos %>%
  filter(Spp_obs == 0) %>%
  select(Id_muestreo, Fecha, Hora, Spp_obs)

# Filter & format so only the point counts where no species were observed remain
No_obs_l <- map2(
  df_metadata, Add_metadata,
  \(meta, add){
    meta %>%
      filter(Spp_obs == 0) %>%
      left_join(add) %>%
      select(
        Ecoregion, Departamento, Nombre_institucion, Uniq_db,
        Id_muestreo, Ano_grp, Fecha, Hora, Spp_obs
      ) %>%
      rename(Pc_start = Hora) %>%
      mutate(Id_muestreo_no_dc = str_remove(Id_muestreo, "^[^-]+-"))
  }
)

No_obs_l$UniLlanos <- No_obs_l$UniLlanos %>% mutate(Fecha = NA)

# Create a 'Rep' column that contains the repetition number for a given survey, add Spp_obs column
Rep_dfs <- map2(df_birds_red, No_obs_l, \(df, No_obs){
  df %>%
    filter(Protocolo_muestreo == "Punto conteo") %>%
    mutate(Spp_obs = 1) %>%
    group_by(Id_muestreo, Ano_grp, Fecha, Same_pc) %>%
    slice_head(n = 1) %>%
    full_join(No_obs) %>% # Join with the point counts where no species were observed remain
    group_by(Id_muestreo, Ano_grp) %>%
    arrange(Fecha, Pc_start) %>%
    mutate(Rep_ano_grp = row_number()) %>%
    ungroup() %>%
    distinct(
      Ecoregion, Departamento, Nombre_institucion, Uniq_db, Id_muestreo, Id_muestreo_no_dc, Ano_grp, Fecha, Pc_start, Pc_length, Same_pc, Rep_ano_grp, Spp_obs
      )
})

# Point count (PC) files -----------------------------------------------
## Create different files based on inclusion of location, date, and habitat #
# Create a file where each row is a unique point count x data collector #
Pc_uniq <- Bird_pcs_all %>% distinct(Uniq_db, Nombre_institucion, Id_group, Ecoregion, Departamento, Id_muestreo, Id_muestreo_no_dc, Id_gcs)

# >Pc_locs ----------------------------------------------------------------
# NOTE: There are 540 unique point count x data collector combinations, but 16 points have multiple coordinates. 556 rows here
Pc_locs_mult <- Bird_pcs_all %>% 
  distinct(Uniq_db, Nombre_institucion, Id_group, Ecoregion, Departamento, Id_gcs, Id_muestreo, Id_muestreo_no_dc, Latitud, Longitud)
nrow(Pc_locs_mult)

# For now, take the average lats & longs of these 16 points.
Pc_locs <- Pc_locs_mult %>%
  group_by(Id_muestreo_no_dc) %>%
  summarize(
    Latitud = round(mean(Latitud), 4),
    Longitud = round(mean(Longitud), 4)
  ) %>%
  full_join(Pc_uniq, by = "Id_muestreo_no_dc")

## Create relevant KMZ files
Pc_locs_sf <- st_as_sf(Pc_locs,
                       coords = c("Longitud", "Latitud"),
                       crs = 4326,
                       remove = F)

if(FALSE){
  # Export shapefile
  st_write(Pc_locs_sf, "Derived_geospatial/shp/Pc_locs.gpkg", layer = "Pc_locs")
  
  # Export reduced set of columns to kml
  Pc_locs_sf %>%
    arrange(Uniq_db, Id_muestreo) %>%
    distinct(Id_muestreo, Departamento, geometry) %>%
    rename(
      name = Id_muestreo
      #DataBase = Uniq_db # ,
      # Farm = Nombre_finca_mixed
    ) %>%
    st_write(
      driver='kml', dsn="Derived_geospatial/kml/Pc_locs.kml", layer = "Pc_locs"
    )
}

# >Pc_hab -----------------------------------------------------------------
# Each point count has a single Habitat_ut except for Gaica distancia points

Pc_hab_ano <- Bird_pcs_all %>%
  group_by(Id_muestreo) %>%
  fill(Habitat_ut) %>%
  ungroup() %>%
  mutate(Habitat_ut = case_when(
    Uniq_db == "Gaica distancia" ~ Habitat,
    .default = Habitat_ut
  ),
  Habitat_sub = case_when(
    Id_group %in% c("OQ", "UBC-MB-M-LBR") ~ "Maduro", 
    Habitat_ut == "Bosque ripario" ~ "Ripario",
    Habitat_ut == "Bosque" & is.na(Habitat_sub) ~ "Secundario",
    Habitat_sub == "Transitorio" ~ NA_character_,
    .default = Habitat_sub
  )
  ) %>%
  mutate(
    Habitat_ut = case_when(
      str_detect(Id_muestreo, "OQ_") ~ "Bosque",
      Habitat_ut == "Bosque ripario" ~ "Bosque",
      .default = Habitat_ut
      ),
    # Create Habitat (consolidated) column, replacing multiple habitat types with NA
    Habitat = if_else(Uniq_db == "Gaica distancia" & is.na(Habitat),
      "Mosaic", Habitat_ut)
  ) %>%
  distinct(
    Id_muestreo, Id_group, Id_muestreo_no_dc, Id_gcs, Uniq_db, Ecoregion, 
    Departamento, Latitud, Longitud, Ano, 
    Habitat_og, Habitat_ut, Habitat_sub, Habitat 
  )

# Remove year
Pc_hab <- Pc_hab_ano %>% distinct(pick(-Ano))

# NOTE:: 1 row for each Id_muestreo
Pc_hab %>% #filter(Uniq_db == "Ubc gaica dom") %>%
  distinct(Ecoregion, Id_muestreo_no_dc, Habitat) %>%
  #filter(is.na(Habitat)) %>% 53 GAICA Distancia points with no predominant habitat type
  filter(Habitat == "Ssp") %>%
  count(Id_muestreo_no_dc, sort = T)
  #filter(Ecoregion == "Piedemonte")

# >Pc_date ----------------------------------------------------------------
# Inclusion of date, time, and Ano_grp
Pc_date <- Bird_pcs_all %>% 
  distinct(Nombre_institucion, Pregunta_gsc, Uniq_db, Ecoregion, Departamento, Nombre_finca, Id_gcs, Id_group, Id_muestreo, Id_muestreo_no_dc, Ano, Mes, Dia, Fecha, Pc_start, Ano_grp, Clima)
nrow(Pc_date)

# Combine Rep_dfs with additional information
Pc_date2 <- Rep_dfs %>%
  bind_rows() %>%
  select(-Same_pc) %>%
  full_join(Pc_date)

# Calculate the total number of reps per PC
Pc_date3 <- Pc_date2 %>% 
  reframe(N_reps = n(), across(.cols = everything()), .by = Id_muestreo)

# Calculate the number of samp_periods for each point count -- The idea is that b/c some point counts were surveyed up to 3 times, but always 2 of which were in the same Ano_grp (e.g., 13-14), using distinct() will remove a row in the same Ano_grp. The point count IDs with 2 rows are possible resurvey sites (although see the temporal sampling plot for potential issues of seasonality).
Pc_samp_periods <- Pc_date2 %>%
  distinct(Id_muestreo, Id_muestreo_no_dc, Uniq_db, Ano_grp, Ecoregion) %>%
  reframe(
    N_samp_periods = n(), across(.cols = everything()),
    .by = Id_muestreo_no_dc
  ) %>%
  distinct(Id_muestreo, Uniq_db, N_samp_periods) %>%
  tibble()

# Merge with Pc_samp_periods
Pc_date4 <- Pc_date3 %>%
  left_join(distinct(Pc_samp_periods, Id_muestreo, N_samp_periods))

## Make educated guesses for UniLlanos Fecha column
# UniLlanos did not record the date (or time) of point counts where they didn't observe any birds

# Educated guesses on survey date
ids_df <- Pc_date4 %>%
  filter(Spp_obs == 0 & is.na(Fecha)) %>%
  distinct(Id_muestreo, Rep_ano_grp)

# Generate Missing data df
date_str <- c("2019-11-12", "2019-11-10", "2019-11-11", "2019-11-19", "2019-11-20", "2019-11-20")
Miss_date <- map_vec(date_str, as.Date)
Miss_date_df <- cbind(ids_df, Fecha_update = Miss_date)

# Example: Likely surveyed on the 12th
Pc_date4 %>%
  filter(Id_muestreo == "U-MB-M-LRO1_10") %>%
  pull(Fecha)

# Use df & coalesce() to fill in the 6 values where Fecha is NA, and calculate the julian date now that every row has Fecha
Pc_date5 <- Pc_date4 %>%
  left_join(Miss_date_df) %>%
  mutate(
    Fecha = coalesce(Fecha, Fecha_update),
    # Julian date
    Julian_day = lubridate::yday(Fecha)
  ) %>%
  select(-Fecha_update)

# Assign Pc_length based on what we know of the data collectors' methodologies.
# Really, Pc_length column is only relevant for CIPAV. Pc_length is helpful for data checking, but in models only CIPAV has meaningful variation (within a Uniq_db), and could just blanket assign values for between Uniq_dbs
Pc_date6 <- Pc_date5 %>%
  mutate(
    Pc_length = case_when(
      is.na(Pc_length) & Uniq_db %in% c("Gaica mbd", "Ubc mbd", "Unillanos mbd") ~
        as_hms("00:00:00"),
      is.na(Pc_length) & Uniq_db == "Gaica distancia" ~
        as_hms("00:10:00"),
      .default = Pc_length
    ),
    AM_PM = case_when(
      Uniq_db == "Gaica distancia" & Pc_start > as_hms("14:00:00") ~ "Afternoon",
      Pc_start < as_hms("14:00:00") ~ "Morning",
      Uniq_db == "Unillanos mbd" ~ "Morning",
      .default = NA
    ),
    Id_group = str_split_i(Id_muestreo, "_", i = 1)
  ) %>%
  # Nested if_else is confusing, but just replacing where AM_PM is NA, to be the OPPOSITE of whatever the other AM_PM is for that Id_muestreo & Fecha
  group_by(Id_muestreo, Fecha) %>%
  mutate(AM_PM = ifelse(
    is.na(AM_PM) & Uniq_db == "Gaica distancia",
    if_else(any(AM_PM == "Afternoon", na.rm = TRUE), "Morning", "Afternoon"),
    AM_PM
  )) %>%
  ungroup()

## Impute missing times
# Ensure that Ecotropico / GAICA fix these two points.
Eco_fix <- c("G-AD-M-LPA_06", "G-AD-M-LCA2_03")

# For now a manual fix, just assigning possible times and dates
Pc_date6 <- Pc_date6 %>%
  mutate(
    Fecha = if_else(Id_muestreo == "G-AD-M-LCA2_03" & Rep_ano_grp == 5, as.Date("2019-10-10"), Fecha),
    Pc_start = case_when(
      Id_muestreo == "G-AD-M-LCA2_03" & Rep_ano_grp == 4 ~ as_hms("08:00:00"),
      Id_muestreo == "G-AD-M-LCA2_03" & Rep_ano_grp == 5 ~ as_hms("15:00:00"),
      .default = as_hms(Pc_start)
    )
  )

# Impute with the average of the times for the rest of that morning or afternoon
Pc_date7 <- Pc_date6 %>%
  # distinct(Id_group, Id_muestreo, Fecha, Pc_start, AM_PM, Spp_obs) %>%
  arrange(desc(AM_PM), Id_group, Fecha) %>%
  mutate(Pc_start = if_else(
    is.na(Pc_start),
    hms::hms(seconds = round(as.numeric(mean(Pc_start, na.rm = TRUE)), 0)),
    Pc_start
  ), .by = c(Id_group, Fecha, AM_PM)) %>%
  mutate(Ano = year(Fecha), Mes = month(Fecha), Dia = day(Fecha))

## Create 'Season' column, allowing for further separation of Repetitions, and ultimate creation of 'Rep_season' column
# NOTE:: Cutoff of 80 days for 2 reasons
# 1) Our sampling schedule, where 'GAICA mbd' surveyed the same point counts ~90 days apart
# 2) Biologically, we believe that >80 days is unlikely to meet the closure assumption (i.e., the period closed to births, deaths, immigration and emigration). Alternative time periods could be considered depending on beliefs of the underlying biology. 

Pc_date8 <- Pc_date7 %>%
  group_by(Id_muestreo, Ano_grp) %>%
  arrange(Id_muestreo, Ano_grp, Fecha, Pc_start) %>%
  mutate(Days_since = Fecha - first(Fecha)) %>%
  mutate(Season = if_else(Days_since < 80, "Early", "Late")) %>%
  group_by(Id_muestreo, Ano_grp, Season) %>%
  mutate(Rep_season = row_number()) %>%
  ungroup() %>%
  select(-c(Days_since))

# NOTE:: There are still some NAs in this dataframe, but I don't think it would be too hard to fill in in the future if necessary
Pc_date8 %>% 
  Na_rows_cols(cols_inc = c(Id_group, Ano_grp, Id_muestreo, Uniq_db, Fecha)) 

# Event covariates --------------------------------------------------------
# Format the information that varies per visit (event) , e.g. weather
# distinct(Ano_grp, Rep_ano_grp) would be unique combos of samp_period and repetition

## For UBC 2024 data event covariates were recorded in the metadata file. 
# Did not record amount of potrero or whether cows were present in Otun Quimbaya because they never ocurred
df_metadata$Ubc_gaica_OQ <- df_metadata$Ubc_gaica_OQ %>% 
  mutate(Percent_potrero = 0,
         Cows_50m = "No")

# Remove 'Ensayo' days to match Birds_df?
Covs_ubc_gaica <- map(df_metadata[4:6], \(df) {
  df %>%
    as_tibble() %>%
    select(
      Id_muestreo, Fecha, Hora, Habitat_predominante,
      contains(c("Percent", "Cow", "Winds", "Noise", "Clouds", "Rain", "agua"))
    ) %>%
    mutate(
      Cuerpo_de_agua = if_else(Cuerpo_de_agua == "__", "None", Cuerpo_de_agua)
    ) %>%
    rename_with(~ str_remove(., "Percent_"), contains(c("bosque", "potrero"))) %>%
    mutate(Per_other = rowSums(select(., starts_with("Percent")), na.rm = TRUE)) %>%
    select(-starts_with("Percent_")) %>%
    rename_with(~ paste0("Per_", .), .cols = contains(c("bosque", "potrero"))) %>%
    relocate(contains("Per_"), .after = Habitat_predominante) %>%
    mutate(across(starts_with("Per_"), replace_na, 0)) %>% 
    replace_with_na_all(condition = ~.x %in% c("__")) %>% 
    mutate(across(.cols = c(Winds, Clouds), ~as.numeric(.x)),
           Rain = if_else(Rain == "Leve", "Light", Rain))
}) %>% list_rbind()

## For UBC 2021 data event covariates were recorded in both the metadata and bird file
# From bird file
Covs_main_ubc <- df_birds_red$Ubc %>% 
  tibble() %>% 
  distinct(Id_muestreo, Fecha, Hora, Cigarras, Ruido, Vacas_menos_50) %>% 
  rename(Noise = Ruido, Cows_50m = Vacas_menos_50) %>%
  mutate(
    Cows_50m = if_else(Cows_50m == "Si", "Yes", Cows_50m), 
    Noise = case_when(
      Noise == "Ninguno" ~ "None",
      Noise == "Ligero" ~ "Light",
      Noise == "Moderado" ~ "Moderate",
      Noise == "Alto" ~ "Loud"
      )
    ) 

## Standardize data where multiple weather variables were taken at each point count
# From metadata file
df_metadata$Ubc <- df_metadata$Ubc %>% 
  rename_with(.fn = ~str_remove(.x, "Observacion_climatica_")) %>% 
  Cap_snake() %>%
  mutate(Clima = case_when(
    Lluvia == "Ligera" ~ "Llovizna",
    Viento > 1 ~ "Brisa",
    Nubes > 2 ~ "Nublado",
    .default = "Despejado"
  ))

Covs_ubc_gaica2 <- Covs_ubc_gaica %>% 
  mutate(Clouds = Clouds + 1, 
         Clima = case_when(
           Rain == "Light" ~ "Llovizna",
           Winds > 1 ~ "Brisa",
           Clouds > 2 ~ "Nublado",
           .default = "Despejado")
         )

## Final set of event (survey) covariates 
# For UBC & GAICA 2024 data
Event_covs_ug <- Covs_ubc_gaica2 %>% 
  rename(Pc_start = Hora) %>%
  distinct(Id_muestreo, Fecha, Pc_start, Noise, Clima, Cows_50m)

Event_covs_ubc <- df_metadata$Ubc %>% 
  left_join(Covs_main_ubc) %>% 
  rename(Pc_start = Hora) %>%
  distinct(Id_muestreo, Fecha, Pc_start,  Noise, Clima, Cows_50m) %>% 
  mutate(Cows_50m = if_else(is.na(Cows_50m), "No", Cows_50m))

# Combined event covariates for 2021 and 2024 (ubc & gaica) field seasons 
Event_covs_ubc_ug <- bind_rows(Event_covs_ubc, Event_covs_ug)

# >Merge with Pc_date -----------------------------------------------------
Event_covs_all <- Pc_date8 %>%
  left_join(
    Event_covs_ubc_ug,
    by = c("Id_muestreo", "Fecha", "Pc_start"),
    suffix = c("", ".new")
  ) %>%
  mutate(Clima = coalesce(Clima, Clima.new)) %>%
  select(-Clima.new)

## Testing - Did this work? YES
# These are points that are in Event_covs_ubc_ug and not in Pc_date8. These are practice points, points that we did not survey but took some measurements (e.g. habitat), etc. 
anti_join(Event_covs_ubc_ug, Event_covs_all)

# Keep (and order) only the relevant columns
Event_covs <- Event_covs_all %>% 
  select(Id_muestreo, Id_muestreo_no_dc, Id_group, Nombre_institucion, Uniq_db, Fecha, Ano_grp, Ano, Mes, Dia, Julian_day, Pc_start, Pc_length, N_samp_periods, N_reps, Rep_ano_grp, Rep_season, Spp_obs, Noise, Clima, Cows_50m)

# Environmental data ---------------------------------------------------
#stop() 
## Download monthly temp & precipitation data
# May want to include additional vars (e.g., precipitation seasonality): https://www.worldclim.org/data/bioclim.html
Wc_col <- map(c("tavg", "prec"), \(var){
  worldclim_country(country = "COL", var = var, path = "../Geospatial_data/Environmental")
})

# To match the bioclim variables (1 & 12), take average of temperature & sum of rainfall
avg.temp <- mean(Wc_col[[1]])
tot.prec <- sum(Wc_col[[2]])

## Elevation
coords <- data.frame(st_coordinates(Pc_locs_sf))
# Ensure every point count has an associated elevation raster. Creates a list with nrow() coords
Elev_90m <- pmap(coords, function(X, Y) {
  geodata::elevation_3s(lon = X, lat = Y, path = "../Geospatial_data/Environmental/elevation_90m")
})

# Bring in the 6 unique tif files & merge into a single elevation file
files.elev <- list.files(path = "../Geospatial_data/Environmental/elevation_90m/elevation", pattern = ".tif")
Elev_90m <- map(.x = files.elev, .f = \(tif)
rast(paste0("../Geospatial_data/Environmental/elevation_90m/elevation/", tif)))
Elev_90m <- do.call(merge, Elev_90m)

## Create list with elevation, temp, & precip
envi.vars <- list(elev.dem = Elev_90m, avg.temp = avg.temp, tot.prec = tot.prec)

# Visualize
if(FALSE){
  imap(envi.vars, \(var, names){
    ggplot() +
      geom_spatraster(data = var) +
      labs(title = names)
  })
}

# Extract environmental vars & create df with envi variables at each PC location
Envi_df <- cbind(
  Pc_locs_sf[,c("Id_muestreo", "Ecoregion", "Departamento", "Uniq_db", "Id_group")],
  coords,
  sapply(envi.vars, terra::extract, Pc_locs_sf, ID = FALSE)
) %>%
  rename(Elev = elev.dem.srtm_21_10, 
         Avg_temp = avg.temp.mean, 
         Tot_prec = tot.prec.sum, 
         Long = X, 
         Lat = Y) %>% 
  mutate(Avg_temp = round(Avg_temp, 3))
Envi_df2 <- Envi_df %>%
  st_drop_geometry() %>%
  slice_head(by = Id_muestreo) %>%
  full_join(distinct(Pc_hab, Id_muestreo, Habitat, Habitat_sub)) %>%
  as_tibble()


# >Site covs --------------------------------------------------------------
Site_covs <- Bird_pcs_all %>% 
  distinct(
    Id_muestreo, Id_muestreo_no_dc, Nombre_institucion, Id_gcs
  ) %>% 
  left_join(Envi_df2) %>% 
  select(-c(Id_muestreo, Uniq_db, Nombre_institucion, Id_group)) %>%
  distinct() 


# >Precipitation ----------------------------------------------------------
# Extract data & create df where each row is a point count and there are 12 'prec' columns, one for each month
PrecPCs <- terra::extract(Wc_col[[2]], Pc_locs_sf, ID = FALSE)
Prec_df <- cbind(Pc_locs_sf[, c("Id_muestreo", "Ecoregion", "Departamento", "Uniq_db")], PrecPCs) %>%
  group_by(Id_muestreo) %>%
  slice_head() %>%
  ungroup() %>%
  st_drop_geometry() %>%
  rename_with(~ str_remove(., "COL_wc2.1_30s_"))

if (FALSE) { # This process is slow
  ## Download daily precipitation data for Cubarral from IDEAM stations for the 4 months before first date + sampling period
  # Unillanos: sampling period (11/10 - 11/26 of 2019, about 16 days)
  Prec_daily19 <- ColOpenData::download_climate(
    code = "50223", start_date = "2019-07-10", end_date = "2019-11-26", tag = "PTPM_CON"
    ) %>% 
    mutate(year = "July - Nov, 2019")
  # UBC: Sampling period (5/28 - 6/18 of 2022, about 20 days)
  Prec_daily22 <- ColOpenData::download_climate(
    code = "50223", start_date = "2022-01-28", end_date = "2022-06-18", tag = "PTPM_CON"
    ) %>% 
    mutate(year = "Jan - June, 2022")
  Prec_daily <- bind_rows(Prec_daily19, Prec_daily22) %>%
    mutate(
      date = as.Date(date),
      md = format(date, "%m-%d")
    ) %>%
    mutate(day = as.numeric(date - min(date)), .by = year)
}

# Also see landcover function: the fraction of a landcover class in each cell, at 30-seconds (1km at equator) spatial resolution
# ?geodata::landcover

# STILL TO DO: Use divipola codes to extract rainfall data for the appropriate municipalities & dates for each set of point counts.
# NOTE:: This is for a few points in the given municipality (of variable size), but should know what the appropriate spatial scale is (2x2 kms^2?) & how far away the stations are from sampling points. As a first pass just do what's easy & include it in models, see if it makes a difference. Eventually could explore kriging or something more advanced
# mpio_sf <- download_geospatial(
# spatial_level = "mpio",
# simplified = TRUE,
# include_geom = TRUE,
# include_cnpv = FALSE
# )

# Has municipality codes
# mpio_sf %>% filter(codigo_municipio == "50223")

# Plot
# ggplot() + geom_sf(data = mpio_sf)

# Save & export -----------------------------------------------------------
stop() 
rm(list = ls()[!(ls() %in% c("Bird_pcs_all", "Birds_comb4", "Pc_date8", "Pc_hab", "Pc_locs", "df_birds", "df_metadata", "df_meta", "df_birds_red", "Mes_mod", "Pc_locs_mult", "Pc_locs_sf", "Envi_df2", "Prec_df", "Prec_daily", "Rep_dfs"))])
#save.image(paste0("Rdata/the_basics_", format(Sys.Date(), "%m.%d.%y"), ".Rdata"))
#save.image("Rdata/the_basics_09.15.25.Rdata") # Manual

## Export Bird_pcs_all as csv
names(Bird_pcs_all)
Bird_pcs_all %>% 
  arrange(Id_group, Id_muestreo, Fecha, Pc_start) %>%
  select(
    Id_muestreo, Id_muestreo_no_dc, Fecha, Pc_start, Species_ayerbe, Distancia_bird, Tipo_registro, Grabacion, Count 
    ) %>% 
  summarize(Count = sum(Count), .by = -Count) %>% 
  write_csv(file = "Derived/Excels/Bird_pcs_all.csv")

## Export covariates as csv
# Site covariates - There are 496 unique locations, so all of these are stable irrespective of which data collector

Site_covs %>% write_csv(file = "Derived/Excels/Site_covs.csv")

# Export Precipitation df for the data_paper_figs script
Prec_df %>% write_csv(file = "Derived/Excels/Prec_df.csv")

# Event covariates - 2691  point count surveys
Event_covs %>% write_csv(file = "Derived/Excels/Event_covs.csv")

## Export Pc_hab to update points manually using Google Earth
# NOTE: This is likely not necessary because can use Mathilde / Natalia digitized landcover & calculate distance to forest edge from point count location. 
if(FALSE){
  Join_year <- Bird_pcs_all %>% distinct(Id_muestreo, Ano, Habitat_og)
  Pc_hab_ano %>% 
    filter(str_detect(Habitat, "Bosque")) %>% 
    distinct(Id_muestreo, Uniq_db, Ecoregion, Ano, pick(starts_with("Habitat"))) %>% 
    arrange(Id_muestreo) %>% 
    data.frame() %>%
    xlsx::write.xlsx("Derived/Excels/Hab_define_forest.xlsx", 
                     showNA = FALSE, row.names = FALSE)
}

st_write(Pc_locs_sf, dsn = "Derived_geospatial/shp", layer = "Pc_locs.shp")

# Working data paper ------------------------------------------------------
# PROBLEM WITH PC_START FOR V-03 AND V-04
Birds_comb4 %>% filter(Id_group == "C-MB-S-V") %>% 
  distinct(Id_muestreo, Pc_start, Pc_length)

# Send screenshot to ecotropico
df_birds$Cipav %>% 
  filter(Departamento == "Santander" & Nombre_finca == "Valparaiso") %>% 
  distinct(Id_muestreo, Ano, Mes, Dia, Hora) %>% 
  arrange(Id_muestreo, Hora) %>% 
  view()

# GPT - I want to create a logical check that examines whether the End time of a given point count (Id_muestreo) is after the start time of any other different point count on the same day, so, for example : INCLUDE DPUT is a problem because there are two point counts occurring simultaneously 
dput(head(Event_covs))

Event_covs2 <- Event_covs %>% mutate(Pc_end = as_hms(Pc_start + Pc_length))
Event_covs2 %>% 
  group_by(Uniq_db) %>%
  mutate(Problem = ifelse(Pc_start < Pc_end & Fecha == Fecha), "Yes", "No")

Event_covs2 %>% left_join(Site_covs) %>% 
  filter(Uniq_db == "Cipav mbd" & Departamento == "Santander" & Id_group == "C-MB-S-V") %>% 
  distinct(Id_muestreo, Pc_start, Pc_end, Pc_length) #%>% 
  #dput()


# Field work --------------------------------------------------------------
# Formatting data 
df_metadata$Ubc_hatico %>% 
  rename(Habitat = Habitat_predominante) %>%
  select(Año, Mes, Dia, Hora, ID_punto_muestreo_FINAL, Habitat) %>% 
  right_join(df_birds$Ubc_hatico) %>% 
  mutate(Hora = sapply(str_split(Hora, " "), function(x) {
    x[2]
  }),
  Hora = as.character(as_hms(Hora)),
  Hora = sapply(str_split(Hora, ":"), function(x) {
    paste0(x[1], ":", x[2])
  })) %>%
  as.data.frame() %>%
  write.xlsx("Data/Ubc_hatico_habitat.xlsx", showNA = FALSE, row.names = FALSE)

# Spatial information
coords <- st_read("Data/ubc-points-el_hatico.kml") %>% st_coordinates() %>%
  as_tibble() %>%
  select(1:2)
pt_names <- st_read("Data/ubc-points-el_hatico.kml") %>% pull(Name)
pt_locs <- tibble(pt_names, coords) %>% 
  select(1:3) %>% 
  rename_with(~c("ID_punto_muestreo_FINAL", "Longitude", "Latitude"))

df_birds$Ubc_hatico %>% select(Id_muestreo) %>% distinct() %>% 
  left_join(pt_locs)
pt_locs

# Join
df_metadata$Ubc_hatico %>% select(Id_muestreo, Latitude, Longitude) %>%
  distinct() %>%
  view()
  right_join(df_birds$Ubc_hatico, 
             by = join_by("Id_punto_muestreo_final" == "ID_punto_muestreo_FINAL")) %>% mutate(Hora = sapply(str_split(Hora, " "), function(x) {
               x[2]
             }),
             Hora = as.character(as_hms(Hora)),
             Hora = sapply(str_split(Hora, ":"), function(x) {
               paste0(x[1], ":", x[2])
             })) %>%
  as.data.frame() %>%
  write.xlsx("Data/Ubc_hatico_coords.xlsx", showNA = FALSE, row.names = FALSE)

df_birds_red$Cipav %>% filter(Nombre_finca == "El hatico") %>% 
  distinct(Id_muestreo, Latitud_decimal, Longitud_decimal)



## Visit in the field 2025
# Create Valledupar point as sf object (note: lon, lat)
Valledupar <- st_sfc(st_point(c(-73.2500, 10.4833)), crs = 4326)

# Calculate distance (in meters by default)
Dist_valledupar <- Pc_locs_sf %>% 
  filter(Ecoregion == "Rio cesar") %>%
  mutate(Dist_Valledupar_km = st_distance(., Valledupar) / 1000) %>% 
  arrange(Dist_Valledupar_km)

Dist_valledupar2 <- Dist_valledupar %>% 
  left_join(Site_covs[,c("Id_muestreo_no_dc", "Id_gcs")]) %>% 
  select(-Nombre_institucion) %>% 
  relocate(Id_gcs, Dist_Valledupar_km, .after = Id_muestreo_no_dc)

st_write(Dist_valledupar2, "Derived_geospatial/shp/Dist_valledupar.gpkg", layer = "Dist_valledupar")

test <- st_read("Derived_geospatial/shp/Dist_valledupar.gpkg") 
test %>% ggplot(aes(color = Dist_Valledupar_km)) +
  geom_sf() + 
  geom_sf(data = Valledupar, color = "red", shape = 3, size = 8)

## Bajo Magdalena
Pc_locs_sf %>% 
  filter(Ecoregion == "Bajo magdalena") %>% 
  select(-Nombre_institucion) %>% 
  st_write("Derived_geospatial/shp/Bajo_magdalena_pts.gpkg", 
           layer = "Bajo_magdalena_pts")

## Boyaca santander
Pc_locs_sf %>% 
  filter(Ecoregion == "Boyaca santander") %>% 
  select(-Nombre_institucion) %>% 
  filter(Id_gcs %in% c(2492, 2921)) %>% 
  pull(Id_muestreo)
#st_write("Derived_geospatial/shp/Boyaca_santander_pts.gpkg", layer = "Boyaca_santander_pts")

## Resurvey with Santiago
Resurvey_santi <- Bird_pcs_all %>% 
  filter(Uniq_db %in% c("Ubc gaica mbd", "Unillanos mbd") & Ecoregion == "Piedemonte") %>% 
  st_as_sf(coords = c("Longitud", "Latitud"), crs = 4326) %>%
  distinct(Id_muestreo, Nombre_institucion, Id_gcs, Nombre_finca, geometry) 

# Distinct farms 
Resurvey_santi %>% 
  pull(Id_gcs) %>% 
  unique()

# Visualize map
Resurvey_santi %>% ggplot() + 
  geom_sf(aes(color = Nombre_institucion))

# Export Google Earth file 
Resurvey_santi %>%
  arrange(Nombre_institucion, Id_muestreo) %>%
  rename(
    name = Id_muestreo,
    Institucion = Nombre_institucion # ,
    # Farm = Nombre_finca_mixed
  ) %>%
  st_write(
    driver='kml', dsn="Derived_geospatial/kml/Resurvey_santi.kml", layer = "Resurvey_santi"
  )

## Examine Id_gcs in the databases before any manipulation
map(df_birds, \(df){
  df %>% mutate(across(starts_with("Id_pr"), ~ na_if(., 0))) %>%
    mutate(Id_gcs = do.call(coalesce, across(starts_with("Id_pr")))) %>% 
    select(Id_gcs) 
}) %>% list_rbind() %>% 
  pull(Id_gcs) %>% 
  unique() %>% 
  tibble(Id_gcs = .) %>%
  data.frame() %>%
  write.xlsx(file = "Derived/Id_gcs.xlsx", row.names = FALSE)

# Use anti_join to see mismatches between metadata & the birds databases
map2(df_birds_red, df_metadata, \(birds, meta){
  detection <- birds %>%
    select(
      Id_muestreo, Fecha, Hora,
      contains(c("Clima", "Cigarras", "Ruido", "Vacas"))
    ) %>%
    distinct()
  meta <- meta %>% distinct(Id_muestreo, Fecha, Hora, Spp_obs)
  comb <- meta %>% full_join(detection)
  comb %>%
    anti_join(detection) %>%
    filter(!str_detect(Id_muestreo, "LCR|JB|LCA") & # In metadata file but 0 (or 1) accompanying PC
             !str_detect(Id_muestreo, "ECOR|PORT") & # Ensayo days
             Spp_obs != 0) # Points where no species were observed
})

# Find the cutoff_times specific to each database
map(df_birds_red, \(df){
  df %>%
    group_by(Id_muestreo, Fecha) %>%
    filter(!str_detect(Id_muestreo, "LIBRE")) %>%
    arrange(Hora) %>%
    mutate(
      Pc_length_day = (Hora - first(Hora) * 1440), # For entire day, not within a PC
      Same_pc = if_else(Pc_length_day < 91, "Same", "Diff")
    ) %>%
    distinct(Pc_length_day) %>%
    arrange(desc(Pc_length_day)) %>%
    filter(Pc_length_day < 91)
})

## FOLLOWUP:
# Ecotropico:: No observations where Hora == 07:36:00 or 07:37:00
df_birds_red$Gaica_dist %>%
  filter(Id_muestreo == "G-AD-M-CO_05") %>%
  distinct(Id_muestreo, Fecha, Hora) %>%
  arrange(Fecha, Hora)

Pc_date8 %>% filter(Spp_obs == 0)

Rep_dfs %>%
  bind_rows() %>%
  filter(if_any(everything(), is.na)) %>%
  # count(Id_muestreo, sort = T) %>%
  left_join(Pc_samp_periods[, c("N_samp_periods", "Uniq_db", "Id_muestreo")]) %>%
  count(Id_muestreo, sort = T)

Rep_dfs %>%
  bind_rows() %>%
  filter(Id_muestreo == "G-MB-Q-ECOR_03")

df_metadata$Gaica_dist %>%
  filter(Id_muestreo == "G-AD-M-LCA2_04") %>%
  distinct(Id_muestreo, Fecha, Hora, Comentario)

# Farm names from FEDEGAN -------------------------------------------------

#578
Birds_comb4 %>% filter(Id_gcs == 578)
Birds_comb4 %>% 
  filter(Nombre_finca %in% c("La renuncia", "El girasol") & Departamento == "Atlantico") %>% 
  pull(Id_gcs) %>% unique()

Ids_mult_farms <- Birds_comb4 %>% 
  distinct(Id_gcs, Nombre_finca) %>% 
  count(Id_gcs, sort = T) %>% 
  filter(n > 1) %>% 
  pull(Id_gcs)

Birds_comb4 %>% 
  distinct(Id_gcs, Nombre_finca) %>% 
  filter(Id_gcs %in% Ids_mult_farms) %>% 
  arrange(Id_gcs)

Birds_comb4 %>% filter(Id_gcs == "Ref_La luisa") %>% pull(Id_muestreo) %>% unique()


Birds_comb4 %>% filter(Nombre_finca == "La herradura") %>% distinct(Id_gcs, Nombre_institucion, Nombre_finca, Ecoregion, Nombre_finca_mixed, Latitud, Longitud) 

#1053
Birds_comb4 %>% filter(Id_gcs %in% c(1053)) %>% 
  distinct(Id_gcs, Nombre_institucion, Nombre_finca, Ecoregion, Nombre_finca_mixed, Latitud, Longitud) %>% view()
Birds_comb4 %>% filter(Nombre_finca == "Cielo lirio" & Ecoregion == "Bajo magdalena") %>% distinct(Id_gcs, Nombre_institucion, Nombre_finca, Nombre_finca_mixed, Latitud, Longitud) #%>% 
pull(Id_gcs) %>% unique()
Birds_comb4 %>% filter(Id_gcs %in% c(2206)) %>% 
  distinct(Id_gcs, Nombre_institucion, Nombre_finca, Ecoregion, Nombre_finca_mixed, Latitud, Longitud)

#259 and 1732
Birds_comb4 %>% filter(Id_gcs %in% c(259, 1732)) %>% 
  distinct(Id_gcs, Nombre_institucion, Nombre_finca, Ecoregion, Nombre_finca_mixed)

Birds_comb4 %>% 
  filter(
    Nombre_institucion %in% c("Ubc", "Ubc gaica") & Ecoregion == "Piedemonte"
    ) %>% 
  distinct(Id_gcs, Id_group, Nombre_finca) %>% 
  arrange(Nombre_finca) %>%
  as.data.frame() %>%
  write.xlsx("/Users/aaronskinner/Library/CloudStorage/OneDrive-UBC/Grad_School/Outreach/Outreach_farmers/Excels/Consolidate_id_gcs.xlsx", row.names = FALSE)

