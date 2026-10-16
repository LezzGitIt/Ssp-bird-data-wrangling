## PhD birds in silvopastoral landscapes ##
## Data wrangling 00 -- Gather the raw point-count records from the data providers' workbooks (not part of the archived pipeline)

## Reads each provider workbook (Data/Aves), keeps only the point-count records and the columns 01 uses, and stacks them into two CSVs in the deposit (DataS1/Raw/), which 01_Gen_wrangling.R starts from: the bird records (Raw_bird-records.csv) and the point-count metadata (Raw_point-count-metadata.csv; one row per point count, including counts with no birds).
## Minimal formatting: raw (Spanish) headers and values are kept, plus a Dataset column naming the source workbook, so 01 can split the rows back into one table per provider and its dataset-specific code runs unchanged.
## Cells: blank = the provider has this column but left the cell empty; NA = the provider's workbook has no such column.
## Deposited only so the archived pipeline can be rerun; Bird-point-counts_all.csv is the bird data for most purposes.
## Run once whenever a provider workbook changes.

## Rows removed:
# Non-point-count protocols: mist-net captures (GAICA mbd), ad-hoc sightings and free-roaming transects (recorridos libres; GAICA mbd, Unillanos).
# Practice days are kept here and removed in 01, so users can choose to include them.

# Libraries ---------------------------------------------------------------
library(tidyverse)
library(readxl)

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

## Name lists
files
short_names <- c("Cipav", "Gaica_dist", "Gaica_mbd", "Ubc_gaica_Caf", "Ubc_gaica_Meta", "Ubc_gaica_OQ", "Ubc_hatico", "Ubc_meta26", "Ubc_meta22", "UniLlanos") # "Ubc_Monroy"
names(df_birds) <- short_names
names(df_metadata) <- short_names

# Keep point counts only --------------------------------------------------
df_birds <- map(df_birds, \(df) df %>% filter(Protocolo_muestreo == "Observacion en puntos fijos de conteo"))

# NOTE in Unillanos:: Distancia_observacion is >50 whenever Comentario_registro == "Fuera" (a column dropped below)
df_birds$UniLlanos %>%
  filter(Comentario_registro == "Fuera") %>%
  distinct(Tipo_registro, Distancia_observacion)

# Remove irrelevant columns -----------------------------------------------
# The first 21 columns plus the fragments 01 uses (matched case-insensitively against the raw headers)
df_birds <- map(df_birds, \(df){
  df %>%
    select(
      1:21,
      contains(c("Id_punto_muestreo", "Protocolo_muestreo", "Año", "Mes", "Hora", "Orden", "Familia", "Especie", "cientifico", "Numero_individuos", "Tipo_registro", "Habitat", "Sistema", "Registrado", "Distancia_observacion", "climatica", "Elevacion", "finca", "grabacion", "Cigarras", "Vacas", "Ruido", "Observaciones", "Estrato_")),
      any_of("Dia")   # exact name: contains("Dia") would also catch GAICA distancia's Dia_muestreo
    ) %>%
    select(-contains(c("Numero_registro", "Id_registro_biologico", "Observacion_climatica_evento", "Concatenar", "repeticion"))) %>%
    # Columns 01 never uses (coordinates as text, taxonomic authority, municipality, provider-specific extras)
    select(-any_of(c("Clase", "Municipio", "Coordenadas_geograficas", "Precision_coordenadas", "Incertidumbre_coordenadas", "Autoria_nombre_cientifico", "Autoria_ nombre_cientifico", "Numero_especie_GAICA", "Tamaño_bosque_BASE_INFOTNC", "Tamaño_silvopastoril_BITACORA", "Tamaño_silvopastoril_BASE_INFOTNC", "Habitat_enfocado_SSP_BASE_INFOTNC", "First_5.", "Vereda", "Esfuerzo_muestreo", "Elevacion", "Habitat1", "Habitat2", "Observacion_climatica_ORIGINAL")))
})

## Metadata: drop the columns 01 never uses (free-text notes, effort descriptions, count tallies, duplicate coordinates)
df_metadata <- map(df_metadata, \(df){
  df %>% select(-any_of(c("Comentario", "Observacion", "Conteo de puntos", "Conteo de visitas", "Conteo dias recorridos LIBRES", "Esfuerzo_muestreo", "Esfuerzo_muestreo_(puntos de conteo por finca y conteos por punto)", "Esfuerzo_muestreo_puntos_de_conteo_por_finca_y_conteos_por_punto", "No. conteo por punto por finca por año", "No_conteo_por_punto_por_finca_por_año", "No_conteo_por_punto_por_finca_por_ano", "Latitude", "Longitude")))
})

# Stack & save ------------------------------------------------------------
## One table per provider -> one CSV: all values as text (numbers to 15 significant digits, Excel's precision; times as HH:MM:SS on the 1899-12-31 date Excel gives them), an empty provider cell written as blank, and the providers stacked with a Dataset column; bind_rows() fills NA where a provider lacks a column
stack_providers <- function(df_list){
  imap(df_list, \(df, dataset){
    df %>%
      mutate(
        across(where(\(x) inherits(x, "POSIXct")), \(x) format(x, "%Y-%m-%d %H:%M:%S", tz = "UTC")),
        across(where(is.double), \(x) if_else(is.na(x), NA_character_, sprintf("%.15g", x))),
        across(everything(), \(x) replace_na(as.character(x), "")),
        Dataset = dataset, .before = 1
      )
  }) %>%
    bind_rows()
}

dir.create("DataS1/Raw", showWarnings = FALSE)
stack_providers(df_birds) %>% write_csv("DataS1/Raw/Raw_bird-records.csv", na = "NA")
stack_providers(df_metadata) %>% write_csv("DataS1/Raw/Raw_point-count-metadata.csv", na = "NA")
