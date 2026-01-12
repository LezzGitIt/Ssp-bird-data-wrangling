## PhD birds in silvopastoral landscapes##
# Outreach farmers -- Create relative abundance plots & tables of the species observed in each farm
## This script creates the necessary farm-specific outputs for compilation in PDFs using LaTeX & Python

# NOTE:: The outputs created in this R script are then accessed in 'create_pdfs.py' script to create farm-specific PDFs, & then merged with the 'Fun_facts_general' PDF using the 'Merge_pdfs.py' script
# Relevant files: Grad_School/Outreach/Outreach_farmers

# Contents
# 1) Load & format personal eBird data (just for Eje Cafetero for now)
# 2) Join with birds observed during formal surveys (e.g. point counts)
# 3) Sum number of individuals to create species counts for each farm
# 4) Print relative abundance bar graphs and save
# 5) Create .tex tables with species observed on farm using stargazer package
# 6) Create Excel to bring into Python (called 'data_list') that ultimately runs through the for loop in Python
# 7) Extra plotting

# Load libraries & data ---------------------------------------------------------------
library(tidyverse)
library(cowplot)
library(raster)
library(sf)
library(gtools)
library(readxl)
library(xlsx)
library(ebirdst)
library(gridExtra)
library(conflicted)
library(stargazer)
ggplot2::theme_set(theme_cowplot())
conflicts_prefer(dplyr::select)
conflicts_prefer(dplyr::filter)

# Data
Bird_pcs_dist <- read_csv(file = "Data_paper/DataS1/Bird_pcs_dist.csv")
Site_covs <- read_csv(file = "Derived/Excels/Site_covs.csv")
Ex_farm_comb <- read_excel("/Users/aaronskinner/Library/CloudStorage/OneDrive-UBC/Grad_School/Outreach/Outreach_farmers/Excels/Consolidate_id_gcs.xlsx") %>% 
  select(Id_gcs, Nombre_finca_datos, Nombre_finca_comb, Dueño) %>% 
  rename(Nombre_finca = Nombre_finca_datos,
         Dueno = Dueño)

# Define spread.df function
spread.df <- function(df, nrows = 50){
  df.spr <- data.frame(
    df[1:nrows, ],
    df[(nrows + 1):(nrows * 2), ],
    df[(nrows * 2 + 1):(nrows * 3), ],
    df[(nrows * 3 + 1):(nrows * 4), ]
  ) %>% 
    select(where(~ !all(is.na(.))))
  colnames(df.spr) <- NULL
  return(df.spr)
}

# eBird checklist --------------------------------------------------
# Define path
path <- "/Users/aaronskinner/Library/CloudStorage/OneDrive-UBC/Grad_School/Outreach/Outreach_farmers/"

# Bring in eBird checklists for relevant farms in the Eje Cafetero and format
farms_eB <- read.csv(paste0(path, "Excels/EBird_Eje_2024.csv")) %>%
  select(Location, Count, Scientific.Name) %>%
  filter(str_detect(Location, "Finca|CAIRO|glamping")) %>%
  rename_all(~ c("Nombre_finca", "Count", "Species_ayerbe")) %>%
  filter(!str_detect(Species_ayerbe, "sp.|/")) %>%
  mutate(Nombre_finca = case_when(
    str_detect(Nombre_finca, "San Jose") ~ "Juan b",
    str_detect(Nombre_finca, "CAIRO") ~ "El cairo",
    str_detect(Nombre_finca, "glamping") ~ "Los arboles",
    str_detect(Nombre_finca, "Carelia") ~ "La carelia",
    str_detect(Nombre_finca, "Portugal") ~ "Portugal",
    .default = Nombre_finca
  )) %>% tibble()

## Hatico species list 
Col_eb <- read_csv(paste0(path, "Excels/eBird_Colombia_2025.csv"))
# Species observed in El Hatico from eBird
Spp_eb_eh <- Col_eb %>% 
  select(location, count, scientific_name) %>%
  filter(location == "RN El Hatico") %>% 
  rename_all(~ c("Nombre_finca", "Count", "Species_ayerbe")) %>% 
  filter(!str_detect(Species_ayerbe, "sp.|/")) %>% 
  mutate(Nombre_finca = "El hatico")

# Join eBird lists
farms_eB2 <- bind_rows(Spp_eb_eh, farms_eB)

# Format ------------------------------------------------------------------
# Add metadata to farms_eB
farms_eB3 <- Site_covs %>% 
  distinct(Nombre_finca, Id_gcs, Ecoregion) %>% 
  right_join(farms_eB2)

# Add metadata and from Site_covs and remove unnecessary columns
Birds_pc_farm <- Site_covs %>% 
  distinct(Id_muestreo_no_dc, Nombre_finca, Id_gcs, Ecoregion) %>% 
  right_join(Bird_pcs_dist) %>%
  select(Species_ayerbe, Count, Nombre_finca, Id_gcs, Ecoregion)

# Merge --------------------------------------------------------
# Merge eBird with the SCR databases 
Birds_comb <- Birds_pc_farm %>% 
  bind_rows(farms_eB3)

# Combine Id_gcs & farm name by owner ----------------------------------------
Duenos_comb <- Ex_farm_comb %>% 
  distinct() %>% 
  count(Dueno, sort = T) %>% 
  filter(n > 1) %>% 
  pull(Dueno)
Duenos_comb_tbl <- Ex_farm_comb %>% filter(Dueno %in% Duenos_comb)

Birds_comb2 <- Birds_comb %>% left_join(Duenos_comb_tbl) %>% 
  mutate(Dueno = coalesce(Dueno, Id_gcs), 
         Nombre_finca_comb = coalesce(Nombre_finca_comb, Nombre_finca)) %>%
  mutate(Id_gcs = Dueno,
         Nombre_finca = Nombre_finca_comb) %>% 
  select(-c(Dueno, Nombre_finca_comb))

# Sum by group ------------------------------------------------------------
# Define custom function to sum the counts for a defined set of variables ('group')
sum_by_group <- function(df, group) {
  df %>%
    group_by(across({{ group }})) %>% 
    summarize(Count = sum(as.numeric(Count), na.rm = TRUE), .groups = "drop") %>% 
    arrange(desc(Count))
}

# Sum by group point count data
Birds_count <- Birds_comb2 %>% 
  sum_by_group(group = c(Species_ayerbe, Ecoregion, Nombre_finca, Id_gcs)) 

# Farms with too many species ---------------------------------------------
# There are a few farms with more than 147 species observed (number that can be printed on a single page). This will be more of an issue if we group by owner
Birds_count %>% 
  summarize(spp_farm = n_distinct(Species_ayerbe), 
            .by = c(Nombre_finca, Id_gcs)) %>% 
  arrange(desc(spp_farm))

# For now this only influences 2 farms, so just remove additional species over 147 
Birds_count2 <- Birds_count %>% 
  slice_head(n = 147, by = c(Nombre_finca, Id_gcs))

# IN THE FUTURE: If this is more of an issue moving forward, see GPT chat 'R help >147 species outreach'. One option is to just print off the few extra species separately and include in the printed copy ? 

# Split by farm --------------------------------------------
# Split so each farm is its own list
Birds_by_farm <- Birds_count2 %>% 
  group_by(Ecoregion, Nombre_finca, Id_gcs) %>%
  group_split(.keep = TRUE)
names(Birds_by_farm) <- sapply(Birds_by_farm, function(df) {
  paste0(unique(df$Nombre_finca), "_", "Id_gcs_", unique(df$Id_gcs))
})

# Bar graphs: Relative abundance ------------------------------------------
create_barplot <- function(df, slice_n, plot_title = NULL){
  df %>% slice_head(n = {{ slice_n }}) %>%
    ggplot(aes(x = reorder(Species_ayerbe, Count), y = Count)) +
    geom_col(color = "black", width = .8) +
    labs(y = "Frequencia relativa", title = plot_title) +
    theme(axis.text.x = element_text(size = 10, vjust = .58, angle = 60), 
          axis.title.x = element_blank(), 
          axis.text.y = element_blank(), 
          axis.ticks.y = element_blank()) +
    scale_x_discrete(labels = function(x) str_wrap(str_replace_all(x, " ", "\n")))
}

#if(FALSE){
# Create barplot by farm & export as png 
imap(.x = Birds_by_farm, \(df, names){
  bar.p <- df %>% create_barplot(slice_n = 10)
  ggsave(plot = bar.p, filename = paste0(path, "Farm_specific_inputs/Relative_Frequency_plots_test/", names, ".png"), bg = "white")
})
#}

# Just to visualize, reate barplot for all regions combined
Birds_count %>% sum_by_group(Species_ayerbe) %>% 
  create_barplot(slice_n = 30, plot_title = "All regions")

# Species lists per farm --------------------------------------------------
# Save .tex tables of species lists per farm

# Works, although there are issues with farms > 147 species
imap(.x = Birds_by_farm, \(df, names){
  df50 <- df %>%
    select(Species_ayerbe) %>% # 50 rows per column
    spread.df(nrows = 49) %>%
    as.matrix() # Matrix prevents rownumbers when printed in stargazer
  stargazer(df50,
    type = "latex",
    out =
      paste0(path, "Farm_specific_inputs/Species_lists_test/", names, ".tex"),
    title = "Especies encontradas en la finca (de mas a menos abundantes)",
    colnames = FALSE,
    rownames = FALSE,
    column.sep.width = "2pt",
    summary = FALSE
  )
})

# Abundance by ecoregion --------------------------------------------------
Ecoregions <- unique(Birds_count$Ecoregion)
Ecoregions <- setNames(Ecoregions, Ecoregions)

# Relative abundance plots by Ecoregion
Birds_region <- Birds_comb2 %>% sum_by_group(group = c(Ecoregion, Species_ayerbe))
Abu_by_region_plots <- imap(Ecoregions, \(region, title){
  Birds_region %>% 
    filter(Ecoregion == region) %>% 
    create_barplot(slice_n = 30, plot_title = title)
})

# Save as PDF
if(FALSE){
  pdf(paste0(path, "Audrey_fun_facts/Abu_plots_by_ecoregion.pdf"), 
      width = 11, height = 8.5) # Landscape orientation
  gridExtra::marrangeGrob(
    grobs = Abu_by_region_plots, 
    nrow = 2, # Number of rows per page
    ncol = 1  # Number of columns per page
  )
  dev.off()
}

## Export Excel
# For each region, take the 40 most abundant species, then pivot wider so abundance is listed for each Ecoregion
Birds_region %>% slice_max(Count, n = 40, by = Ecoregion) %>%
  pivot_wider(id_cols = Species_ayerbe, 
              names_from = Ecoregion, 
              values_from = Count) %>% 
  mutate(Non_na_count = rowSums(!is.na(across(-Species_ayerbe)))) %>%
  arrange(desc(Non_na_count)) %>% 
  as.data.frame() %>%
  write.xlsx(paste0(path, "Excels/Abundance_by_ecoregion.xlsx"),
             row.names = FALSE, showNA = FALSE, append = TRUE)

# Export Excel for .py ------------------------------------------------------
# Create Excel to bring into Python (called 'data_list') & run through for loop in Python
Birds_comb2 %>%
  distinct(Id_gcs, Nombre_finca, Ecoregion) %>%
  mutate(Identifier = paste0(Nombre_finca, "_", "Id_gcs_", Id_gcs)) %>%
  as.data.frame() %>%
  write.xlsx(paste0(path, "Excels/Farm_names_IDs_test.xlsx"), row.names = FALSE)
stop()

# Export KML --------------------------------------------------------------
## KML file for extensionists to deliver the printed outreach documents

# This function allows us to change the output kml depending on the level of detail we hope to provide.. If we set var1 to Id_group it produces a single point per farm
create.extension.file <- function(df, var1, tbl = TRUE, output_name = NULL, output_type = NULL) {
  var1 <- ensym(var1)
  if(rlang::as_string(var1) == "Id_group"){
    df <- df %>% group_by(!!var1) %>% 
      mutate(Latitud = round(mean(Latitud, na.rm = TRUE), 3),
             Longitud_decimal = round(mean(Longitud_decimal, na.rm = TRUE), 3))
  }
  df <- df %>% 
    st_as_sf(coords = c("Longitud_decimal", "Latitud"), crs = 4326) %>%
    filter(Departamento == "Meta") %>%
    distinct(Nombre_finca, Id_gcs, Ano, !!var1, geometry) %>%
    group_by(Nombre_finca, Id_gcs, !!var1, geometry) %>%
    arrange(Ano) %>%
    mutate(Ano_num = paste0("Ano_", row_number())) %>%
    pivot_wider(names_from = Ano_num, values_from = Ano) %>% 
    ungroup() %>% 
    arrange(Nombre_finca)
  if(tbl == FALSE){
    if(output_type == "KML"){
      if(rlang::as_string(var1) == "Id_group"){ 
        df <- df %>% dplyr::rename(name = Nombre_finca)
      } else{
        df <- df %>% dplyr::rename(name = rlang::as_string(var1))
      }
    }
    if(output_type == "Excel"){
      df %>% as.data.frame() %>%
        select(-geometry) %>% 
        xlsx::write.xlsx(file = paste0('/Users/aaronskinner/Library/CloudStorage/OneDrive-UBC/Grad_School/PhD/Analysis/Colombia-SCR-Rd3/Derived/Excels/', output_name, ".xlsx"), row.names = F, showNA = FALSE)
    } else{
      st_write(df, driver='kml', dsn= paste0("/Users/aaronskinner/Library/CloudStorage/OneDrive-UBC/Grad_School/PhD/Analysis/Colombia-SCR-Rd3/Derived_geospatial/", output_name, ".kml"))
    }
  } else{
    df
  }
}

# Print tbl
create.extension.file(df = Birds_all3, var1 = Id_group, tbl = TRUE)

# Call function to create KML varying the grouping variable 
create.extension.file(df = Birds_all3, var1 = Id_group, tbl = FALSE,
                      output_name = "Farms_Meta_Outreach_Id_group", output_type = "KML")
create.extension.file(df = Birds_all3, var1 = Id_muestreo, tbl = FALSE,
                      output_name = "Farms_Meta_Outreach_Id_muestreo", output_type = "KML")

# Call function to create Excel 
create.extension.file(df = Birds_all3, var1 = Id_group, tbl = FALSE,
                      output_name = "Farms_Meta_Outreach_Id_group", output_type = "Excel")
