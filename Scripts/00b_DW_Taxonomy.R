## PhD birds in silvopastoral landscapes##
# Data wrangling 00b -- Taxonomy
## This script generates the data frame of taxonomic equivalents (Tax_df)

# Contents
# 1) Og_name & Ayerbe -- Create dataframe with unique combinations of Og_name & Ayerbe and format
# 2) Ayerbe 2019 shapefiles -- Get list of all 1890 spp. contained within Ayerbe shapefiles & bring in spatial files for the subset of species that were detected in our data collection
# Taxonomy
# 3) GBIF -- Use taxize to pull in alternative GBIF names, including order & family
# 4) Avonet -- Bring in & format the 3 Avonet files ("BirdLife", "eBird", "BirdTree")
# 5) Tax_df -- Use Avonet's crosswalk file to link the BirdLife & BirdTree taxonomies to create a data frame of taxonomic equivalents
# 6) Save & export

# NOTES:: Remember that Ecotropico & I already went through the process of updating the species names to Ayerbe 2018.
## Notes: Certain species were actually in the original Ayerbe files but named differently.. I updated these manually. I never received clarification why some of these have "_M" or "_1" at the end of the file names.
# "Accipiter bicolor" (didn't have all files), "Crypturellus soui_1", "Parkesia noveboracensis_M" Picumnus olivaceus_M",  "Scytalopus latrans_M", "Nyctibius_grandis"

# Libraries ---------------------------------------------------------------
load("Rdata/the_basics_09.15.25.Rdata")
source("/Users/aaronskinner/Library/CloudStorage/OneDrive-UBC/Grad_School/Rcookbook/Themes_funs.R")

library(tidyverse)
library(stringi)
library(naniar)
library(taxize)
library(readxl)
library(sf)
library(xlsx)
library(cowplot)
library(conflicted)
library(gtools)
ggplot2::theme_set(theme_cowplot())
conflicts_prefer(dplyr::select)
conflicts_prefer(dplyr::filter)

# Og_name & Ayerbe ---------------------------------------------------------
# Data frame with distinct combinations of Og_name & Ayerbe.
Tax_OG_Ay <- Birds_all3 %>% # Taxonomy Og_name + Ayerbe
  distinct(Nombre_cientifico_original, Species_ayerbe) %>%
  rename(Og_name = Nombre_cientifico_original)

# Create rm object of non-species names
rm <- map(Tax_OG_Ay, \(col){
  unique(col[str_detect(col, "Na | sp$|1|/|Desconocido|Ceratopipra o manacus|indeterminado|Sin identificar")])
})
rm$Og_name <- rm$Og_name[!is.na(rm$Og_name)]

# Remove non-species names from both Og_name & Species_ayerbe
Tax_OG_Ay2 <- Tax_OG_Ay %>%
  filter(!Og_name %in% rm$Og_name) %>%
  filter(!Species_ayerbe %in% rm$Species_ayerbe)
sciNames <- unique(Tax_OG_Ay2$Species_ayerbe)
length(sciNames) # 592 vs 617

# Ayerbe 2019 shapefiles ---------------------------------------
# Ayerbe 2019 shapefiles, using the American Ornithologists’ Society–South American Classiﬁcation Committee (AOS–SACC) taxonomy
# Get list of all 1890 spp. contained within Ayerbe shapefiles
Ayerbe_all_spp <- list.files(path = "../Geospatial_data/Ayerbe_shapefiles_1890spp", pattern = "\\.dbf$")
Ayerbe_all_spp <- substr(Ayerbe_all_spp, 1, nchar(Ayerbe_all_spp) - 4) # Remove the .dbf extension

# CHECK:: any individual species that we're having difficulty with 
TF <- str_detect(Ayerbe_all_spp, "hemichrysus") #"tyrannina"
Ayerbe_all_spp[TF]

# CHECK:: Species that are listed in SCR databases and are not in Ayerbe's files.
TF <- !sciNames %in% Ayerbe_all_spp
sciNames[TF]

# Read in shapefiles & store in list
Ayerbe_sf_l <- list()
for (i in 1:length(sciNames)) {
  tryCatch(
    {
      Ayerbe_sf_l[[i]] <- st_read(dsn = "../Geospatial_data/Ayerbe_shapefiles_1890spp", layer = sciNames[i])
    },
    error = function(e) {}
  )
}
names(Ayerbe_sf_l) <- sciNames
Ayerbe_sf <- do.call(rbind, Ayerbe_sf_l)
names(Ayerbe_sf)[1] <- "Species_ayerbe"

## NOTE:: Certain species were actually in the original Ayerbe files but named differently.. I updated these manually. I never received clarification why some of these have "_M" or "_1" at the end of the file names.
# "Accipiter bicolor" (didn't have all files), "Crypturellus soui_1", "Parkesia noveboracensis_M" Picumnus olivaceus_M",  "Scytalopus latrans_M", "Nyctibius_grandis"
# Remove the letters and numbers after the "_" in species names
Ayerbe_sf$Species_ayerbe <- sapply(str_split(Ayerbe_sf$Species_ayerbe, "_"), function(x) {
  x[1]
})

# Add in the original name (Og_name) before updating taxonomy
Ayerbe_sf2 <- Ayerbe_sf %>%
  left_join(Tax_OG_Ay2, by = "Species_ayerbe")

# Taxonomy ---------------------------------------------------
# >GBIF ------------------------------------------------
# Use Gbif to pull in additional synonyms, and to standardize the family, order, and species. GBIF is the only source that has information for all input species

if(FALSE){ # Very slow
  gbif_list <- get_gbifid_(sciNames, method = "backbone", rows = 1)
  
  gbif_names <- bind_rows(gbif_list, .id = "input_name")
  # Importantly, notice that gbif provides something for every single input
  nrow(gbif_names)
  
  # Update autoria column
  gbif_names$autoria <- str_remove(gbif_names$scientificname, pattern = gbif_names$canonicalname)
  
  gbif_names %>% mutate(across(phylum:genus, ifelse(is.na, fill, .)))
  
  # Fill in information for Ortalis guttata
  gbif_names <- gbif_names %>%
    mutate(across(phylum:species, 
                  ~ if_else(str_detect(input_name, "Ortalis") & is.na(.),
                            case_when(
                              cur_column() == "phylum" ~ "Chordata",
                              cur_column() == "order"  ~ "Galliformes",
                              cur_column() == "family" ~ "Cracidae",
                              cur_column() == "genus"  ~ "Ortalis",
                              cur_column() == "species"  ~ "Ortalis guttata",
                              TRUE ~ .
                            ),
                            .)))
  
  #write_csv(gbif_names, file = "../Taxonomy/gbif_names.csv")
}

# Bring in gbif data frame to avoid slow get_gbifid_() 
gbif_names <- read_csv("../Taxonomy/gbif_names.csv") %>% 
  filter(!is.na(order)) 

# >Avonet list -----------------------------------
# See metadata tab in Excel for information on what each column contains
# Bring in here as this is used to create Tax_df
Traits_path <- "../Datasets_external/Avonet_Data/TraitData/"
filesAvo <- list.files(path = Traits_path, pattern = ".xlsx")
sheetsAvo <- str_split_i(filesAvo, ".x", 1)
dfsAvo <- list() # dfs Ecotropico
for (i in 1:length(filesAvo)) {
  print(i)
  dfsAvo[[i]] <- read_excel(
    path = paste0(Traits_path, filesAvo[i]), sheet = sheetsAvo[i]
    ) #%>% 
    #clean_names()
}
names(dfsAvo) <- c("BirdLife", "eBird", "BirdTree")

# Remove the 1 2 & 3 for easy removal of irrelevant columns
names <- lapply(dfsAvo, function(x) {
  str_remove(names(x), "1|2|3")
})
for (i in 1:length(dfsAvo)) {
  names(dfsAvo[[i]]) <- names[[i]]
}
# Remove irrelevant columns
Avo_traits_l <- lapply(dfsAvo, function(x) {
  select(x, -c(Family, Order, Female, Male, Unknown, Mass.Source, Mass.Refs.Other, Inference, Traits.inferred, Reference.species))
}) # Avonet morphology list

# >Tax_df -------------------------------------------------------------------
## Use the crosswalk file to create a data frame of true taxonomic equivalents (at least for BL & BT)
Crosswalk <- read.csv("../Datasets_external/Avonet_Data/PhylogeneticData/BirdLife-BirdTree crosswalk.csv") %>%
  rename(Species_bl = Species1, Species_bt = Species3) %>%
  select(1:4)
unique(Crosswalk$Match.notes) # Very few notes 
HBW <- data.frame(read_excel("/Users/aaronskinner/Library/CloudStorage/OneDrive-UBC/Grad_School/PhD/Analysis/Datasets_external/Handbook of the Birds of the World and BirdLife International Digital Checklist of the Birds of the World_Version_7.xlsx", sheet = "HBW-BirdLife v7 ", skip = 3)) %>%
  filter(!is.na(Common.name)) %>% 
  rename(Common_name = Common.name)
head(HBW)

Tax_df <- Tax_OG_Ay2 %>%
  distinct(Species_ayerbe) %>% 
  left_join(gbif_names[, c("input_name", "order", "family", "species", "autoria")],
    by = join_by("Species_ayerbe" == "input_name")
  ) %>%
  left_join(Crosswalk, join_by("Species_ayerbe" == "Species_bt")) %>%
  ## Manual changes (using Avibase & Crosswalk file) of species that don't match w/ BL, BT, or eBird
  mutate(Species_bl = case_when(
    Species_ayerbe == "Thectocercus acuticaudatus" ~ "Psittacara acuticaudatus",
    Species_ayerbe == "Ixothraupis guttata" ~ "Tangara guttata",
    TRUE ~ Species_bl
  )) %>%
  left_join(Crosswalk, join_by("Species_ayerbe" == "Species_bl")) %>%
  left_join(dfsAvo$eBird[, c("Species")], by = join_by("Species_ayerbe" == "Species"), keep = T) %>%
  mutate(
    Match.type = coalesce(Match.type.x, Match.type.y),
    Match.notes = coalesce(Match.notes.x, Match.notes.y)
  ) %>%
  select(-ends_with(c(".x", ".y"))) %>%
  rename_with(.fn = ~ paste0(., "_gbif"), .cols = 2:5) %>%
  rename(Species_ayerbe = Species_ayerbe, Species_eb = Species) %>%
  select(2, 3, 1, 4, 6:10, 5)
nrow(Tax_df)
head(Tax_df)

# Note there are still some species that have NAs in either BL or BT b/c it was always joining "Species_ayerbe" with something. Let's fill in these missing species names by matching back up with the Crosswalk df and creating the 'Missing_' data frames.
Missing_BL <- Tax_df %>%
  select(-c(Match.type, Match.notes)) %>%
  filter(is.na(Species_bl) & !is.na(Species_bt)) %>%
  select(-Species_bl) %>%
  left_join(Crosswalk, join_by("Species_bt" == "Species_bt"))

Missing_BT <- Tax_df %>%
  select(-c(Match.type, Match.notes)) %>%
  filter(!is.na(Species_bl) & is.na(Species_bt)) %>%
  select(-Species_bt) %>%
  left_join(Crosswalk, join_by("Species_bl" == "Species_bl"))

Missing <- rbind(Missing_BT, Missing_BL)
head(Missing)

# Create a data frame containing the rows with NAs that will be replaced
anti_j <- Tax_df %>% filter(!is.na(Species_bl) & is.na(Species_bt) | is.na(Species_bl) & !is.na(Species_bt))

# Update the Tax_df dataframe by removing rows present in anti_j and appending Missing data
Tax_df2 <- Tax_df %>%
  anti_join(anti_j) %>%
  bind_rows(Missing) %>%
  # Add in common names from the HBW
  left_join(
    HBW[, c("Common_name", "Scientific.name")],
    join_by("Species_bl" == "Scientific.name")
  ) %>% Cap_snake()

rownames(Tax_df2) <- NULL
nrow(Tax_df2)

# NOTE:there are no NAs in the gbif columns order or family, so it makes sense to use these for repeatability and so all are from a single source
Tax_df2 %>%
  select(Species_ayerbe, ends_with("gbif"), -Autoria_gbif) %>%
  #naniar::where_na()
  drop_na() %>%
  nrow()
# 2 NAs in species_gbif
Tax_df2 %>% filter(Species_ayerbe == "Tangara cyanoptera") 
# NOTE: Same with BT & BL
Tax_df2 %>% filter(is.na(Species_bt) | is.na(Species_bl))

# Save & export -----------------------------------------------------------
stop()
## Export relevant files to Excel
#Tax_df2 %>% write.xlsx("../Taxonomy/Taxonomy.xlsx", sheetName = "Taxonomic_equivalents", row.names = F, showNA = FALSE)

# All species observed in the project, irrespective of methodology
Tax_df2 %>% write_csv(file = "Derived/Excels/Taxonomy_all.csv")

## For data paper only export species taxonomy that were observed during point counts
Spp_pc <- Bird_pcs %>% 
  pull(Species_ayerbe) %>% 
  unique()

# Subset just point count species, final formatting
Tax_pcs <- Tax_df2 %>% 
  filter(Species_ayerbe %in% Spp_pc) %>%
  select(-c(Match_notes)) %>% 
  distinct() 

# Export Taxonomy as csv 
Tax_pcs %>% select(-c(Common_name)) %>% 
  write_csv(file = "Derived/Excels/Taxonomy.csv")

rm(list = ls()[!(ls() %in% c("Ayerbe_all_spp", "Ayerbe_sf2", "Tax_df2", "Tax_pcs", "Avo_traits_l"))])
#save.image(paste0("Rdata/Taxonomy_", format(Sys.Date(), "%m.%d.%y"), ".Rdata"))
# Manual
#save.image("Rdata/Taxonomy_09.15.25.Rdata")

# EXTRAS -----------------------------------------------------------
# These 'EXTRAS' are no longer critical given that the 40 species' names that had no match according to Ayerbe's taxonomy were updated manually and incorporated into the primary workflow. I left this code as these were previously important steps in the workflow. The three components are
# 1) Yellow warbler example -- Example showing why Ayerbe 2018 is best choice at the moment
# 2) Manual taxonomic update -- GAICA & CIPAV manually corrected certain species
# 3) Synonyms -- Finding synonyms for the species reported by data collectors that had no matches in the Ayerbe-Quinones (2018) shapefiles,
# 4) Ecotropico Extras -- Providing updated taxonomy in the format that Ecotropico requested
# 5) Order & Family -- If I remember correctly, there was an issue with the orders and families being scrambled relative to the species names?

# >YEWA example -----------------------------------------------------------
## I recommend using Ayerbe (2018) as final bird species list as these have been vetted and all species accounted for / clarified. Other taxonomies may have other types of errors, for example the Yellow warbler. See Final report (6.30.23) to TNC for more information
## EXAMPLE: Yellow warbler & locations
Birds_all3 %>%
  filter(Nombre_cientifico_original == "Setophaga aestiva" | Nombre_cientifico_original == "Setophaga petechia") %>%
  select(Nombre_cientifico_original, Species_ayerbe, Departamento) %>%
  arrange(Nombre_cientifico_original) %>%
  rename(Species_original = Nombre_cientifico_original) %>%
  distinct() # %>% write.xlsx("Yellow_warbler_example.xlsx")


# >Manual taxonomic update -------------------------------------------------
## VISUALIZE the 40 changes when you make the taxonomic updates
# Bring in GAICA & CIPAV's updated taxonomy files
Gaica_Tax <- read_excel("/Users/aaronskinner/Library/CloudStorage/OneDrive-UBC/Grad_School/PhD/Analysis/Taxonomy/Ayerbe2019_GAICA_Response.xlsx")
CIPAV_Tax <- read_excel("/Users/aaronskinner/Library/CloudStorage/OneDrive-UBC/Grad_School/PhD/Analysis/Taxonomy/Ayerbe_missing_CIPAV_Response.xls")
TaxUpManG <- select(Tax_OG_Ay2, Og_name) %>% # Manual taxonomic update GAICA
  inner_join(
    y = Gaica_Tax[, c("Nombre_GAICA", "Species_ayerbe")],
    join_by("Og_name" == "Nombre_GAICA")
  ) %>%
  mutate(Source = "GAICA")
TaxUpManC <- select(Tax_OG_Ay2, Og_name) %>% # Manual taxonomic update CIPAV
  inner_join(
    CIPAV_Tax[, c("Nombre_CIPAV", "Species_ayerbe")],
    join_by("Og_name" == "Nombre_CIPAV")
  ) %>%
  mutate(Source = "CIPAV")
TaxUpMan <- bind_rows(TaxUpManG, TaxUpManC)

# Append to the Taxonomy Excel
# TaxUpMan %>% rename(Species_original = Og_name,	Species_ayerbe = Species_ayerbe) %>%
# as.data.frame() %>%
# write.xlsx("../Taxonomy/Taxonomy.xlsx", sheetName = "Updated_spp_names", row.names = F, append = T)

# When you updated the taxonomy some species are a one to one switch, while others end up with multiple rows b/c they had multiple Og_names for the one new name. So that is why there are only 22 additional rows when you do distinct() on Birds_all (21 rows where count = 2, + 1 additional for Spinus psaltria where count = 3).
Tax_rows <- Tax_OG_Ay %>%
  count(Species_ayerbe, sort = T) %>%
  filter(Species_ayerbe %in% TaxUpMan$Species_ayerbe)
Tax_OG_Ay %>% filter(Species_ayerbe == "Spinus psaltria")
## Add this info as sheet to the Excel
# Tax_rows %>% mutate(Number_of_rows_in_final_db = 1) %>%
# rename(Species_ayerbe = Species_ayerbe, Number_of_rows_in_original_db = n) %>%
# write.xlsx("../Taxonomy/Taxonomy.xlsx", sheetName = "Accounting_spp_changes", row.names = F, append = T)


# >Find synonyms ---------------------------------------------------------
## I spent substantial time trying to find alternative scientific names (i.e., synonyms) for the species that didn't have a match in the Ayerbe (2018) shapefiles. I was able to find ~13 of the 40 missing species using taxize to produce other synonyms.
# Overall, gnr_resolve does very poor job making changes, even across all of these different data bases very few spp. are changed. I spent substantial time playing around with this function so I'll leave it here as I think it has potential, but was not very relevant for me in the end

# vignette(package = taxize) #None, but can access online here: https://mran.microsoft.com/snapshot/2015-10-23/web/packages/taxize/vignettes/taxize_vignette.html

# Get list of spp. ranges still missing
TFa <- Tax_OG_Ay2$Og_name %in% Ayerbe_sf$Species_ayerbe
table(TFa) # 3 spp. not found in Ayerbe
Ayerb_miss <- sciNames[!TFa]

View(gnr_datasources()) # Can see sources and when they were updated
# Try data sources 175 (BirdLife), 185 (IOC), 187 (ebird), 188 (AOS), 189 (H&M BOW)
# AOS should have been helpful as Ayerbe uses (AOS–SACC) taxonomy
# updated.names <- gnr_resolve(sci = sciNames, preferred_data_sources = c(188, 175, 185, 187), best_match_only = F, canonical = T) #fields = "all"

# Try synonyms to get additional names to try.. I tried all db and "itis" was only one that was helpful
syn.df.itis <- synonyms(sci_id = Ayerb_miss, db = "itis", rows = 1, fields = "all")
syn.df.itis2 <- syn.df.itis[!is.na(syn.df.itis)]
syn.df.itis2 <- bind_rows(syn.df.itis2, .id = "submitted_name")

# Combine more options from synonyms() and the gbif changes
synon1 <- data.frame(cbind(rep(syn.df.itis2$submitted_name, 2), c(syn.df.itis2$syn_name, syn.df.itis2$acc_name)))
names(synon1) <- c("Og_name", "synonym")
synon2 <- rbind(synon1, gbif_changes) %>% distinct()

## Lets look at the missing spp. and see if they are hiding somewhere in the Ayerbe files.. Make these 4 changes
TFfn <- str_detect(Ayerbe_all_spp, regex(str_c(Ayerb_miss, collapse = "|"), ignore_case = TRUE)) # fn = full name
changeNames <- Ayerbe_all_spp[TFfn] # Originally was 5 additional spp. are there
setwd("/Users/aaronskinner/Library/CloudStorage/OneDrive-UBC/Grad_School/PhD/Analysis/Ayerbe_shapefiles_1890spp")
st_read(dsn = getwd(), layer = "Accipiter bicolor") # Missing .sbn and .sbx files
corrNames <- sapply(str_split(changeNames, "_"), function(x) {
  x[1]
})
synon3 <- rbind(synon2, data.frame(synonym = changeNames, Og_name = corrNames))

Ayerbe_sf2 <- list()
for (i in 1:nrow(synon3)) {
  tryCatch(
    {
      Ayerbe_sf2[[i]] <- st_read(dsn = getwd(), layer = synon3[i, 2])
    },
    error = function(e) {}
  )
}
setwd("..")


Ayerbe_sf2 <- do.call(rbind, Ayerbe_sf2) # 3 additional spp. found
names(Ayerbe_sf2)[1] <- "Species_ayerbe"
Ayerbe_sf2 <- merge(Ayerbe_sf2, synon3, by.x = "Species_ayerbe", by.y = "synonym")
Ayerbe_sf2 <- Ayerbe_sf2 %>% filter(synonym != "Ortalis guttata") # Ortalis columbiana is a separate species w/ a different range, so don't make this change
TFa2 <- Ayerbe_sf2$synonym %in% sciNames ## Notice 5 spp. were already in original data base (i.e. the same spp. with multiple scientific names)
Ayerbe_sf2 <- rbind(Ayerbe_sf, Ayerbe_sf2)
TF2 <- sciNames %in% Ayerbe_sf2$Species_ayerbe
Ayerb_miss2 <- sciNames[!TF2]

# Create a list of all spp. that have either same genus or species as the missing species. These could be good species to look into as they may be close relatives
missGenSp <- data.frame(str_split_fixed(Ayerb_miss3, " ", n = 2))
missGenSp <- c(missGenSp$X1, missGenSp$X2)
TFgs <- str_detect(Ayerbe_all_spp, str_c(missGenSp, collapse = "|")) # gs = genus species
Ayerbe_all_spp[TFgs]

## Add updated taxonomy columns and merge w/ data frame for Ecotropico & export#
sppChanged <- data.frame(Ayerbe_sf2[!TFa2, ])[, 1:2] # List of spp. that were changed
genSp <- data.frame(str_split_fixed(sppChanged$synonym, " ", n = 2)) # genus species
sppChanged2 <- cbind(sppChanged, genSp) %>% rename(genusUp = X1, sppUp = X2) # Updated
dfEcoUp <- merge(dfEco, sppChanged2[2:4], by = "Og_name", all.x = T)
dfEcoUp %>% filter(!is.na(genusUp)) # CHECK::Confirm that it worked

## Export table of updated & or missing spp. for GAICA / CIPAV  to check##
Ayerb_miss3 <- str_replace(Ayerb_miss2, c("Crypturellus soui|Parkesia noveboracensis|Picumnus olivaceus|Scytalopus latrans"), replacement = NA_character_)
Ayerb_miss3 <- sort(Ayerb_miss3[!is.na(Ayerb_miss3)])
Ayerb_miss3 <- data.frame(Nombre_CIPAV = Ayerb_miss3, Species_ayerbe = rep(NA, length(Ayerb_miss3)))
Ayerb_miss3 <- rbind(Ayerb_miss3, data.frame(Nombre_CIPAV = Ayerbe_sf2$Og_name, Species_ayerbe = Ayerbe_sf2$synonym))
View(TaxUpMan)

# Update to include the source data frame where records come from
missingSource <- dfEco %>%
  filter(dfEco$Og_name %in% Ayerb_miss3$Nombre_CIPAV) %>%
  group_by(Og_name, Source) %>%
  summarize() %>%
  rename(Nombre_CIPAV = Og_name) %>%
  mutate(Present = "Y") %>%
  pivot_wider(names_from = Source, values_from = Present, values_fill = "N")

Ayerb_miss3_Source <- merge(Ayerb_miss3, missingSource, by = "Nombre_CIPAV") %>% arrange(!is.na(Species_ayerbe))
Ayerb_miss3_Source <- Ayerb_miss3_Source %>% filter(Nombre_CIPAV != "Machaeropterus regulus" & Nombre_CIPAV != "Leptotila verreauxi") # Seems like leptotila verreauxi isn't in the Ayerbe data base
Ayerb_miss3_Source %>%
  filter(GaicaPCs != "Y" & Distancia != "Y") %>%
  select(Nombre_GAICA)
# write.csv(Ayerb_miss3_Source, "Ayerbe_missing_for_CIPAV.csv", row.names = F)
getwd()

# Generate possible synonyms where gbif pulled an alternative name from the input name
TFgbif <- gbif_names$input_name == gbif_names$species
table(TFgbif) # 38 alternative possibilities
gbif_changes <- gbif_names[!TFgbif, c("input_name", "species")] %>% filter_all(any_vars(!is.na(.)))
names(gbif_changes) <- c("Og_name", "synonym")

# >Ecotropico Extras -----------------------------------------------------
## Generate updated Excel for Ecotropico w/ OF matched, DELETE once confirmed everything is OK
AAS <- read_excel("/Users/aaronskinner/Library/CloudStorage/OneDrive-UBC/Grad_School/PhD/SCR_data/AVES_Updated5.11.23/AVES_UBC_MONITOREO-BD_V11.xlsx", sheet = "Base_de_datos")

AAS$Especie <- paste(AAS$Genero, AAS$Epiteto_especifico)

AAS_exp <- AAS %>% left_join(distinct(Tax_df2[, c("Species_ayerbe", "order_gbif", "family_gbif", "autoria_gbif")]), by = join_by("Especie" == "Species_ayerbe"))
write.xlsx(data.frame(AAS_exp), "/Users/aaronskinner/Library/CloudStorage/OneDrive-UBC/Grad_School/PhD/Analysis/Taxonomy/Taxonomia_Ecotropico/AAS_match_OF.xlsx", sheetName = "Base_de_datos", row.names = F)

AAS %>% filter(Especie == "Chaetura brachSiura")

anti_join(AAS, AAS_exp, by = "Especie") %>%
  select(Especie) %>%
  distinct() %>%
  arrange(Especie) %>%
  pull(Especie)
## Delete through here


## Provide the updated taxonomy in the format that Ecotropico requested
setwd("Taxonomy/Taxonomia_Ecotropico/Updated")
getwd()
# setwd('..')
Uniq_dbs <- unique(Birds_all$Uniq_db)
i <- 1
for (i in 1:length(Uniq_dbs)) {
  df <- Birds_all %>% filter(Uniq_db == Uniq_dbs[i])
  df <- df %>% distinct(Departamento, Municipio, Og_name, Species_ayerbe)
  df <- df %>% left_join(Tax_df2[, c("Species_original", "order_gbif", "family_gbif", "autoria_gbif", "Changed_Spp", "Changed_OF")], by = join_by("Og_name" == "Species_original"))
  df <- df %>% rename(Species_original = Og_name, Species_ayerbe = Species_ayerbe)
  if (i == 1) {
    write.xlsx(df, file = "Taxonomy_Updates_Uniq_db2.xlsx", sheetName = Uniq_dbs[i], row.names = F)
  }
  if (i %in% 2:length(Uniq_dbs)) {
    write.xlsx(df, file = "Taxonomy_Updates_Uniq_db2.xlsx", sheetName = Uniq_dbs[i], append = T, row.names = F)
  }
}
setwd("./../../..")


# >Order & Family --------------------------------------------------------
## Examine order and family and determine differences between original dataset and gbif
nrow(gbif_names)
head(gbif_names)
Birds_all_OF <- Birds_all %>%
  select(Uniq_db, Nombre_institucion, Species_ayerbe, Orden, Familia) %>%
  left_join(
    y = gbif_names[, c("input_name", "order", "family")],
    by = join_by("Species_ayerbe" == "input_name")
  ) %>%
  rename(order_gbif = order, family_gbif = family) # OF = Orden familia
distinctOF <- Birds_all_OF %>%
  filter(!is.na(Orden)) %>%
  distinct(Species_ayerbe, Orden, Familia, order_gbif, family_gbif)
# These species have different orders / families in gbif relative to the original data collectors
ProbOrdFamNames <- distinctOF %>%
  count(Species_ayerbe, sort = T) %>%
  filter(n > 1) %>%
  pull(Species_ayerbe)
Tax_df2 <- Tax_df2 %>% mutate(
  Changed_Spp = ifelse(Species_original == Species_ayerbe, "N", "Y"),
  Changed_OF = ifelse(Species_ayerbe %in% ProbOrdFamNames, "Y", "N")
)

# Show the species where order / family differ between gbif & the original dataset
head(Birds_all_OF)
Birds_all_OF %>%
  filter(Species_ayerbe %in% ProbOrdFamNames & Nombre_institucion != "UBC") %>%
  distinct(Uniq_db, Nombre_institucion, Species_ayerbe, Orden, Familia, order_gbif, family_gbif) %>%
  arrange(Species_ayerbe) %>%
  filter(Familia != family_gbif | Orden != order_gbif)

unique(Birds_all$Og_name) %in% gbif_names$input_name
table(test$Familia == test$family)

# Bring in GAICA & CIPAV's updated taxonomy file to change these names!
Gaica_Tax <- read_excel("/Users/aaronskinner/Library/CloudStorage/OneDrive-UBC/Grad_School/PhD/Analysis/Taxonomy/Ayerbe2019_GAICA_Response.xlsx") %>%
  mutate(Source = "GAICA") %>%
  rename(Og_name = Nombre_GAICA)
CIPAV_Tax <- read_excel("/Users/aaronskinner/Library/CloudStorage/OneDrive-UBC/Grad_School/PhD/Analysis/Taxonomy/Ayerbe_missing_CIPAV_Response.xls") %>%
  mutate(Source = "CIPAV") %>%
  rename(Og_name = Nombre_CIPAV)

TaxUpMan <- bind_rows(Gaica_Tax, CIPAV_Tax) %>%
  select(Og_name, Species_ayerbe, Source)
