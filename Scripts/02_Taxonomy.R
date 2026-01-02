## PhD birds in silvopastoral landscapes##
# Data wrangling 00b -- Taxonomy
## This script generates a data frame of taxonomic equivalents using Suarez Castro et al 2024 (providing a link between SACC, BirdLife, and eBird) and an Excel with taxonomic equivalents between SACC 2018 - Avilist 2025 (provided by Denis Lepage). 
# Importantly, the final product (Tax_df_final) only contains species equivalents that are present in Colombia (instead of globally). 

# Contents
# 1) Og_name & Ayerbe -- Create dataframe with unique combinations of Og_name & Ayerbe and format. Remove 'sp' and '/' species
## Joins
# 2) Suarez Castro et al, 2024 -- Join with Suarez Castro et al, 2024, which links SACC taxonomy with BirdLife and eBird. 
# 3) SACC Avilist -- Use SACC 2018 and Avilist 2025 crosswalk (provided by Denis Lepage), also obtaining Avibase IDs
# 4) Note that Suarez Castro et al, 2024 does not include migratory species, so there are ~70 species that don't have a match from 
# 5) Use Avonet crosswalk file to link BirdLife & BirdTree taxonomies 
# 6) Save & export

# Libraries ---------------------------------------------------------------
library(janitor)
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

# Load data ---------------------------------------------------------------
## Bring in data 
# NOTE: This is just point counts
Bird_pcs_all_spp <- read_csv("Derived/Excels/Bird_pcs/Bird_pcs_all_spp.csv")
Site_covs <- read_csv("Derived/Excels/Site_covs.csv")

## Use the crosswalk file to create a data frame of true taxonomic equivalents (at least for BL & BT)
Crosswalk <- read.csv(
  "../Datasets_external/Avonet_Data/PhylogeneticData/BirdLife-BirdTree crosswalk.csv"
) %>% rename(Species_bl = Species1, Species_bt = Species3) %>%
  select(Species_bl, Species_bt) %>% # Match.type, Match.notes
  tibble()
# Taxonmy from SACC available in (Suarez Castro et al, 2024)
Tax_suarez <- read_csv(
  "../Datasets_external/Elev_ranges/Suarez_castro_AOH_birds_table_S3_V3.csv"
) %>% clean_names() %>% 
  select(1:3) %>% 
  rename_with(~c("Species_sacc", "Species_bl", "Species_eB")) %>% 
  mutate(Species_bl = if_else(
    Species_sacc == "Chlorostilbon melanorhynchus", "Chlorostilbon mellisugus", Species_bl)
  )

# Match between SACC and Avilist 
Sacc_avilist <- read.xlsx("../Datasets_external/sacc18 vs avilist Colombia.xlsx", sheetIndex = 1) %>% 
  as_tibble() 
Sacc_avilist2 <- Sacc_avilist %>% 
  select(starts_with("latin_name"), starts_with("common_name"), concept_id)

# Taxonomic / transcription changes ---------------------------------------
# Bring in taxonomic equivalents from manual review of species observed with Nick and data collectors (GAICA and CIPAV)
Tax_equivalents <- read_csv("Data/Taxonomic_changes.csv") %>% 
  select(starts_with("Species"), Departamentos_afectados)

# Match bird observations with taxonomic equivalents and create Species_ayerbe column
Bird_pcs_all_spp2 <- Bird_pcs_all_spp %>%
  left_join(Site_covs[, c("Id_muestreo_no_dc", "Departamento")]) %>%
  left_join(Tax_equivalents) %>% 
  mutate(Species_ayerbe = coalesce(Species_ayerbe, Species_original)) %>%
  # If the changes are restricted to certain departments AND the current Departamento is NOT in that list, then overwrite with Species_original. Otherwise keep Species_ayerbe
  mutate(Species_ayerbe = case_when(
    !is.na(Departamentos_afectados) & !str_detect(Departamentos_afectados, Departamento) ~ Species_original,
    .default = Species_ayerbe
  ))

# Og_name & Ayerbe ---------------------------------------------------------
# Data frame with distinct combinations of Og_name & Ayerbe.
Tax_OG_Ay <- Bird_pcs_all_spp2 %>% # Taxonomy Og_name + Ayerbe
  distinct(Species_original, Species_ayerbe) %>%
  rename(Og_name = Species_original)

# Create rm object of non-species names
rm <- map(Tax_OG_Ay, \(col){
  unique(col[str_detect(col, "Na | sp$|1|/|Desconocido|Ceratopipra o manacus|indeterminado|Sin identificar")])
})
rm$Og_name <- rm$Og_name[!is.na(rm$Og_name)]

# Remove non-species names from both Og_name & Species_ayerbe
Tax_OG_Ay2 <- Tax_OG_Ay %>%
  filter(!Og_name %in% rm$Og_name) %>%
  filter(!Species_ayerbe %in% rm$Species_ayerbe)

# Joins --------------------------------------------------------
# >SACC-Avilist ------------------------------------------------------------
# Join the species observed in the project with SACC Avilist 
Tax_df <- Tax_OG_Ay2 %>%
  mutate(latin_name_sacc = Species_ayerbe) %>%
  distinct(latin_name_sacc, Species_ayerbe) %>% 
  left_join(Sacc_avilist2) %>% 
  rename(concept_id_sacc = concept_id)

# >Find matches -----------------------------------------------------------
# 71 species didn't have a match
Missing_df <- Tax_df %>% filter(is.na(latin_name_avilist))
Missing_df 
Missing <- Missing_df %>% pull(latin_name_sacc)

# We need to find matches for these 70 species.. 
# Step 1) Join species that have the same scientific name in both sacc and avilist
Sacc <- Sacc_avilist2 %>% 
  select(contains("sacc"), concept_id) %>% 
  distinct() %>% 
  filter(!is.na(latin_name_sacc))
Avilist <- Sacc_avilist2 %>% select(contains("avilist"), concept_id) %>% 
  distinct() %>%
  filter(!is.na(latin_name_avilist))
Sacc_avilist3 <- Sacc %>% 
  left_join(Avilist, by = join_by("latin_name_sacc" == "latin_name_avilist"),
            keep = TRUE) %>%
  rename(concept_id_sacc = concept_id.x,
         concept_id_avilist = concept_id.y)
Spp_same_name <- Sacc_avilist3 %>% filter(latin_name_sacc %in% Missing) %>% 
  left_join(Missing_df[, c('latin_name_sacc', 'Species_ayerbe')])

# Remove Missing species and join 
Tax_df2 <- Tax_df %>% filter(!latin_name_sacc %in% Missing) %>% 
  bind_rows(Spp_same_name) 
# Still 13 species without a match, will have to fill these in manually 
Manual_adjust <- Tax_df2 %>% filter(is.na(latin_name_avilist))
Manual_adjust2 <- Tax_df %>% filter(is.na(concept_id_sacc))

# Pine warbler only species that wasn't in SACC, but confirmed it is in Ayerbe (2018) field guide as Setophaga pinus. 
# Megascops colombianus is a (sub)species of Megascops ingens, depending on taxonomy
Manual_adjust2
Manual_adjust_comb <- bind_rows(Manual_adjust, Manual_adjust2) %>% 
  # These two species incorrectly match to the wrong subspecies, ie prasinus = Northern emerald toucanet, cinereus = Southern tropical pewee, and grisea = Southern White-fringed Antwren (also in Colombia, but our observations were all in N Colombia)
  add_row(latin_name_sacc = c(
    "Aulacorhynchus prasinus", "Contopus cinereus", "Formicivora grisea")
    )
Manual_adjust_comb

# >Manual adjustments -----------------------------------------------------
## Export and fill out manually 
Manual_adjust_comb %>% 
  data.frame() #%>% 
#write.xlsx("Derived/Excels/Taxonomy/Tax_to_fill.xlsx", showNA = FALSE, row.names = FALSE)

## Bring back in and add to final taxonomy list
# NOTE: Xenops genibarbis is in Meta while Xenops mexicanus is in the other ecoregions we observed, thus Xenops minutus has two rows 
Tax_manual_match <- read.xlsx("Derived/Excels/Taxonomy/Tax_manual_match.xlsx", sheetIndex = 1)
Spp_rm <- Manual_adjust_comb %>% pull(latin_name_sacc)

# Add manual updates
Tax_df3 <- Tax_df2 %>% 
  filter(!latin_name_sacc %in% Spp_rm) %>% 
  bind_rows(Tax_manual_match) %>% 
  mutate(concept_id_avilist = coalesce(concept_id_avilist, concept_id_sacc))

# >BirdLife and eBird --------------------------------------------------
# Match 'latin_name_sacc' with Suarez Castro et al, 2024 to obtain BirdLife and eBird
Tax_df4 <- Tax_df3 %>% 
  left_join(Tax_suarez, by = join_by("latin_name_sacc" == "Species_sacc"))
# NOTE: Suarez Castro et al, 2024 does not have all species (e.g., it does not include migratory species)
Tax_df4 %>% filter(is.na(Species_bl)) 

## Use the Avonet crosswalk to fill in the missing species
# Create a tibble to join 
Tax_join <- Tax_df4 %>% select(-Species_eB) %>% 
  filter(is.na(Species_bl)) %>% 
  left_join(
    Crosswalk, by = join_by("latin_name_avilist" == "Species_bl"), keep = TRUE
  ) %>%
  mutate(Species_bl = coalesce(Species_bl.x, Species_bl.y)) %>%
  select(-c(Species_bl.x, Species_bl.y))

Tax_join2 <- Tax_join %>% 
  filter(is.na(Species_bl)) %>% 
  left_join(
    Crosswalk, by = join_by("latin_name_sacc" == "Species_bl"), keep = TRUE
  ) %>% 
  mutate(Species_bl = coalesce(Species_bl.x, Species_bl.y)) %>% 
  select(-c(ends_with(".x"), ends_with(".y"))) 

Tax_join3 <- Tax_join %>% filter(!is.na(Species_bl)) %>% 
  bind_rows(Tax_join2) 

# Manually add BirdLife species that didn't match from Avonet Crosswalk
Tax_join4 <- Tax_join3 %>% mutate(Species_bl = case_when(
  latin_name_sacc == "Lepidopyga goudoti" ~ "Amazilia goudoti", 
  latin_name_sacc == "Lepidopyga coeruleogularis" ~ "Amazilia coeruleogularis", 
  latin_name_sacc == "Hylocharis cyanus" ~ "Amazilia cyanus",
  latin_name_sacc == "Tachyphonus luctuosus" ~ "Islerothraupis luctuosa",
  latin_name_sacc == "Butorides virescens" ~ "Butorides striata",
  .default = Species_bl
)) %>% select(-c(Species_bt)) %>% 
  distinct()

# Remove species missing BirdLife taxonomy and then join with replacements
Tax_df5 <- Tax_df4 %>% filter(!is.na(Species_bl)) %>% 
  bind_rows(Tax_join4) 

# >BirdTree ---------------------------------------------------------------
## Add in BirdTree taxonomy
Tax_df6 <- Tax_df5 %>% 
  left_join(Crosswalk, by = "Species_bl",
            relationship = "many-to-many")
# Avonet is global, so the previous join added some BirdTree species that are not present in Colombia. For each latin_name_sacc with multiple BirdTree matches, manually determine which of the BirdTree species are found in Colombia
Tax_df6 %>% count(latin_name_sacc, sort = T) %>% 
  filter(n > 1)

# E.g. Myrmotherula fluminensis (Rio de janeiro antwren) is not found in Colombia, so it should not be in the Taxonomic equivalents dataframe
Tax_df6 %>% 
  filter(latin_name_sacc == "Butorides virescens") %>% # Myrmotherula axillaris
  select(latin_name_sacc, Species_bt)
# These species (in latin_name_sacc taxonomy) only have one BirdTree species present in Colombia. 
Spp_col <- c("Accipiter striatus", "Accipiter bicolor", "Falco peregrinus", "Cyanocorax yncas", "Falco peregrinus", "Geothlypis aequinoctialis", "Myrmotherula axillaris", "Trogon collaris")
# These species have an issue with reciprocality, where both pairs are in Colombia so they should be listed 2x (not 4x).The solution is the same as with Spp_col
Spp_recip <- c("Butorides striata", "Butorides virescens", "Chlorostilbon melanorhynchus", "Chlorostilbon mellisugus")
Spp_use <- c(Spp_col, Spp_recip)

# The following latin_name_sacc species correspond to multiple BirdTree species that are present in Colombia: Chlorostilbon mellisugus, Piranga flava, Setophaga petechia, Turdus albicollis, and Xenops minutus
# Use the Spp_use object to select the BirdTree species present in Colombia
Tax_df7 <- Tax_df6 %>%
  filter(
    !latin_name_sacc %in% Spp_use |
      Species_bt == latin_name_sacc
  ) %>% distinct()

# Reorder the columns
Tax_df_final <- Tax_df7 %>% 
  relocate(c(starts_with("concept_id"), starts_with("common_name")), 
           .after = Species_bt) %>%
  rename_with(~str_replace(.x, "latin_name", "Species")) %>% 
  rename(Species_sacc_18 = Species_sacc,
         Species_avilist_25 = Species_avilist)

# Examine -----------------------------------------------------------------
nrow(Tax_df_final) #594

# 4 species with multiple rows: "Piranga flava", "Setophaga petechia", "Turdus albicollis", "Xenops minutus" 
Tax_df_final %>% count(Species_sacc_18, sort = T)

# There are 72 species that don't have an eBird species name
df <- Tax_df_final %>% drop_na()
anti_join(Tax_df_final, df)

# All other columns have no NAs except for Setophaga pinus. Setophaga pinus is not present in SACC because it is very rarely recorded in South America and may not have had verifiable records that met SACC inclusion criteria at the time of the 2018 checklist
Tax_df_final %>% select(-Species_eB) %>%
  filter(if_any(everything(), is.na))

# Difference between Species_sacc_18 and Species_ayerbe is pine warbler
Tax_df_final %>% filter(Species_sacc_18 != Species_ayerbe | is.na(Species_sacc_18))

# Export ------------------------------------------------------------------
stop()

Tax_df_final %>% write_csv("Derived/Excels/Taxonomy/Taxonomy_all.csv")
Bird_pcs_all_spp2 %>% 
  filter(Species_ayerbe %in% Tax_df_final$Species_ayerbe) %>% 
  relocate(c(Species_ayerbe, Count), .after = Species_original) %>% 
  select(-c(contains("Departamento"), Species_original)) %>% 
  write_csv("Derived/Excels/Bird_pcs/Bird_pcs_all.csv")

# EXTRAS ------------------------------------------------------------------
# >Names Ayerbe 2019 ------------------------------------------------------
# Ayerbe 2019 shapefiles, using the American Ornithologists’ Society–South American Classiﬁcation Committee (AOS–SACC) taxonomy
# Get list of all 1890 spp. contained within Ayerbe shapefiles
Ayerbe_all_spp <- list.files(path = "../Geospatial_data/Ayerbe_shapefiles_1890spp", pattern = "\\.dbf$")
Ayerbe_all_spp <- substr(Ayerbe_all_spp, 1, nchar(Ayerbe_all_spp) - 4) # Remove the .dbf extension

# CHECK:: any individual species that we're having difficulty with 
TF <- str_detect(Ayerbe_all_spp, "Machaeropterus") #"tyrannina"
Ayerbe_all_spp[TF]

Spp_obs_sacc <- Tax_df_final %>% pull(Species_ayerbe) %>% 
  unique()
TF <- Spp_obs_sacc %in% Ayerbe_all_spp
Spp_obs_sacc[!TF]
