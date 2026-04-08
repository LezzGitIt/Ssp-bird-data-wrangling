## PhD birds in silvopastoral landscapes##
## Data wrangling 00c -- Functional traits (FT) & elevational ranges
## This script generates the outputs related to functional traits (Avo_traits_final), & elevational ranges (Elev_ranges) that will be used in future scripts 

## Contents
# 1) Functional traits database -- Use Tax_df & the Avonet files to create FT database using a for loop & the Match_type (e.g., "1BL to 1BT") column to ensure that species are matched appropriately with their FT. Each species has a single row. 
# 2) Elevational ranges -- Use 3 databases to pull elevational ranges for each species 
# 3) Join dfs -- Join the dfs to create a single df (Elev_ranges) where each species has a single row with relevant elevational information, including the elevational range (a functional trait) for each species
# 4) Understand Elevational ranges -- Additional information regarding the elevational range information
# 5) Add elev range to FT df
# 6) Save & export 

# Libraries ---------------------------------------------------------------
# Load libraries
library(readxl)
library(tidyverse)
library(naniar)
library(janitor)
library(sf)
library(chron)
library(ggpubr)
library(cowplot)
library(conflicted)
library(stringi)
ggplot2::theme_set(theme_cowplot())
conflicts_prefer(dplyr::select)
conflicts_prefer(dplyr::filter)

source("/Users/aaronskinner/Library/CloudStorage/OneDrive-UBC/Grad_School/Rcookbook/Themes_funs.R")

## Load data and custom functions
Bird_pcs_all <-  read_csv(file = "Derived/Excels/Bird_pcs/Bird_pcs_all.csv")
Tax_df <- read_csv("Derived/Excels/Taxonomy/Taxonomy.csv") #%>% 
#mutate(Avibase.ID = str_sub(concept_id_avilist, 1, 8)) # Match with Avonet

# Avonet list  ------------------------------------------------------------
# See metadata tab in Excel (.xlsx) for information on what each column contains
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

Avo_traits_l$BirdLife <- Avo_traits_l$BirdLife %>% 
  mutate(Avibase.ID = str_remove(Avibase.ID, "AVIBASE-"))

# Match with Avonet
Ft_df <- Tax_df %>% 
  distinct(Species_ayerbe, Species_bl, Species_avilist_25) %>%
  left_join(Avo_traits_l$BirdLife, by = join_by("Species_bl" == "Species")) 
# A single species from each species SACC
Ft_df %>% count(Species_ayerbe, sort = T)

# Format & remove extraneous columns
names(Ft_df)
Ft_df2 <- Ft_df %>%
  select(-c(Sequence, Avibase.ID)) %>%
  mutate(
    Migration = case_when(
      Migration == 1 ~ "Sedentary",
      Migration == 2 ~ "Partial",
      Migration == 3 ~ "Long_distance"
    ),
    Habitat.Density = case_when(
      Habitat.Density == 1 ~ "Dense",
      Habitat.Density == 2 ~ "Semi_open",
      Habitat.Density == 3 ~ "Open"
    ),
    Habitat.Density = as.factor(Habitat.Density),
    Migration = as.factor(Migration),
    across(c(ends_with("tude"), "Range.Size"), as.numeric)
  ) %>%
  # Create habitat type, forest vs non-forest
  mutate(Forest_bin = if_else(
    Habitat %in% c("Forest", "Woodland", "Riverine"), "Forest", "Non-forest"
  )) %>%
  mutate(across(where(is.numeric), \(x) round(x, 2))) 

# Examine a few key traits
lapply(Ft_df2[17:21], table)
# Woodland (= medium stature tree-dominated habitats, including Acacia woodland, riparian woodlands, mangrove forests, forest edges, also more open parkland with scattered taller trees);
# Forest (= tall tree-dominated vegetation with more or less closed canopy, including palm forest)

# Birdbase ----------------------------------------------------------------

Birdbase <- read_excel("../Datasets_external/BIRDBASE v2025.1 Sekercioglu et al. Final.xlsx", skip = 1) %>% clean_names()
Birdbase2 <- Birdbase %>% 
  select(avi_list_v1_2025, primary_diet, db, hb, esi)
Ft_df3 <- Ft_df2 %>% left_join(Birdbase2, by = join_by("Species_avilist_25" == "avi_list_v1_2025")) %>%
  select(-Species_avilist_25) %>%
  distinct() 

# Elevational ranges ------------------------------------------------------
# Elevational ranges will be used for 2 things, to: 1) look for possible species misidentifications or other errors in the data, 2) to use as a trait representing environmental niche breadth. For #2 having a standardized source for elevations would be ideal (QJ has fewest NAs). One possibility would be to apply a correction across different sources (see below in EXTRAS, search 'correction'), another would be to use QJ estimates from across other countries

# From Hilty guidebook, thanks to Hazen
Hilty_elev <- read_xlsx(
  "../Datasets_external/Elev_ranges/Hazen_Elev_ranges_Hilty.xlsx"
) %>% select(Species_ayerbe, contains("Hilty"))

# From Suarez Castro et al (2024)
Ayerbe_elev <- read_csv(
  "../Datasets_external/Elev_ranges/Suarez_castro_AOH_birds_table_S3_V3.csv"
) %>% rename(
  Species_bl = BirdLife..IUCN.,
  Min_ayerbe = Minimum.elevation,
  Max_ayerbe = Maximum.elevation
) %>% rename(Species_ayerbe = Scientific.Name)

# From Ayerbe-Quiñones (2018) field guide 
Ayerbe_elev_hazen <- read_xlsx(
  "../Datasets_external/Elev_ranges/Hazen_Elev_ranges_Ayerbe.xlsx"
) %>% select(-Elev_range_ayerbe)

# Join Suarez Castro et al (2024) and Hazen's work from Ayerbe field guide
Ayerbe_elev2 <- Ayerbe_elev %>% bind_rows(Ayerbe_elev_hazen)

# Traits from Bird et al.
bird20t <- read_excel("../Datasets_external/Bird_et_al_Generation_length_2020/cobi13486-sup-0003-tables3.xlsx")
bird20t <- bird20t %>%
  rename_with(make.names) %>%
  rename(Min_B20 = Minimum.altitude, Max_B20 = Maximum.altitude)

# Pull elevational ranges of the species of Colombia from Quintero & Jetz 2018, 'Global elevational diversity and diversification of birds'
elev_rangesQJ <- read_excel("../Datasets_external/Elev_ranges/Quintero_Jetz_Elevational_ranges_2018.xlsx") # QJ = Quintero Jetz

# Ben Freeman's science paper using eBird to generate elevational ranges
Free22 <- read.csv("../Datasets_external/Elev_ranges/Freeman_Code_MSM_Elev_2022/Part1/output/elevational-ranges.csv")
eB_Tax <- read_excel("../Vignettes/ebird_vignettes/eBird-Clements-v2023-integrated-checklist-October-2023.xlsx") %>%
  filter(category == "species") %>%
  rename_with(make.names)

#head(ebirdst_runs)
# Subset Colombia and add in scientific names from ebirdst package
Free22_Co <- Free22 %>%
  filter(region %in% c("n_tropical_andes", "choco")) %>%
  group_by(species_code, common_name) %>%
  summarize(Min_eB = min(lower), Max_eB = max(upper)) %>%
  # 22ish species did not have matching species code, resulting in problematic NAs. Use inner_join
  inner_join(eB_Tax[, c("species_code", "scientific.name")], 
             by = "species_code")

# Subset species from Colombia
Col_elev_QJ <- elev_rangesQJ %>%
  filter(Country %in% c("COL")) %>% # All species in Andes (Mountain ID = 404 have something for the 'Country' column, so can just subset COL)
  mutate(Year = stri_extract_first_regex(`Source and Notes`, "[0-9]+")) %>%
  mutate(Year = ifelse(Species == "Picumnus squamulatus", 2010, Year)) %>%
  rename(
    Min_QJ = `Minimum elevation`,
    Max_QJ = `Maximum elevation`
  )

# >Join data frames -----------------------------------------------------
## Data wrangling to combine data frames and format for a minimum and maximum for each dataset 
# For now we leave out Bird (2020) as it is global so these ranges might be too large & don't really make sense for Colombia specifically.

# Join tables
elev_raw <- Tax_df %>%
  select(Species_ayerbe, Species_bt, Species_eB, Species_bl) %>%
  left_join(Hilty_elev,      by = join_by("Species_ayerbe" == "Species_ayerbe")) %>%
  left_join(Col_elev_QJ,     by = c("Species_bt" = "Species")) %>%
  #left_join(bird20t,         by = c("Species_bl" = "Scientific.name")) %>%
  left_join(
    Ayerbe_elev2 %>% select(-Species_bl)
  ) %>%
  left_join(
    Free22_Co %>% select(scientific.name, Min_eB, Max_eB),
    by = c("Species_eB" = "scientific.name")
  ) %>% distinct(
    Species_ayerbe, Species_eB, Species_bl, 
    Min_Hilty, Max_Hilty, Elev_range_Hilty, 
    Min_QJ, Max_QJ,
    #Min_B20, Max_B20, 
    Min_ayerbe, Max_ayerbe,
    Min_eB, Max_eB, 
    Year
  )

# >Min max ----------------------------------------------------------------
# Custom functions to take the minimum value or return NA 
safe_min <- function(x) if (all(is.na(x))) NA_real_ else min(x, na.rm = TRUE)
safe_max <- function(x) if (all(is.na(x))) NA_real_ else max(x, na.rm = TRUE)

# Generate tbl with the minimum and maximum values across all sources. The idea behind the min and max elevation combined columns (these are the broadest elevational ranges) is this should be useful for checking specific observations with data collectors. The goal is to make the list manageable and to just consist of the species that really are likely mistakes
Elev_min_max <- elev_raw %>%
  group_by(Species_ayerbe) %>%
  summarize(
    across(starts_with("Min"), safe_min),
    across(starts_with("Max"), safe_max),
    Year = first(Year),
    .groups = "drop"
  ) %>%
  # combined min/max across Colombia-specific sources
  mutate(
    Min_elev_comb = pmin(Min_Hilty, Min_QJ, Min_eB, Min_ayerbe, na.rm = TRUE),
    Max_elev_comb = pmax(Max_Hilty, Max_QJ, Max_eB, Max_ayerbe, na.rm = TRUE),
    Source_comb_elev =
      case_when(
        Min_elev_comb == Min_QJ & Max_elev_comb == Max_QJ ~ "QJ",
        Min_elev_comb == Min_eB & Max_elev_comb == Max_eB ~ "Freeman",
        Min_elev_comb == Min_Hilty & Max_elev_comb == Max_Hilty ~ "Hilty",
        Min_elev_comb == Min_ayerbe & Max_elev_comb == Max_ayerbe ~ "Ayerbe",
        .default = "Multiple"
      ),
    # QJ has the year column, which is variable, whereas all others are a single year
    Year = case_when(
      Source_comb_elev == "QJ" ~ Year,
      Source_comb_elev == "Hilty" ~ "2021", 
      Source_comb_elev == "Ayerbe" ~ "2018", 
      Source_comb_elev == "Freeman" ~ "2022",
      Source_comb_elev == "Multiple" ~ NA_character_,
      .default = NA_character_)
  )

# >Elev ranges ------------------------------------------------------------
# Generate tbl with elevational ranges according to each source
Elev_ranges <- Elev_min_max %>%
  mutate(
    Elev_range_Hilty = Max_Hilty - Min_Hilty,
    Elev_range_ayerbe = Max_ayerbe - Min_ayerbe,
    Elev_range_QJ    = Max_QJ    - Min_QJ,
    Elev_range_eB    = Max_eB    - Min_eB,
    Elev_range_Hilty = ifelse(Elev_range_Hilty == 0, NA, Elev_range_Hilty), 
    # Combine Hilty, QJ, ayerbe, and Freeman (2022), all Colombia-specific, into a single column.
    Elev_range_comb  = Max_elev_comb - Min_elev_comb
  )

# >Differences sources ----------------------------------------------------
# Take the differences of elevational ranges between all pairwise combinations of sources
Elev_diffs <- Elev_ranges %>% 
  mutate(
    Diff_QJ_eB       = abs(Elev_range_eB - Elev_range_QJ),
    Diff_Hilty_eB    = abs(Elev_range_Hilty - Elev_range_eB),
    Diff_ayerbe_eB = abs(Elev_range_eB - Elev_range_ayerbe),
    Diff_Hilty_QJ    = abs(Elev_range_Hilty - Elev_range_QJ),
    Diff_Hilty_ayerbe = abs(Elev_range_Hilty - Elev_range_ayerbe),
    Diff_QJ_ayerbe = abs(Elev_range_QJ - Elev_range_ayerbe)
  ) %>%
  select(Species_ayerbe, matches("Elev_range|Diff"), Year)

# Identify which sources show agreement for each species.
diff_tbl <- Elev_diffs %>%
  select(Species_ayerbe, starts_with("Diff_")) %>%
  pivot_longer(
    cols = starts_with("Diff_"),
    names_to = "source",
    values_to = "diff"
  ) %>%
  group_by(Species_ayerbe) %>%
  mutate(
    min_diff = if (all(is.na(diff))) NA_real_ else min(diff, na.rm = TRUE)
  ) %>%
  filter(diff == min_diff | is.na(min_diff)) %>%
  arrange(Species_ayerbe, diff, source) %>%
  slice(1) %>%  
  mutate(source = ifelse(is.na(diff), NA, source)) %>%
  select(-diff) %>% 
  ungroup()

## Examine
# There are many species where the minimum difference between sources is >500m
# Some of these may be due to taxonomy... Some of these have QJ in it, and may be very old (as far back as 1986) 
diff_tbl %>% filter(min_diff > 500) %>% 
  arrange(desc(min_diff))

## Select a chosen range 
source_choice <- diff_tbl %>%
  mutate(
    chosen_range = case_when(
      str_detect(source, "ayerbe") ~ "Ayerbe",
      str_detect(source, "Hilty") ~ "Hilty",
      str_detect(source, "QJ")    ~ "QJ",
      str_detect(source, "eB")    ~ "eB",
      TRUE ~ NA_character_
    )
  )
source_choice %>% tabyl(chosen_range)
source_choice %>% filter(min_diff > 500) %>% 
  arrange(min_diff)

#KEY - 'We looked for concordance between 2 Colombia-specific elevational range sources, and prioritized sources in the following order: Ayerbe-Quiñones (2018), Hilty (2021), QJ (variable)'
Elev_final <- Elev_ranges %>%
  left_join(source_choice %>% select(Species_ayerbe, chosen_range),
            by = "Species_ayerbe") %>%
  mutate(
    Elev_range_final = case_when(
      chosen_range == "Hilty" ~ Elev_range_Hilty,
      chosen_range == "QJ"    ~ Elev_range_QJ,
      chosen_range == "Ayerbe"   ~ Elev_range_ayerbe,
      chosen_range == "eB"    ~ Elev_range_eB,
      TRUE ~ NA_real_
    )
  )

Elev_final %>% select(Species_ayerbe, Elev_range_final, chosen_range)

# >Understand elevational ranges-----------------------------------------
# Several checks to better understand the elevational range data

# Nearly all of the species have elevational ranges from at least two sources (only 6 have one source)
Elev_ranges %>% select(Species_ayerbe, contains("range")) %>% 
  select(-Elev_range_comb) %>%
  mutate(n_miss = naniar::n_miss_row(.)) %>% 
  arrange(desc(n_miss))


# Examine where elevational ranges come from (mostly 2018, some as old as 1986)
Elev_ranges %>%
  pull(Year) %>%
  table()

# Examine correlations between different sources
Elev_ranges %>%
  select(Elev_range_Hilty, Elev_range_ayerbe, Elev_range_QJ, Elev_range_eB) %>%
  cor(use = "complete.obs") %>%
  data.frame() %>%
  mutate(across(everything(), round, 2))

# Plotting relationships between elevational ranges from different sources
source <- c("Hilty", "QJ", "ayerbe", "eB")
pairs <- combn(source, 2, simplify = FALSE)

compare_tbl_long <- map_dfr(pairs, \(pair){
  Elev_ranges %>% 
    select(Species_ayerbe, starts_with("Elev_range_")) %>%
    transmute(
      Species_ayerbe,
      x = .data[[paste0("Elev_range_", pair[1])]],
      y = .data[[paste0("Elev_range_", pair[2])]],
      x_lab = pair[1],
      y_lab = pair[2],
      panel = paste0(pair[1], " vs ", pair[2])
    )
}) 

# Additional formatting if desired
nice_names <- c(
  Hilty = "Hilty (2001)",
  QJ = "Quintero & Jetz (2018)",
  ayerbe = "Ayerbe-quinones (2018)",
  eB = "Freeman et al. (2022)"
)
compare_tbl_long2 <- compare_tbl_long %>% 
  mutate(panel = paste0(nice_names[x_lab], "\nvs\n", nice_names[y_lab]))

# Plot 
ggplot(compare_tbl_long2, aes(x = x, y = y)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  facet_wrap(~panel, scales = "free") +
  labs(x = "Source A", y = "Source B",
       title = "Pairwise comparisons of elevational ranges across sources") +
  theme_bw()

# If you wanted to fill in blanks of QJ with ranges from another source.. Could consider applying a correction based on the beta estimate(?)
summary(lm(Elev_range_eB ~ Elev_range_QJ, data = Elev_ranges))

# NOTE:: This still does not provide elevational ranges for all species, likely due to name changes in BirdLife & some missing elevational ranges. Could check the species missing from Freeman (see above)
Miss_elev <- Elev_ranges %>% 
  filter(is.na(Elev_range_comb)) %>% #view()
  pull(Species_ayerbe)
Tax_df %>%
  filter(Species_ayerbe %in% Miss_elev) %>%
  select(Species_ayerbe, Species_bl, Species_bt, Species_eB) %>%
  distinct()

# CHECK:: There are some species Ayerbe that have multiple Bird Tree species equivalents, and thus can have multiple Min & max elevations. It makes sense to combine these elevations since it is a single species according to Ayerbe.. Thus in the case of Chlorostilbon mellisugus the min & max should be 0 to 2000. The only other species this is relevant for is Ramphastos ambiguus
Tax_df[, c("Species_ayerbe", "Species_bt")] %>%
  left_join(Col_elev_QJ, join_by("Species_bt" == "Species")) %>%
  filter(Species_ayerbe == "Chlorostilbon mellisugus")
Elev_ranges %>%
  filter(Species_ayerbe == "Chlorostilbon mellisugus") %>%
  select(matches("comb|Elev_range"))

# For this reason the '1BL to many BT' matches are not an issue. 'Many BL to 1BT' are also not an issue b/c the one BT matches cleanly with the QJ database
table(elev_raw$Match_type)

# IUCN status -------------------------------------------------------------
HBW <- read_excel("/Users/aaronskinner/Library/CloudStorage/OneDrive-UBC/Grad_School/PhD/Analysis/Datasets_external/Handbook of the Birds of the World and BirdLife International Digital Checklist of the Birds of the World_Version_7.xlsx", sheet = "HBW-BirdLife v7 ", skip = 3) %>% tibble() %>% 
  clean_names() %>% 
  rename(iucn_red_list = x2022_iucn_red_list_category)
# Reduce file down just to recognized species 
HBW_sp <- HBW %>% filter(subsp_seq == 0 & iucn_red_list != "NR")

# Taxonomic difference Species_bl in Tax_df uses genus 'Amazilia'
Hummer_no_match <- c("Amazilia fimbriata", "Amazilia cyanus", "Amazilia saucerottei", "Amazilia oenone", "Amazilia coeruleogularis", "Amazilia goudoti", "Amazilia versicolor")
Hummer_hbw_name <- c("Chionomesa fimbriata", "Chlorestes cyanus", "Saucerottia saucerottei", "Chrysuronia oenone", "Chrysuronia coeruleogularis", "Chrysuronia goudoti", "Chrysuronia versicolor")
Hummer_name_equivalents <- tibble(Hummer_no_match, Hummer_hbw_name) 

# Merge and replace 
HBW_sp2 <- HBW_sp %>% 
  left_join(Hummer_name_equivalents, 
            by = join_by("scientific_name" == "Hummer_hbw_name")) %>% 
  mutate(scientific_name = coalesce(Hummer_no_match, scientific_name)) %>% 
  select(-Hummer_no_match)


iucn_status <- Tax_df %>% distinct(Species_bl) %>%
  left_join(HBW_sp2[, c("scientific_name", "iucn_red_list")], 
            by = join_by("Species_bl" == "scientific_name"))

# Nearly all species observed are least concern
iucn_status %>% tabyl(iucn_red_list)

# r-k life history --------------------------------------------------------
## r-k continuum traits. It would be interesting to see if species that are on the k-side of the continuum are impacted more greatly than r-selected species. Look at Wolfe et al 2025 too. 

# Pace of life traits from Bird et al.
Bird_pl <- read_excel("../Datasets_external/Bird_et_al_Generation_length_2020/cobi13486-sup-0004-tables4.xlsx") %>% clean_names()

# Variables are highly correlated
Bird_pl %>% select(adult_survival:gen_length) %>%
  GGally::ggcorr(label = T, label_size = 2, label_round = 2, hjust = 0.75, size = 3, layout.exp = 1.01)

# Replace Amazilia genus with updated hummer names used by Bird et al
Hummer_name_equivalents2 <- Hummer_name_equivalents %>% 
  mutate(Hummer_bird_20 = case_when(
    Hummer_no_match == "Amazilia coeruleogularis" ~ "Lepidopyga coeruleogularis",
    Hummer_no_match == "Amazilia goudoti" ~ "Lepidopyga goudoti",
    Hummer_no_match == "Amazilia cyanus" ~ "Hylocharis cyanus", 
    Hummer_no_match == "Amazilia oenone" ~ "Chrysuronia oenone"
))

# Generation length is the average interval between the birth of individuals and the birth of their (first?) offspring
Gen_length <- Bird_pl %>%
  left_join(Hummer_name_equivalents2, 
            by = join_by("scientific_name" == "Hummer_bird_20")) %>% 
  mutate(scientific_name = coalesce(Hummer_no_match, scientific_name)) %>% 
  select(-Hummer_no_match)

# Join 
Gen_length2 <- Tax_df %>% distinct(Species_bl) %>%
  left_join(Gen_length[, c("scientific_name", "gen_length")], 
            by = join_by("Species_bl" == "scientific_name"))

# Histogram
Gen_length2 %>%
  ggplot() + geom_histogram(aes(x = gen_length))


# Nesting -----------------------------------------------------------------
# >Clutch size ------------------------------------------------------------
scrape <- read_csv("Derived/Excels/Traits/clutch_size_migrants_30.csv") %>% 
  rename(Scientific.name = scientific_name)
# First & last authors from Bird Life , so assuming that they use birdlife taxonomy
Life_history <- bird20t %>% 
  filter(Scientific.name %in% Tax_df$Species_bl) %>% 
  replace_with_na_all(condition = ~.x == "NA") %>% 
  select(Scientific.name, Mean.clutch.size, Survival, Age.at.first.breeding, Max.longevity) %>% 
  rename(Clutch = Mean.clutch.size) %>% 
  mutate(across(Clutch:Max.longevity, as.numeric))

Life_history %>% right_join(scrape) %>% 
  select(Scientific.name, contains("clutch"))

# Inspect missingness
gg_miss_var(Life_history)

# 341 species with clutch size information
Life_history %>%
  filter(!is.na(Clutch)) %>% 
  ggplot() + geom_histogram(aes(x = Clutch))

## Compare clutch and generation length
# Correlation = -0.17
Gen_length2 %>% 
  left_join(Life_history, by = join_by("Species_bl" == "Scientific.name")) %>% 
  ggplot(aes(x = gen_length, y = Clutch)) +
  geom_point() +
  geom_smooth(method = "lm")

# >Nest type --------------------------------------------------------------
# Nest type data from Sheard, Catherine, et al. "Nest traits for the world's birds" Global Ecology and Biogeography 33.2 (2024): 206-214.
Nesting_path <- "../Datasets_external/Sheard_et_al_geb_Nesting_traits_2023"
Nesting_sheard1 <- read_csv(paste0(Nesting_path, "/Dataset-S1.csv")) %>%
  clean_names() %>%
  distinct(species_scientific_name, veg_type) %>% 
  filter(!is.na(veg_type) & veg_type != c("no info"))
Nesting_sheard2 <- read_csv(paste0(Nesting_path, "/Dataset-S2.csv")) %>%
  clean_names()

# Replace 'unknown' (u) with NA, turn numeric, join with 'veg_type' from Nesting_sheard1
Nesting <- Nesting_sheard2 %>% 
  replace_with_na_all(condition = ~.x == "u") %>%
  select(species_scientific_name, starts_with("str"), starts_with("loc")) %>%
  mutate(across(.cols = -species_scientific_name, as.numeric)) %>% 
  left_join(Nesting_sheard1) %>% 
  distinct()

# Subset with species observed
Nesting2 <- Tax_df %>% 
  left_join(Nesting, by = join_by("Species_bl" == "species_scientific_name")) %>%
  select(Species_ayerbe,  starts_with("str"), starts_with("loc"), veg_type)

# >>Nest structure --------------------------------------------------------
Nest_str <- Nesting2 %>%
  select(Species_ayerbe, starts_with("str")) %>%
  pivot_longer(
    cols = -c(Species_ayerbe), 
    names_to = "Nest_structure", 
    values_to = "Binary"
  ) %>% 
  filter(Binary == 1) %>%
  select(-Binary) %>%
  mutate(Nest_structure = str_remove(Nest_structure, "str_")) %>% 
  distinct()

# >>Nest location ---------------------------------------------------------
Nest_loc <- Nesting2 %>%
  select(Species_ayerbe, starts_with("loc"), veg_type) %>%
  pivot_longer(
    cols = -c(Species_ayerbe, veg_type),
    names_to = "Nest_location", 
    values_to = "Binary"
  ) %>% 
  filter(Binary == 1) %>%
  select(-Binary) %>%
  mutate(Nest_location = str_remove(Nest_location, "loc_"),
         veg_type = ifelse(Nest_location == "veg", veg_type, NA), 
         veg_type = str_to_sentence(veg_type)) %>% 
  relocate(Nest_location, .before = veg_type)

## Location == "veg" is some combination of bush, tree, or reed
Nest_loc2 <- Nest_loc %>% 
  # Recode Bush + uncertain stuff as just Bush
  mutate(veg_type = ifelse(
    str_detect(veg_type, "Bush, uncertain"), "Bush", veg_type
    ))

# Of the options that contain bush but not tree..
Nest_loc2 %>% filter(
  str_detect(veg_type, "Bush|bush") & !str_detect(veg_type, "Tree|tree")
) %>% pull(veg_type) %>% unique()
# Relabel these as 'Bush'
Label_bush <- c("Bush", "Bush, reed")
# Relabel
Nest_loc3 <- Nest_loc2 %>% mutate(Nest_location = ifelse(
    veg_type %in% Label_bush, "bush", Nest_location
    )) %>% distinct(Species_ayerbe, Nest_location)
Num_nest_types <- Nest_loc3 %>% 
  count(Species_ayerbe, sort = T, name = "N_nest_locs") 
# Of the species with only one nest type, how is nest location distributed?
Sing_nest_loc <- Num_nest_types %>% 
  filter(N_nest_locs == 1) %>% 
  pull(Species_ayerbe)
Nest_loc3 %>% filter(Species_ayerbe %in% Sing_nest_loc) %>% 
  #filter(Nest_location == "tree_hole") %>% view()
  tabyl(Nest_location)

Nest_loc4 <- Nest_loc3 %>% left_join(Num_nest_types)
# These two species nest in >1 habitat but both are ground / bush
Spp_bush_ground <- c("Arremonops tocuyensis", "Geothlypis philadelphia")
Nest_loc4 %>% filter(Species_ayerbe %in% Spp_bush_ground)

Nest_loc5 <- Nest_loc4 %>% mutate(Ground_bush = case_when(
  N_nest_locs == 1 & Nest_location %in% c("ground", "bush") ~ TRUE,
  Species_ayerbe %in% Spp_bush_ground ~ TRUE,
  .default = FALSE
)) %>% distinct(Species_ayerbe, Ground_bush, N_nest_locs)

# >>Nest exposure ---------------------------------------------------------
# Exposure_comb_key is classification of exposure (open, semi-open, enclosed) based on combinations of nest location and nest structure
Exposure_comb_key <- read_csv("Derived/Excels/Traits/Nest_exposure.csv")
# Combine structure and location 
Nesting_comb <- Nest_loc4 %>% 
  full_join(Nest_str) %>% 
  left_join(Exposure_comb_key) %>% 
  # Hard code exposure if clearly 'Enclosed'
  mutate(Exposure = case_when( 
    Nest_location %in% c("tree_hole", "earth_hole") ~ "Enclosed", 
    Nest_structure %in% c("dome", "dome_and_tube", "ex_w_nest", "excavation", "cavity_mod") ~ "Enclosed",
    .default = Exposure
    ))
# Some species have multiple exposures
Spp_to_classify <- Nesting_comb %>% 
  distinct(Species_ayerbe, Exposure) %>% 
  filter(!is.na(Exposure)) %>% 
  count(Species_ayerbe, sort = T) %>% 
  filter(n > 1) %>% 
  pull(Species_ayerbe)

# Classify these conservatively, removing the species if it has both 'Open' & 'Enclosed', and otherwise classifying as 'Semi-open'
Spp_classified <- Nesting_comb %>%
  filter(Species_ayerbe %in% Spp_to_classify) %>%
  group_by(Species_ayerbe) %>%
  summarise(
    has_open = any(Exposure == "Open"),
    has_enclosed = any(Exposure == "Enclosed"),
    has_semi = any(Exposure == "Semi-open"),
    .groups = "drop"
  ) %>%
  mutate(
    Action = case_when(
      has_open & has_enclosed ~ "Remove",
      has_semi ~ "Semi-open",
      .default = NA_character_
    )
  ) %>% filter(Action != "Remove") %>% 
  rename(Exposure = Action) %>% 
  select(Species_ayerbe, Exposure)

# Create exposure classification where each species has a single row
Nest_exposure <- Nesting_comb %>% 
  filter(!Species_ayerbe %in% Spp_to_classify) %>% 
  bind_rows(Spp_classified) %>% 
  distinct(Species_ayerbe, Exposure) %>% 
  filter(!is.na(Exposure))

## Combine
# Join final location tbl with exposure tbl
Nesting_final <- Nest_loc5 %>% full_join(Nest_exposure) %>% 
  rename(Nest_ground_bush = Ground_bush, 
         Nest_exposure = Exposure)

## STILL TO DO - NEED TO KNOW IF BREEDING IN COLOMBIA vs MIGRATORY 
if(FALSE){
  Ft_final %>% filter(Migration != "Sedentary") %>% 
    distinct(Species_ayerbe, Migration) %>% 
    arrange(Migration)
}

# Eye_size ----------------------------------------------------------------
# Load in data
path <- "../Datasets_external/Eye Size Files/"

Final_Book_Join <- read_excel(paste0(path, "Final_Book_Join.xlsx")) %>% 
  mutate(Species_sacc_18 = str_to_sentence(Species_sacc_18), 
         Ausprey_trop_2021_methodology = str_to_sentence(Ausprey_trop_2021_source)) %>% 
  rename(Jones_methodology = Jones_measurement) 

# Create Eye size tbl for joining 
Eye_size_tbl <- Final_Book_Join %>% 
  select(Species_sacc_18, ends_with("eye"), ends_with("methodology")) %>% 
  # Order of columns determines which source takes priority
  mutate(Transverse_eye = coalesce(Ausprey_trop_2021_eye, Jones_eye),
         Methodology = coalesce(Ausprey_trop_2021_methodology, Jones_methodology)) %>% 
  select(Species_sacc_18, Ausprey_2024_eye, Transverse_eye, Methodology)

# Adjust photos by factor of 1.03 as in Ausprey et al 2021
Eye_size_tbl2 <- Eye_size_tbl %>% 
  mutate(Transverse_eye = if_else(
    Methodology == "Photo", Transverse_eye * 1.03, Transverse_eye
  ))

# How closely do Transverse_eye and Ausprey_2024_eye measurement track each other? 
Eye_size_tbl2 %>% ggplot(aes(x = Transverse_eye, y = Ausprey_2024_eye)) + 
  geom_point() +
  geom_smooth(method = "lm") +
  geom_abline(slope = 1, linetype = "dashed", color = "red")

# Ian's suggestion - translate the values taken on live specimens or photos (Ausprey 2021, Jones 2023) to the Ausprey 2024 full eye measurements
mod_eye <- lm(Ausprey_2024_eye ~ Transverse_eye, data = Eye_size_tbl2)
summary(mod_eye) # 86% of the variation explained 
# Predict full eye size using model
Eye_predict <- Eye_size_tbl2 %>% filter(!is.na(Transverse_eye) & is.na(Ausprey_2024_eye))
Eye_predict$Pred_Ausprey_2024 <- predict(mod_eye, Eye_predict)
Eye_predict2 <- Eye_predict %>% select(Species_sacc_18, Pred_Ausprey_2024) %>% 
  mutate(Source = "Predicted")

# Join repository measurements with predicted measurements
Eye_size_tbl3 <- Eye_size_tbl2 %>% 
  left_join(Eye_predict2) %>% 
  mutate(Source = if_else(
    is.na(Source) & !is.na(Ausprey_2024_eye), "Repository", Source
    ), 
    Eye_comb = coalesce(Ausprey_2024_eye, Pred_Ausprey_2024)) %>% 
  select(Species_sacc_18, Ausprey_2024_eye, Pred_Ausprey_2024, Eye_comb, Source) %>% 
  filter(!is.na(Eye_comb)) %>% 
  rename(Source_eye = Source)

## Calculate residual eye size
# Join with mass information
Eye_size_tbl4 <- Eye_size_tbl3 %>% 
  left_join(Ft_df[, c("Species_ayerbe", "Mass")],
            by = join_by("Species_sacc_18" == "Species_ayerbe")) %>% 
  filter(!is.na(Mass))

# Run model & extract residuals
mod_eye_allometry <- lm(Eye_comb ~ Mass, data = Eye_size_tbl4)
Eye_size_tbl5 <- Eye_size_tbl4 %>% 
  mutate(Eye_resid = resid(mod_eye_allometry))

# Combine Ft_final -------------------------------------------------
# Merge with functional traits database
Ft_final <- Ft_df3 %>%
  full_join(
    Elev_final[,c("Species_ayerbe", "Elev_range_final", "Source_comb_elev")]
  ) %>% 
  full_join(
    Eye_size_tbl5[,c("Species_sacc_18", "Eye_resid", "Source_eye")],
    by = join_by("Species_ayerbe" == "Species_sacc_18")
    ) %>% 
  full_join(
    Life_history[,c("Scientific.name", "Clutch")], 
    by = join_by("Species_bl" == "Scientific.name")
    ) %>% 
  full_join(iucn_status) %>%
  full_join(Gen_length2) %>% 
  full_join(Nesting_final)

# Save & export -----------------------------------------------------------
stop()

# Export functional traits file as csv 
Ft_final %>%
  rename_with(.cols = everything(), .fn = ~str_remove(., "_comb")) %>%
  write_csv(file = "Derived/Excels/Traits/Functional_traits.csv")

# Export full elevation file as csv 
Elev_final %>% write_csv(file = "Derived/Excels/Elev_ranges_all_sources.csv")

#rm(list = ls()[!(ls() %in% c("Elev_ranges", "Avo_traits_final"))])
#save.image(paste0("Rdata/Traits_elev_", format(Sys.Date(), "%m.%d.%y"), ".Rdata"))
# Manual
#save.image("Rdata/Traits_elev_12.29.24.Rdata")

# Extras ------------------------------------------------------------------
# >Hazen elevation scratch pad ---------------------------------------------
# With the Hilty guide book, it is worth checking the species with extreme differences in ranges, or without data from the most trustworthy sources. This has been iterative as Hazen has gone through the Hilty guidebook and we have had more information available to us.
Old_path <- ("/Users/aaronskinner/Library/CloudStorage/OneDrive-UBC/Grad_School/PhD/Mentorship/Hazen/Elev_ranges_diff_qj_ebird_10.2.25.xlsx") 
Hilty_elev <- read_xlsx("../Datasets_external/Elev_ranges/Elev_ranges_Hazen.xlsx")

# Check elevational limits of coffee region species
Elev_min_max %>% select(Species_ayerbe, matches("Min|Max")) %>% 
  filter(str_detect(Species_ayerbe, "Xenops")) 

# Lengthen the tibble
Hazen_elev <- Hilty_elev %>%
  select(Species_ayerbe, contains("Hilty"), contains("range")) %>%
  filter(!is.na(Elev_range_Hilty) & Elev_range_Hilty != 0) 

Hazen_elev %>% filter(str_detect(Species_ayerbe, "Phyllomyias"))

# Examine correlations between different data collectors
Hazen_elev %>% 
  select(-c(Species_ayerbe, matches("Max|Min"))) %>%
  cor(use = "complete.obs") %>% 
  data.frame() %>% 
  mutate(across(everything(), round, 2))

# Compare the Hilty ranges to the other 3 data collectors and determine which of the other 3 is closest to Hilty
Hazen_elev2 <- Hazen_elev %>%
  pivot_longer(cols = -c(Species_ayerbe, Elev_range_Hilty, matches("Max|Min")),
               names_to = "Source", values_to = "Elev_range") %>%
  mutate(Source = str_remove_all(Source, "Elev_range_|Elev_range_")) %>%
  mutate(Diff = abs(Elev_range - Elev_range_Hilty)) %>% 
  slice_min(order_by = Diff, by = Species_ayerbe) %>% 
  summarize(across(-Source, first), 
            Closest_to = str_c(sort(unique(Source)), collapse = ", "),
            .by = Species_ayerbe) %>%
  rename(Elev_range_closest = Elev_range) %>% 
  select(-c(Elev_range_closest, Diff))

# Compare closest sources 
Hazen_elev2 %>% tabyl(Closest_to)

# Plot 
Hazen_elev2 %>%
  pivot_longer(cols = -c(Species_ayerbe,  matches("Max|Min")), 
               names_to = "Source",
               values_to = "Elev_range") %>% 
  mutate(Source = str_remove_all(Source, "Elev_range_|Elev_range_"),
         Source = factor(Source, levels = c("B20", "QJ", "Hilty", "eB"))) %>% 
  ggplot(aes(x = Source, y = Elev_range, 
             color = Species_ayerbe, group = Species_ayerbe)) + 
  geom_point() + 
  geom_line() +
  guides(color = "none") +
  labs(y = "Elevational range")

# >Habitat preferences -----------------------------------------------------
# Generate habitat preferences for 580 of 607 species observed in our point counts. Note some species are not able to be matched b/c of differences in taxonomy.. Would have to generate complete list of equivalents for Ayerbe -> Species_bl
library(traits) # Traits data
# traits::traitbank() #also see functions related to EOL

load("Hab_types.Rdata")

HBWsp
# subset just relevant species of Colombia
HBWco <- distinct(Tax_df[c("Species_bl")]) %>%
  inner_join(HBWsp[c("Scientific name", "SISRecID")], join_by("Species_bl" == "Scientific name"))

HBW_hab_pref <- list()
for (i in 1:nrow(HBWco)) {
  print(i)
  HBW_hab_pref[[i]] <- birdlife_habitat(id = HBWco[i, 2])
}
Hab_types <- bind_rows(HBW_hab_pref) %>% inner_join(HBWco, join_by("id" == "SISRecID"))
head(Hab_types)
lapply(Hab_types[2:3], table)


# save(Hab_types, file = "/Users/aaronskinner/Library/CloudStorage/OneDrive-UBC/Grad_School/R_Files/PhD/Hab_types.Rdata")

birdlife_threats(22689248)