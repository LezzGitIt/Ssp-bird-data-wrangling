## PhD birds in silvopastoral landscapes##
## Data wrangling 00c -- Functional traits (FT) & elevational ranges
## This script generates the outputs related to functional traits (Avo_traits_final), & elevational ranges (Elev_ranges) that will be used in future scripts 

# Contents
# 1) Functional traits database -- Use Tax_df & the Avonet files to create FT database using a for loop & the Match.type (e.g., "1BL to 1BT") column to ensure that species are matched appropriately with their FT. Each species has a single row. 
# 2) Elevational ranges -- Use 3 databases to pull elevational ranges for each species 
# 3) Join dfs -- Join the dfs to create a single df (Elev_ranges) where each species has a single row with relevant elevational information, including the elevational range (a functional trait) for each species
# 4) Understand Elevational ranges -- Additional information regarding the elevational range information 
# 4) Add elev range to FT df
# 5) Save & export 

# Libraries ---------------------------------------------------------------
library(readxl)
library(tidyverse)
library(sf)
library(chron)
library(ggpubr)
library(cowplot)
library(conflicted)
ggplot2::theme_set(theme_cowplot())
conflicts_prefer(dplyr::select)
conflicts_prefer(dplyr::filter)

#Load data
load("Rdata/the_basics_12.24.24.Rdata")
load("Rdata/Taxonomy_11.14.24.Rdata")
source("/Users/aaronskinner/Library/CloudStorage/OneDrive-UBC/Grad_School/Rcookbook/Themes_funs.R")

# Functional traits database ----------------------------------------------
# Used a loop to ensure that we are taking trait data from a single species instead of a subspecies. For example, we want to ensure we're grabbing from BirdTree for Momotus momota and not the BirdLife subspecies
Tax_df3 %>% filter(Species_ayerbe == "Momotus momota")

# For loop create functional traits data base
SpMT <- distinct(Tax_df3, Species_ayerbe, Match.type) # Species match type
Avo_traits_loop <- vector("list", length = nrow(SpMT))

for (i in 1:length(SpMT$Species_ayerbe)) {
  print(i)
  if (!is.na(SpMT$Match.type[i]) & SpMT$Match.type[i] %in% c("1BL to 1BT", "Many BL to 1BT")) {
    Avo_traits_loop[[i]] <- SpMT[i, ] %>% left_join(Avo_traits_l$BirdTree,
                                                    by = join_by("Species_ayerbe" == "Species")
    )
    Avo_traits_loop[[i]]$Source <- "BT"
  }
  # There are some cases where Species_ayerbe is not found in SpeciesBL.. This resulted in rows of NAs
  if (!is.na(SpMT$Match.type[i]) & SpMT$Species_ayerbe[i] %in% Tax_df3$Species_bl & SpMT$Match.type[i] %in% c("1BL to 1BT", "1BL to many BT")) {
    Avo_traits_loop[[i]] <- SpMT[i, ] %>% left_join(Avo_traits_l$BirdLife,
                                                    by = join_by("Species_ayerbe" == "Species")
    )
    Avo_traits_loop[[i]]$Source <- "BL"
  }
  # Adding is.null creates the 1 row df with NAs, allowing next if() statement to work
  if (is.na(SpMT$Match.type[i]) | is.null(Avo_traits_loop[[i]])) {
    Avo_traits_loop[[i]] <- SpMT[i, ] %>% left_join(Avo_traits_l$eBird,
                                                    by = join_by("Species_ayerbe" == "Species")
    )
    Avo_traits_loop[[i]]$Source <- "eB"
  }
  # If slot is still NAs link with eBird
  if (is.na(Avo_traits_loop[[i]]$Total.individuals)) {
    Avo_traits_loop[[i]] <- SpMT[i, ] %>% left_join(Avo_traits_l$eBird,
                                                    by = join_by("Species_ayerbe" == "Species")
    )
    Avo_traits_loop[[i]]$Source <- "eB"
  }
}

Avo_traits_comb <- smartbind(list = Avo_traits_loop)
# CHECK:: #Should be 0
Avo_traits_comb %>%
  filter(is.na(Total.individuals)) %>%
  nrow()

# Format & remove extraneous columns
Avo_traits_form <- Avo_traits_comb %>%
  select(-c(Sequence, Avibase.ID)) %>%
  mutate(
    Habitat.Density = as.factor(Habitat.Density),
    Migration = as.factor(Migration),
    across(c(ends_with("tude"), "Range.Size"), as.numeric)
  ) %>%
  mutate(across(where(is.numeric), \(x) round(x, 2))) %>%
  relocate(Source, .after = Match.type)
nrow(Avo_traits_form)
head(Avo_traits_form)

# Examine a few key traits
lapply(Avo_traits_form[17:21], table)
# Woodland (= medium stature tree-dominated habitats, including Acacia woodland, riparian woodlands, mangrove forests, forest edges, also more open parkland with scattered taller trees);
# Forest (= tall tree-dominated vegetation with more or less closed canopy, including palm forest)

# Elevational ranges ------------------------------------------------------
# Elevational ranges will be used for 2 things, to: 1) look for possible species misidentifications or other errors in the data, 2) to use as a trait representing environmental niche breadth. For #2 having a standardized source for elevations would be ideal (QJ has fewest NAs). One possibility would be to apply a correction across different sources (see below in EXTRAS, search 'correction'), another would be to use QJ estimates from across other countries

# Pull elevational ranges of the species of Colombia from Quintero & Jetz 2018, 'Global elevational diversity and diversification of birds'
elev_rangesQJ <- read_excel("/Users/aaronskinner/Library/CloudStorage/OneDrive-UBC/Grad_School/PhD/Datasets_external/Quintero_Jetz_Elevational_ranges_2018.xlsx") # QJ = Quintero Jetz
head(elev_rangesQJ)
# Traits (including global elevational ranges) from Bird et al. 2020
bird20t <- read_excel("/Users/aaronskinner/Library/CloudStorage/OneDrive-UBC/Grad_School/PhD/Datasets_external/Bird_et_al_Generation_length_2020/cobi13486-sup-0003-tables3.xlsx") %>% as.data.frame()

# Ben Freeman's science paper using eBird to generate elevational ranges
Free22 <- read.csv("/Users/aaronskinner/Library/CloudStorage/OneDrive-UBC/Grad_School/PhD/Datasets_external/Freeman_Code_MSM_Elev_2022/Part1/output/elevational-ranges.csv")
eB_Tax <- read_excel("/Users/aaronskinner/Library/CloudStorage/OneDrive-UBC/Grad_School/PhD/Analysis/Vignettes/ebird_vignettes/eBird-Clements-v2023-integrated-checklist-October-2023.xlsx") %>%
  filter(category == "species") %>%
  rename_with(make.names)
names(Free22)
#head(ebirdst_runs)
# Subset Colombia and add in scientific names from ebirdst package
Free22_Co <- Free22 %>%
  filter(region %in% c("n_tropical_andes", "choco")) %>%
  group_by(species_code, common_name) %>%
  summarize(lower_eB = min(lower), upper_eB = max(upper)) %>%
  # 22ish species did not have matching species code, resulting in problematic NAs. Use inner_join
  inner_join(eB_Tax[, c("species_code", "scientific.name")], by = "species_code")

bird20t <- bird20t %>%
  rename_with(make.names) %>%
  rename(Min_alt_B20 = Minimum.altitude, Max_alt_B20 = Maximum.altitude)

# Subset species from Colombia
Col_elev_QJ <- elev_rangesQJ %>%
  filter(Country %in% c("COL")) %>% # All species in Andes (Mountain ID = 404 have something for the 'Country' column, so can just subset COL)
  mutate(Year = stri_extract_first_regex(`Source and Notes`, "[0-9]+")) %>%
  mutate(Year = ifelse(Species == "Picumnus squamulatus", 2010, Year)) %>%
  rename(
    Min_elevQJ = `Minimum elevation`,
    Max_elevQJ = `Maximum elevation`
  )
nrow(Col_elev_QJ) # 1531 species for Colombia specifically

# >Join data frames -----------------------------------------------------
# Data wrangling, & calculating Elevation range size
# Ignore warning messages
Elev_ranges <- Tax_df3[, c("Species_ayerbe", "Species_bt", "Species_eb", "Species_bl", "Match.type")] %>%
  left_join(Col_elev_QJ, join_by("Species_bt" == "Species")) %>%
  left_join(bird20t, join_by("Species_bl" == "Scientific.name")) %>%
  left_join(
    Free22_Co[, c("scientific.name", "lower_eB", "upper_eB")],
    join_by("Species_eb" == "scientific.name")
  ) %>%
  distinct(
    Species_ayerbe, Species_bt, Species_eb, Species_bl, Min_elevQJ, Max_elevQJ,
    Min_alt_B20, Max_alt_B20, lower_eB, upper_eB, Match.type, Year
  ) %>%
  group_by(Species_ayerbe) %>%
  summarize(
    Min_elevQJ = min(Min_elevQJ, na.rm = T), Max_elevQJ = max(Max_elevQJ, na.rm = T),
    lower_eB = min(lower_eB, na.rm = T), upper_eB = max(upper_eB, na.rm = T),
    Min_alt_B20 = min(Min_alt_B20, na.rm = T), Max_alt_B20 = max(Max_alt_B20, na.rm = T),
    across()
  ) %>%
  arrange(Year) %>%
  slice_head() %>%
  summarize(across(),
            elev_range_QJ = Max_elevQJ - Min_elevQJ,
            elev_range_B20 = Max_alt_B20 - Min_alt_B20,
            elev_range_eB = upper_eB - lower_eB
  ) %>%
  # Several species in Bird (2020) with elevation ranges = 0, clearly a mistake
  mutate(
    elev_range_B20 = ifelse(elev_range_B20 == 0, NA, elev_range_B20),
    Max_alt_B20 = ifelse(Max_alt_B20 == 0, NA, Max_alt_B20)
  ) %>%
  # Combine QJ & Freeman (2022), both Colombia specific, into a single column. Bird (2020) is global so these ranges might be too large & don't really make sense for Colombia specifically.
  rowwise() %>%
  mutate(
    Max_elev_comb = max(c_across(cols = c(Max_elevQJ, upper_eB)), na.rm = T), # Max_alt_B20
    Min_elev_comb = min(c_across(cols = c(Min_elevQJ, lower_eB)), na.rm = T), # Min_alt_B20
    Source_elev = case_when(
      Max_elevQJ > upper_eB & Min_elevQJ < lower_eB ~ "QJ",
      Max_elevQJ < upper_eB & Min_elevQJ > lower_eB ~ "Freeman",
      Max_elevQJ >= upper_eB & Min_elevQJ >= lower_eB |
        Max_elevQJ <= upper_eB & Min_elevQJ <= lower_eB ~ "Both"
    )
  ) %>%
  mutate(Year = ifelse(Source_elev == "Freeman", "2022", Year)) %>%
  mutate(elev_range_comb = Max_elev_comb - Min_elev_comb) %>%
  replace_with_na_all(condition = ~ .x %in% c(Inf, -Inf))

# >Understand elevational ranges ----------------------------------------------------------
# Several checks to better understand the elevational range data

## CHECK:: A few of these to ensure that things are working as expected
Elev_ranges %>% select(-starts_with("Species"), -contains("B20"))
# Examine where elevational ranges come from (mostly 2014, some as old as 1986)
Elev_ranges %>%
  pull(Year) %>%
  table()

## Let's see how the Bird (2020) and Freeman (2022) data compares to QJ. Correlation is lower than I would have thought in all cases, and notice that Bird overestimates the Colombia-specific elevation in many cases
Elev_ranges %>%
  select(elev_range_QJ, elev_range_B20, elev_range_eB) %>%
  cor(use = "complete.obs") %>%
  data.frame() %>%
  mutate(across(everything(), round, 2))

# Notice number of NAs in each variable
Elev_ranges %>%
  select(elev_range_QJ, elev_range_B20, elev_range_eB) %>%
  summarize_all(~ sum(is.na(.)))

# Visualize. NOTE generally Freeman (2022) has much more expansive elevational ranges.
ggplot(data = Elev_ranges, aes(x = elev_range_QJ, y = elev_range_eB)) +
  geom_point() +
  geom_smooth() +
  geom_abline(slope = 1, color = "red") +
  coord_fixed() +
  labs(x = "Quintero & Jetz (2018)", y = "Freeman et al. (2022)")

# If you wanted to fill in blanks of QJ with ranges from another source.. Could consider applying a correction based on the beta estimate(?)
summary(lm(elev_range_eB ~ elev_range_QJ, data = Elev_ranges))

# NOTE::These are the species that don't match up between the Avonet BirdLife taxonomy & the Bird 2020.. I did try with BirdTree & eBird but they had even fewer matches compared to BirdLife
Tax_df3 %>%
  select(Species_bl) %>%
  anti_join(bird20t, join_by("Species_bl" == "Scientific.name"))

# TO DO:When you have guide books could be worth checking a few extreme ranges to confirm that things looking OK
Elev_ranges %>% filter(elev_range_comb < 500 | elev_range_comb > 4000)

# NOTE:: This still does not provide elevational ranges for all species, likely due to name changes in BirdLife & some missing elevational ranges. Could check the species missing from Freeman (see above)
Miss_elev <- Elev_ranges %>% filter(is.na(elev_range_comb)) %>% # View()
  pull(Species_ayerbe)
Tax_df3 %>%
  filter(Species_ayerbe %in% Miss_elev) %>%
  select(Species_ayerbe, Common.name, Species_bl, Species_bt, Species_eb) %>%
  distinct()

# CHECK:: There are some species Ayerbe that have multiple Bird Tree species equivalents, and thus can have multiple Min & max elevations. It makes sense to combine these elevations since it is a single species according to Ayerbe.. Thus in the case of Chlorostilbon mellisugus the min & max should be 0 to 2000. The only other species this is relevant for is Ramphastos ambiguus
Tax_df3[, c("Species_ayerbe", "Species_bt", "Match.type")] %>%
  left_join(Col_elev_QJ, join_by("Species_bt" == "Species")) %>%
  filter(Species_ayerbe == "Chlorostilbon mellisugus")
Elev_ranges %>%
  filter(Species_ayerbe == "Chlorostilbon mellisugus") %>%
  select(matches("QJ|comb"))

# For this reason the '1BL to many BT' matches are not an issue. 'Many BL to 1BT' are also not an issue b/c the one BT matches cleanly with the QJ database
table(Elev_ranges$Match.type)

# >Add elev range to FT df -------------------------------------------------
# Merge with functional traits database
Avo_traits_final <- Avo_traits_form %>% full_join(Elev_ranges[, c("Species_ayerbe", "elev_range_comb")])

# Save & export -----------------------------------------------------------
# Export Avonet morphology table
# write.xlsx(Avo_traits_final, "Intermediate_products/Excels/Avo_traits_final.xlsx", sheetName = "Traits", row.names = F, showNA = FALSE)

names(Elev_ranges)

rm(list = ls()[!(ls() %in% c("Elev_ranges", "Avo_traits_final"))])
save.image(paste0("Rdata/Traits_elev_", format(Sys.Date(), "%m.%d.%y"), ".Rdata"))
