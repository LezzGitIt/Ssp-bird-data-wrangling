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
library(sf)
library(chron)
library(ggpubr)
library(cowplot)
library(conflicted)
ggplot2::theme_set(theme_cowplot())
conflicts_prefer(dplyr::select)
conflicts_prefer(dplyr::filter)

#source("/Users/aaronskinner/Library/CloudStorage/OneDrive-UBC/Grad_School/Rcookbook/Themes_funs.R")

## Load data and custom functions
Bird_pcs_all <-  read_csv(file = "Derived/Excels/Bird_pcs/Bird_pcs_all.csv")
Tax_df <- read_csv("Derived/Excels/Taxonomy/Taxonomy_all.csv") #%>% 
#mutate(Avibase.ID = str_sub(concept_id_avilist, 1, 8)) # Match with Avonet

# Avonet list  ------------------------------------------------------------
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

Avo_traits_l$BirdLife <- Avo_traits_l$BirdLife %>% 
  mutate(Avibase.ID = str_remove(Avibase.ID, "AVIBASE-"))

# Match with Avonet
Ft_df <- Tax_df %>% 
  distinct(Species_ayerbe, Species_bl) %>%
  left_join(Avo_traits_l$BirdLife, by = join_by("Species_bl" == "Species")) 
# A single species from each species SACC
Ft_df %>% count(Species_ayerbe, sort = T)

# Format & remove extraneous columns
names(Ft_df)
Ft_df2 <- Ft_df %>%
  select(-c(Sequence, Avibase.ID)) %>%
  mutate(
    Habitat.Density = as.factor(Habitat.Density),
    Migration = as.factor(Migration),
    across(c(ends_with("tude"), "Range.Size"), as.numeric)
  ) %>%
  mutate(across(where(is.numeric), \(x) round(x, 2))) 

# Examine a few key traits
lapply(Ft_df2[17:21], table)
# Woodland (= medium stature tree-dominated habitats, including Acacia woodland, riparian woodlands, mangrove forests, forest edges, also more open parkland with scattered taller trees);
# Forest (= tall tree-dominated vegetation with more or less closed canopy, including palm forest)

# Elevational ranges ------------------------------------------------------
# Elevational ranges will be used for 2 things, to: 1) look for possible species misidentifications or other errors in the data, 2) to use as a trait representing environmental niche breadth. For #2 having a standardized source for elevations would be ideal (QJ has fewest NAs). One possibility would be to apply a correction across different sources (see below in EXTRAS, search 'correction'), another would be to use QJ estimates from across other countries

# From Hilty guidebook, thanks to Hazen
Hilty_elev <- read_xlsx(
  "../Datasets_external/Elev_ranges/Elev_ranges_Hazen.xlsx"
) %>% select(Species_ayerbe, contains("Hilty"))

# From Suarez Castro et al (2024)
Ayerbe_elev <- read_csv(
  "../Datasets_external/Elev_ranges/Suarez_castro_AOH_birds_table_S3_V3.csv"
) %>% rename(
  Species_bl = BirdLife..IUCN.,
  Min_ayerbe = Minimum.elevation,
  Max_ayerbe = Maximum.elevation
) #%>% select(Species_bl, ends_with("ayerbe"))

# Traits from Bird et al. 2020, likely want to add some longevity traits here. Maybe from Wolfe et al 2025 too? It would be interesting to see if species that are on the k-side of the r-k continuum are impacted more greatly than r-selected species.
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
    Ayerbe_elev %>% select(-Species_bl), 
    by = join_by("Species_ayerbe" == "Scientific.Name")) %>%
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

# >Calculate ayerbe -------------------------------------------------------
## Calculate using the same methodology as Suarez Castro et al (2024)
# TEMPORARY - Until Hazen provides results from Ayerbe field guide
Spp_elev_to_extract <- elev_raw %>% 
  select(contains("ayerbe")) %>% 
  filter(is.na(Min_ayerbe)) %>% 
  pull(Species_ayerbe)

# Use Ayerbe shapefiles to calculate the elevational range per species
Ayerbe_path <- "../Geospatial_data/Ayerbe_shapefiles_1890spp/"
#Elev_90m <- geodata::elevation_3s(lat = 4, lon = -75, path = tempdir())
Elev_1km <- geodata::elevation_30s(country = "COL", path = tempdir())
ggplot() + geom_spatraster(data = Elev_1km)

# Confirmed that smoothr::drop_crumbs does reduce the area of polygons 
thresh <- units::set_units(1, km^2) 
Ayerbe_calc_tbl <- map_dfr(Spp_elev_to_extract, \(Spp){
  sv <- terra::vect(paste0(Ayerbe_path, Spp, ".shp"))  
  sv_clean <- sv %>% makeValid() %>%
    smoothr::drop_crumbs(thresh) # drop small polygons
  rast_range <- terra::extract(Elev_1km, sv_clean)[[2]]
  tibble(Species_ayerbe = Spp,
         elev05 = quantile(rast_range, .05, na.rm = TRUE),
         elev95 = quantile(rast_range, .95, na.rm = TRUE))
})

elev_raw2 <- elev_raw %>% left_join(Ayerbe_calc_tbl) %>% 
  mutate(Min_ayerbe = coalesce(Min_ayerbe, elev05), 
         Max_ayerbe = coalesce(Max_ayerbe, elev95)) %>% 
  select(-c(elev05, elev95)) %>% 
  group_by(Species_ayerbe) %>%
  fill(c(Min_QJ, Max_QJ, Year), .direction = "downup") %>% 
  ungroup() %>% 
  distinct() 
elev_raw2 %>% count(Species_ayerbe, sort = T)

# >Min max ----------------------------------------------------------------
# Custom functions to take the minimum value or return NA 
safe_min <- function(x) if (all(is.na(x))) NA_real_ else min(x, na.rm = TRUE)
safe_max <- function(x) if (all(is.na(x))) NA_real_ else max(x, na.rm = TRUE)

# Generate tbl with the minimum and maximum values across all sources. The idea behind the min and max elevation combined columns (these are the broadest elevational ranges) is this should be useful for checking specific observations with data collectors. The goal is to make the list manageable and to just consist of the species that really are likely mistakes
Elev_min_max <- elev_raw2 %>%
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
# I bet many of these are due to taxonomy... Notice nearly all of these have QJ in it, and may be very old (as far back as 1986) 
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

## CHECK:: A few of these to ensure that things are working as expected
Elev_ranges %>% select(-starts_with("Species"), -contains("B20"))
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

# >Add elev range to FT df -------------------------------------------------
# Merge with functional traits database
Ft_final <- Ft_df2 %>%
  full_join(
    Elev_final[,c("Species_ayerbe", "Elev_range_final", "Source_comb_elev")]
  ) %>% 
  tibble()

Elev_final

# Eye_size ----------------------------------------------------------------
# Load in data
path <- "../Datasets_external/Eye Size Excel Audrey Hanson.xlsx"
Aaron_tax <- read_xlsx(path, sheet = "Taxonomy") %>% 
  clean_names()
Ausprey_raw <- read_xlsx(path, sheet = "Ausprey, 2024 Raw Data") %>% 
  clean_names()

## Format Ausprey
# Take mean per species
Ausprey_form <- Ausprey_raw %>% 
  summarize(Td_mean = mean(td1), .by = species_clements_2018)

# Join
Aaron_join <- Aaron_tax %>% 
  left_join(Ausprey_form, by = join_by("species_eb" == "species_clements_2018"))

## Inspect 
# 292 species with eye size data 
Aaron_join %>% filter(!is.na(Td_mean)) %>% 
  distinct(species_ayerbe, Td_mean)

# But also many without - are these true absences from Ausprey's data set? Issues with taxonomy? Or coding errors? 
Aaron_join %>% filter(is.na(Td_mean)) %>% 
  distinct(species_ayerbe, Td_mean)

# Save & export -----------------------------------------------------------
stop()

# Export functional traits file as csv 
Ft_final %>%
  rename_with(.cols = everything(), .fn = ~str_remove(., "_comb")) %>%
  write_csv(file = "Derived/Excels/Functional_traits.csv")

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

HBW <- data.frame(read_excel("/Users/aaronskinner/Library/CloudStorage/OneDrive-UBC/Grad_School/PhD/Analysis/Taxonomy/Handbook of the Birds of the World and BirdLife International Digital Checklist of the Birds of the World_Version_7.xlsx", sheet = "HBW-BirdLife v7 ", skip = 3))
HBWsp <- HBW %>% filter(`Subsp..Seq.` == 0) # Reduce file down just to recognized species
head(HBWsp)
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