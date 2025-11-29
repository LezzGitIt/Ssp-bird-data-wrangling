## PhD birds in silvopastoral landscapes##
## Data wrangling 00c -- Functional traits (FT) & elevational ranges
## This script generates the outputs related to functional traits (Avo_traits_final), & elevational ranges (Elev_ranges) that will be used in future scripts 

# Contents
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

## Load data and custom functions
load("Rdata/Taxonomy_09.29.25.Rdata")
Bird_pcs_all <-  read_csv(file = "Derived/Excels/Bird_pcs_all.csv")

# Traits from Bird et al. 2020, likely want to add some longevity traits here. Maybe from Wolfe et al 2025 too? It will be interesting to see if species that are on the k-side of the r-k continuum are impacted more greatly than r-selected species.
bird20t <- read_excel("../Datasets_external/Bird_et_al_Generation_length_2020/cobi13486-sup-0003-tables3.xlsx")
bird20t <- bird20t %>%
  rename_with(make.names) %>%
  rename(Min_B20 = Minimum.altitude, Max_B20 = Maximum.altitude)

source("/Users/aaronskinner/Library/CloudStorage/OneDrive-UBC/Grad_School/Rcookbook/Themes_funs.R")

# Functional traits database ----------------------------------------------
# Used a loop to ensure that we are taking trait data from a single species instead of a subspecies. For example, we want to ensure we're grabbing from BirdTree for Momotus momota and not the BirdLife subspecies
Tax_df2 %>% filter(Species_ayerbe == "Momotus momota")

# For loop create functional traits data base
SpMT <- distinct(Tax_df2, Species_ayerbe, Match_type) # Species match type
Avo_traits_loop <- vector("list", length = nrow(SpMT))

for (i in 1:length(SpMT$Species_ayerbe)) {
  print(i)
  if (!is.na(SpMT$Match_type[i]) & SpMT$Match_type[i] %in% c("1BL to 1BT", "Many BL to 1BT")) {
    Avo_traits_loop[[i]] <- SpMT[i, ] %>% left_join(Avo_traits_l$BirdTree,
                                                    by = join_by("Species_ayerbe" == "Species")
    )
    Avo_traits_loop[[i]]$Source <- "BT"
  }
  # There are some cases where Species_ayerbe is not found in SpeciesBL.. This resulted in rows of NAs
  if (!is.na(SpMT$Match_type[i]) & SpMT$Species_ayerbe[i] %in% Tax_df2$Species_bl & SpMT$Match_type[i] %in% c("1BL to 1BT", "1BL to many BT")) {
    Avo_traits_loop[[i]] <- SpMT[i, ] %>% left_join(Avo_traits_l$BirdLife,
                                                    by = join_by("Species_ayerbe" == "Species")
    )
    Avo_traits_loop[[i]]$Source <- "BL"
  }
  # Adding is.null creates the 1 row df with NAs, allowing next if() statement to work
  if (is.na(SpMT$Match_type[i]) | is.null(Avo_traits_loop[[i]])) {
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
  select(-c(Sequence, Avibase.ID, Species.Status)) %>%
  mutate(
    Habitat.Density = as.factor(Habitat.Density),
    Migration = as.factor(Migration),
    across(c(ends_with("tude"), "Range.Size"), as.numeric)
  ) %>%
  mutate(across(where(is.numeric), \(x) round(x, 2))) %>%
  relocate(Source, .after = Match_type)
nrow(Avo_traits_form)
head(Avo_traits_form)

# Examine a few key traits
lapply(Avo_traits_form[17:21], table)
# Woodland (= medium stature tree-dominated habitats, including Acacia woodland, riparian woodlands, mangrove forests, forest edges, also more open parkland with scattered taller trees);
# Forest (= tall tree-dominated vegetation with more or less closed canopy, including palm forest)

# Elevational ranges ------------------------------------------------------
# Elevational ranges will be used for 2 things, to: 1) look for possible species misidentifications or other errors in the data, 2) to use as a trait representing environmental niche breadth. For #2 having a standardized source for elevations would be ideal (QJ has fewest NAs). One possibility would be to apply a correction across different sources (see below in EXTRAS, search 'correction'), another would be to use QJ estimates from across other countries

# From Hilty guidebook, thanks to Hazen
Hilty_elev <- read_xlsx(
  "../Datasets_external/Elev_ranges/Elev_ranges_Hazen.xlsx"
  ) %>% select(Species_ayerbe, contains("Hilty"), Comments)

# From Suarez Castro et al (2024)
Ayerbe_elev <- read_csv(
  "../Datasets_external/Elev_ranges/Suarez_castro_AOH_birds_table_S3_V3.csv"
) %>% rename(
  Species_bl = BirdLife..IUCN.,
  Min_ayerbe = Minimum.elevation,
  Max_ayerbe = Maximum.elevation
) #%>% select(Species_bl, ends_with("ayerbe"))


Ayerbe_elev %>% head() %>% view()

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
elev_raw <- Tax_df2 %>%
  select(Species_ayerbe, Species_bt, Species_eb, Species_bl, Match_type) %>%
  left_join(Hilty_elev,      by = "Species_ayerbe") %>%
  left_join(Col_elev_QJ,     by = c("Species_bt" = "Species")) %>%
  #left_join(bird20t,         by = c("Species_bl" = "Scientific.name")) %>%
  left_join(Ayerbe_elev) %>%
  left_join(
    Free22_Co %>% select(scientific.name, Min_eB, Max_eB),
    by = c("Species_eb" = "scientific.name")
  ) %>% distinct(
    Species_ayerbe, Species_bt, Species_eb, Species_bl, 
    Min_Hilty, Max_Hilty, Elev_range_Hilty, 
    Min_QJ, Max_QJ,
    #Min_B20, Max_B20, 
    Min_ayerbe, Max_ayerbe,
    Min_eB, Max_eB, 
    Match_type, Year
  )

# Custom functions to take the minimum value or return NA 
safe_min <- function(x) if (all(is.na(x))) NA_real_ else min(x, na.rm = TRUE)
safe_max <- function(x) if (all(is.na(x))) NA_real_ else max(x, na.rm = TRUE)

#Reformatted code for brevity and clarity. Implemented a clearer deviation in goals between the 'Elev_min_max' tibble and the 'Elev_ranges' tibble. Leverage Ayerbe's elevational ranges from Suarez_castro et al (2024) paper. 

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

# Generate tbl with elevational ranges according to each source
Elev_ranges <- Elev_min_max %>%
  mutate(
    Elev_range_Hilty = Max_Hilty - Min_Hilty,
    Elev_range_ayerbe = Max_ayerbe - Min_ayerbe,
    Elev_range_QJ    = Max_QJ    - Min_QJ,
    Elev_range_eB    = Max_eB    - Min_eB,
    Elev_range_Hilty = ifelse(Elev_range_Hilty == 0, NA, Elev_range_Hilty), 
    # Combine Hilty, QJ, ayerbe, and Freeman (2022), all Colombia-specific, into a single column.
    Elev_range_comb  = coalesce(Elev_range_Hilty, Elev_range_ayerbe, Elev_range_QJ)
  )

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

# NOTE::These are the species that don't match up between the Avonet BirdLife taxonomy & the Bird 2020.. I did try with BirdTree & eBird but they had even fewer matches compared to BirdLife
Tax_df2 %>%
  select(Species_bl) %>%
  anti_join(bird20t, join_by("Species_bl" == "Scientific.name"))

# NOTE:: This still does not provide elevational ranges for all species, likely due to name changes in BirdLife & some missing elevational ranges. Could check the species missing from Freeman (see above)
Miss_elev <- Elev_ranges %>% 
  filter(is.na(Elev_range_comb)) %>% # View()
  pull(Species_ayerbe)
Tax_df2 %>%
  filter(Species_ayerbe %in% Miss_elev) %>%
  select(Species_ayerbe, Common_name, Species_bl, Species_bt, Species_eb) %>%
  distinct()

# CHECK:: There are some species Ayerbe that have multiple Bird Tree species equivalents, and thus can have multiple Min & max elevations. It makes sense to combine these elevations since it is a single species according to Ayerbe.. Thus in the case of Chlorostilbon mellisugus the min & max should be 0 to 2000. The only other species this is relevant for is Ramphastos ambiguus
Tax_df2[, c("Species_ayerbe", "Species_bt", "Match_type")] %>%
  left_join(Col_elev_QJ, join_by("Species_bt" == "Species")) %>%
  filter(Species_ayerbe == "Chlorostilbon mellisugus")
Elev_ranges %>%
  filter(Species_ayerbe == "Chlorostilbon mellisugus") %>%
  select(matches("comb|Elev_range"))

# For this reason the '1BL to many BT' matches are not an issue. 'Many BL to 1BT' are also not an issue b/c the one BT matches cleanly with the QJ database
table(elev_raw$Match_type)

# >Add elev range to FT df -------------------------------------------------
# Merge with functional traits database
Avo_traits_final <- Avo_traits_form %>%
  full_join(
    Elev_ranges[,c("Species_ayerbe", "Elev_range_comb", "Source_comb_elev")]
    ) %>% 
  tibble()

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
## Export functional traits (primarily from Avonet)
# Export as Excel 
Avo_traits_final %>% as.data.frame() #%>% 
  #write.xlsx("Derived/Excels/Avo_traits_final.xlsx", sheetName = "Traits", row.names = F, showNA = FALSE)

## Export elevational range data for Hazen
# Create dataframe
Elev_hazen_exp <- Tax_df2 %>% 
  distinct(Order_gbif, Family_gbif, Species_ayerbe) %>% 
  full_join(Elev_ranges) %>% 
  left_join(Hilty_elev) %>% 
  select(-c(matches("comb|_bt|_bl"), Match_type, Species_eb)) %>% 
  mutate(Diff_QJ_B20 = abs(Elev_range_QJ - Elev_range_B20)) %>% 
  relocate(c(Diff_QJ_eB, Closest_to), .after = Diff_QJ_B20) %>%
  arrange(desc(is.na(Elev_range_QJ)), 
          desc(is.na(Diff_QJ_B20)), 
          desc(Diff_QJ_B20))

# Filter & export dataframe for Hazen
if(FALSE){
  Elev_hazen_exp %>% 
    filter(is.na(Diff_QJ_B20) | Diff_QJ_eB > 500 | Diff_QJ_B20 > 500) %>% 
    as.data.frame() %>%
    xlsx::write.xlsx("Derived/Excels/Elev_ranges_diff_qj_ebird.xlsx", 
                     row.names = FALSE, showNA = FALSE)
}

## Export the final functional traits database
# The species that were observed in point counts
Spp_pc <- Tax_pcs %>% pull(Species_ayerbe) %>% 
  unique()

# Export as csv 
Avo_traits_final %>% 
  filter(Species_ayerbe %in% Spp_pc) %>%
  rename_with(.cols = everything(), .fn = ~str_remove(., "_comb")) %>%
  write_csv(file = "Derived/Excels/Functional_traits.csv")

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

