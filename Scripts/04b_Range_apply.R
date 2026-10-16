## PhD birds in silvopastoral landscapes ##
## Data wrangling 04b -- Apply the curated out-of-range decisions to the observations

## Reads the manually reviewed remove/change list (Spp_remove_change.csv, built from the maps and Excels that 04a_Range_screening.R generates) and applies it to Bird_pcs_all.csv, writing Bird_pcs_dist.csv for 05/06.
## The list is deposited as DataS1/Range-review-decisions.csv and the logic lives in flag_range_decisions() (Scripts/Pipeline_fns.R), so readers can apply the same decisions to DataS1/Bird-point-counts_all.csv, which keeps the original identifications.
## Deterministic and light (tidyverse only) -- run it every pipeline pass; 04a is the heavy geospatial review step and is run only when the range screening needs refreshing.

# Load libraries & data ---------------------------------------------------
library(tidyverse)
library(conflicted)
conflicts_prefer(dplyr::select)
conflicts_prefer(dplyr::filter)
source("Scripts/Pipeline_fns.R")

# guess_max = Inf: Recording is sparse (only ~180 non-NA, near the end) so the default type guess misreads it
Bird_pcs_all <- read_csv("Derived/Excels/Bird_pcs/Bird_pcs_all.csv", guess_max = Inf)
Site_covs <- read_csv("Derived/Excels/Site_covs.csv")

# Manually generated list of species to remove / change, from the 04a review. Elev_max limits a decision to observations at or below that elevation (e.g. Henicorhina leucophrys is within range above 600 m around Santa Marta)
Remove_change <- read_csv("Derived/Excels/Spp_remove_change.csv")

# Apply decisions ---------------------------------------------------------
Bird_pcs_flagged <- flag_range_decisions(Bird_pcs_all, Remove_change, Site_covs)

Bird_pcs_dist <- Bird_pcs_flagged %>%
  filter(is.na(Range_decision) | Range_decision != "Remove") %>%
  select(-c(Species_reported, Range_decision))

# Examine / confirm --------------------------------------------------
## Observations and species affected by each decision type
Bird_pcs_flagged %>%
  filter(!is.na(Range_decision)) %>%
  group_by(Range_decision) %>%
  summarize(Observations = n(), Species = n_distinct(Species_reported))

## Every global-remove species should now be absent (expect 0 rows)
Spp_rm <- Remove_change %>%
  filter(Decision == "Remove" & is.na(Departments_affected)) %>%
  pull(Species_ayerbe)
Bird_pcs_dist %>% filter(Species_ayerbe %in% Spp_rm)

## Example: Myiarchus apicalis -> ferox in Meta, apicalis removed in Guajira
Bird_pcs_flagged %>% filter(Species_reported == "Myiarchus apicalis") %>% count(Species_ayerbe, Range_decision)

# Export ------------------------------------------------------------------
Bird_pcs_dist %>% write_csv("Derived/Excels/Bird_pcs/Bird_pcs_dist.csv")
