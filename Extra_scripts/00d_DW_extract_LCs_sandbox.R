## PhD birds in silvopastoral landscapes##
# Backup file for 
# The purpose of this script is to generate files that Mathilde will need to digitize landcover from satellite imagery in QGIS, as well as to examine the files that she digitized & compare them to other publicly available spatial layers

## NOTE:: Actively developing this file, at present it is in a state of disarray

# TO DO:
# 1) Could it make sense to just combine all these files right away? Search LCs_comb
# 2) Once we've buffered live fences, could pull data_year(s) from the surrounding polygon(s)
# 3) If there are overlaps, could union (aggregate I think?) polygons of the same landcover & data_year

# NOTE:: I copied & pasted most of the code from section 3 to section 2, so will likely want to go back in and clean up. Probably best to make a new script for the comparison of Mathilde's data & Brandt's data.

# Load libraries & data ---------------------------------------------------
#library(geobuffer)
library(terra)
library(tidyterra)
library(sf)
library(tidyverse)
library(janitor)
library(cowplot)
library(gridExtra)
library(ggpubr)
library(conflicted)
ggplot2::theme_set(theme_cowplot())
conflicts_prefer(dplyr::select)
conflicts_prefer(dplyr::filter)

source("/Users/aaronskinner/Library/CloudStorage/OneDrive-UBC/Grad_School/Rcookbook/Themes_funs.R")
load("Rdata/the_basics_05.10.25.Rdata")
load("Rdata/Lsm_l.Rdata")

# Extract LC covs ---------------------------------------------------------
# Aggregate polygons by lc & id group (or just lc?) first, & then try erasing? I TRIED AGGREGATING JUST BY LC_TYP AND HAD NO LUCK
# Focus on Id_group first, try to aggregate by Id_group as well
# Figure out a workflow with a single Id_group (can more easily plot), then expand
# Once Mathilde uploads original polygons, bring this in & remove all invalid polygons & then try workflow in terra or (especially sf)
# Even without the old polygons, could try workflow in sf again, using st_union()
# Go back to step 3
# Go back to remove polygon step -- are the rows in the dataframe same as # of polygons ?
# Plot LC_holes2, are there holes in it? If LC_middle is good , you could continue trying to iron out the rest of work flow, something might occur to you?
# See erase() help file, is there an example?
# Look back at old 'Misc' section

# Terra approach --------------------------------------------------------------
## Goal:: Each point count needs landcover values in 300m radius that are temporally matched with when the point count was sampled

# Bring in data -----------------------------------------------------------
tbl_row <- 3 # 1 = middle, 2 = "past", 3 = "ubc"

# Create Index_tbl
Uniq_db <- unique(Bird_pcs$Uniq_db)
Index_tbl <- tibble(name = c("middle", "past", "ubc"), index = c(2,3,4), data_collector = list(Uniq_db[c(1:3,6)], c("Gaica mbd", "Cipav mbd"), "Ubc mbd"))

# Bring in Mathilde shapefiles
path <- "../../Mentorship/Digitization_Mathilde/Final_docs/final_shp_for_natalia"
files.shp <- list.files(path = path, pattern = "final") #, recursive = TRUE)
#files.shp <- files.shp[-2] # Remove old lc_middle file

shp.lc.L <- map(.x = files.shp, \(shp)
                vect(paste0(path, "/", shp)) %>%
                  clean_names())
names(shp.lc.L) <- files.shp

# Remove small polygons ---------------------------------------------------
# Calculate area 
shp.lc.L[c(2:4)] <- map(shp.lc.L[c(2:4)], \(polys_df){
  polys_df$area <- polys_df %>% expanse()
  polys_df
})

# Past -- Poly_num is not unique in the past file
shp.lc.L[[3]] <- shp.lc.L[[3]] %>% mutate(poly_num = row_number())

## Identify small polys to remove
Small_polys <- shp.lc.L[[Index_tbl[tbl_row,]$index]] %>% filter(area < 2)  

# Small polys plot
Small_pp <- map(seq_along(Small_polys), \(row){
  ggplot() + 
    geom_spatvector(data = Small_polys[row,]) +
    labs(title = Small_polys[row,]$poly_num)
})

## Visually inspect small polygons and make note of polygons that are good to keep
Small_pp

# >Dataset_specific -------------------------------------------------------
## UBC:: Polys to delete
polys_keep_ubc <- c(16, 86)
Del_polys_ubc <- Small_polys %>% pull(poly_num) 
TF <- !Del_polys_ubc %in% polys_keep_ubc

# Subset shapefile to remove prob polys UBC
shp.lc.L[[Index_tbl[tbl_row,]$index]] <- shp.lc.L[[Index_tbl[tbl_row,]$index]] %>% 
  filter(!poly_num %in% Del_polys_ubc[TF])

# ubc - Delete polys of larger size (between 2 & 100)
Del_lrg_ubc <- c(1600, 437, 191, 357, 359)
shp.lc.L[[Index_tbl[tbl_row,]$index]] <- shp.lc.L[[Index_tbl[tbl_row,]$index]] %>% filter(!poly_num %in% Del_lrg_ubc)

## Past:: Polys to delete
polys_keep_past <- c(446, 440, 419, 318, 114, 84) # but should investigate
Del_polys_past <- Small_polys %>% pull(poly_num) 
# Add 16 as it causes problems down the line
Del_polys_past[length(Del_polys_past) + 1] <- 16 #REDO THIS ONE 
TF <- !Del_polys_past %in% polys_keep_past

# Subset shapefile to remove small area polys past
shp.lc.L[[Index_tbl[tbl_row,]$index]]<-shp.lc.L[[Index_tbl[tbl_row,]$index]] %>% 
  filter(!poly_num %in% Del_polys_past[TF])

# Ensure poly_num are unique
map(shp.lc.L[c(2:4)], \(polys_df){
  polys_df %>% data.frame() %>% 
    count(poly_num, sort = T) %>% 
    filter(n > 1)
})


# Export as gpkg ----------------------------------------------------------
# Export updated files that will be read into the main DW_LC scripts
shp.lc.L[[Index_tbl[tbl_row,]$index]] %>% writeVector(paste0("../../Mentorship/Digitization_Mathilde/Final_docs/final_shp_for_natalia/Lc_", Index_tbl[[tbl_row, "name"]], "_final.gpkg"), overwrite = TRUE) #, overwrite = TRUE






## Remove invalid polygons identified in sf
# For whatever reason it shows that polygons are valid in terra, but not in sf
map(shp.lc.L, ~ table(is.valid(.x))) 
Tf_l <- map(shp.lc.L, \(shp){
  sf_shp <- sf::st_as_sf(shp) 
  sf_shp %>% st_is_valid() %>% 
    table()
  sf_shp2 <- sf_shp %>% st_make_valid()
  TF <- sf_shp2 %>% st_is_valid()
  return(TF)
}) 
valid_polys <- shp.lc.L[[2]][Tf_l[[2]], ] %>% mutate(poly_num = row_number())

# Add rownumber for easy identification & communication with Mathilde
shp.lc.L[c(2:4)] <- map(shp.lc.L[c(2:4)], \(polys){
  polys %>% mutate(poly_num = row_number())
})

# Visualize the polygon that is still invalid after st_make_valid
invalid <- shp.lc.L[[2]] %>% filter(poly_num %in% which(!Tf_l[[2]])) 
ggplot() + 
  geom_spatvector(data = invalid) 

# Could it make sense to just combine all these files right away? Polys can overlap, & we have data_year. Within a data_year (e.g., 2013) we shouldn't have overlap..
LCs_comb <- pmap(
  .l = list(shp.lc.L[2], shp.lc.L[3], shp.lc.L[4]),
  \(polys, trees, lf){
    rbind(polys, trees, lf)
  }
)
sum(sapply(shp.lc.L[2:4], nrow))

# Inconsistencies habitat: continuous & binary -------------------------------
# >Continuous --------------------------------------------------------------
## Our working assumption has been that data collectors used the predominant landcover within the point count to classify the landcover type (except when LC == Ssp, e.g. live fence). Thus, we'd expect that >50% of the manually digitized landcover within the 50m radius should be in agreement with the data collector's assigned habitat type

# Select landcovers within 25m if CIPAV & 50m otherwise, ie the appropriate buffer size
Lsm_long_buf <- Lsm_long %>% 
  filter(
    (str_starts(Id_muestreo, "C") & buffer == 25) |
      (!str_starts(Id_muestreo, "C") & buffer == 50)
  )

# Plot the digitized landcover within 25m (CIPAV) or 50m (all others) for each categorical landcover assigned by the data collectors
Lsm_long_buf %>% 
  ggplot(aes(x = Habitat_cons, y = Percent_cover, color = Lc_manual)) +
  geom_hline(yintercept = 50, linetype = "dashed") +
  geom_boxplot() + 
  geom_point(position = position_jitterdodge(jitter.width = .05), 
             alpha = .3) + 
  facet_wrap(~lc_file) + 
  theme(axis.text.x = element_text(angle = 75, hjust=1)) + 
  labs(title = "Landcover within 25/50m of point count")

# Create tibble of continuous inconsistencies (generate & filter on 'Investigate' column)
Inconsistencies_cont <- Lsm_long_buf %>% mutate(Investigate = case_when(
  Habitat_cons %in% c("Bosque", "Bosque ripario") & Lc_manual == "forest" & Percent_cover < 50 ~ "Y", 
  Habitat_cons %in% "Pastizales" & Lc_manual == "intpast" & Percent_cover < 50 ~ "Y",
  Habitat_cons %in% "Ssp" & Lc_manual == "ssp" & Percent_cover < 10 ~ "Y", 
  .default = NA
)) %>% filter(Investigate == "Y") %>% 
  left_join(Min_dist_forest) %>%
  arrange(lc_file, Habitat_cons, Lc_manual, Id_muestreo)
Inconsistencies_cont 

# >Binary -----------------------------------------------------------------
## Identify point counts that are within digitized forest polygons, but that data collectors called something other than forest (or vice a versa). Note that this was only done for the 'middle' digitized file. 
# Min_dist_forest (created in 00e_DW_Lsm.R) contains a categorical 'In_forest' column, that indicates whether the point count centroid is within a forest polygon or not. 
Inconsistencies_bin <- Pc_hab %>% distinct(Id_muestreo, Habitat_cons) %>%
  filter(!is.na(Habitat_cons)) %>%
  left_join(Min_dist_forest) %>% 
  mutate(lc_file = "middle") %>%
  filter(Habitat_cons %in% c("Bosque", "Bosque ripario") & In_forest == 0 | !Habitat_cons %in% c("Bosque", "Bosque ripario") & In_forest == 1) %>% 
  filter(!is.na(Habitat_cons)) %>% 
  arrange(desc(In_forest), desc(Dist_to_edge)) 
Inconsistencies_bin

# Create file to export
Inconsistencies_exp <- Inconsistencies_cont %>% 
  full_join(Inconsistencies_bin) %>% 
  # Given that binary check was only done in the 'middle' file, manually set to NA for the "past" file
  mutate(
    In_forest = if_else(lc_file == "past", NA_real_, In_forest),
    Dist_to_edge = if_else(lc_file == "past", NA_real_, Dist_to_edge),
    forest_typ = if_else(lc_file == "past", NA, forest_typ)
  ) %>%
  mutate(Forest_type_reviewer = NA, Revision_made = NA, Comments = NA) %>%
  select(-Investigate) %>% 
  rename(Habitat_reported = Habitat_cons)

# NOTE:: These are points that passed the 'continuous' check, but not the binary check
Inconsistencies_exp %>% filter(is.na(Percent_cover))

# Export
if(FALSE){
  Inconsistencies_exp %>% as.data.frame() %>%
    write.xlsx(file = paste0("Derived/Excels/Lsm/Inconsistencies_lc_", format(Sys.Date(), "%m.%d.%y"), ".xlsx"), showNA = FALSE, row.names = FALSE)
}

# EXPLORATION -------------------------------------------------------------

# Explore metadata  ------------------------------------------------------
## The metadata associated with the shapefiles is key for workflow
# Polygons
map(shp.lc.L[c(2:4)], \(polys){
  names(polys)
})

# Trees percent
map(shp.lc.L[c(2:4)], \(polys){
  table(polys$trees_perc)
})

# LCs with trees_perc != NA, these should only be distrees? 
map(shp.lc.L[c(2:4)], \(polys){
  polys %>%
    filter(!is.na(trees_perc)) %>%
    pull(lc_typ) %>%
    table()
})

# Number of polygons 
## NEED TO DO FOR MIDDLE UBC & PAST AS WELL
map(shp.lc.L[c(2:4)], \(polys){
  polys %>% filter(lc_typ == "distrees" & is.na(trees_perc))
})

# Forest type
# SHOULD BE ONLY FOREST
map(shp.lc.L[c(2:4)], \(polys){
  polys %>%
    filter(!is.na(forest_typ)) %>%
    pull(lc_typ) %>%
    table()
})

# Crop type
map(shp.lc.L[c(2:4)], \(polys){
  polys %>%
    filter(!is.na(crop_typ)) %>%
    pull(lc_typ) %>%
    table()
})

# LC type
# PLEASE STANDARDIZE (E.G. SETOSFOR)
map(shp.lc.L[c(2:4)], \(polys){
  table(polys$lc_typ)
})

# Data_years#
# Data_year
map(shp.lc.L[c(2:4)], \(polys){
  tabyl(polys$data_year)
})

# Data_year2
map(shp.lc.L[c(2:4)], \(polys){
  table(polys$data_year2)
})

# Data_year3
map(shp.lc.L[c(2:4)], \(polys){
  table(polys$data_year3)
})

# NOTE:: With data_year the first value is not always the earliest.. For example in some data_year1 is 2017 & data_year2 is 2016
shp.lc.L$landcovers_middle_final %>% 
  filter(data_year == 2016 & !is.na(data_year2))
shp.lc.L$landcovers_middle_shapefile %>%
  filter(data_year2 == 2016) %>%
  pull(data_year)

# Image date
map(shp.lc.L[c(2:4)], \(polys){
  tabyl(polys$image_date)
})

# DOES IT MAKE SENSE THAT THERE ARE POLYGONS WITH DATA YEAR = 2022 IN THIS SHAPEFILE?
shp.lc.L$landcovers_middle_final %>% filter(data_year == 2022)
shp.lc.L$landcovers_middle_final %>% filter(data_year2 == 2022)
shp.lc.L$landcovers_middle_final %>% filter(!is.na(data_year3))

# Or do it by database instead of by column across all databases
lapply(shp.lc.L$landcovers_middle_final, table)
lapply(shp.lc.L$landcovers_middle_ubc_final, table)
lapply(shp.lc.L$landcovers_past_final, table)

# Check NAs and NaN (not a number)
lapply(shp.lc.L$landcovers_middle_final, \(col) table(is.na(col)))
lapply(shp.lc.L$landcovers_middle_final, \(col) table(is.nan(col)))

## Linear features
map(shp.lc.L[c(5:7)], \(lf){
  names(lf)
})

# CHECK:: Ensure tree_shrub category has only F (forest remnant), S (shrub), and T (tree)
map(shp.lc.L[c(5:7)], \(lf){
  table(lf$tree_shrub)
})

# >Explore invalid polygons -----------------------------------------
# NOTE:: several polygons are not valid
map(shp.lc.L, \(shp){
  table(is.valid(shp))
})

## We lose 4 polygons in the middle shapefile
# Before makeValid
map(shp.lc.L[c(2:4)], \(polys){
  polys %>% nrow()
})

# Now only 3439 polys using makeValid
map(shp.lc.L[c(2:4)], \(polys){
  polys %>%
    makeValid() %>%
    nrow()
})

# These are the 4 polygon numbers that are lost, & causing mismatch with number of rows & number of polygons
valid_nums <- shp.lc.L$landcovers_middle_shapefile %>%
  makeValid() %>%
  pull(poly_num)
prob_polys <- setdiff(1:3443, valid_nums) # problematic polygons
# Note extent is NA, geometry is none, causing major issues.
shp.lc.L$landcovers_middle_shapefile[prob_polys, ] %>% head(n = 4)

# Can't plot these 4 polygons
ggplot() +
  geom_spatvector(data = shp.lc.L$landcovers_middle_shapefile[prob_polys, ])

# Generate list of invalid polygons
invalid <- map(shp.lc.L[2:4], \(polys){
  polys %>%
    mutate(is_valid = is.valid(.)) %>%
    filter(!is_valid)
})

# Make polygons valid & then plot
# It seems like the polygons still have weird stuff going on, may be best to have Mathilde go in & fix these issues?
valid <- map(invalid, \(polys_df){
  makeValid(polys_df)
})

# Plot invalid polygons, the # corresponds to the row numbers in the attribute table of QGIS
Invalid_poly_plots <- map2(invalid, valid, \(invalid_polys, valid_polys)
                           map(seq_len(nrow(valid_polys)), function(i) {
                             invalid_plot <- ggplot() +
                               geom_spatvector(data = invalid_polys[i, ], fill = "red", color = "black") +
                               labs( # title = list_name,
                                 subtitle = paste("Invalid Polygon", pull(invalid_polys[i, ], poly_num))
                               ) +
                               theme_min
                             valid_plot <- ggplot() +
                               geom_spatvector(data = valid_polys[i, ], fill = "red", color = "black") +
                               labs( # title = list_name,
                                 subtitle = paste("Valid Polygon", pull(valid_polys[i, ], poly_num))
                               ) +
                               theme_min
                             ggarrange(invalid_plot, valid_plot)
                           }))

# Print 3 PDFs (for each shapefile) with the invalid polygons in the left column, and the valid column (after using makeValid) on the right side
imap(Invalid_poly_plots, \(poly_plots, name){
  pdf(file = paste0("/Users/aaronskinner/Library/CloudStorage/OneDrive-UBC/Grad_School/PhD/Mentorship/Digitization_Mathilde/Feedback/invalid_polygons_", name, format(Sys.Date(), "%m.%d.%y"), ".pdf"), width = 8.5, height = 11, bg = "white")
  print(marrangeGrob(grobs = poly_plots, ncol = 3, nrow = 3, layout_matrix = matrix(1:3, 3, 1, TRUE)))
  dev.off()
})

# >Make polygons valid -----------------------------------------------------
# SHORT TERM SOLUTION -- Delete all invalid polygons
# LONG TERM SOLUTION -- Need to redraw

# Identify the problematic polygons
TF <- map(shp.lc.L[c(2:4)], \(polys){
  polys %>% is.valid()
})

# Remove all non-valid polygons to iron out the rest of the workflow
shp.lc.L[c(2:4)] <- map2(shp.lc.L[c(2:4)], TF, \(polys, TF){
  polys[TF, ]
})

# Make polygon files valid
shp.lc.L[c(2:4)] <- map(shp.lc.L[c(2:4)], \(polys){
  polys %>% makeValid()
})

# NOTE:: All polygons are valid at this point
map(shp.lc.L, \(shp){
  table(is.valid(shp))
})

# >Overlapping polygons ----------------------------------------------------
## Mathilde says, 'In one shapefile, polygons cannot overlap, it is not something that Q allows you to do'. However, some polygons DO overlap.

# Problematic coordinates given in the error message
problematic_coords <- matrix(c(
  #-73.793410454502194, 3.6727849683307507, #,
  -73.793538544036281, 3.6730780784730706
  #-75.07352327711844, 10.800882487507739 # fixed
), ncol = 2, byrow = TRUE)
problematic_points <- terra::vect(problematic_coords, type = "points", crs = crs(shp.lc.L[[2]]))
problematic_buffers <- problematic_points %>% buffer(.5)
extent <- problematic_buffers %>% ext()
extent2 <- extent %>% extend(.1)

# Identify the polygons that overlap with the given points 
TF <- terra::relate(shp.lc.L[[2]], problematic_points, 
                    pairs = FALSE, relation = "intersects") 
table(TF[,1])
Prob_poly_id <- which(TF[,1])
Prob_poly <- shp.lc.L[[2]] %>% filter(poly_num %in% Prob_poly_id)

# Use this polygon to bring in additional surrounding polygons:
TF <- terra::relate(shp.lc.L[[2]], Prob_poly, 
                    pairs = FALSE, relation = "intersects") 
table(TF[,1])
Prob_poly_ids <- which(TF[,1])
Prob_polys <- shp.lc.L[[2]] %>% filter(poly_num %in% Prob_poly_ids)

# Point the problematic point and the surrounding polygons 
ggplot() +
  geom_spatvector(data = Prob_polys) +
  geom_spatvector(data = Prob_poly, fill = "red") +
  geom_spatvector(data = problematic_points, color = "blue", alpha = 0.5) + 
  scale_x_continuous(labels = scales::label_number(accuracy = 0.1)) +  # Round to 1 decimal place
  labs(x = "Longitude",
       y = "Latitude") +
  theme_minimal() 

## REMOVE: Prob_polygons
shp.lc.L[[2]] <- shp.lc.L[[2]] %>% filter(!poly_num %in% Prob_poly_ids)

# Find the polygon containing this point
Prob_poly <- setNames(Prob_poly, Prob_poly)

# Examine these polygons 
shp.lc.L[[2]] %>% filter(poly_num %in% Prob_poly) %>% 
  select(where(~ !all(is.na(.))))

# NOTE:: Several of these do have very small areas & would be removed if small areas were removed
shp.lc.L$landcovers_middle_final %>%
  filter(!poly_num %in% Prob_poly) %>%
  pull(area)

# Simplify the problematic polygons
shp.lc.L[[2]][problematic_polygons2] <- simplifyGeom(
  shp.lc.L[[2]][problematic_polygons2], 
  tolerance = 0.001  # Adjust tolerance as needed
)

# Polygon 1 is problematic!
shp.lc.L[[2]][1] <- simplifyGeom(
  shp.lc.L[[2]][1] 
  #tolerance = 0.001  # Adjust tolerance as needed
)

# Remove the problematic polygons & run relate() again to see if it works
shp.lc.L[[2]] %>% 
  #simplifyGeom() %>%
  # expanse() %>% sum() #52642450 - 48569810 (after simplify)
  filter(!poly_num %in% Prob_poly_ids) %>%
  relate(relation = "overlaps", pairs = TRUE)

prob_poly_plots <- imap(problematic_polygons2, \(prob_poly, names){
  shp.lc.L$landcovers_middle_final %>%
    terra::simplifyGeom() %>%
    filter(poly_num %in% prob_poly) %>%
    ggplot() +
    geom_spatvector() +
    labs(title = names) +
    theme_min
})

ggpubr::ggarrange(prob_poly_plots[[1]], prob_poly_plots[[2]])
ggpubr::ggarrange(prob_poly_plots[[1]], prob_poly_plots[[2]], 
                  prob_poly_plots[[3]], prob_poly_plots[[4]])

?simplifyGeom

# Plot problematic polygons
shp.lc.L$landcovers_middle_final %>%
  filter(poly_num %in% problematic_polygons2) %>%
  ggplot() +
  geom_spatvector() + #aes(fill = factor(poly_num)), alpha = .5) +
  guides(color = "none")

# NOTE:: With these few polygons removed ALL relationships are valid!
relationships <- c("intersects", "touches", "crosses", "overlaps", "within", "contains", "covers", "coveredby", "disjoint")
map(relationships, \(relat){
  shp.lc.L[[2]] %>%
    filter(!poly_num %in% problematic_polygons2) %>%
    relate(relation = relat, pairs = TRUE)
}, .progress = TRUE)

## Delete overlapping polygons
no_overlap <- map(shp.lc.L[c(3:4)], \(polys){
  erase(polys, sequential = TRUE)
})

# Compare areas
map_dbl(shp.lc.L[c(3:4)], \(polys){
  sum(expanse(polys))
})
map_dbl(no_overlap, \(polys){
  sum(expanse(polys))
})

# Determine if any polygons are completely isolated (not touching any other polygons)
touching <- map(shp.lc.L[c(3:4)], \(polys){
  relate(polys, relation = "intersects", pairs = TRUE) # intersects == if they touch, overlap, or are within
})

# Detect the polygons that are completely isolated
setdiff(1:475, unique(touching$landcovers_middle_UBC_shapefile[, 1]))
setdiff(1:634, unique(touching$landcovers_past_shapefile[, 1]))

# DELETE
# Try snapping to see if it fixes validity -- snap may be making invalid geometries that cannot be fixed with makeValid()
# snapped <- map(shp.lc.L[2:4], \(sf){
# terra::snap(sf, tolerance = 1)
# })

map(snapped, \(sf){
  table(is.valid(sf))
})
# Through here

# >Format -----------------------------------------------------------------
# Formatting of dataframe
shp.lc.L[c(2:4)] <- map(shp.lc.L[c(2:4)], \(polys){
  polys %>% mutate(
    image_date = as.Date(image_date),
    img_date_yr = lubridate::years(image_date),
    trees_perc = as.numeric(trees_perc)
  )
})

# Format linear features & individual trees
# Add LC
shp.lc.L[c(1, 5:7)] <- map2(
  shp.lc.L[c(1, 5:7)], c("distrees", rep("livefence", 3)),
  \(lf, lc_typ){
    lf %>% mutate(lc_typ = lc_typ)
  }
)

# Remove empty id column & set the trees percentage = 100
shp.lc.L[[1]] <- shp.lc.L[[1]] %>%
  select(-id) %>%
  mutate(trees_perc = 100)

# >Buffer -----------------------------------------------------------------
# Buffer trees & live fences (lf) by appropriate distances.
# Discuss buffers with Mathilde - "S" for shrub should be 2m radius (NOT diameter), "T" should be 5m wide, and "F" should be ~Xm radius, indiv trees should be radius 5m at low elevation & 3m at high elevation? See Mathilde's metadata file
# STILL NEED TO DIFFERENTIATE BUFFER SIZE BY TREE_SHRUB COLUMN
trees_lf_buff <- pmap(
  .l = list(shp.lc.L[c(1, 5:7)], buf.rad = c(4, 5, 5, 5)), # Buffer radii
  .f = \(shpL, buf.rad) buffer(shpL, buf.rad)
)

map(trees_lf_buff, \(shp){
  sum(expanse(shp))
})

# After buffering there may still be overlap between individual trees & live fences, but there shouldn't be. Cut out live fences using the individual trees polygons
trees_lf_buff[2:4] <- map2(trees_lf_buff[2:4], trees_lf_buff[1], \(lfs, trees){
  terra::erase(lfs, trees)
})

# CHECK:: Should be same dimensions
nrow(trees_lf_buff[[2]])
length(trees_lf_buff[[2]]$lc_typ)

# >Combine polygons -------------------------------------------------------
## Cut out the buffered individual trees & live fences from the LC polygons
# Live fences
LCs_holes1 <- map2(shp.lc.L[2:4], trees_lf_buff[2:4], \(polys, lf){
  polys %>% terra::erase(lf)
})

# Individual trees
LCs_holes2 <- map2(LCs_holes1, trees_lf_buff[1], \(polys, trees){
  polys %>% terra::erase(trees)
})

# Combine layers & visualize an example
LCs_comb_l <- pmap(
  .l = list(LCs_holes2, trees_lf_buff[1], trees_lf_buff[2:4]),
  \(polys, trees, lf){
    rbind(polys, trees, lf)
  }
)

LCs_comb_l$landcovers_middle_shapefile %>%
  filter(lc_typ == "livefence" & !is.na(data_year)) %>%
  data.frame()


# CONFIRMED that LCs_comb_l is the sum of all of these SpatVectors
460 + 1255 + 1066
622 + 1255 + 1023

# CHECK:: Should be no NAs in lc_typ
map(LCs_comb_l, \(polys){
  polys %>%
    filter(is.na(lc_typ)) %>%
    nrow()
})

# Visualize an example buffer
ggplot() +
  geom_spatvector(data = LCs_comb_l[[1]], aes(fill = lc_typ), alpha = .4) +
  geom_spatvector(data = union_ms$b100, alpha = .3) + # Add buffers
  geom_sf(data = Pc_locs_sf, size = .5) + # Add centroids
  coord_sf(xlim = c(-73.683, -73.666), ylim = c(3.323, 3.343))

## TROUBLESHOOT::
# The terra::erase() function modifies the geometries, and if there are any invalid geometries or empty geometries after the erase operation, this could lead to inconsistencies in the number of rows when you later try to visualize the data.
ggplot() +
  geom_spatvector(data = LCs_comb_l[[2]], aes(fill = lc_typ), alpha = .4) +
  geom_spatvector(data = union_ms$b100, alpha = .3) + # Add buffers
  geom_sf(data = Pc_locs_sf, size = .5) + # Add centroids
  coord_sf(xlim = c(-73.683, -73.666), ylim = c(3.323, 3.343))

# Difference in number of polygons & number of lc types
nrow(shp.lc.L[[2]]) # 3308 rows before erase function
nrow(LCs_holes1[[1]]) # 3308 rows after erase
length(LCs_holes1[[1]]$lc_typ) # 3303

miss_ids <- setdiff(shp.lc.L[[2]]$poly_num, LCs_holes1[[1]]$poly_num) 
miss_ids <- setNames(miss_ids, miss_ids)
miss_polys <- shp.lc.L[[2]] %>% filter(poly_num %in% miss_ids)
# Generally very small polygons that are causing problems 
miss_polys %>% expanse()

# Visualize polygons that are lost
imap(miss_ids, \(id, name){
  miss_poly <- shp.lc.L[[2]] %>% filter(poly_num == id)
  ggplot() + 
    geom_spatvector(data = miss_poly) + 
    labs(title = name)
})

# NOTE:: It seems like Mathilde left the linear features in the 'past' shapefile. Maybe this doesn't matter?
ggplot() +
  geom_spatvector(data = LCs_comb_l[[3]], aes(fill = lc_typ), alpha = .4) +
  geom_spatvector(data = union_ms$b100, alpha = .3) + # Add 50m radius buffers
  geom_sf(data = Pc_locs_sf, size = .5) + # Add centroids
  coord_sf(xlim = c(-73.683, -73.666), ylim = c(3.323, 3.343))


# >LC within 300m buffer ---------------------------------------
# See notes in notebook & at the top of script for ideas on how to do this
buffers_ms$b300

UBC_pts <- Pc_locs_sf %>% filter(Nombre_institucion == "UBC")

# Finca El Porvenir (both UniLlanos & I surveyed) have the data_year == 2019 but are not in the ‘middle_ubc’ file as well, or they don’t have data_year2 == 2022 (if there is no more recent imagery).
map(LCs_comb_l[c(1, 2)], \(polys){
  polys %>%
    filter(data_year == 2022) %>%
    ggplot() +
    geom_spatvector(aes(fill = lc_typ), alpha = .4) +
    geom_sf(data = UBC_pts, size = .5)
})

# >Id_group ----------------------------------------------------------------
## THIS IS AN OPTION TO CONSIDER, BUT IT MAY NOT BE NECESSARY. NEED TO TALK WITH MATHILDE & BETTER UNDERSTAND WHAT'S GOING ON. THINK BIG PICTURE, DON'T SPEND A BUNCH OF TIME ON THIS UNTIL WE'RE CONFIDENT IT IS THE BEST WAY FORWARD. Think about definition of 'intersects', is that what you want?

## Append the Id_group of each polygon to the spatvector data frame##
# relate() produces a matrix where column 1 is the Id # of the LC polygons & column 2 is the Id number of the buffer in union b300.
ints.df <- LCs_comb_l[[1]] %>%
  relate(union_ms$b300, "intersects", pairs = TRUE) %>%
  data.frame() %>%
  rename_with(~ c("id.lc", "id.buff")) %>%
  tibble()

# NOTE:: There are some digitized polygons that are contained in multiple buffers
overlaps4 <- ints.df %>%
  count(id.lc, sort = T) %>%
  filter(n == 4) %>%
  pull(id.lc)

## Let's visualize some example polygons + buffers that have 4 overlaps
test_lcs <- filter(LCs_comb_l[[1]], poly_num %in% as.character(overlaps4[1:20]))
extent <- ext(test_lcs) #+ c(-.0003, .0003, -.0000, .0000)

# Subset appropriate buffer IDs
ints.df %>% filter(id.lc == test_lcs$poly_num[1])
buff <- union_ms$b300[c(71, 75, 73, 78), ]


# Plot to better understand -- Andorra? Most of these polygons should only have 2 overlaps not 4 according to plot
ggplot() +
  geom_spatvector(
    data = test,
    aes(fill = lc_typ), alpha = .4
  ) +
  geom_spatvector(data = buff, aes(color = Id_group), alpha = .3) #+ #Add buffers
# geom_sf(data = Pc_locs_sf, size = .5) #+ #Add centroids
# coord_sf(xlim = c(extent[1], extent[2]), ylim = c(extent[3], extent[4]), expand = TRUE) #+
# ggrepel::geom_text_repel(data = union_ms$b300, aes(label = Id_group))


### NEXT STEP:: See if all polygons are contained within union_ms$b300
LCs_comb_l[[1]]

# Easy solution for now, just take the first Id_group, otherwise will need multiple columns for each Id_group.
# Kind of confusing b/c the 2020 file is formatted differently than the other files will be... Could also figure out which years were sampled when & select Id_group based on year somehow (think about this in more detail with Mathilde).
id.buffs <- ints.df %>%
  slice_head(by = id.lc) %>%
  pull(id.buff)
length(id.buffs)
nrow(LCs_comb_l[[1]])
LCs_comb_l[[1]]$Id_group <- union_ms$b300[id.buffs, ]$Id_group
unique(LCs_comb_l[[1]]$Id_group)

# Visualize the different Id_groups
# NOTE: The 3 of these overlap , examine with Mathilde
LCs_comb_l[[1]] %>%
  filter(Id_group %in% c("G-AD-M-CO", "G-MB-M-EA", "G-AD-M-LP")) %>%
  ggplot() +
  geom_spatvector(aes(fill = Id_group), alpha = .4)

# >Aggregate --------------------------------------------------------------
# NOTE:: Better to aggregate second because aggregate does not weigh the 'trees percent' column by the size of the polygon if you set fun = mean. Instead calculate the 'effective number of treed hectares' for each polygon & then sum them in the aggregate call.
# ALTERNATIVE:: Could probably just avoid aggregating at all for these calculations...
# To add 'crops_type' to aggregate call I think would need to paste lc & cropstype (e.g., crops_fruit). Doesn't really matter too much as there are very few crop polygons that are trees
LCs_comb %>%
  distinct(Id_group, lc, crops_type) %>%
  data.frame()
# The agg_tree_cover_ha is equal to the 'effective number of treed hectares'
LCs_agg_ID_lc <- aggregate(LCs_comb_a,
                           by = c("Id_group", "lc"), # "crops_type"
                           fun = function(x) {
                             sum(x, na.rm = TRUE)
                           }
)

# Visualize just the predominant landcovers to make legend easier to read
LCs_agg_ID_lc %>%
  filter(lc %in% c("intpast", "forest", "crops", "distrees", "livefence", "shrub")) %>%
  ggplot() +
  geom_spatvector(aes(fill = lc), alpha = .4) +
  coord_sf(xlim = c(-73.683, -73.666), ylim = c(3.323, 3.343))

# Inspect results
LCs_comb_a %>%
  data.frame() %>%
  dplyr::select(Id_group, lc, area, trees_prop, tree_cover_ha) %>%
  mutate(across(where(is.numeric), \(x) round(x, digits = 2))) %>%
  # filter(!is.na(crops_type)) %>%
  head(n = 30)

# LC areas by category
LCs_comb_a %>%
  as.data.frame() %>%
  group_by(lc) %>% # Id_group
  summarize(sum.area.ha = sum(area)) %>%
  arrange(desc(sum.area.ha))


# >Extract LC -------------------------------------------------------------
test <- terra::crop(LCs_comb_l[[3]], union_ms$b300)


# >Exploring options -------------------------------------------------------
# Calculate and add area attribute for each polygon
shp.lc.L[c(2:4)] <- map(shp.lc.L[c(2:4)], function(polys) {
  areas <- expanse(polys, unit = "m") # Calculate area in square meters
  polys$area <- round(areas, 5) # Add area as a new attribute
  return(polys)
})

# Filter out small polygons
shp.lc.L[c(2:4)] <- map(shp.lc.L[c(2:4)], function(polys) {
  polys %>% filter(area > 0.001)
})

# Apply a small buffer
small_buffer <- map(shp.lc.L[c(2:4)], function(polys) {
  polys %>% buffer(width = .00001)
})

# Try disaggregating
disagg_ubc <- shp.lc.L$landcovers_middle_UBC_shapefile %>% disagg()

# None of these potential solutions fix the issue. What else is there other than erase?
nrow(no_overlap$landcovers_middle_UBC_shapefile)
length(no_overlap$landcovers_middle_UBC_shapefile$lc_typ)
nrow(shp.lc.L$landcovers_middle_UBC_shapefile)

# Tried aggregating before using erase, but this didn't work, not sure why
# shp.lc.L[[1]] <- buffer(shp.lc.L[[1]], width = 4)

# shp.agg <- map(shp.lc.L, \(shp){
#  shp %>% aggregate(by = "lc_typ", dissolve = TRUE)
# })

# Sf approach -------------------------------------------------------------
# Original motivation is that st_distinct() might handle things differently than terra's erase().
# This is mostly the same code as above just using sf syntax

# ENSURE you are paying attention to sf_use_s2... Standard geometries are flat, while S2 geometries account for the Earth's curvature. Generally..
# Flat Geometries: Suitable for local studies, small regions, or when working with data that does not span large areas.
# S2 Geometries: Ideal for global datasets, spatial analyses over long distances, and complex geometry manipulations.
sf_use_s2(TRUE)

shp.lc.L <- map(.x = files.shp, \(shp){
  st_read(paste0(path, "/", shp)) %>%
    clean_names()
})
names(shp.lc.L) <- files.shp

# Add rownumber for easy identification & communication with Mathilde
shp.lc.L[c(2:4)] <- map(shp.lc.L[c(2:4)], \(polys){
  polys %>% mutate(poly_num = row_number())
})

# >Explore invalid polygons -----------------------------------------
# NOTE:: several polygons are not valid
map(shp.lc.L, \(polys){
  polys %>%
    st_is_valid() %>%
    table()
})

## We don't lose any polygons if working in sf
# Before makeValid
map(shp.lc.L[c(2:4)], \(polys){
  polys %>% nrow()
})

# But there are still a few polygons that are not actually valid
map(shp.lc.L[c(2:4)], \(polys){
  polys %>%
    st_is_valid() %>%
    table()
})

# Identify the problematic polygons
TF <- map(shp.lc.L[c(2:4)], \(polys){
  polys %>% st_is_valid()
})
# Visualize
map2(shp.lc.L[c(2:4)], TF, \(polys, TF){
  polys[!TF, ] %>% ggplot() +
    geom_sf()
})
# Subset just valid polygons for now to continue hammering out workflow
shp.lc.L[c(2:4)] <- map2(shp.lc.L[c(2:4)], TF, \(polys, TF){
  polys[TF, ]
})

# Generate list of invalid polygons
invalid <- map(shp.lc.L[2:4], \(polys){
  polys %>%
    mutate(is_valid = st_is_valid(.)) %>%
    filter(!is_valid)
})

# Make polygons valid & then plot
# It seems like the polygons still have weird stuff going on, may be best to have Mathilde go in & fix these issues?
valid <- map(invalid, \(polys_df){
  st_make_valid(polys_df)
})

# ERROR
# Plot invalid polygons, the # corresponds to the row numbers in the attribute table of QGIS
Invalid_poly_plots <- map2(invalid, valid, \(invalid_polys, valid_polys)
                           map(seq_len(nrow(invalid_polys)), function(i) {
                             invalid_plot <- ggplot() +
                               geom_sf(data = invalid_polys[i, ], fill = "red", color = "black") +
                               labs( # title = list_name,
                                 subtitle = paste("Invalid Polygon", pull(invalid_polys[i, ], poly_num))
                               ) +
                               theme_min
                             valid_plot <- ggplot() +
                               geom_sf(data = valid_polys[i, ], fill = "red", color = "black") +
                               labs( # title = list_name,
                                 subtitle = paste("Valid Polygon", pull(valid_polys[i, ], poly_num))
                               ) +
                               theme_min
                             ggarrange(invalid_plot, valid_plot)
                           }))

# Troubleshoot
for (i in c(1:25)) {
  p <- ggplot() +
    geom_sf(data = valid$landcovers_middle_UBC_shapefile[i, ], fill = "red", color = "black") +
    labs( # title = list_name,
      subtitle = paste("Valid Polygon", pull(valid$landcovers_middle_UBC_shapefile[i, ], poly_num))
    )
  print(p)
}

# Print 3 PDFs (for each shapefile) with the invalid polygons in the left column, and the valid column (after using makeValid) on the right side
imap(Invalid_poly_plots, \(poly_plots, name){
  pdf(file = paste0("/Users/aaronskinner/Library/CloudStorage/OneDrive-UBC/Grad_School/PhD/Mentorship/Digitization_Mathilde/Feedback/invalid_polygons_", name, format(Sys.Date(), "%m.%d.%y"), ".pdf"), width = 8.5, height = 11, bg = "white")
  print(marrangeGrob(grobs = poly_plots, ncol = 3, nrow = 3, layout_matrix = matrix(1:3, 3, 1, TRUE)))
  dev.off()
})

# >Make polygons valid -----------------------------------------------------
# SHORT TERM SOLUTION -- Delete these 4 corrupt polygons
# LONG TERM SOLUTION -- Need to redraw
shp.lc.L <- map(shp.lc.L, \(polys){
  polys %>% st_make_valid()
})


# NOTE:: Not all polygons are valid at this point
map(shp.lc.L, \(shp){
  table(st_is_valid(shp))
})

# Remove the single polygon that wasn't fixed
shp.lc.L[c(2:4)] <- map2(shp.lc.L[c(2:4)], TF, \(polys, TF){
  polys %>% filter(TF)
})

# >Buffer -----------------------------------------------------------
# Define the buffer radii for each type
buffer_sizes <- c(4, 5, 5, 5)

# Buffer trees and live fences by appropriate distances
trees_lf_buff <- pmap(
  .l = list(shp.lc.L[c(1, 5:7)], buffer_sizes),
  .f = \(shpL, buf_rad) st_buffer(shpL, dist = buf_rad)
)

# Define the helper function
st_erase <- function(x, y) {
  st_difference(x, st_union(st_combine(y)))
}

# Notice this produces error related to the trees! I don't know why the trees would be problematic, but after playing with different subsets of both sets of polygons, it is these trees that are causing problems.
trees_lf_buff[2:4] <- map2(trees_lf_buff[2:4], trees_lf_buff[1], \(lfs, trees){
  st_erase(lfs, trees)
})

# Join all polygons
union_trees <- st_union(trees_lf_buff[[1]])

# Notice there is no error when trees are unioned
map2(trees_lf_buff[2], union_trees, \(lfs, trees){
  lfs %>% st_erase(union_trees)
})

# Understanding the error message... I think this means
# Visualize the problematic trees
trees_lf_buff[[1]][c(261:262), ] %>%
  ggplot() +
  geom_sf()

trees_lf_buff[[1]][c(922:923), ] %>%
  ggplot() +
  geom_sf()


# >Combine polygons -------------------------------------------------------
## Cut out the buffered individual trees & live fences from the LC polygons
# Live fences
LCs_holes1 <- map2(shp.lc.L[2:4], trees_lf_buff[2:4], \(polys, lf){
  polys %>% st_erase(lf)
})

# ChatGPT
# Erase live fences from LC polygons
LCs_holes1 <- Map(function(lc, lf) {
  st_erase(lc, lf) # Erase all of lf from lc
}, sf_lc_layers, sf_lf_buffers)

# Erase individual trees from the resulting LC polygons
sf_trees_buffer <- st_as_sf(trees_lf_buff[[1]])
LCs_holes2 <- lapply(LCs_holes1, function(lc) {
  st_erase(lc, sf_trees_buffer) # Erase all of trees from each lc layer
})
