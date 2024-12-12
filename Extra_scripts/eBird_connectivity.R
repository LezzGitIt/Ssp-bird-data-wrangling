##eBird status & trends products to assess connectivity in Colombia##
# eBird occ & abu------------------------------------------------------------
#set_ebirdst_access_key("8rh7l6t8edbd")
library(ebirdst)
library(dplyr)
library(ggplot2)
library(lubridate)
library(sf)
library(terra)
ggplot2::theme_set(theme_cowplot())
select <- dplyr::select
#library(auk) #see vignette on working with raw eBird data, i.e., eBird Basic Dataset (EBD): https://cornelllabofornithology.github.io/ebird-best-practices/ebird.html
#Size seems like a big issue, so read section 2.6 carefully

#eBird will save files here 
ebirdst_data_dir()

View(ebirdst_runs)

path_yebsap <- ebirdst_download(species = "example_data") #, tifs_only = FALSE)
abu_lr <- load_raster(path_yebsap, product = "abundance", resolution = "lr") #cube, 52 weeks of raster layers.
parse_raster_dates(abu_lr)

abu_ss <- abu_lr[[1:3]] #abundance subsample 
plot(abu_ss) #Notice that extent is global (or at least all W hemisphere), so need to reduce the extent 


# Centroids_movement ------------------------------------------------------
#Trying to think about movement of spp, where high difference values mean abundance was high at one point in year and then low at another point in the year. IF THE CENTROIDS APPROACH WORKS THIS SEEMS MUCH CLEANER

###NOTE:: DANIEL FINK WASN'T CONVINCED THE CENTROID APPROACH REALLY MAKES SENSE FOR MUCH. SEE AMANDA'S EMAIL ON 5/30/23 WHERE FINK SAYS (AND MORE): "controlling for the huge amount of variation in the eBird observation process that will BIAS YOUR RESULTS!  (All caps because BIAS was the concern underlying circularity. So just generating more (possibly different) bias, does not seem like a good fall back position." I THINK IT COULD STILL BE USED GENERALLY TO GET AN IDEA ABOUT THE AMOUNT OF MOVEMENT. 
dims <- dim(abu_lr)[1] * dim(abu_lr)[2] #need some way to standardize by size
dif <- (max(abu_ss) - min(abu_ss))
dif/dims
global(dif, 'sum', na.rm = T) #large sums represent lots of movement


lbhe <- ebirdst_download(species = "Long-billed hermit", pattern = "centroid", tifs_only = F)
abu_lbhe <-  load_raster(lbhe, product = "abundance", resolution = "lr")

#Long-billed hermit
runsCol <- ebirdst_runs %>% filter(scientific_name %in% AyerbeAllspp) #ebirdst_runs Colombia
View(runsCol)
runsCol$common_name
length(runsCol$scientific_name)
table(runsCol$resident)
sppCent <- abundances <- list()
for(i in c(1:50)){ #i in 
  eBpath <- ebirdst_download(species = runsCol[i,]$scientific_name, pattern = "centroids") #eBird path
  ebirdst_download(species = runsCol[i,]$scientific_name, pattern = "abundance_median_lr")
  #species Centroid. The "prev_dist_km" column is in these centroid file 
  sppCent[[i]] <- read.csv(paste0(eBpath, "/weekly/", runsCol[i,]$species_code, "_centroids.csv"))
  abundances[[i]] <- load_raster(eBpath, product = "abundance", resolution = "lr")
}

sppCents <- bind_rows(sppCent, .id = NULL)
sppCents <- sppCents %>% filter(!is.na(latitude))
sppCentsSf <- st_as_sf(x = sppCents,                         
                       coords = c("longitude", "latitude"),
                       crs = 4326)

spps <- unique(sppCents$species_code)
subset <- distsmat <- Overall_Dist <- list()
for(i in 1:length(spps)){
  subset[[i]] <- sppCentsSf %>% filter(species_code == spps[i])
  distsmat[[i]] <- st_distance(subset[[i]], by_element = F)
  Overall_Dist[[i]] <- data.frame(species_code = spps[i], overall_dist_max_km = max(as.numeric(distsmat[[i]]) / 1000)) #Largest dist moved between 2 centroids over the year
}
Overall_Dist_df <- bind_rows(Overall_Dist)

move_dists <- sppCents %>% group_by(species_code) %>% summarize(consec_move_mn = mean(prev_dist_km, na.rm = T), consec_move_sd = sd(prev_dist_km,na.rm = T), consec_move_max = max(prev_dist_km, na.rm = T)) #movement distances 
move_dists <- merge(move_dists, Overall_Dist_df, by = "species_code") 
runsDists <- merge(move_dists, runsCol, by = "species_code") #ebirdst_runs w/ distances moved

#CHECK: All should be true
table(runsDists$overall_dist_max_km > runsDists$consec_move_mn)


runsDists %>% group_by(resident) %>% summarize(dist_mn = mean(consec_move_mn, na.rm = T), max_dist = max(consec_move_mn, na.rm = T)) #Mean & max distances of mean over all 52 weeks
runsDists %>% dplyr::select(c(1:8)) %>% arrange(desc(resident), overall_dist_max_km)




# Habitat_importance ------------------------------------------------------
#Summary of predictors used in eBird Status & Trends models
?ebirdst_predictors
View(ebirdst_predictors)

#Example with yellow bellied sapsucker
#From convo with Tom Auer:

#PIs tell us importance of given variable for each stixel. See ranger package (package used for random forest classifications) documentation for more information on how these 'importance' scores are calculated. 
#PDs allow us to plot the relationship between a given 'predictor variable' (e.g., % habitat) and the 'response' (the probability of occurrence or the abundance data; notice column names). 
pis_yebsap <- load_pis(path_yebsap)
pds_yebsap <- load_pds(path_yebsap)
View(pds_yebsap)
View(pis_yebsap)

#Define extent. The extent can be a polygon as well (e.g., Colombia polygon)
pis_yebsap %>% summarize(xmin = min(longitude), xmax = max(longitude), ymin = min(latitude), ymax = max(latitude))
ext_yebsap <- ebirdst_extent(c(xmin = -90.1, xmax = -82.5, ymin = 41.8, ymax = 46.9))
?ebirdst_extent
pis_yebsap <- ebirdst_subset(pis_yebsap, ext = ext_yebsap) #key function I was missing
plot_pis(pis_yebsap, ext = ext_yebsap, by_cover_class = TRUE, n_top_pred = 15)

#Plot relationships from the loaded pds.. X-axis is the predictor value (e.g., % forest), and y-axis is the expected (E) difference from the mean of occurrence on the logit scale
plot_pds(pds_yebsap, predictor = "mcd12q1_lccs2_fs_c36_1500_pland", ext = ext_yebsap) #Cropland
plot_pds(pds_yebsap, predictor = "mcd12q1_lccs1_fs_c14_1500_pland", ext = ext_yebsap) #Broadleaf

#Habitat associations for yebsap#
#The ebirdst_habitat() function uses both the PIs (which tell us importance) and the PDs to show how habitat associations change during each week of the year for the given extent we've defined.PIs tell you the importance of the relationship (from random forest), PDs define the relationship (show you the slope) 
#The object created by ebirdst_habitat() has importance values (similar to a beta coefficient, but is always positive), the prob_pos_slope column has the proportion of stixels within your extent that have a positive relationship, and the direction is a trinary column with either a 1 when the prob_pos_slope column > .7, a -1 when the the prob_pos_slope is < .3, and NA if the prob_pos_slope is between .3 and .7
?ebirdst_habitat
hab_yebsap <- ebirdst_habitat(path = path_yebsap, ext = ext_yebsap)
plot(hab_yebsap, n_habitat_types = 18)

#Get stationary associations (one estimate for full year). I'm not positive how this is calculated, but it is NOT just a summary of the weekly habitat associations. The last two associations also seem to be an error (all prob_pos_slopes and 'direction' in the weekly habitat associations are NA, yet they're 1 and -1 in the stationary file)
hab_yebsap_stat <- ebirdst_habitat(path = path_yebsap, ext = ext_yebsap, stationary_associations = T)
arrange(hab_yebsap_stat, desc(importance))

#Tried to do my own summaries, but it doesn't quite work. Notice there are some variable that have neg relationships at one point in year and pos relationships at other points of year (see ntl_mean)
hab_yebsap %>% group_by(predictor) %>% summarize(avg_pi = mean(importance, na.rm = T) * mean(direction, na.rm = T)) %>% arrange(desc(avg_pi))
View(hab_yebsap)



##Try w/ a Colombian species, long-billed hermit##
path_lbhe <- ebirdst_download(species = "Long-billed hermit", pattern = "stixel_summary", tifs_only = F)

pis_lbhe <- load_pis(path_lbhe, return_sf = T) #Notice warning message indicating invalid latitude
pds_lbhe <- load_pds(path_lbhe)
View(pds_lbhe)
View(pis_lbhe)
pis_lbhe %>% summarize(xmin = min(longitude), xmax = max(longitude), ymin = min(latitude), ymax = max(latitude))
pis_lbhe %>% arrange(latitude) #Latitudes of -180 is problematic
ext_lbhe <- ebirdst_extent(c(xmin = -100, xmax = -64.8, ymin = -90, ymax = -.05)) #Changed ymin to -90
pis_lbhe_ext <- ebirdst_subset(pis_lbhe, ext = ext_lbhe)
plot_pis(pis_lbhe, ext = ext_lbhe, by_cover_class = TRUE, n_top_pred = 15)

#Plot relationships from the loaded pds.. X-axis is basically the predictor value, and y-axis is the expected (E) difference from the mean of occurrence on the logit scale
plot_pds(pds_lbhe, predictor = "mcd12q1_lccs2_fs_c36_1500_pland", ext = ext_lbhe) #Cropland
plot_pds(pds_lbhe, predictor = "mcd12q1_lccs1_fs_c14_1500_pland", ext = ext_lbhe) #Broadleaf


hab_lbhe <- ebirdst_habitat(path = path_lbhe, ext = ext_lbhe)
View(hab_lbhe)
plot(hab_lbhe) #Only enough data in December and January? 

#Stationary habitat associations
hab_lbhe_stat <- ebirdst_habitat(path = path_lbhe, ext = ext_lbhe, stationary_associations = T)
arrange(hab_lbhe_stat, desc(importance))
