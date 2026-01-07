## PhD birds in silvopastoral landscapes ##
## Data wrangling 08 -- Phylogeny & metrics of phylogenetic diversity
## This script generates metrics of phylogenetic diversity and visualizes the phylogeny of all birds observed in the SCR project

## Contents:
# 1) Taxonomy: Use Taxonomy tbl to change to BirdTree names
# 2) Phylogeny: Read in & prune phylogeny from BirdTree
# 3) Order & family: Link Order & family information with species names (tips of phylogeny)
# 4) Phylopic: Download silhouettes of the most common families (represent each order)
# 5) MRCA: Identify most recent common ancestor for plotting
# 6) Visualize phylogeny 
# 7) Generates metrics of phylogenetic diversity
# 8) Generate phylogenetic correlation matrix, as well as [PURPOSE OF cophenetic.phylo function]

# TO DO: What was purpose of cophenetic.phylo function? 
## Incorporate information from all 1000 trees... 
# install.packages("phangorn") # maxCladeCred() http://blog.phytools.org/2016/04/average-trees-and-maximum-clade.html

# Libraries ---------------------------------------------------------------
## Load libraries
library(xlsx)
library(phytools)
library(ggtree)
library(ggrepel)
library(tidytree)
library(conflicted)
library(rphylopic)
conflicts_prefer(purrr::map)
conflicts_prefer(dplyr::filter)

# Load data ---------------------------------------------------------------
source("/Users/aaronskinner/Library/CloudStorage/OneDrive-UBC/Grad_School/Rcookbook/Themes_funs.R")

Taxonomy <- read_csv("Derived/Excels/Taxonomy/Taxonomy.csv")
Bird_pcs_all <- read_csv("Derived/Excels/Bird_pcs/Bird_pcs_all.csv")

# BirdTree equivalents ----------------------------------------------------
Spp_join_bt <- Taxonomy %>%
  distinct(Species_ayerbe, Species_bt, Order, Family)

# We only want one BirdTree option for each species Ayerbe (a single representative subspecies)
Spp_join_bt %>% count(Species_ayerbe, sort = T) %>% 
  filter(n > 1)
# Example, there are two BirdTree equivalents equal to Piranga flava. Looking online, we want Piranga lutea which is in Colombia 
Spp_join_bt %>% filter(Species_ayerbe == "Piranga flava")

# Manually choose a single BirdTree option 
Spp_join_bt2 <- Spp_join_bt %>% 
  mutate(Species_bt = case_when(
    Species_ayerbe == "Piranga flava" ~ "Piranga lutea", 
    Species_ayerbe == "Setophaga petechia" ~ "Dendroica petechia", 
    Species_ayerbe == "Turdus albicollis" ~ "Turdus albicollis",
    .default = Species_bt
)) %>% distinct() %>% 
  mutate(Species_bt_ = str_replace(Species_bt, " ", "_")) 

# NOTE:: Species with Species_bt repeated have multiple names for species_ayerbe, which is OK
Spp_join_bt2 %>% count(Species_bt, sort = T)

# Phylogenies we downloaded use BirdTree taxonomy and have "_" separating genus & species. Create vector of the species we observed in BirdTree taxonomy
Spp_obs_bt <- Spp_join_bt2 %>% 
  pull(Species_bt_) %>% 
  unique()

# Read in tree ------------------------------------------------------------
## Bring in phylogenetic tree downloaded from BirdTree
# NOTE:: There are 10 “sets", or possible trees from a distribution of Hackett-based trees, built under the Mayr taxonomy. These are independent replicates that differ in how they resolve those parts of the avian tree where the data are ambiguous.
# https://data.vertlife.org/?basetree=birdtree May want to download additional sets?
#phylo_ape <- ape::read.tree("Data/Stage2_MayrAll_Hackett_set1_decisive.tre") 

# Instead of reading in 1000 trees (slow) & pruning, load in single tree that is already pruned 
phylo_obs <- read.tree(file = "Derived/Single_tree.tre")

# Prune tree --------------------------------------------------------------
# Would want to factor in uncertainty of trees in analysis, so going to maintain 10 trees in a list as would have in a full analysis
#phylo_red <- phylo_ape %>% keep_at(1:10) 

## Prune(?) the tree down to just the species we have 
#phylo_obs_l <- map(phylo_red, \(phylo){
#  ape::keep.tip(phylo, Spp_obs_bt, trim.internal = TRUE)
#})

# Export
#write.tree(phylo_obs_l[[1]], file = "Derived/Single_tree.tre")

# Taxonomy: Order & Family ------------------------------------------------
## Bring in data on orders or families 
Tax_join <- Spp_join_bt2 %>% 
  select(-Species_ayerbe) %>% 
  distinct() #%>% 
  #rename_with(~ str_remove(., ""))

# Take species from pruned tree, join with taxonomy, & add in node information
Tax_tbl_nodes <- tibble(tip.label = phylo_obs$tip.label) %>% 
  left_join(Tax_join, by = join_by("tip.label" == "Species_bt_")) %>% 
  mutate(Genus = str_split_i(string = tip.label, pattern = "_", i = 1)) %>%
  mutate(node = row_number()) # Not sure why this works. Seems like multiple species would share a single node? 

# Should be no NAs
Tax_tbl_nodes %>% filter(is.na(Order))

# Summary table -----------------------------------------------------------
# Create summary table with number of families, genera, & spp per order
Tax_summary <- Tax_tbl_nodes %>%
  summarise(
    N_fam = n_distinct(Family),
    N_gen = n_distinct(Genus),
    N_spp = n_distinct(tip.label),
    .by = Order
  ) %>% arrange(desc(N_fam), desc(N_gen), desc(N_spp))

# MRCA --------------------------------------------------------------------
## Identify the nodes for the most recent common ancestor (MRCA) for each Order (important for plotting)
Mrca_nodes <- map(Tax_summary$Order, \(.order){
  nodes <- Tax_tbl_nodes %>% filter(Order == .order) %>% 
    pull(node)
  phy_otu <- groupOTU(phylo_obs, nodes)
  tibble(.order, Mrca_node = MRCA(phy_otu, nodes))
}) %>% list_rbind() %>% 
  rename(Order = .order)

# Phylopic -----------------------------------------------------
## Phylopic: Download silhouttes to use in plot of phylogeny

# We want to select the most representative icons for each order, so determine which families dominate each order 
# NOTE:: Can use pic_n to select a different silhoutte as needed

Family_dominant <- Tax_tbl_nodes %>% 
  summarize(N_spp_fam = n_distinct(tip.label), 
            .by = c(Order, Family)) %>%
  slice_max(order_by = N_spp_fam, by = Order, with_ties = TRUE) %>%
  # We only want a single Gruiform and family == Rallidae is more representative than the limpkin (Aramus guarana). 
  # NOTE:: This does not affect calculation of # spp, genera, or families per order
  filter(Family != "Aramidae") %>% 
  select(-N_spp_fam) %>% 
  mutate(pic_n = 1)

## Download & adjust the phylopic silhouettes
# NOTE:: Only need to download and adjust the images once, so skip this code if the folder already exists
Phylopic_path <- "Figures/Phylogeny/Phylopic/Bird_orders_"
if(!file.exists(paste0(Phylopic_path, "final/"))){
  # For each order download the phylopic silhouette
  Phylopic_tbl <- pmap(Family_dominant, function(Order, Family_dom, pic_n){
    image <- pick_phylopic(name = Family_dom, auto = 2) # n = pic_n, auto = 2
    tibble(Order = Order, Uuid = get_uuid(img = image))
  }) %>% list_rbind()
  
  # Modify images (rotations & size adjustments) & save
  uuid_labs <- setNames(Phylopic_tbl$Uuid, Phylopic_tbl$Order)
  Make_larger <- c("Falconiformes", "Trogoniformes", "Cuculiformes", "Caprimulgiformes")
  flip <- c("Coraciiformes", "Galbuliformes", "Cathartiformes", "Charadriiformes", "Cuculiformes")
  imap(uuid_labs, \(uuid, order){
    img <- get_phylopic(uuid = uuid) 
    if(order %in% flip){
      img <- flip_phylopic(img)
    }
    # Save images to Figures folder
    if(order %in% Make_larger){
      save_phylopic(img, path = paste0(Phylopic_path, "auto2/", order, ".png"), 
                    width = 1000, height = 1000) #580
    } 
    else{
      save_phylopic(img, path = paste0(Phylopic_path, "auto2/", order, ".png"))
    }
    # Default dimensions are 480 x 480
  })
}

# >Combine icons in final -------------------------------------------------
# Move a few orders that look better from auto_2 
if(!file.exists(paste0(Phylopic_path, "final/"))){
  Orders_move <- c("Falconiformes", "Cathartiformes", "Charadriiformes", "Cuculiformes")
  map(Orders_move, \(order){
    file.copy(from = paste0(Phylopic_path, "auto2/", order, ".png"),  
              to = paste0(Phylopic_path, "final/", order, ".png"), 
              overwrite = TRUE)
  })
}

# Copy Piciformes from pic_n
file.copy(from = paste0(Phylopic_path, "picn/Piciformes.png"),  
          to = paste0(Phylopic_path, "final/Piciformes.png"), 
          overwrite = TRUE)

# Visualize ---------------------------------------------------------------
## Plot phylogeny coloring species names by family and including orders on the outside of the phylogeny 
# Remove the orders with only 1 species to improve visibility on plot
Orders_plot <- Mrca_nodes %>% 
  full_join(Tax_summary) %>%
  filter(N_spp > 1 & Order != "Anseriformes") %>% 
  rowwise() %>%
  mutate(
    Image_path = paste0(Phylopic_path, "final/", Order, ".png"),
    Label = paste0(Order, " (", N_spp, ")") # N_fam, ",",
  ) %>% ungroup()

# Plot full phylogeny
ggtree(phylo_obs, layout='circular', aes(color = Family)) %<+%
  Tax_tbl_nodes + 
  #geom_tiplab(size = 3, aes(color = family, angle = angle)) +
  geom_cladelab(
    data        = Orders_plot,
    mapping     = aes(node = Mrca_node, label = Label, image = Image_path),
    geom        = "image", # "phylopic"
    imagecolor  = "black",
    inherit.aes = FALSE,
    offset      = 3,    # distance from the clade
    barsize     = 0.3,  
    show.legend = FALSE
  ) + guides(color = "none") + 
  geom_cladelab(data = Orders_plot,
                mapping = aes(node = Mrca_node, label = Label), 
                fontsize = 3,
                angle = "auto", 
                offset = 10, 
                barsize = 0) + # auto-rotates text radially
  theme(plot.margin = margin(30, 30, 20, -40)) # Control
ggsave("Data_paper/Figures/Phylogeny_equal_sizes.png",
       height = 8, width = 7.35)

## Visualize each order that has more than 1 species
plots_order <- pmap(Orders_plot[,c("Order", "Mrca_node", "N_spp")], 
                    function(Order, Mrca_node, N_spp) {
  if(N_spp > 1){
    tree_sub <- tree_subset(phylo_obs, Mrca_node, levels_back = 0)
    ggtree(tree_sub) %<+%
      Tax_tbl_nodes + # Add family labs later if desired
      geom_tiplab() +
      labs(title = Order)
  } 
})
# compact(plots_order) # Remove NULLs & visualize

# Export  -------------------------------------------------------------
stop()
# Write a single tree
write.tree(phylo_obs, file = "Derived/Single_tree.tre")

## Export summary table
# Recreate summary_tbl using all species (not just BirdTree)
Tax_summary_exp <- Taxonomy %>%
  distinct(Species_ayerbe, Order, Family) %>%
  #rename_with(~ str_remove(., "")) %>%
  mutate(Genus = str_split_i(Species_ayerbe, " ", 1)) %>% 
  summarise(
    N_fam = n_distinct(Family),
    N_gen = n_distinct(Genus),
    N_spp = n_distinct(Species_ayerbe),
    .by = Order
  ) %>% arrange(desc(N_fam), desc(N_gen), desc(N_spp))
write.csv(Tax_summary_exp, row.names = FALSE, 
          file = "Derived/Excels/Tax_summary.csv")

# Phylogenetic diversity (PD) -----------------------------------------------
# ALSO SEE BIOL314 SCRIPT

## Background: Multiple models of evolution...

## Brownian model of evolution
# In Brownian motion, changes in traits occur slowly through time -- it is the time passed that is important for understanding the diversity of traits. Thus, we want to sum the evolutionary time (measured in millions of years)
sum(phylo_obs$edge.length) # Total phylogenetic diversity in millions of years

## Punctuated model of evolution
# There are long periods of stasis followed by periods of rapid change of traits (associated with speciation) 
# In punctuated evolution, it is the speciation events that are important for understanding diversity of traits. Thus, usually measured by the number of diversification events 
# Rescale so every branch length is the same length
# NOTE:: The number at the end in the rescale function is sort of a weighting function for the model.. If it is 0, all branch lengths identical, whereas a value of .5 implies that PD is a mix of brownian and punctuated evolution
phylo_kappa <- phytools::rescale(phylo_obs, model = "kappa", 0) #.5
PD_punctuated <- sum(phylo_kappa$edge.length) 
PD_punctuated
# NOTE:: If each node bifurcates, this is the same the number of internal nodes 
PD_punctuated / 2
#?rescale.phylo # From geiger package 

## Visualization of the two models
plot(phylo_obs)
plot(phylo_kappa)

# >Future analyses ---------------------------------------------------------
# Compute the pairwise distances between the pairs of tips from a phylogenetic tree using its branch lengths
# PURPOSE:: Can't quite remember, maybe this is important if we want to understand how evolutionary history influences responses to habitat change? Ie. Species that are further away will have less similar responses, and viceversa?
# Or maybe understand how much evolutionary history there is in a community? 
Evol_dist <- round(cophenetic.phylo(phylo_obs), 2)
Evol_dist[1:6, 1:6]

# Correlation matrix
Phylo_cor <- round(vcv(phylo_obs, cor = TRUE), 2)
Phylo_cor[1:6, 1:6]

stop()

# EXTRAS ------------------------------------------------------------------
# Other packages to explore in the future
picante # Community analysis 
caper # trait correlations, phylogenetic regressions

# >Individual image -------------------------------------------------------
# Download individual image
grass_uuid <- get_uuid("Poa pratensis")
grass_img <- get_phylopic(grass_uuid)
grass_img <- recolor_phylopic(img = grass_img, alpha = 0.5, fill = "darkgreen")
save_phylopic(grass_img, "Grass.png")

# >Manually pick photos ---------------------------------------------------
## NOTE:: No longer necessary -- instead just use the data to determine the most speciose families, & pick a silhoutte from that family
## Manual adjustment to pick silhouttes that are more effective
# There are some diverse orders that have many families 
Diverse_orders <-  c("Suliformes", "Apodiformes", "Piciformes", "Coraciiformes")
Tax_tbl_nodes %>% filter(order %in% Diverse_orders) %>% 
  janitor::tabyl(order, family)

tibble(Order = Diverse_orders, Family_dom = c("Phalacrocoracidae", "Trochilidae", "Picidae", "Alcedinidae"), Common_name = c("Cormorant", "Humming_birds", "Woodpeckers", "Kingfishers"))

# View options for given group
pick_phylopic(name = "Piciformes", n = 10)

Alternate_pics2 <- c("Trogoniformes", "Suliformes", "Tinamiformes")
Alternate_pics3 <- c("Falconiformes", "Passeriformes", "Apodiformes")
Order_pic <- tibble(Orders, Pic_n) %>% 
  mutate(Pic_n = case_when(
    Orders %in% Alternate_pics2 ~ 2,
    Orders %in% Alternate_pics3 ~ 3,
    Orders %in% Alternate_pics4 ~ 4,
    Orders == "Piciformes" ~ 7, 
    Orders == "Coraciiformes" ~ 9,
    .default = Pic_n
  ))
