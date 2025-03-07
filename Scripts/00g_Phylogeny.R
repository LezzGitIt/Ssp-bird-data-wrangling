
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
load("Rdata/the_basics_02.27.25.Rdata")
load("Rdata/Taxonomy_12.29.24.Rdata")
load("Rdata/Occ_abu_inputs_01.09.25.Rdata")
source("/Users/aaronskinner/Library/CloudStorage/OneDrive-UBC/Grad_School/Rcookbook/Themes_funs.R")

# BirdTree equivalents ----------------------------------------------------
Spp_join_bt <- Tax_df3 %>% as_tibble() %>%
  distinct(Species_ayerbe, Species_bt, order_gbif, family_gbif) 
## Filter just the bird species observed in Birds_analysis, join with BirdTree taxonomy, and keep only the first BirdTree option for each species (according to Ayerbe). This way we only keep a single representative subspecies when there are multiple BirdTree subspecies
Bt_equivalents <- Birds_analysis %>% distinct(Nombre_ayerbe) %>% 
  left_join(Spp_join_bt, 
            by = join_by("Nombre_ayerbe" == "Species_ayerbe")) %>%
  reframe(Species_bt = first(Species_bt),
          .by = Nombre_ayerbe)

# Phylogenies we downloaded use BirdTree taxonomy and have "_" separating genus & species. Create vector of the species we observed in BirdTree taxonomy
Spp_obs_bt <- Bt_equivalents %>% 
  mutate(Species_bt_ = str_replace(Species_bt, " ", "_")) %>% 
  pull(Species_bt_)


# Read in tree ------------------------------------------------------------
## Bring in phylogenetic tree downloaded from BirdTree
# NOTE:: There are 10 “sets", or possible trees from a distribution of Hackett-based trees, built under the Mayr taxonomy. These are independent replicates that differ in how they resolve those parts of the avian tree where the data are ambiguous.
# https://data.vertlife.org/?basetree=birdtree May want to download additional sets?
# phylo_ape <- ape::read.tree("Data/Stage2_MayrAll_Hackett_set1_decisive.tre") 
#phylo_red <- phylo_ape %>% keep_at(1:10) # Keep 10 trees for now

# Instead of reading in 1000 trees (slow) & pruning, load in single tree that is already pruned 
phylo_obs <- read.tree(file = "Derived/Single_tree.tre")

# Prune tree --------------------------------------------------------------
## Prune(?) the tree down to just the species we have 
#phylo_obs <- map(phylo_red, \(phylo){
  #ape::keep.tip(phylo, Spp_obs_bt, trim.internal = TRUE)
#})

# Taxonomy: Order & Family ------------------------------------------------
## Bring in data on orders or families 
Tax_join <- Spp_join_bt %>% 
  select(-Species_ayerbe) %>% 
  distinct() %>% 
  mutate(
    Species_bt = str_replace(Species_bt, " ", "_"), 
    order_gbif = if_else(Species_bt == "Ortalis_guttata", "Galliformes", order_gbif), family_gbif = if_else(Species_bt == "Ortalis_guttata", "Galliformes", family_gbif)
  ) %>% rename_with(~ str_remove(., "_gbif"))

# Take species from pruned tree, join with taxonomy, & add in node information
Tax_tbl_nodes <- tibble(tip.label = phylo_obs$tip.label) %>% 
  left_join(Tax_join, by = join_by("tip.label" == "Species_bt")) %>% 
  mutate(node = row_number()) # Not sure why this works. Seems like multiple species would share a single node? 

# Phylopic & Mrca -----------------------------------------------------
## Phylopic: Download silhouttes to use in plot of phylogeny
## MRCA:: Identify the nodes for the most recent common ancestor (MRCA) for each Order (important for plotting)

# We want to select the most representatitve icons for each order, so determine which families dominate each order 
# NOTE:: Can use pic_n to select a different silhoutte as needed
Family_dominant <- Tax_tbl_nodes %>% count(order, family) %>% 
  slice_max(order_by = n, by = order, with_ties = TRUE) %>%
  filter(family != "Aramidae") %>%
  select(-n) %>% 
  mutate(pic_n = 1)

# Iterate over rows in tbl to identify MRCA & download the phylopic silhoutte
Orders_tbl <- pmap(Family_dominant, function(order, family, pic_n){
  nodes <- Tax_tbl_nodes %>% filter(order == {{ order }}) %>% 
    pull(node)
  phy_otu <- groupOTU(phylo_obs, nodes)
  # Phylopic - Get a single image uuid for a species
  image <- pick_phylopic(name = {{ family }}, auto = 2) # n = pic_n
  uuid <- get_uuid(img = image)
  # Combine in tbl
  tibble(Order = {{ order }}, Mrca_node = MRCA(phy_otu, nodes), 
         Uuid = uuid, N_spp = length(nodes))
}) %>% list_rbind()

# Join Orders and dominant family tbls
Orders_tbl2 <- Family_dominant %>% 
  full_join(Orders_tbl, by = join_by("order" == "Order")) %>% 
  rename(family_dom = family)

# Modify images (rotations & size adjustments) & save
Phylopic_path <- "Figures/Phylogeny/Phylopic/Bird_orders/"
if(FALSE){
  uuid_labs <- setNames(Orders_tbl$Uuid, Orders_tbl$Order)
  Make_larger <- c("Falconiformes", "Trogoniformes", "Cuculiformes", "Caprimulgiformes")
  imap(uuid_labs, \(uuid, order){
    img <- get_phylopic(uuid = uuid) 
    if(order == "Charadriiformes"){
      img <- flip_phylopic(img)
    }
    # Save images to Figures folder
    if(order %in% Make_larger){
      save_phylopic(img, path = paste0(Phylopic_path, order, ".png"), 
                    width = 1000, heigh = 1000) #580
    } 
    else{
      save_phylopic(img, path = paste0(Phylopic_path, order, ".png"))
    }
    # Default dimensions are 480 x 480
  })
}

# Visualize ---------------------------------------------------------------
## Plot phylogeny coloring species names by family and including orders on the outside of the phylogeny 
# NOTE:: Spent a lot of time trying to get clade labels on a circular tree
# This function created from phytools guy could work? http://blog.phytools.org/2017/03/clade-labels-on-circular-fan-tree.html?m=1
# What about some cheats, like removing some of the images in the really dense areas? 
Orders_tbl %>% arrange(N_spp) 
Orders_tbl2 <- Orders_tbl %>% filter(N_spp > 1 & Order != "Anseriformes") %>% 
  rowwise() %>%
  mutate(
    Image_path = paste0(Phylopic_path, Order, ".png")
  ) %>% 
  ungroup()

# Plot full phylogeny
ggtree(phylo_obs, layout='circular', aes(color = family)) %<+%
  Tax_tbl_nodes + 
  #geom_tiplab(size = 3, aes(color = family_gbif, angle = angle)) +
  geom_cladelab(
    data        = Orders_tbl2,
    mapping     = aes(node = Mrca_node, label = Order, image = Image_path),
    geom        = "image", # "phylopic"
    imagecolor  = "black",
    inherit.aes = FALSE,
    offset      = 3,    # distance from the clade
    barsize     = 0.3,  
    show.legend = FALSE
  ) + guides(color = "none") + 
  geom_cladelab(data = Orders_tbl2,
                mapping = aes(node = Mrca_node, label = Order), 
                fontsize = 3,
                angle = "auto", 
                offset = 10, 
                barsize = 0) # auto-rotates text radially
#geom_rootedge(5)#+ 
#xlim(-10, NA)
ggsave("Figures/Phylogeny/Phylogeny_equal_sizes.png")

# Visualize each order that has more than 1 species
plots_order <- pmap(Orders_tbl2[,c("order", "Mrca_node", "N_spp")], 
                    function(order, Mrca_node, N_spp) {
  if(N_spp > 1){
    tree_sub <- tree_subset(phylo_obs, Mrca_node, levels_back = 0)
    ggtree(tree_sub) %<+%
      Tax_tbl_nodes + # Add family labs later if desired
      geom_tiplab() +
      labs(title = order)
  } 
})
# compact(plots_order) # Remove NULLs & visualize

# Export tree -------------------------------------------------------------

# Write a single tree for BIOL314 class
write.tree(phylo_obs, file = "Derived/Single_tree.tre")

# Phylogenetic diversity (PD) -----------------------------------------------
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
