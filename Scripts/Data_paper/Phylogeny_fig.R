## PhD birds in silvopastoral landscapes ##
## Data paper -- phylogeny figure + per-order taxonomy summary table
## Prunes the BirdTree phylogeny to the observed species and produces the two data-paper deliverables: Figures/Static/Phylogeny_equal_sizes.png and Derived/Excels/Taxonomy/Tax_summary.csv. The phylogenetic-diversity metrics that used to live here moved to Scripts/_ch1_pending/Phylogenetic_diversity.R (bound for Ch1-ssp-birds).

## Contents:
# 1) Taxonomy: use Taxonomy.csv to switch to BirdTree names
# 2) Phylogeny: read in the pre-pruned single tree, prune to observed species
# 3) Order & family: link order/family to the tree tips
# 4) Summary table: families / genera / species per order -> Tax_summary.csv
# 5) MRCA: most recent common ancestor per order (for plotting)
# 6) Phylopic: download / curate one silhouette per order
# 7) Visualize: circular phylogeny -> Figures/Static/Phylogeny_equal_sizes.png

## TO DO: incorporate all 1000 BirdTree trees (maxCladeCred, phangorn) rather than the single pre-pulled tree.

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
source("/Users/aaronskinner/Library/CloudStorage/OneDrive-UBC/Academia/Rcookbook/Themes_funs.R")

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
Spp_join_bt2 %>% filter(Species_bt == "Momotus momota")

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

### Guard: every BirdTree name we mapped an observed species to must be a tip in the tree
## A name in Spp_obs_bt but not in the tree is a mismatch to fix (bad crosswalk / not in this BirdTree set), not a species to silently lose
Bt_names_missing_from_tree <- setdiff(Spp_obs_bt, phylo_obs$tip.label)
if (length(Bt_names_missing_from_tree) > 0) {
  stop("BirdTree names mapped from observed species but absent from the phylogeny: ",
       paste(Bt_names_missing_from_tree, collapse = ", "))
}

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
  Flip <- c("Coraciiformes", "Galbuliformes", "Cathartiformes", "Cuculiformes",
            "Charadriiformes", "Galliformes")

  # Map through images saving final form
  imap(uuid_labs, \(uuid, order){
    img <- get_phylopic(uuid = uuid)
    if(order %in% Flip){
      img <- flip_phylopic(img)
    }
    else{
      save_phylopic(img, path = paste0(Phylopic_path, "auto2/", order, ".png"))
    }
  })
}

# >Combine icons in final -------------------------------------------------
## Manually paste auto1 into _final folder, then move a few orders that look better from auto_2 or that were manually downloaded
Move_auto2 <- c("Cathartiformes", "Charadriiformes", "Coraciiformes", "Cuculiformes", "Falconiformes")
if(!file.exists(paste0(Phylopic_path, "final/"))){
  # From auto_2
  map(Move_auto2, \(order_auto2){
    file.copy(from = paste0(Phylopic_path,  "auto2/", order_auto2, ".png"),
              to = paste0(Phylopic_path, "final/", order_auto2, ".png"),
              overwrite = TRUE)
  })
  # From manual download
  Orders_move_png <- list.files(paste0(Phylopic_path, "manual/"))
  map(Orders_move_png, \(order_manual){
    file.copy(from = paste0(Phylopic_path,  "manual/", order_manual),
              to = paste0(Phylopic_path, "final/", order_manual),
              overwrite = TRUE)
  })
}

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

# Adjust size
Make_way_larger <- c("Falconiformes",  "Caprimulgiformes", "Cathartiformes")
Make_larger <- c("Trogoniformes", "Piciformes", "Galliformes", "Accipitriformes", "Pelecaniformes")
Make_smaller <- c("Coraciiformes", "Charadriiformes", "Tinamiformes") #"Galbuliformes"

# Create image_size column
Orders_plot2 <- Orders_plot %>%
  mutate(image_size = case_when(
    Order %in% Make_way_larger  ~ 0.07,
    Order %in% Make_larger  ~ 0.06,
    Order %in% Make_smaller ~ 0.04,
    .default = 0.05
  ))

# Plot full phylogeny
Phylo_plot <- ggtree(phylo_obs, layout='circular', aes(color = Family)) %<+%
  Tax_tbl_nodes +
  # This geom_cladelab() controls the images
  geom_cladelab(
    data        = Orders_plot2,
    mapping     = aes(node = Mrca_node,
                      label = Label,
                      image = Image_path,
                      size = image_size),
    geom        = "image", # "phylopic"
    imagecolor  = "black",
    inherit.aes = FALSE,
    offset      = 3,    # distance from the clade
    barsize     = 0.3,
    show.legend = FALSE
  ) + scale_size_identity() +
  guides(color = "none") +
  # This geom_cladelab() controls the text
  geom_cladelab(data = Orders_plot,
                mapping = aes(node = Mrca_node, label = Label),
                fontsize = 3,
                angle = "auto",
                offset = 11,
                barsize = 0) + # auto-rotates text radially
  theme(plot.margin = margin(30, 30, 20, -40)) # Control

ggsave("Figures/Static/Phylogeny_equal_sizes.png", plot = Phylo_plot,
       height = 8, width = 7.35)
print(Phylo_plot)

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
# Write the pruned single tree (read back by Scripts/_ch1_pending/Phylogenetic_diversity.R)
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
          file = "Derived/Excels/Taxonomy/Tax_summary.csv")

stop()

# EXTRAS ------------------------------------------------------------------
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
