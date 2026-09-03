## PhD birds in silvopastoral landscapes ##
## Phylogenetic diversity (PD) metrics
## Split out of the old Scripts/09_Phylogeny.R; bound for Ch1-ssp-birds (chapter analysis, not part of the data paper).
## Reads the pruned tree written by Scripts/Data_paper/Phylogeny_fig.R (Derived/Single_tree.tre) -- vendor a copy of that tree into Ch1 when this moves.

## TO DO: document the purpose of the cophenetic.phylo step; incorporate all 1000 BirdTree trees (maxCladeCred).

# Setup -----------------------------------------------------------------------
library(phytools)

phylo_obs <- read.tree("Derived/Single_tree.tre")

# Other packages to explore in the future:
# picante -- community phylogenetics
# caper   -- trait correlations, phylogenetic regressions

# Phylogenetic diversity (PD) ------------------------------------------------
## Background: multiple models of evolution.

## Brownian model -- changes in traits accrue slowly through time, so the time
## passed is what matters; sum the evolutionary time (millions of years).
sum(phylo_obs$edge.length) # total phylogenetic diversity in millions of years

## Punctuated model -- long stasis then rapid change at speciation, so the
## speciation events matter; rescale so every branch length is equal (the trailing
## number weights the model: 0 = fully punctuated, .5 = a Brownian/punctuated mix).
phylo_kappa <- phytools::rescale(phylo_obs, model = "kappa", 0) # .5
PD_punctuated <- sum(phylo_kappa$edge.length)
PD_punctuated
# If every node bifurcates this equals the number of internal nodes:
PD_punctuated / 2

## Visualize the two models
plot(phylo_obs)
plot(phylo_kappa)

# Future analyses ----------------------------------------------------------
## Pairwise (cophenetic) distances between tips from branch lengths -- possibly
## useful for asking how much evolutionary history a community contains, or whether
## phylogenetically distant species respond differently to habitat change.
Evol_dist <- round(cophenetic.phylo(phylo_obs), 2)
Evol_dist[1:6, 1:6]

## Phylogenetic correlation matrix
Phylo_cor <- round(vcv(phylo_obs, cor = TRUE), 2)
Phylo_cor[1:6, 1:6]
