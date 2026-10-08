## Shared helpers for the data-paper scripts (Figs_tables.R and the manuscript qmd)

# Ecoregion display names: the data store sentence-cased names without accents (the pipeline's convention, e.g. "Eje cafetero"); figures, tables, and text show the Spanish proper names
Ecoregion_labels <- c(
  "Bajo magdalena"      = "Bajo Magdalena",
  "Cordillera oriental" = "Cordillera Oriental",
  "Eje cafetero"        = "Eje Cafetero",
  "Piedemonte"          = "Piedemonte",
  "Rio cesar"           = "Río Cesar"
)
ecoregion_label <- function(x) {
  out <- unname(Ecoregion_labels[as.character(x)])
  ifelse(is.na(out), as.character(x), out)
}
