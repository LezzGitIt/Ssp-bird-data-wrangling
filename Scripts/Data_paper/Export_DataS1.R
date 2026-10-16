## PhD birds in silvopastoral landscapes ##
## Build the DataS1 deposit -- copy the six curated pipeline outputs and the curated range-decision list from Derived/Excels/ into the tracked DataS1/ folder, with a provenance manifest that also lists the raw point-count records (written to DataS1/Raw/ directly by 00_Merge_raw.R).

## DataS1/ holds only the final deposit tables; mid-pipeline artifacts that a reader can reproduce from the pipeline (e.g. Bird_pcs_dist.csv, the 04a range-screening outputs) are deliberately excluded. The hand-curated range decisions (Spp_remove_change.csv) are included because they cannot be regenerated: Bird_pcs_all.csv keeps the original identifications, and flag_range_decisions() (Scripts/Pipeline_fns.R) applies the list to reproduce Bird_pcs_analysis.csv.

## The pipeline writes English headers, so this step is mostly a freeze-into-place. It also validates every header against Suppfiles/column_names.csv (run Translate_column_names.R to refresh that). A Spanish-headed deposit is deferred; Translate_column_names.R still carries the machinery.

## The raw point-count records are not re-written here: they keep the providers' original headers (not in the crosswalk) and their blank vs NA distinction (blank = empty cell, NA = no such column), which re-writing with na = "" would erase.
## Run this AFTER a full pipeline run (Scripts/00_ .. 06_). Column selection / ordering happen in each script's export section.
## Column-definitions.csv is hand-maintained and is NOT overwritten here.
## The deposit's Metadata.md and README.md are rendered from Scripts/Data_paper/qmd/Metadata.qmd and Deposit_README.qmd (render both first); this script copies them, and the figures the Metadata shows, into DataS1/.

# Setup -----------------------------------------------------------------------
library(readr)
library(purrr)
library(dplyr)

# Map DataS1 file name (Ecology convention: hyphens between words, underscores for groupings) -> its producing script's Derived/Excels/ path
source_paths <- c(
  `Bird-point-counts_all`      = "Derived/Excels/Bird_pcs/Bird_pcs_all.csv",       # 02
  `Bird-point-counts_analysis` = "Derived/Excels/Bird_pcs/Bird_pcs_analysis.csv",  # 06_Analysis_wrangling
  `Survey-covariates`          = "Derived/Excels/Event_covs.csv",                  # 05_wvsc
  `Functional-traits`          = "Derived/Excels/Traits/Functional_traits.csv",    # 03
  `Site-covariates`            = "Derived/Excels/Site_covs.csv",                   # 01
  Taxonomy                     = "Derived/Excels/Taxonomy/Taxonomy.csv",           # 02
  `Range-review-decisions`     = "Derived/Excels/Spp_remove_change.csv"            # hand-curated from the 04a review; applied by 04b
)
raw_paths <- c(
  `Raw_bird-records`         = "DataS1/Raw/Raw_bird-records.csv",          # 00, written in place
  `Raw_point-count-metadata` = "DataS1/Raw/Raw_point-count-metadata.csv"   # 00, written in place
)
crosswalk_path <- "Suppfiles/column_names.csv"

# Guards --------------------------------------------------------------------
missing <- c(source_paths, raw_paths)[!file.exists(c(source_paths, raw_paths))]
if (length(missing) > 0) {
  stop("Missing pipeline outputs -- run the upstream scripts first:\n",
       paste0("  ", names(missing), ": ", missing, collapse = "\n"))
}
if (!file.exists(crosswalk_path)) stop("Missing ", crosswalk_path, " -- run Translate_column_names.R first.")

crosswalk <- read_csv(crosswalk_path, show_col_types = FALSE) %>%
  mutate(name_en = coalesce(na_if(name_en, ""), name_current))
to_en <- setNames(crosswalk$name_en, crosswalk$name_current)

# Build DataS1/ -----------------------------------------------------------
dir.create("DataS1", showWarnings = FALSE)

exported <- imap(source_paths, function(src, name) {
  df <- read_csv(src, show_col_types = FALSE)
  unmapped <- base::setdiff(names(df), names(to_en))
  if (length(unmapped)) {
    stop(name, ".csv has headers absent from the crosswalk: ", paste(unmapped, collapse = ", "),
         "\n  -> run Translate_column_names.R to refresh Suppfiles/column_names.csv")
  }
  names(df) <- unname(to_en[names(df)])   # identity for already-English headers; a safety net for drift
  dest <- file.path("DataS1", paste0(name, ".csv"))
  write_csv(df, dest, na = "")   # missing values as blank cells (matches the manuscript)
  tibble(file = paste0(name, ".csv"), source = src, rows = nrow(df))
}) %>% list_rbind()

## Raw point-count records: already in DataS1/Raw/, just counted for the manifest
exported_raw <- imap(raw_paths, function(src, name) {
  rows <- nrow(read_csv(src, col_types = cols(.default = col_character()), na = "NA"))
  tibble(file = paste0("Raw/", name, ".csv"), source = src, rows = rows)
}) %>% list_rbind()
exported <- bind_rows(exported, exported_raw)

## Metadata, README, and the Metadata's figures: figure paths point at the repo's Figures/ folder from Scripts/Data_paper/qmd/, so rewrite them to the deposit's Figures/ folder and copy the images
docs_dir <- "Scripts/Data_paper/qmd"
docs <- c("Metadata.md", "README.md")
missing_docs <- docs[!file.exists(file.path(docs_dir, docs))]
if (length(missing_docs)) stop("Render first: ", paste(missing_docs, collapse = ", "), " (quarto render Metadata.qmd / Deposit_README.qmd in ", docs_dir, ")")
dir.create("DataS1/Figures", showWarnings = FALSE)
walk(docs, function(doc) {
  txt <- readLines(file.path(docs_dir, doc), encoding = "UTF-8")
  fig_paths <- unique(unlist(regmatches(txt, gregexpr("\\.\\./\\.\\./\\.\\./Figures/[^)\" ]+\\.png", txt))))
  walk(fig_paths, \(f) file.copy(file.path(docs_dir, f), file.path("DataS1/Figures", basename(f)), overwrite = TRUE))
  txt <- gsub("\\.\\./\\.\\./\\.\\./Figures/(Static/)?", "Figures/", txt)
  writeLines(txt, file.path("DataS1", doc), useBytes = TRUE)
})

# Provenance manifest ------------------------------------------------------
git_sha <- tryCatch(system("git rev-parse --short HEAD", intern = TRUE),
                    error = function(e) NA_character_, warning = function(w) NA_character_)
manifest <- c(
  "DataS1 export manifest",
  paste("exported_at:", format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")),
  paste("pipeline_commit:", git_sha),
  "headers: English (Raw/: the providers' original Spanish headers)",
  "documents: Metadata.md, README.md, Figures/ (rendered from Scripts/Data_paper/qmd/)",
  "",
  "file,source,rows",
  paste(exported$file, exported$source, exported$rows, sep = ",")
)
writeLines(manifest, "DataS1/EXPORT_manifest.txt")

# Console report --------------------------------------------------------------
cat("Exported", nrow(exported), "files to DataS1/ (commit", git_sha, ")\n")
print(as.data.frame(exported[c("file", "rows")]))
cat("Review with:  git diff --stat DataS1/\n")
