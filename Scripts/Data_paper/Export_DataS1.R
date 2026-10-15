## PhD birds in silvopastoral landscapes ##
## Build the DataS1 deposit -- copy the six curated pipeline outputs and the curated range-decision list from Derived/Excels/ into the tracked DataS1/ folder, plus a provenance manifest.

## DataS1/ holds only the final deposit tables; mid-pipeline artifacts that a reader can reproduce from the pipeline (e.g. Bird_pcs_dist.csv, the 04a range-screening outputs) are deliberately excluded. The hand-curated range decisions (Spp_remove_change.csv) are included because they cannot be regenerated: Bird_pcs_all.csv keeps the original identifications, and flag_range_decisions() (Scripts/Pipeline_fns.R) applies the list to reproduce Bird_pcs_analysis.csv.

## The pipeline writes English headers, so this step is mostly a freeze-into-place. It also validates every header against Suppfiles/column_names.csv (run Translate_column_names.R to refresh that). A Spanish-headed deposit is deferred; Translate_column_names.R still carries the machinery.

## Run this AFTER a full pipeline run (Scripts/01_ .. 06_). Column selection / ordering happen in each script's export section.
## Column_definitions_final.xlsx is hand-maintained and is NOT overwritten here.

# Setup -----------------------------------------------------------------------
library(readr)
library(purrr)
library(dplyr)

# Map DataS1 table -> its producing script's Derived/Excels/ path -------------
source_paths <- c(
  Bird_pcs_all      = "Derived/Excels/Bird_pcs/Bird_pcs_all.csv",       # 02
  Bird_pcs_analysis = "Derived/Excels/Bird_pcs/Bird_pcs_analysis.csv",  # 06_Analysis_wrangling
  Event_covs        = "Derived/Excels/Event_covs.csv",                  # 05_wvsc
  Functional_traits = "Derived/Excels/Traits/Functional_traits.csv",    # 03
  Site_covs         = "Derived/Excels/Site_covs.csv",                   # 01
  Taxonomy          = "Derived/Excels/Taxonomy/Taxonomy.csv",           # 02
  Spp_remove_change = "Derived/Excels/Spp_remove_change.csv"            # hand-curated from the 04a review; applied by 04b
)
crosswalk_path <- "Suppfiles/column_names.csv"

# Guards --------------------------------------------------------------------
missing <- source_paths[!file.exists(source_paths)]
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

# Provenance manifest ------------------------------------------------------
git_sha <- tryCatch(system("git rev-parse --short HEAD", intern = TRUE),
                    error = function(e) NA_character_, warning = function(w) NA_character_)
manifest <- c(
  "DataS1 export manifest",
  paste("exported_at:", format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")),
  paste("pipeline_commit:", git_sha),
  "headers: English",
  "",
  "file,source,rows",
  paste(exported$file, exported$source, exported$rows, sep = ",")
)
writeLines(manifest, "DataS1/EXPORT_manifest.txt")

# Console report --------------------------------------------------------------
cat("Exported", nrow(exported), "files to DataS1/ (commit", git_sha, ")\n")
print(as.data.frame(exported[c("file", "rows")]))
cat("Review with:  git diff --stat DataS1/\n")
