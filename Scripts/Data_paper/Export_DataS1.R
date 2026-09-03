## PhD birds in silvopastoral landscapes ##
## Export the DataS1 deposit -- copy the seven curated pipeline outputs from Derived/Excels/ into the tracked DataS1/ folder, plus a provenance manifest.

## Run this AFTER a full pipeline run (Scripts/01_ .. 09_). The Derived/Excels/ outputs are already in deposit format (column selection + naming happen in each script's export section); this step only freezes them into the versioned DataS1/ folder so a re-export is a clean `git diff`.

## Column_definitions_final.xlsx is hand-maintained and is NOT overwritten here.

# Setup -----------------------------------------------------------------------
library(readr)
library(purrr)

# Map DataS1 filename -> its producing script's Derived/Excels/ path ----------
# forest_typ / canopy / land-cover columns in Event_covs come from 05->06->07; if those columns are dropped from the deposit (pending decision), re-run 07 first so Event_covs.csv reflects that.
source_paths <- c(
  Bird_pcs_all      = "Derived/Excels/Bird_pcs/Bird_pcs_all.csv",       # 02
  Bird_pcs_dist     = "Derived/Excels/Bird_pcs/Bird_pcs_dist.csv",      # 04
  Bird_pcs_analysis = "Derived/Excels/Bird_pcs/Bird_pcs_analysis.csv",  # 08_Analysis_wrangling
  Event_covs        = "Derived/Excels/Event_covs.csv",                  # 07
  Functional_traits = "Derived/Excels/Traits/Functional_traits.csv",    # 03
  Site_covs         = "Derived/Excels/Site_covs.csv",                   # 01
  Taxonomy          = "Derived/Excels/Taxonomy/Taxonomy.csv"            # 02
)

# Guard: every source must exist ---------------------------------------------
missing <- source_paths[!file.exists(source_paths)]
if (length(missing) > 0) {
  stop(
    "Missing pipeline outputs -- run the upstream scripts first:\n",
    paste0("  ", names(missing), ": ", missing, collapse = "\n")
  )
}

# Copy into DataS1/ ----------------------------------------------------------
dir.create("DataS1", showWarnings = FALSE)

copied <- imap_chr(source_paths, function(src, name) {
  dest <- file.path("DataS1", paste0(name, ".csv"))
  file.copy(src, dest, overwrite = TRUE)
  dest
})

# Provenance manifest ------------------------------------------------------
git_sha <- tryCatch(
  system("git rev-parse --short HEAD", intern = TRUE),
  error = function(e) NA_character_, warning = function(w) NA_character_
)

row_counts <- map_int(source_paths, \(p) nrow(suppressMessages(read_csv(p, show_col_types = FALSE))))

manifest <- c(
  "DataS1 export manifest",
  paste("exported_at:", format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")),
  paste("pipeline_commit:", git_sha),
  "",
  "file,source,rows",
  paste(paste0(names(source_paths), ".csv"), source_paths, row_counts, sep = ",")
)
writeLines(manifest, "DataS1/EXPORT_manifest.txt")

# Console report --------------------------------------------------------------
cat("Exported", length(copied), "files to DataS1/ (commit", git_sha, ")\n")
print(data.frame(file = paste0(names(source_paths), ".csv"), rows = row_counts, row.names = NULL))
cat("\nReview with:  git diff --stat DataS1/\n")
