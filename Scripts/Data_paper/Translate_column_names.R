## PhD birds in silvopastoral landscapes ##
## Translate the DataS1 column headers to Spanish -- the deposit ships with English headers; this writes Spanish-headed copies of the tables from a hand-maintained crosswalk.

## The crosswalk lives in Suppfiles/column_names_es.csv (name_en, name_es, tables). The "Refresh crosswalk" section keeps every filled translation, adds any new deposit column with a blank name_es, and drops columns no longer in the deposit. The "Write translated copies" section, below the stop(), applies the completed crosswalk.

# Setup ---------------------------------------------------------------------
library(readr)
library(dplyr)
library(purrr)
library(tibble)

deposit_dir    <- "DataS1"
crosswalk_path <- "Suppfiles/column_names_es.csv"
out_dir        <- "Derived/DataS1_es"   # gitignored; regenerated, not the deposit (whether Spanish copies are deposited is still open)

# One entry per deposited table -- Bird_pcs_dist is intentionally not part of the deposit
deposit_files <- c("Bird_pcs_all", "Bird_pcs_analysis", "Event_covs",
                   "Site_covs", "Taxonomy", "Functional_traits")

# Current headers of every deposit table -----------------------------------
headers <- map(set_names(deposit_files), \(f) {
  names(read_csv(file.path(deposit_dir, paste0(f, ".csv")), n_max = 0, show_col_types = FALSE))
})

# One row per distinct English header, with the tables it appears in
headers_tbl <- imap(headers, \(cols, f) tibble(name_en = cols, table = f)) |>
  list_rbind() |>
  summarize(tables = paste(sort(unique(table)), collapse = "; "), .by = name_en)

# Refresh crosswalk -------------------------------------------------------
## Left-join current headers onto the existing crosswalk: filled name_es carries over, new columns come in blank, retired columns fall away
existing <- if (file.exists(crosswalk_path)) {
  read_csv(crosswalk_path, show_col_types = FALSE) |> select(name_en, name_es)
} else {
  tibble(name_en = character(), name_es = character())
}

crosswalk <- headers_tbl |>
  left_join(existing, by = "name_en") |>
  select(name_en, name_es, tables) |>
  arrange(name_en)

write_csv(crosswalk, crosswalk_path, na = "")

# Console report --------------------------------------------------------------
untranslated <- crosswalk |> filter(is.na(name_es) | name_es == "")
cat("Crosswalk:", nrow(crosswalk), "distinct columns;",
    nrow(untranslated), "still need a Spanish name.\n")
if (nrow(untranslated) > 0) print(untranslated, n = Inf)

# Write translated copies -------------------------------------------------
stop()   # guard -- only run past here once every name_es is filled

if (nrow(untranslated) > 0) {
  stop("Fill name_es for every row in ", crosswalk_path, " before writing translated copies.")
}

dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
rename_map <- deframe(select(crosswalk, name_en, name_es))

walk(deposit_files, \(f) {
  df <- read_csv(file.path(deposit_dir, paste0(f, ".csv")), show_col_types = FALSE)
  names(df) <- rename_map[names(df)]
  write_csv(df, file.path(out_dir, paste0(f, "_es.csv")))
})

cat("Wrote", length(deposit_files), "Spanish-headed CSVs to", out_dir, "/\n")
