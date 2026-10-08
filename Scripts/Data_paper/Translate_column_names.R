## PhD birds in silvopastoral landscapes ##
## Maintain the column-name crosswalk. The pipeline writes English headers, so DataS1/ is English; this crosswalk carries the Spanish equivalents for a possible future Spanish-headed deposit.
## Spanish output never goes into DataS1/ -- the optional section at the end writes Spanish-headed preview copies to Derived/DataS1_es/ (gitignored).

## Crosswalk: Suppfiles/column_names.csv (name_current, name_en, name_es, tables).
## name_current = the header the pipeline writes in Derived/Excels/ (English after the 2026 rename).
## name_en      = the English deposit header (defaults to name_current).
## name_es      = the Spanish deposit header (fill every row).

## This script only refreshes the crosswalk against the current pipeline outputs and reports
## what still needs a Spanish name. It writes nothing into the deposit.

# Setup ---------------------------------------------------------------------
library(readr)
library(dplyr)
library(purrr)
library(tibble)

crosswalk_path <- "Suppfiles/column_names.csv"

# Deposit source files -- must stay in sync with Export_DataS1.R's source_paths
deposit_source <- c(
  Bird_pcs_all      = "Derived/Excels/Bird_pcs/Bird_pcs_all.csv",
  Bird_pcs_analysis = "Derived/Excels/Bird_pcs/Bird_pcs_analysis.csv",
  Event_covs        = "Derived/Excels/Event_covs.csv",
  Site_covs         = "Derived/Excels/Site_covs.csv",
  Taxonomy          = "Derived/Excels/Taxonomy/Taxonomy.csv",
  Functional_traits = "Derived/Excels/Traits/Functional_traits.csv"
)

# Current headers of every deposit table -----------------------------------
headers_tbl <- imap(deposit_source, \(path, tbl) {
  tibble(name_current = names(read_csv(path, n_max = 0, show_col_types = FALSE)), table = tbl)
}) |>
  list_rbind() |>
  summarize(tables = paste(sort(unique(table)), collapse = "; "), .by = name_current)

# Refresh crosswalk -------------------------------------------------------
## Carry over filled name_en / name_es; new columns arrive with name_en = name_current; retired columns drop out
existing <- if (file.exists(crosswalk_path)) {
  read_csv(crosswalk_path, show_col_types = FALSE) |> select(name_current, name_en, name_es)
} else {
  tibble(name_current = character(), name_en = character(), name_es = character())
}

crosswalk <- headers_tbl |>
  left_join(existing, by = "name_current") |>
  mutate(name_en = coalesce(na_if(name_en, ""), name_current)) |>
  select(name_current, name_en, name_es, tables) |>
  arrange(tables, name_current)   # group by deposit table so name_es is easy to fill in one pass

write_csv(crosswalk, crosswalk_path, na = "")

# Console report --------------------------------------------------------------
no_es <- crosswalk |> filter(is.na(name_es) | name_es == "")
cat("Crosswalk:", nrow(crosswalk), "columns;", nrow(no_es), "still need name_es.\n")
if (nrow(no_es)) cat("  ", paste(no_es$name_current, collapse = ", "), "\n")
cat("Edit", crosswalk_path, "then run Export_DataS1.R.\n")

stop()   # nothing below -- Export_DataS1.R does the deposit build

# Optional: local preview of the Spanish-headed tables (Derived/ is gitignored) ----
if (nrow(no_es) > 0) stop("Fill every name_es first.")
rename_to <- function(df, from, to) { m <- stats::setNames(crosswalk[[to]], crosswalk[[from]]); names(df) <- m[names(df)]; df }
walk2(names(deposit_source), deposit_source, \(tbl, path) {
  dir.create("Derived/DataS1_es", showWarnings = FALSE, recursive = TRUE)
  df <- read_csv(path, show_col_types = FALSE) |> rename_to("name_en", "name_es")
  write_csv(df, file.path("Derived/DataS1_es", paste0(tbl, ".csv")))
})
cat("Wrote Spanish-headed preview copies to Derived/DataS1_es/\n")
