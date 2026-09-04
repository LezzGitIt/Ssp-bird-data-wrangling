## PhD birds in silvopastoral landscapes ##
## Standardize DataS1 column headers to English, then produce Spanish-headed copies -- a few pipeline headers are still Spanish (Fecha, Departamento, ...); the deposit ships all-English (Diego C267), with Spanish copies alongside.

## The crosswalk is Suppfiles/column_names.csv (name_current, name_en, name_es, tables). "Refresh crosswalk" keeps filled rows, adds any new deposit column (name_en pre-filled to name_current as a starting point), and drops retired ones. "Write copies" applies it: name_current -> name_en for DataS1_en/, name_en -> name_es for DataS1_es/.

# Setup ---------------------------------------------------------------------
library(readr)
library(dplyr)
library(purrr)
library(tibble)

deposit_dir    <- "DataS1"
crosswalk_path <- "Suppfiles/column_names.csv"
out_dir_en     <- "Derived/DataS1_en"   # gitignored -- all-English headers
out_dir_es     <- "Derived/DataS1_es"   # gitignored -- Spanish headers

# One entry per deposited table -- Bird_pcs_dist is intentionally not part of the deposit
deposit_files <- c("Bird_pcs_all", "Bird_pcs_analysis", "Event_covs",
                   "Site_covs", "Taxonomy", "Functional_traits")

# Current headers of every deposit table -----------------------------------
headers <- map(set_names(deposit_files), \(f) {
  names(read_csv(file.path(deposit_dir, paste0(f, ".csv")), n_max = 0, show_col_types = FALSE))
})

# One row per distinct current header, with the tables it appears in
headers_tbl <- imap(headers, \(cols, f) tibble(name_current = cols, table = f)) |>
  list_rbind() |>
  summarize(tables = paste(sort(unique(table)), collapse = "; "), .by = name_current)

# Refresh crosswalk -------------------------------------------------------
## Carry over filled name_en / name_es; new columns arrive with name_en = name_current as a default to override
existing <- if (file.exists(crosswalk_path)) {
  read_csv(crosswalk_path, show_col_types = FALSE) |> select(name_current, name_en, name_es)
} else {
  tibble(name_current = character(), name_en = character(), name_es = character())
}

crosswalk <- headers_tbl |>
  left_join(existing, by = "name_current") |>
  mutate(name_en = coalesce(name_en, name_current)) |>
  select(name_current, name_en, name_es, tables) |>
  arrange(name_current)

write_csv(crosswalk, crosswalk_path, na = "")

# Console report --------------------------------------------------------------
no_es <- crosswalk |> filter(is.na(name_es) | name_es == "")
cat("Crosswalk:", nrow(crosswalk), "columns;",
    sum(crosswalk$name_en != crosswalk$name_current), "renamed to English so far;",
    nrow(no_es), "still need name_es.\n")
cat("Edit", crosswalk_path, "-- set name_en for the Spanish headers, then fill every name_es.\n")

# Write copies -----------------------------------------------------------
stop()   # guard -- only run past here once name_en and name_es are settled

if (nrow(no_es) > 0) stop("Fill name_es for every row in ", crosswalk_path, " first.")

write_renamed <- function(from_dir, to_dir, from_col, to_col) {
  dir.create(to_dir, showWarnings = FALSE, recursive = TRUE)
  rename_map <- deframe(select(crosswalk, all_of(c(from_col, to_col))))
  walk(deposit_files, \(f) {
    df <- read_csv(file.path(from_dir, paste0(f, ".csv")), show_col_types = FALSE)
    names(df) <- rename_map[names(df)]
    write_csv(df, file.path(to_dir, paste0(f, ".csv")))
  })
}

write_renamed(deposit_dir, out_dir_en, "name_current", "name_en")   # -> Derived/DataS1_en/
write_renamed(out_dir_en,  out_dir_es, "name_en",      "name_es")   # -> Derived/DataS1_es/

cat("Wrote English copies to", out_dir_en, "and Spanish copies to", out_dir_es, "\n")
