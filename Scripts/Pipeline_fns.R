## Shared helpers for the pipeline scripts (Scripts/01_ .. 06_), the data-paper figure scripts and the manuscript qmd
## Package-qualified throughout so a script can source this file without attaching anything
## The formatting, quality-check and plotting helpers were moved here from Rcookbook/Themes_funs.R (2026-10-15) so the repo runs without that external file

# Formatting ---------------------------------------------------------------

## Standardize the values of a character column: underscores to spaces, sentence case, squished whitespace. Transforms each unique value once, so it is fast on long columns.
standardize_column_contents <- function(col) {
  unique_vals <- unique(col)
  transformed <- unique_vals |>
    stringr::str_replace_all("_", " ") |>
    stringr::str_to_sentence() |>
    stringr::str_squish()
  transformed_vals <- stats::setNames(transformed, unique_vals)
  transformed_vals[col]
}

## Standardize column names: snake_case, then capitalize the first letter (e.g. "ID_punto_muestreo_FINAL" -> "Id_punto_muestreo_final")
Cap_snake <- function(df) {
  df <- janitor::clean_names(df, case = "snake")
  names(df) <- stringr::str_to_sentence(names(df))
  df
}

# Quality checks -----------------------------------------------------------
## Return the rows and columns of `df` that contain missing values (restricted to cols_inc), plus id_cols to identify the rows; distinct = TRUE drops duplicate rows
Na_rows_cols <- function(df, cols_inc = dplyr::everything(), id_cols = NULL, distinct = TRUE) {
  df2 <- dplyr::select(df, {{ cols_inc }})
  row_col_locs <- naniar::where_na(df2)   # matrix of (row, col) positions of the NAs
  result <- df2 |>
    dplyr::slice(row_col_locs[, 1]) |>
    dplyr::select({{ id_cols }}, unique(row_col_locs[, 2]))
  if (distinct) result <- dplyr::distinct(result)
  result
}

# Plotting -----------------------------------------------------------------
## Round each value of x to the nearest multiple of base (e.g. mround(4.37, 0.25) = 4.25)
mround <- function(x, base) {
  base * round(x / base)
}

## Like ggplot2::theme_void() but keeps the axis lines: drops axis titles, text and ticks. A function (add as `+ theme_min()`) so sourcing this file does not load ggplot2.
theme_min <- function() {
  ggplot2::theme(
    axis.title.x = ggplot2::element_blank(),
    axis.text.x = ggplot2::element_blank(),
    axis.ticks.x = ggplot2::element_blank(),
    axis.title.y = ggplot2::element_blank(),
    axis.text.y = ggplot2::element_blank(),
    axis.ticks.y = ggplot2::element_blank()
  )
}

# Range decisions ----------------------------------------------------------
## Flag the observations that the curated remove/change list (Spp_remove_change.csv, deposited in DataS1/) applies to, and apply its species reassignments.
# A decision applies to an observation when the species matches and, if Departments_affected is filled (comma-separated), the observation's department is listed; if Elev_max is filled, only at or below that elevation (m).
# Returns every row of `observations` with Species_ayerbe updated for "Change" decisions, plus Species_reported (the name before any change) and Range_decision ("Remove", "Change", or NA). Drop the "Remove" rows to reproduce Bird_pcs_dist.csv / the analysis subset.
flag_range_decisions <- function(observations, decisions, site_covs) {
  rules <- tidyr::separate_rows(decisions, Departments_affected, sep = ",\\s*")
  rule_keys <- rules[c("Species_ayerbe", "Departments_affected", "Decision")]
  if (anyDuplicated(rule_keys)) {
    stop("Duplicated species x department x decision in the decision list -- each observation must match at most one rule")
  }

  # Mark the rows matched by one class of rule (decision type x department-specific or global); rows already decided are skipped, so each observation gets at most one decision
  match_rules <- function(df, decision_type, dept_specific) {
    r <- rules |>
      dplyr::filter(Decision == decision_type, (!is.na(Departments_affected)) == dept_specific) |>
      dplyr::select(Species_ayerbe, Departments_affected, .new_species = Species_changed, .elev_max = Elev_max) |>
      dplyr::mutate(.matched = TRUE)
    if (dept_specific) {
      by <- c("Species_ayerbe", "Department" = "Departments_affected")
    } else {
      r <- dplyr::select(r, -Departments_affected)
      by <- "Species_ayerbe"
    }
    df |>
      dplyr::left_join(r, by = by) |>
      dplyr::mutate(
        .matched = dplyr::coalesce(.matched, FALSE) & is.na(Range_decision) &
          (is.na(.elev_max) | is.na(Elev) | Elev <= .elev_max),
        Range_decision = dplyr::if_else(.matched, decision_type, Range_decision),
        Species_ayerbe = dplyr::if_else(.matched & decision_type == "Change", .new_species, Species_ayerbe)
      ) |>
      dplyr::select(-c(.new_species, .elev_max, .matched))
  }

  observations |>
    dplyr::left_join(dplyr::select(site_covs, Id_survey_no_dc, Department, Elev), by = "Id_survey_no_dc") |>
    dplyr::mutate(Species_reported = Species_ayerbe, Range_decision = NA_character_) |>
    match_rules("Remove", dept_specific = TRUE) |>
    match_rules("Remove", dept_specific = FALSE) |>
    match_rules("Change", dept_specific = TRUE) |>
    match_rules("Change", dept_specific = FALSE) |>
    dplyr::select(-c(Department, Elev))
}
