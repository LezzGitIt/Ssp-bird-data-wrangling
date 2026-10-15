## Shared helpers for the pipeline scripts (Scripts/01_ .. 06_), the data-paper figure scripts and the manuscript qmd
## Package-qualified throughout so a script can source this file without attaching anything

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
