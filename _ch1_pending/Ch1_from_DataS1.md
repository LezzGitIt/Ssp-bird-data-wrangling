# Building Chapter 1 inputs from this repo

Notes for a future session working in **`Ch1-ssp-birds`**. This repo
(`Ssp-bird-data-wrangling`) is the SCR bird-data pipeline + the *Ecology* data
paper. Ch1 consumes its outputs. As of 2026-09 three things changed here that
Ch1 needs to absorb:

1. every column name is English now (§1)
2. the data-paper deposit (`DataS1/`) drops the functional traits Ch1 relies on —
   they must be rebuilt (§2)
3. the canopy (`05`) and analysis-filtering (`06`) steps were reworked (§3, §4)

`_ch1_pending/` already holds the digitized-land-cover scripts (`Extract_lcs.R`,
`LSM.R`), the riparian work (`Riparian/`), and the phylogenetic-diversity script —
all with the English rename applied. Move them into Ch1 once its tree is clean
(see `_ch1_pending/README.md` for the per-file recipe).

---

## 1. Column names are English

Any Ch1 code written before 2026-09 that references the old Spanish headers must
be updated. The full map:

| old | new | old | new |
|---|---|---|---|
| `Id_muestreo` | `Id_survey` | `Clima` | `Weather` |
| `Id_muestreo_no_dc` | `Id_survey_no_dc` | `Registrado_por` | `Registered_by` |
| `Id_gcs` | `Id_scr` | `Nombre_institucion` | `Institution_name` |
| `Fecha` | `Date` | `Nombre_finca` | `Farm_name` |
| `Ano` | `Year` | `Grabacion` | `Recording` |
| `Ano_grp` | `Year_grp` | `Tipo_registro` | `Obs_type` |
| `Rep_ano_grp` | `Rep_year_grp` | `Departamento` | `Department` |
| `Mes` | `Month` | `Distancia_bird` | `Distance_bird` |
| `Dia` | `Day` | `Distancia_farm` | `Distance_farm` |

Not renamed: raw provider coords `Latitud` / `Longitud` (they become `Lat` / `Long`
where the pipeline standardises them). Some cell *values* are still Spanish
(`Obs_type == "Sobrevuelo"` for flyovers; `Habitat`, `Habitat_sub` categories).

`Suppfiles/column_names.csv` (`name_current → name_en → name_es`) carries the
crosswalk; the optional last section of `Scripts/Data_paper/Translate_column_names.R`
writes Spanish-headed copies to `Derived/DataS1_es/`.

---

## 2. Functional traits the deposit drops

`DataS1/Functional_traits.csv` has 38 columns: AVONET morphology, BIRDBASE
diet / clutch / migration / specialization, BirdLife generation length + IUCN
status, and a single-source Ayerbe-Quiñones (2018) `Elev_range`. `Scripts/03_FT_elev.R`
builds an object `Ft_final` that also carries the traits below; the deposit
`select()` at the end of `03` is the only thing that trims them out.

| Ch1 trait column(s) | `03` section (`# … ----`) | external data it needs |
|---|---|---|
| `Eye_resid`, `Source_eye` | `# Eye_size` (~L703) — combined eye size, allometric residual on `Mass` | `Datasets_external/Eye Size Files/Final_Book_Join.xlsx` (Ausprey 2021/2024, Jones 2023) |
| `Clutch` (BirdLife) | `# >Clutch size` (~L505), via `Life_history` from `bird20t` | `bird20t` — already loaded in `03` for generation length; no extra file |
| `Nest_ground_bush`, `N_nest_locs`, `Nest_exposure` (also `Nest_structure`, `Nest_location`) | `# Nesting` (L504–699) | `Datasets_external/Sheard_et_al_geb_Nesting_traits_2023/Dataset-S{1,2}.csv`; `Derived/Excels/Traits/Nest_exposure.csv` (hand-classified nest exposure) |
| `Source_elev` + the multi-source `Elev_range` | `# >Differences sources` / `# >Elev ranges` (L286–357) | none extra — `Derived/Excels/Elev_ranges_all_sources.csv` already holds every source (Ayerbe, Hilty 2021, Quintero & Jetz 2018, eBird/Freeman 2022) + the `Source_elev` label |

The join key for every trait is `Species_ayerbe`.

### How to rebuild them

**Option A (preferred) — carry a full `03` into Ch1.** Copy `Scripts/03_FT_elev.R`
into Ch1. It already produces `Ft_final` with every trait. Either take `Ft_final`
directly (before the deposit `select()`), or extend that `select()` with the Ch1
columns. Inputs: `DataS1/Taxonomy.csv` (as `Tax_df`), `DataS1/Bird_pcs_all.csv`,
the AVONET / BIRDBASE / eye-size / Sheard files under `Datasets_external/`.

**Option B — slim add-on.** Read `DataS1/Functional_traits.csv`, run only the
Eye_size + Nesting + BirdLife-clutch blocks of `03`, and `left_join` the extras by
`Species_ayerbe`. Less code, but those blocks still need `Tax_df`
(= `DataS1/Taxonomy.csv`) and `Ft_df[, c("Species_ayerbe", "Mass")]` from the
deposit table for the eye-size allometry.

### Elevational range — note the change

The deposit's `Elev_range` is now **single-source Ayerbe-Quiñones (2018)** (covers
586 / 587 species) — more defensible for the paper than the old coalesce across
four sources. If Ch1 wants the multi-source version back, `03`'s
`# >Elev ranges` section still builds `Elev_final` with all sources + `Source_elev`,
and `Derived/Excels/Elev_ranges_all_sources.csv` is written every run.

---

## 3. `05_wvsc.R` — canopy cover + height

Adds two per-survey covariates to `Event_covs`: `Canopy_cover` (mean over a 1 km
buffer) and `Canopy_height_m` (max over a 50 m buffer), matched to the survey year.

- **Inputs:** `Derived/Excels/Event_covs_pcs.csv` (from `01`) and
  `Derived/Geospatial/shp/Pc_locs.gpkg`. The `.gpkg` must carry English field
  names — it is regenerated by `01`'s `if(FALSE)` `st_write` block (~L780). `05`
  `stop()`s if it finds a duplicated `Id_survey_no_dc` (stale `.gpkg`).
- **Source rasters:** Colombia Woody Vegetation Structure & Change,
  <https://zenodo.org/records/18154841> — `Colombia_WVCC_<year>.tif` (canopy
  cover %, trees/shrubs > 2 m) and `Colombia_WVCH_<year>.tif` (canopy height,
  decimetres). Years used: 2013, 2014, 2016, 2017, 2019, 2022, 2024.
- **`generate` toggle** (top of script):
  - `FALSE` (default) — reads pre-cropped per-year buffer tifs from
    `Derived/Geospatial/tif/wsvc/{Canopy_cover_1k,Canopy_height_50m}/`. Seconds.
  - `TRUE` — re-crops from the full national rasters (~15 min). Only needed if the
    point set or buffer sizes change; then re-run the `if(FALSE)` `writeRaster`
    block at the end to refresh the pre-cropped tifs.
- **Extraction:** `exactextractr::exact_extract` through the local
  `extract_buffers()` helper. It keeps only cells at least half inside the buffer
  (`coverage_fraction >= 0.5`), which reproduces `terra::extract`'s centroid rule
  while running in C++ — height matches the previous values exactly, cover to
  within 0.04 of a percentage point.
- **WVSC ends 2024**, so 2025 / 2026 surveys join to the 2024 layer via a
  temporary `Year_wvsc` column (the real `Year` is never overwritten).
- **Scale-of-effect is a Ch1 job.** `Buffers_soe <- seq(1000, 5000, 1000)` sits at
  the top of the script but is unused — the deposit only needs the single 1 km
  cover buffer + 50 m height buffer. Ch1 should loop `extract_buffers()` over
  `Buffers_soe` for the multi-scale analysis.
- **Output:** `Derived/Excels/Event_covs.csv`; `Export_DataS1.R` copies it to
  `DataS1/Event_covs.csv`.

---

## 4. `06_Analysis_wrangling.R` — analysis-ready subset

Filters `Bird_pcs_dist.csv` (written by `04`) down to `Bird_pcs_analysis.csv`
(→ `DataS1/Bird_pcs_analysis.csv`): observations within the 50 m fixed radius that
used the habitat. **The script header says these steps are suggestions to adapt
to your analysis** — Ch1 should decide each one deliberately:

1. `Count = 1` where `Count` is `NA`
2. drop recording-only IDs — `filter(is.na(Recording) | Recording == "Cf")`
3. drop flyovers — `filter(is.na(Obs_type) | Obs_type != "Sobrevuelo")`
   (CIPAV never recorded flyovers, so their data may still contain some)
4. distance ≤ 50 m — bands (`"0-15"`, `"15-30"`, `"30-50"`, `"<50"`, …) map to
   numeric metres; Trochilidae + Pipridae with a missing distance are kept
   (impossible to ID beyond 50 m anyway)
5. sum `Count` per species per point count
6. cap the 4 counts > 50 individuals at 50

`Bird_pcs_dist.csv` is **both an input and an output of `04`**. On a first run
after a header change, `04` (L39) and `06` (L15) Anglicise the columns on read via
`rename(any_of(...))`; once everything has been re-run through with English
headers these shims are harmless no-ops and can be dropped.

---

## 5. Suggested Ch1 run order

1. This repo's `01`–`04` populate `Derived/Excels/`, **or** read the frozen
   `DataS1/` tables directly.
2. Ch1's `03`-equivalent → full functional-trait table (adds `Eye_resid`,
   `Clutch`, nest traits, multi-source elevation if wanted).
3. `05` → canopy covariates; loop `extract_buffers()` over `Buffers_soe` for the
   scale-of-effect analysis.
4. `_ch1_pending/Extract_lcs.R` → `_ch1_pending/LSM.R` → digitized-land-cover
   landscape metrics; `_ch1_pending/Riparian/` for the riparian classification.
5. `06`-equivalent → analysis subset with Ch1's own filtering choices.
