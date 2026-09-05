# _ch1_pending/

Staging area for material that belongs in **`Ch1-ssp-birds`**, not this repo.
Parked here (tracked) until Ch1's working tree is clean, then moved.

| File | Origin | How it moves |
|---|---|---|
| `Phylogenetic_diversity.R` | split out of the old `Scripts/09_Phylogeny.R` | plain copy (no history) |
| `Math.qmd` | was `Scripts/Manuscripts/Math.qmd` (never tracked before) | plain copy (no history) |
| `Rivers_dem_whitebox.R` | riparian-corridor delineation from a DEM; started as `Rivers_hydrography90m_check.R` | `git filter-repo` to keep the full history (Hydrography90m → cop30 → FABDEM) |
| `Extract_lcs.R` | was `Scripts/05_Extract_lcs.R` — digitized land-cover polys → `Snapped_*.gpkg` | `git mv`, history preserved |
| `LSM.R` | was `Scripts/06_LSM.R` — landscape metrics from the snapped land cover | `git mv`, history preserved |

`Rivers_dem_whitebox.R` still `source()`s `Scripts/01_Gen_wrangling.R` and writes to
`Derived/`/`Figures/` — run it from the repo root; it is not otherwise part of any
pipeline here. `Extract_lcs.R` / `LSM.R` likewise run from the repo root against the
populated `Derived/` and `../../Mentorship/Digitization_Mathilde/` digitized layers.

## Derivations still in `Scripts/03_FT_elev.R`, Ch1-bound

The eye-size, nest-trait, and BirdLife-clutch sections of `03` build `Eye_resid` /
`Source_eye`, `Nest_ground_bush` / `N_nest_locs` / `Nest_exposure`, and `Clutch` —
all dropped from the deposit (`03`'s export `select(-...)`s them). Pull those sections
into Ch1 when it resumes; the rest of `03` stays (it builds the deposited traits).

Also bound for Ch1 but **not** here: `DAGs.R` — already `git rm`'d from the working
tree; extract it from this repo's history with `git filter-repo --path
Extra_scripts/DAGs.R` when doing the Ch1 move.
