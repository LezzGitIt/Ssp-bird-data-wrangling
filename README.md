# Bird diversity in productive landscapes of Colombia

This repository holds (1) the **data-wrangling pipeline** that assembles the
Sustainable Cattle Ranching (SCR) bird point-count data into clean, covariate-linked
tables, and (2) the **Ecology data paper** ("Bird diversity in productive landscapes
of Colombia") built from those tables.

The project uses data from 500+ unique point count locations surveyed 2013–2025
across five Colombian ecoregions to study how silvopasture affects bird taxonomic,
functional, and phylogenetic diversity in fragmented landscapes. Downstream
dissertation-chapter repositories consume this repo's outputs for the analyses
(multi-species occupancy/abundance, alpha/beta diversity, functional and
phylogenetic diversity).

## The dataset (`DataS1/`)

`DataS1/` is the curated deposit that accompanies the data paper:

| File | Contents | Join keys |
|---|---|---|
| `Bird_pcs_all.csv` | every point-count observation | `Id_muestreo`, `Id_muestreo_no_dc`, `Species_ayerbe` |
| `Bird_pcs_dist.csv` | after distributional/elevational range screening | same |
| `Bird_pcs_analysis.csv` | analysis-ready subset (50 m radius, used the habitat) | same |
| `Event_covs.csv` | per-survey covariates (date, time, observer, weather, land cover) | `Id_muestreo`, `Id_muestreo_no_dc` |
| `Site_covs.csv` | per-location covariates (elevation, climate, distance to nearest farm) | `Id_muestreo_no_dc` |
| `Taxonomy.csv` | SACC ↔ BirdLife ↔ eBird ↔ BirdTree crosswalk (Colombia) | `Species_ayerbe` |
| `Functional_traits.csv` | per-species trait table (mainly AVONET) | `Species_ayerbe` |
| `Column_definitions_final.xlsx` | field definitions for each table | |

`Scripts/Data_paper/Data_filtering_example.R` and `Data_joining_example.R` show how
the analysis subset is derived and how the tables join — starting points, to be
adapted to your own analysis.

The versioned deposit of record (with a DOI) is on **Dryad/Zenodo**; `DataS1/` here
is the working copy, kept in sync with the pipeline.

## The pipeline (`Scripts/`)

`Scripts/01_…` through `Scripts/08_…` run in sequence; most end with a deliberate
`stop()` before their export section. See `Project_notes.md` for the full run
order, inputs, and outputs.

| Script | Builds |
|---|---|
| `01_Gen_wrangling.R` | base point-count df, site/event covariates, point locations, climate |
| `02_Taxonomy.R` | `Taxonomy.csv`, taxonomy-standardized observations |
| `03_FT_elev.R` | `Functional_traits.csv`, elevational ranges |
| `04_Out_range.R` | `Bird_pcs_dist.csv` (range screening) |
| `05_Extract_lcs.R` → `06_LSM.R` → `07_wvsc.R` | digitized land cover, landscape metrics, woody-vegetation structure |
| `08_Analysis_wrangling.R` | `Bird_pcs_analysis.csv` |

`Scripts/Data_paper/Phylogeny_fig.R` prunes the BirdTree phylogeny and builds
the `@fig-phylogeny` plot + `Tax_summary.csv`.

## Reproducing the manuscript

Requires a populated `Derived/` (run the pipeline first), the Elsevier Quarto
extension, and `lualatex`.

```r
# 1. Install the journal format (once)
#    quarto add quarto-journals/elsevier      # already vendored in _extensions/

# 2. Build the figures + metadata the manuscript embeds
source("Scripts/Data_paper/Figs.R")
source("Scripts/Data_paper/Phylogeny_fig.R")

# 3. Render
#    quarto render Scripts/Data_paper/qmd/Data_paper_ecology.qmd
```

Output (`Data_paper_ecology.pdf` and `.docx`) lands next to the qmd in `Scripts/Data_paper/qmd/`.

## Repository layout

```
Scripts/        01–08 pipeline; Data_paper/ (figure + example scripts); qmd/ (manuscript);
                _ch1_pending/ (staged for Ch1-ssp-birds)
DataS1/         curated deposit (tracked)
Suppfiles/      bibliography, author/affiliation metadata, title-page partial
_extensions/    Elsevier Quarto format
Figures/Static/ manuscript figures no script regenerates (sampling map, example landscape, phylogeny)
Figures/        script-generated figures (gitignored, rebuilt by the Data_paper/ scripts)
Data/ Derived/ Rdata/   raw + recreatable (gitignored); geospatial outputs in Derived/Geospatial/
Docs/           methodology notes, feedback, planning docs (gitignored)
```

## Contact

The data are not yet public. To collaborate, email skinnerayayron93 [at] gmail [dot] com.

## Acknowledgments

Thanks to the Sustainable Cattle Ranching project and my advisors for providing data
and context on cattle ranching in Colombia; to the NGO SELVA for hosting a Fulbright
year in Colombia with logistical, conceptual, and taxonomic training; and to my
advisors at UBC and TNC for funding and guidance.
