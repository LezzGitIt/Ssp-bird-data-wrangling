# Bird diversity in productive landscapes of Colombia

This repository holds the curated Sustainable Cattle Ranching (SCR) bird point-count dataset together with the **pipeline that produces and documents it**. The **Ecology data paper** ("Bird diversity in productive landscapes of Colombia") is the write-up of that dataset — what it contains, how it was collected, and how it was processed — and `Scripts/01_…06_` is the executable provenance behind it, assembling the raw provider data into the clean, covariate-linked tables deposited as `DataS1/`.

The project uses data from 500+ unique point count locations surveyed 2013–2026 across five Colombian ecoregions to study how silvopasture affects bird taxonomic, functional, and phylogenetic diversity in fragmented landscapes. Downstream dissertation-chapter repositories consume this repo's outputs for the analyses (multi-species occupancy/abundance, alpha/beta diversity, functional and phylogenetic diversity).

## The dataset (`DataS1/`)

`DataS1/` is the curated deposit that accompanies the data paper. Tables are joined with 'join keys':

| File | Contents | Join keys |
|------------------------|------------------------|------------------------|
| `Bird_pcs_all.csv` | every point-count observation | `Id_survey`, `Id_survey_no_dc`, `Species_ayerbe` |
| `Bird_pcs_analysis.csv` | analysis-ready subset (50 m radius, used the habitat) | same |
| `Event_covs.csv` | per-survey covariates (e.g., date, time, observer) | `Id_survey`, `Id_survey_no_dc` |
| `Site_covs.csv` | per-location covariates (elevation, climate, etc.) | `Id_survey_no_dc` |
| `Taxonomy.csv` | SACC ↔ BirdLife ↔ eBird ↔ BirdTree crosswalk (Colombia) | `Species_ayerbe` |
| `Functional_traits.csv` | per-species trait table | `Species_ayerbe` |
| `Column_definitions_final.xlsx` | column definitions for each table (one sheet per table) |  |
| `EXPORT_manifest.txt` | when the deposit was exported, from which pipeline commit, and the row count of each file |  |

`Scripts/Data_paper/Data_joining_example.R` shows how the `DataS1/` tables join — a starting point, to be adapted to your own analysis. `Scripts/06_Analysis_wrangling.R` documents how `Bird_pcs_analysis.csv` was derived from the full observation set.

On publication, the versioned deposit of record (with a DOI) will be archived on **Dryad/Zenodo**; `DataS1/` here is the working copy, kept in sync with the pipeline.

## The pipeline (`Scripts/`)

`Scripts/01_…` through `Scripts/06_…` run in sequence; most end with a deliberate `stop()` before their export section. Each script's header comment notes its inputs and outputs.

| Script | Builds |
|------------------------------------|------------------------------------|
| `01_Gen_wrangling.R` | base point-count df, site/event covariates, point locations, climate |
| `02_Taxonomy.R` | `Taxonomy.csv`, taxonomy-standardized observations |
| `03_FT_elev.R` | `Functional_traits.csv`, elevational ranges |
| `04a_Range_screening.R` | elevational + distributional range screening → review Excels + PDF maps (heavy geospatial; run only to refresh the review) |
| `04b_Range_apply.R` | applies the curated remove/change list → `Bird_pcs_dist.csv` (intermediate) |
| `05_wvsc.R` | woody-vegetation canopy cover + height, added to `Event_covs.csv` |
| `06_Analysis_wrangling.R` | `Bird_pcs_analysis.csv` |

`Scripts/Data_paper/Phylogeny_fig.R` prunes the BirdTree phylogeny and builds the phylogeny figure + `Tax_summary.csv`.

## Reproducing the manuscript

The raw provider data are not in this repository, so the pipeline and manuscript currently build only on the maintainer's machine; a fully from-clone reproducible bundle will be assembled for the Dryad/Zenodo deposit. Rendering additionally needs a populated `Derived/` (from the pipeline), the Elsevier Quarto extension, and `xelatex`.

``` r
# 1. Install the journal format (once per machine; it lands in _extensions/, which is gitignored)
#    quarto add quarto-journals/elsevier

# 2. Build the figures + the column-metadata tables the manuscript embeds
source("Scripts/Data_paper/Figs_tables.R")
source("Scripts/Data_paper/Phylogeny_fig.R")

# 3. Render
#    quarto render Scripts/Data_paper/qmd/Data_paper_ecology.qmd
```

Output (`Data_paper_ecology.pdf` and `.docx`) lands next to the qmd in `Scripts/Data_paper/qmd/`.

## Repository layout

```         
Scripts/        01–06 pipeline; Data_paper/ (figure + example scripts); qmd/ (manuscript)
DataS1/         curated deposit (tracked)
Suppfiles/      bibliography, author/affiliation metadata, title-page partial
_extensions/    Elsevier Quarto format (gitignored; `quarto add quarto-journals/elsevier`)
Figures/Static/ manuscript figures no script regenerates (example landscape, phylogeny)
Figures/        script-generated figures (gitignored, rebuilt by the Data_paper/ scripts)
Data/ Derived/ Rdata/   raw + recreatable (gitignored); geospatial outputs in Derived/Geospatial/
```

## Contact

The data are not yet public. Please email skinnerayayron93 \[at\] gmail \[dot\] com with any questions.

## Acknowledgments

Thanks to the Sustainable Cattle Ranching project and my advisors for providing data and context on cattle ranching in Colombia; to the NGO SELVA for hosting a Fulbright year in Colombia with logistical, conceptual, and taxonomic training; and to my advisors at UBC and TNC for funding and guidance.
