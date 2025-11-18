# DEJU-NEON-Site-Root-Chemistry-and-Soil-Density

Reproducible analysis of root chemistry and soil bulk density at the NEON DEJU megapit.

This repository implements a small, reusable workflow for exploring how root carbon and nitrogen relate to soil physical properties (especially bulk density) along the soil profile at the NEON DEJU site.

## What this project does

- Cleans and merges NEON megapit datasets:
  - Root chemistry (`megapit_carbon_nitrogen`)
  - Root samples / biomass
  - Soil bulk density
  - Soil chemistry (optional)
- Extracts depth and size-class information from NEON sample IDs
- Computes root C:N ratios and depth classes
- Links root chemistry to soil bulk density by depth
- Produces:
  - Publication-quality figures (ggplot2)
  - Summary tables ecologists can drop into a manuscript or report
  - A simple, reusable pipeline other NEON users can adapt to their own sites

## Project structure

```text
DEJU-NEON-Site-Root-Chemistry-and-Soil-Density/
├─ DEJU-NEON-Site-Root-Chemistry-and-Soil-Density.Rproj
├─ README.md
├─ DESCRIPTION                 # optional, if you want to make it a package
├─ .Rbuildignore              # optional, if package-like
├─ .gitignore
│
├─ data_raw/
│   ├─ megapit_carbon_nitrogen.csv
│   ├─ megapit_root_samples.csv
│   ├─ soil_bulk_density.csv
│   ├─ soil_chemistry.csv
│   └─ megapit_biomass.csv
│
├─ data_processed/
│   ├─ root_chemistry_DEJU.csv
│   ├─ soil_bulk_density_DEJU.csv
│   └─ root_soil_merged_DEJU.csv
│
├─ R/
│   ├─ data_processing.R       # loading/cleaning/merging functions
│   ├─ analysis_models.R       # models & statistical helpers
│   ├─ visualization.R         # ggplot2 plotting functions
│   ├─ data_download.R         # optional: neonUtilities retrieval
│   └─ main_analysis.R         # run_deju_pipeline(), etc.
│
├─ scripts/
│   ├─ run_all.R               # master script
│   ├─ 01_prepare_data.R
│   ├─ 02_run_models.R
│   └─ 03_make_figures.R
│
├─ figures/
│   ├─ root_cn_vs_depth.png
│   ├─ soil_bulk_density_vs_depth.png
│   ├─ cn_ratio_vs_bulk_density.png
│   └─ depth_profiles.png
│
├─ vignettes/
│   └─ neon_root_soil_analysis.Rmd
│
└─ tests/
    ├─ testthat.R              # if you want package-style tests
    ├─ test_data_processing.R
    └─ test_analysis.R
```

## Installation

This project assumes:

* R >= 4.2
* Recommended packages:

```r
install.packages(c(
  "tidyverse",
  "glue",
  "patchwork"
  # plus "here" if you decide to use it
))
```

Clone the repo:

```bash
git clone https://github.com/Notsergioocampo/DEJU-NEON-Site-Root-Chemistry-and-Soil-Density.git
cd DEJU-NEON-Site-Root-Chemistry-and-Soil-Density
```

Place your NEON megapit CSVs into `data_raw/` (or use the supplied example files).

## How to run the full workflow

From the project root:

```bash
Rscript scripts/run_all.R
```

This will:

1. Process NEON data for DEJU (root chemistry + soil bulk density)
2. Save cleaned tables to `data_processed/`
3. Fit simple ecological models (optional)
4. Generate figures in `figures/`

## Outputs

Key figures:

* `figures/root_cn_vs_depth.png`
  Root C:N ratio as a function of depth.

* `figures/soil_bulk_density_vs_depth.png`
  Soil bulk density profile with depth.

* `figures/cn_ratio_vs_bulk_density.png`
  Relationship between root C:N and bulk density.

* `figures/depth_profiles.png`
  Side-by-side depth profiles for root C:N and bulk density.

Key tables:

* `data_processed/root_chemistry_DEJU.csv`
* `data_processed/soil_bulk_density_DEJU.csv`
* `data_processed/root_soil_merged_DEJU.csv`

The vignette (`vignettes/neon_root_soil_analysis.Rmd`) walks through a short example analysis using these outputs.

## Adapting to another NEON site

To reuse this framework for another site:

1. Download the corresponding NEON megapit datasets for your site.
2. Drop them into `data_raw/` with the same filenames.
3. Change the `site_id` argument in `scripts/run_all.R`:

   ```r
   run_deju_pipeline(site_id = "YOUR_SITE_ID", ...)
   ```
4. Run `Rscript scripts/run_all.R` again.

Most of the logic (depth parsing, C:N calculation, depth classes, merging with soil bulk density) should work unchanged, as long as the sample ID patterns and NEON product formats are similar.

## Tests

Basic tests are provided in `tests/` using `testthat`. To run them:

```r
library(testthat)
testthat::test_dir("tests")
```

These tests focus on:

* Parsing depth and size-class from NEON sample IDs
* Creating depth categories
* Cleaning and merging data without silently dropping valid observations

## Author & context

This project was built as part of an ecological data science exercise using NEON data at the DEJU site, with an emphasis on **open, reproducible workflows** that other researchers can adapt.

Contributions, suggestions, and pull requests from the NEON and soil/root ecology community are welcome.

## License

MIT License - see LICENSE file for details.

## Contact

For questions or issues, please contact: socampo3@dons.usfca.edu
