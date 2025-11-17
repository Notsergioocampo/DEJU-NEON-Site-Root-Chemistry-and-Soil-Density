# NEON Root Chemistry and Soil Bulk Density Analysis Framework

A professional R framework for analyzing root chemistry and soil bulk density relationships using NEON (National Ecological Observatory Network) terrestrial site data. Designed for ecological researchers studying belowground carbon and nitrogen dynamics.

## Overview

This framework provides a complete pipeline for analyzing NEON root-soil relationships, from raw data processing through publication-ready results. It focuses on the Delta Junction (DEJU) site in Alaska as a case study, with extensible architecture for additional NEON sites.

## Key Features

### Data Processing
- Automated loading and cleaning of NEON data products
- Quality control and validation procedures
- Depth extraction from NEON sample IDs
- Merging of root chemistry and soil bulk density datasets

### Statistical Analysis
- Descriptive statistics and summary tables
- Root size class comparisons (fine vs. coarse roots)
- Depth-stratified analysis across soil horizons
- Linear models and basic mixed models for soil-root relationships
- Effect size calculations with ecological interpretation

### Visualization
- Publication-quality figures with consistent ecological themes
- Root chemistry distributions and relationships
- Soil-root correlation plots
- Depth profiles showing vertical patterns
- Multi-panel layouts for complex comparisons

### Reproducible Research
- Complete documentation with roxygen2
- Comprehensive test suite
- Version control integration
- Reproducible workflows with session tracking

## Requirements

### R Packages
- dplyr (>= 1.0.0)
- tidyr (>= 1.0.0)
- ggplot2 (>= 3.3.0)
- readr (>= 1.4.0)
- stringr (>= 1.4.0)
- purrr (>= 0.3.0)
- broom (>= 0.7.0)
- ggthemes (>= 4.2.0)
- lme4 (optional, for mixed models)

### System Requirements
- R version 4.0.0 or higher
- Pandoc (optional, for research paper rendering)

## Installation

```r
# Install required packages
install.packages(c("dplyr", "tidyr", "ggplot2", "readr", "stringr", 
                   "purrr", "broom", "ggthemes"))

# Optional: Install lme4 for mixed models
install.packages("lme4")

# Clone the repository
git clone https://github.com/Notsergioocampo/DEJU-NEON-Site-Root-Chemistry-and-Soil-Density.git
```

## Quick Start

### Run Complete Analysis
```r
# Navigate to project directory
setwd("path/to/DEJU-NEON-Site-Root-Chemistry-and-Soil-Density")

# Run complete analysis pipeline
Rscript scripts/run_analysis.R
```

### Custom Analysis
```r
# Load the framework
source("scripts/run_analysis.R")

# Run with specific parameters
results <- run_deju_pipeline(
  site_id = "DEJU",
  data_dir = "data/raw_data",
  output_dir = "output",
  run_models = TRUE,
  create_figures = TRUE,
  render_report = TRUE
)

# Access results
summary(results)
```

## Data Requirements

This framework uses two NEON data products:

1. **DP1.10066.001** - Root biomass and chemistry (megapit protocol)
2. **DP1.00096.001** - Soil physical and chemical properties

Expected file structure:
```
data/raw_data/
├── megapit_carbon_nitrogen.csv
├── megapit_root_samples.csv
├── soil_bulk_density.csv
└── soil_chemistry.csv
```

## Output Structure

```
output/
├── figures/                    # Publication-quality plots
│   ├── nitrogen_distribution.png
│   ├── carbon_nitrogen_relationship.png
│   ├── depth_profiles.png
│   └── ...
├── tables/                     # Statistical summaries
│   ├── root_chemistry_summary.csv
│   ├── depth_category_summary.csv
│   └── model_summary.csv
├── research_paper.html         # Complete analysis report
├── research_paper.docx         # Word version
└── project_report.html         # Fallback summary (if Pandoc unavailable)
```

## Supported NEON Sites

Pre-configured sites with ecological parameters:

| Site | Domain | Ecosystem Type | Location |
|------|--------|----------------|----------|
| DEJU | D19 | Boreal forest-tundra | Alaska |
| HARV | D01 | Temperate deciduous | Massachusetts |
| BART | D01 | Northern hardwood | New Hampshire |
| NIWO | D13 | Alpine/subalpine | Colorado |

## Statistical Methods

### Data Processing
- Outlier detection using 4-standard-deviation rule
- Missing data handling with systematic removal
- Variable transformation where appropriate
- Comprehensive range and consistency checks

### Statistical Modeling
- Linear models for relationship analysis
- Basic mixed models (lme4) for hierarchical data
- Two-sample t-tests for group comparisons
- ANOVA for multi-group comparisons
- Effect size calculations (Cohen's d)

### Hypothesis Testing
- Pearson correlation for continuous relationships
- Post-hoc tests (Tukey HSD) for multiple comparisons
- Significance testing at α = 0.05
- Effect size reporting for ecological interpretation

## Testing

Run the comprehensive test suite:
```r
library(testthat)
test_dir("tests")
```

## Research Paper

The framework generates a complete research paper incorporating:
- Your original scientific writing and analysis
- Dynamically generated figures and tables
- Professional formatting suitable for submission
- Multiple output formats (HTML, Word, PDF if Pandoc available)

If Pandoc is not available system-wide, the framework:
- Creates a fallback HTML summary
- Preserves your RMarkdown paper for manual knitting in RStudio
- Provides clear instructions for installing Pandoc

## Contributing

We welcome contributions from the ecological community:

1. Fork the repository
2. Create a feature branch
3. Implement your changes with tests
4. Submit a pull request

Please ensure all contributions:
- Include appropriate documentation
- Pass the existing test suite
- Follow the established coding style
- Are scientifically sound and reproducible

## Citation

If you use this framework in your research, please cite:

```bibtex
@software{neon_root_soil_2024,
  title = {NEON Root Chemistry and Soil Bulk Density Analysis Framework},
  author = {Ocampo, Sergio},
  year = {2024},
  url = {https://github.com/Notsergioocampo/DEJU-NEON-Site-Root-Chemistry-and-Soil-Density}
}
```

## License

This project is licensed under the MIT License - see the LICENSE file for details.

## Acknowledgments

- National Ecological Observatory Network (NEON) for providing high-quality ecological data
- NSF for funding the National Ecological Observatory Network
- The ecological community for feedback and testing

## Contact

For questions, suggestions, or collaboration opportunities:
- Issues: [GitHub Issues](https://github.com/Notsergioocampo/DEJU-NEON-Site-Root-Chemistry-and-Soil-Density/issues)
- Email: socampo3@dons.usfca.edu

---

**Note:** This framework is designed for ecological research and should be used responsibly. Always consider the ecological context of your study sites and consult relevant literature when interpreting results.
