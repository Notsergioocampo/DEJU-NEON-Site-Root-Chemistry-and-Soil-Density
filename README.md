# NEON DEJU Site Root Chemistry and Soil Density Analysis

[![R-CMD-check](https://github.com/Notsergioocampo/DEJU-NEON-Site-Root-Chemistry-and-Soil-Density/workflows/R-CMD-check/badge.svg)](https://github.com/Notsergioocampo/DEJU-NEON-Site-Root-Chemistry-and-Soil-Density/actions)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

## Overview

This project analyzes root chemistry (carbon, nitrogen, C:N ratios) and soil bulk density relationships at the NEON DEJU (Delta Junction) site in Alaska. The study focuses on understanding how root nutrient composition varies with soil physical properties in boreal forest ecosystems with discontinuous permafrost.

## 🌲 Ecological Context

The DEJU site represents a critical transition zone between boreal forest and tundra ecosystems, characterized by:
- Subarctic climate with long cold winters
- Discontinuous permafrost
- Nutrient-limited soils
- Black spruce (Picea mariana) and white spruce (Picea glauca) dominance

Understanding root-soil relationships in these environments is crucial for predicting ecosystem responses to climate change and permafrost thaw.

## 📊 Data Sources

This analysis uses standardized data products from the National Ecological Observatory Network (NEON):

- **DP1.10066.001**: Root biomass and chemistry data
- **DP1.00096.001**: Soil physical and chemical properties

## 🚀 Quick Start

### Prerequisites

- R (version 4.0 or higher)
- RStudio (recommended)

### Installation

1. Clone this repository:
```bash
git clone https://github.com/Notsergioocampo/DEJU-NEON-Site-Root-Chemistry-and-Soil-Density.git
cd DEJU-NEON-Site-Root-Chemistry-and-Soil-Density
```

2. Install required R packages:
```r
install.packages(c("dplyr", "tidyr", "ggplot2", "readr", "stringr", 
                   "broom", "ggthemes", "neonUtilities", "testthat"))
```

3. Run the complete analysis:
```r
# From R console
source("scripts/run_analysis.R")
```

Or from command line:
```bash
Rscript scripts/run_analysis.R
```

## 📁 Project Structure

```
DEJU-NEON-Site-Root-Chemistry-and-Soil-Density/
├── R/                          # R functions
│   ├── data_processing.R       # Data cleaning and processing functions
│   ├── visualization.R         # Plotting functions
│   └── analysis.R              # Statistical analysis functions
├── scripts/                    # Analysis scripts
│   ├── run_analysis.R          # Main analysis pipeline
│   └── download_data.R         # Data download script
├── data/
│   ├── raw_data/               # Raw NEON data files
│   ├── processed/              # Cleaned and processed data
│   └── documentation/          # Data documentation
├── figures/                    # Generated plots and visualizations
├── output/                     # Analysis results and reports
├── tests/                      # Unit tests
└── docs/                       # Additional documentation
```

## 🔧 Key Functions

### Data Processing
- `clean_root_chemistry()`: Filters and validates root chemistry data
- `clean_soil_data()`: Processes soil bulk density measurements
- `merge_root_data()`: Combines root chemistry with sample metadata
- `extract_depth_info()`: Parses depth information from NEON sample IDs

### Visualization
- `plot_nitrogen_distribution()`: Histogram of root nitrogen content
- `plot_carbon_nitrogen_relationship()`: Scatter plot with correlation
- `plot_root_size_comparison()`: Comparison by root diameter classes
- `plot_soil_root_relationship()`: Soil bulk density vs root chemistry
- `plot_depth_profiles()`: Vertical distribution patterns

### Statistical Analysis
- `compare_root_sizes()`: T-tests between fine and coarse roots
- `analyze_soil_root_correlation()`: Correlation analysis
- `perform_regression()`: Linear regression modeling
- `perform_comprehensive_analysis()`: Complete statistical workflow

## 📈 Analysis Pipeline

The main analysis script (`scripts/run_analysis.R`) runs a complete pipeline:

1. **Data Processing**: Clean and validate raw NEON data
2. **Data Merging**: Combine root chemistry with soil properties
3. **Visualization**: Generate publication-quality figures
4. **Statistical Analysis**: Perform comprehensive statistical tests
5. **Report Generation**: Create final analysis report

## 🎯 Key Research Questions

1. **How do root carbon and nitrogen concentrations vary with soil bulk density?**
2. **Do fine roots (≤4mm) differ from coarse roots (>4mm) in chemical composition?**
3. **What are the depth-related patterns in root-soil relationships?**
4. **How do these relationships inform our understanding of boreal forest nutrient cycling?**

## 📋 Example Outputs

### Summary Statistics
| Variable | Mean ± SD | Range | n |
|----------|-----------|-------|---|
| Root Carbon (%) | 44.2 ± 8.1 | 8.2 - 53.6 | 65 |
| Root Nitrogen (%) | 0.68 ± 0.23 | 0.2 - 1.4 | 65 |
| C:N Ratio | 68.4 ± 25.3 | 14.6 - 283.0 | 65 |
| Soil Bulk Density (g/cm³) | 1.25 ± 0.35 | 0.57 - 1.62 | 10 |

### Key Findings
- Root C:N ratios show high variability (CV = 37%), indicating diverse decomposition potential
- No significant relationships between soil bulk density and root chemistry variables
- Fine roots have slightly higher nitrogen content than coarse roots
- Depth patterns suggest complex root-soil interactions in permafrost-affected soils

## 🧪 Testing

Run unit tests to ensure code quality:
```r
library(testthat)
test_dir("tests")
```

## 🤝 Contributing

Contributions are welcome! Please feel free to submit a Pull Request. For major changes, please open an issue first to discuss what you would like to change.

### Development Guidelines
1. Follow tidyverse style guidelines
2. Add unit tests for new functions
3. Update documentation
4. Ensure all tests pass before submitting

## 🔄 Adapting for Other NEON Sites

This analysis framework can be easily adapted for other NEON sites:

1. **Change site code**: Replace "DEJU" with your target site (e.g., "HARV", "UNDE")
2. **Update ecological context**: Modify site description and ecological interpretation
3. **Adjust depth categories**: Customize depth breaks for your ecosystem
4. **Modify statistical tests**: Adapt tests based on your specific research questions

Example for Harvard Forest:
```r
# In download_data.R, change:
site = c("HARV")  # Instead of "DEJU"
```

## 📚 Citation

If you use this code in your research, please cite:

```bibtex
@software{deju_neon_analysis,
  author = {Ocampo, Sergio},
  title = {NEON DEJU Site Root Chemistry and Soil Density Analysis},
  year = {2025},
  url = {https://github.com/Notsergioocampo/DEJU-NEON-Site-Root-Chemistry-and-Soil-Density}
}
```

And cite the NEON data:
```bibtex
@misc{neon_data,
  title={National Ecological Observatory Network (NEON) Data},
  author={NEON},
  year={2025},
  url={https://www.neonscience.org}
}
```

## 📄 License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## 🙏 Acknowledgments

- National Ecological Observatory Network (NEON) for providing open ecological data
- University of San Francisco Department of Biology for support
- R community for developing excellent open-source tools

## 📞 Contact

For questions or collaboration opportunities:
- GitHub Issues: [Create an issue](https://github.com/Notsergioocampo/DEJU-NEON-Site-Root-Chemistry-and-Soil-Density/issues)
- Email: socampo3@dons.usfca.edu

---

**Happy analyzing!** 🌱📊
