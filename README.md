# Non-Metallic Inclusions (NMI) Analysis Toolkit

## Overview

This repository contains a comprehensive toolkit for analyzing non-metallic inclusions in steel materials. The toolkit provides tools for classification, comparison analysis, and visualization of inclusion data using both R and MATLAB environments.

## Project Structure

```
├── basic_NMIs_classification/          # Basic inclusion classification tools
│   ├── enostavna_klasifikacija.R      # Simple classification script
│   ├── comparison_analysis_multipleplots.r  # Multi-steel comparison analysis
│   ├── README.md                      # Classification documentation
│   └── COMPARISON_README.md           # Comparison analysis documentation
├── hexagonal_ternary_plot/            # Hexagonal ternary visualization
│   └── create_joint_ternary_diagram.R # Joint ternary diagram creator
├── double_normalisation_for_ternary_plot/  # Ternary plot normalization tools
│   ├── DYNAMIC_TERNARY_ANALYZER.m     # Interactive ternary analyzer
│   ├── fractions.m                    # Fractional normalization
│   ├── fractionsA.m                   # Alternative fractional normalization
│   ├── ternplot.m                     # Ternary plotting function
│   ├── ternplotA.m                    # Alternative ternary plotting
│   └── README.md                      # Ternary tools documentation
└── test_data/                         # Sample data for testing
    ├── test_data.xlsx                 # Test dataset
    └── README.md                      # Test data documentation
```

## Features

### 🔬 Basic Classification (`basic_NMIs_classification/`)
- **Chemical Classification**: Automatic classification of sulfides, oxides, complex oxides, and alkali oxides
- **Morphological Classification**: ISO-standard morphological analysis (spherical, globular, elongated, etc.)
- **Geometric Analysis**: Comprehensive geometric metrics (circularity, aspect ratio, ECD, etc.)
- **Multi-Steel Comparison**: Statistical comparison across different steel types
- **Visualization**: Violin plots, box plots, histograms, heatmaps, and scatter plots

### 📊 Ternary Plotting (`double_normalisation_for_ternary_plot/`)
- **Interactive Analysis**: Dynamic column selection from Excel files
- **Dual Normalization**: Both fractional and percentage-based normalization methods
- **Customizable Plots**: Marker styles, sizes, and colors
- **Export Capabilities**: High-resolution PNG export
- **Multiple Column Support**: Combine multiple columns per component
**Note**: The `DYNAMIC_TERNARY_ANALYZER.m` requires Carl Sandrock's ternplot package (https://github.com/alchemyst/ternplot) to function properly. Please ensure this package is installed before using the ternary analysis tools.


### 🔷 Hexagonal Visualization (`hexagonal_ternary_plot/`)
- **Joint Diagrams**: Create hexagonal ternary diagrams
- **Element Combination**: Support for complex element combinations
- **High-Quality Output**: Publication-ready visualizations

## Quick Start

### Prerequisites

#### R Environment
```r
# Install required R packages
install.packages(c("tidyverse", "readxl", "writexl", "ggplot2", 
                   "gridExtra", "RColorBrewer", "scales", "pheatmap",
                   "magick", "openxlsx", "Ternary", "PlotTools", 
                   "grid", "png", "PeriodicTable"))
```

#### MATLAB Environment
```matlab
% Install required MATLAB packages
pkg install -forge statistics
pkg install -forge io

% IMPORTANT: Install Carl Sandrock's ternplot package
% Download from: https://github.com/alchemyst/ternplot
% This is REQUIRED for DYNAMIC_TERNARY_ANALYZER.m to function
```

### Basic Usage

#### 1. Simple Classification
```r
# Set your data file path
datoteka_pot <- "path/to/your/data.xlsx"

# Run classification
source("basic_NMIs_classification/enostavna_klasifikacija.R")
```

#### 2. Multi-Steel Comparison
```r
# Quick comparison analysis
source("basic_NMIs_classification/comparison_analysis_multipleplots.r")
```

#### 3. Interactive Ternary Analysis
```matlab
% Run interactive ternary analyzer
run('double_normalisation_for_ternary_plot/DYNAMIC_TERNARY_ANALYZER.m')
```

## Data Requirements

### Input Data Format
- **Excel files** (.xlsx) with inclusion measurement data
- **Required columns**: Chemical composition (Wt%), geometric measurements
- **Headers**: First row should contain column names
- **Data types**: Numerical values for measurements

### Expected Columns
- Chemical composition: `Element (Wt%)` format (e.g., `Al (Wt%)`, `O (Wt%)`)
- Geometric measurements: `Area (sq. µm)`, `Length (µm)`, `Perimeter (µm)`, `ECD (µm)`
- Shape parameters: `Aspect Ratio`, `Circularity`, etc.

## Output Files

### Classification Results
- **Excel files** with multiple sheets:
  - `Data`: Complete classified dataset
  - `Kemijski_Povzetek`: Chemical classification summary
  - `Morfoloski_Povzetek`: Morphological classification summary
  - `Statisticni_Povzetki`: Statistical summaries
  - Additional analysis sheets

### Visualization Outputs
- **PNG files** with timestamped names
- **High-resolution plots** (600 DPI)
- **Publication-ready** figures

## Dependencies

### R Packages
- `tidyverse`: Data manipulation and visualization
- `readxl`/`writexl`: Excel file handling
- `ggplot2`: Advanced plotting
- `gridExtra`: Plot arrangement
- `RColorBrewer`: Color palettes
- `scales`: Scale functions
- `pheatmap`: Heatmap creation
- `magick`: Image processing
- `openxlsx`: Excel file operations
- `Ternary`: Ternary plotting
- `PlotTools`: Plot utilities
- `PeriodicTable`: Chemical element data

### MATLAB Packages
- `statistics`: Statistical functions
- `io`: Input/output operations
- **Carl Sandrock's ternplot package**: Essential for ternary plotting
  - Download: https://github.com/alchemyst/ternplot
  - Citation: Sandrock, C. (2025). alchemyst/ternplot. GitHub. Retrieved September 18, 2025.

## Citation

If you use this toolkit in your research, please cite:

```
Kuder Marušič, V. (2025). PROGRAM-TOOLS-FOR-THE-EVALUATION-OF-NON-METALLIC-INCLUSIONS-IN-STEELS. 
GitHub Repository. Retrieved from https://github.com/vidkudermarusic/PROGRAM-TOOLS-FOR-THE-EVALUATION-OF-NON-METALLIC-INCLUSIONS-IN-STEELS
```

## License

[License Information]

## Contributing

[Contributing Guidelines]

## Support

For questions and support, please contact:
- **Author**: Vid Kuder Marušič
- **Email**: vidkm30@gmail.com
- **Repository**: https://github.com/vidkudermarusic/PROGRAM-TOOLS-FOR-THE-EVALUATION-OF-NON-METALLIC-INCLUSIONS-IN-STEELS

## Changelog

### Version 1.0.0
- Initial release
- Basic classification functionality
- Multi-steel comparison analysis
- Interactive ternary plotting
- Hexagonal visualization tools

---

