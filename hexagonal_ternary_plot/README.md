# Hexagonal Ternary Plot

## Overview

This folder contains R tools for creating hexagonal ternary diagrams, which provide an alternative visualization approach for three-component systems. The hexagonal layout offers unique advantages for certain types of compositional analysis and can be particularly useful for displaying complex element combinations.

## Files Description

### `create_joint_ternary_diagram.R`

**Purpose**: Creates joint hexagonal ternary diagrams from Excel data with support for complex element combinations.

**Key Features**:
- **Hexagonal Layout**: Alternative to traditional triangular ternary plots
- **Element Combination Support**: Combine multiple elements per component (e.g., "SiO2 + Al2O3")
- **Automatic Package Installation**: Installs required R packages if not present
- **High-Quality Output**: Publication-ready visualizations
- **Timestamped Output**: Automatic timestamping of output files
- **Customizable Configuration**: Flexible element set definitions

**Function Signature**:
```r
create_joint_ternary_diagram(
    xlsx_file,
    output_dir = NULL,
    working_dir = NULL,
    ...
)
```

**Parameters**:
- `xlsx_file`: Path to Excel file containing the data
- `output_dir`: Output directory (optional, defaults to "plots_hexagonal")
- `working_dir`: Working directory (optional)
- `...`: Exactly 7 element strings defining the hexagonal configuration

## Usage

### Basic Usage

```r
# Load the function
source("create_joint_ternary_diagram.R")

# Create hexagonal ternary diagram
create_joint_ternary_diagram(
    xlsx_file = "path/to/your/data.xlsx",
    "Cr.(Wt%)",                    # Component 1
    "C.(Wt%)+N.(Wt%)+O.(Wt%)",    # Component 2 (combined)
    "Al.(Wt%)+Si.(Wt%)",          # Component 3 (combined)
    "Mg.(Wt%)+Ca.(Wt%)",          # Component 4
    "Mn.(Wt%)+Ti.(Wt%)",          # Component 5
    "S.(Wt%)+P.(Wt%)",            # Component 6
    "Fe.(Wt%)"                    # Component 7
)
```

### Advanced Usage

```r
# With custom output directory
create_joint_ternary_diagram(
    xlsx_file = "data.xlsx",
    output_dir = "custom_output",
    working_dir = "/path/to/working/directory",
    "Element1.(Wt%)",
    "Element2.(Wt%)+Element3.(Wt%)",
    "Element4.(Wt%)",
    "Element5.(Wt%)+Element6.(Wt%)",
    "Element7.(Wt%)",
    "Element8.(Wt%)",
    "Element9.(Wt%)"
)
```

## Input Requirements

### Excel File Format
- **File Type**: .xlsx format
- **Structure**: Data in first sheet (Sheet1)
- **Headers**: Column names should match element specifications
- **Data**: Numerical values (typically weight percentages)

### Element Specification
- **Format**: `Element.(Wt%)` for single elements
- **Combination**: `Element1.(Wt%)+Element2.(Wt%)` for multiple elements
- **Examples**:
  - `"Al.(Wt%)"` - Single aluminum element
  - `"SiO2.(Wt%)+Al2O3.(Wt%)"` - Combined silica and alumina
  - `"C.(Wt%)+N.(Wt%)+O.(Wt%)"` - Combined carbon, nitrogen, and oxygen

## Output

### Generated Files
- **Hexagonal Plots**: High-resolution PNG files
- **Timestamped Names**: Format: `YYYYMMDD_HHMMSS`
- **Custom Folders**: Organized by element combinations
- **Automatic Directory Creation**: Creates output directories if they don't exist

### File Naming Convention
```
plots_hexagonal/
└── [ElementCombination]_charge[FileName]_[Timestamp]/
    ├── hexagonal_plot_1.png
    ├── hexagonal_plot_2.png
    ├── hexagonal_plot_3.png
    ├── hexagonal_plot_4.png
    ├── hexagonal_plot_5.png
    └── hexagonal_plot_6.png
```

## Dependencies

### Required R Packages
The function automatically installs missing packages:

```r
# Core packages
install.packages(c(
    "magick",      # Image processing
    "openxlsx",    # Excel file handling
    "Ternary",     # Ternary plotting
    "PlotTools",   # Plot utilities
    "grid",        # Grid graphics
    "png"          # PNG file handling
))
```

### Package Descriptions
- **magick**: Advanced image processing and manipulation
- **openxlsx**: Reading and writing Excel files
- **Ternary**: Ternary plotting functions
- **PlotTools**: Additional plotting utilities
- **grid**: Low-level graphics system
- **png**: PNG image file support

## Hexagonal Configuration

The hexagonal ternary diagram uses a specific configuration with 6 triangular plots arranged in a hexagonal pattern:

```
    Plot 1: A vs B vs C
    Plot 2: C vs D vs A  
    Plot 3: D vs C vs E
    Plot 4: F vs E vs C
    Plot 5: C vs G vs F
    Plot 6: G vs C vs B
```

Where:
- A, B, C, D, E, F, G are the 7 element sets provided as parameters
- Each plot shows the relationship between three components
- The hexagonal arrangement provides comprehensive visualization

## Advantages of Hexagonal Layout

### Compared to Traditional Ternary Plots
- **Multiple Perspectives**: 6 different views of the same data
- **Element Relationships**: Better visualization of complex element interactions
- **Comprehensive Analysis**: Single diagram shows multiple ternary relationships
- **Space Efficiency**: More information in a single plot

### Use Cases
- **Complex Compositions**: Multi-element systems
- **Phase Diagram Analysis**: Multiple phase relationships
- **Quality Control**: Comprehensive material characterization
- **Research Visualization**: Publication-ready complex plots

## Troubleshooting

### Common Issues

#### "You must provide exactly 7 element strings"
- **Cause**: Incorrect number of parameters
- **Solution**: Provide exactly 7 element strings as parameters

#### Package installation errors
- **Cause**: Network issues or package conflicts
- **Solution**: Install packages manually:
  ```r
  install.packages(c("magick", "openxlsx", "Ternary", "PlotTools", "grid", "png"))
  ```

#### Excel file reading errors
- **Cause**: File format or path issues
- **Solution**: 
  - Ensure file is .xlsx format
  - Check file path is correct
  - Verify file is not open in Excel

#### Element column not found
- **Cause**: Column names don't match element specifications
- **Solution**: 
  - Check column names in Excel file
  - Ensure exact match with element specifications
  - Use correct format: `Element.(Wt%)`

### Error Messages

#### "Error in read.xlsx"
- **Solution**: Check file path and format
- **Alternative**: Try converting to .csv and modify function

#### "Package not available"
- **Solution**: Install required packages manually
- **Check**: R version compatibility

## Examples

### Steel Analysis Example
```r
create_joint_ternary_diagram(
    "steel_analysis.xlsx",
    "Cr.(Wt%)",                    # Chromium
    "C.(Wt%)+N.(Wt%)+O.(Wt%)",    # Interstitials
    "Al.(Wt%)+Si.(Wt%)",          # Deoxidizers
    "Mg.(Wt%)+Ca.(Wt%)",          # Desulfurizers
    "Mn.(Wt%)+Ti.(Wt%)",          # Alloying elements
    "S.(Wt%)+P.(Wt%)",            # Impurities
    "Fe.(Wt%)"                    # Iron matrix
)
```

### Ceramic Analysis Example
```r
create_joint_ternary_diagram(
    "ceramic_data.xlsx",
    "Al2O3.(Wt%)",                # Alumina
    "SiO2.(Wt%)+TiO2.(Wt%)",      # Silica + Titania
    "CaO.(Wt%)+MgO.(Wt%)",        # Alkaline earth oxides
    "Na2O.(Wt%)+K2O.(Wt%)",       # Alkali oxides
    "Fe2O3.(Wt%)+MnO.(Wt%)",      # Iron oxides
    "P2O5.(Wt%)+SO3.(Wt%)",       # Phosphorus and sulfur
    "H2O.(Wt%)"                   # Water content
)
```

## Version Information

- **Version**: 1.0.0
- **Author**: Vid Kuder Marušič
- **Email**: vidkm30@gmail.com
- **Repository**: https://github.com/vidkudermarusic/PROGRAM-TOOLS-FOR-THE-EVALUATION-OF-NON-METALLIC-INCLUSIONS-IN-STEELS
- **Last Updated**: 2025
- **Compatibility**: R 3.6.0 or later
- **Dependencies**: See package list above

## Support

For issues related to:
- **Function usage**: Check examples and troubleshooting section
- **Package installation**: Verify R version and package compatibility
- **Data format**: Ensure Excel file structure matches requirements
- **General support**: Contact Vid Kuder Marušič (vidkm30@gmail.com)

---

**Note**: This function requires specific R packages to be installed. The function will attempt to install missing packages automatically, but manual installation may be required in some environments.
