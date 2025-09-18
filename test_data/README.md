# Test Data

## Overview

This folder contains sample datasets for testing and demonstrating the functionality of the Non-Metallic Inclusions (NMI) Analysis Toolkit. The test data is designed to help users understand the expected data formats and validate their installations.

## Files Description

### `test_data.xlsx`

**Purpose**: Sample Excel file containing inclusion measurement data for testing all toolkit functions.

**Content**: 
- **Inclusion measurements** with chemical composition and geometric parameters
- **Multiple data types** to test various classification scenarios
- **Standardized format** matching the expected input structure for all tools

## Data Structure

### Expected Column Format

The test data follows the standard format expected by all toolkit functions:

#### Chemical Composition Columns
- **Format**: `Element (Wt%)` or `Element (mas. %)`
- **Examples**:
  - `Al (Wt%)` - Aluminum weight percentage
  - `O (Wt%)` - Oxygen weight percentage
  - `Si (Wt%)` - Silicon weight percentage
  - `Ca (Wt%)` - Calcium weight percentage
  - `Mn (Wt%)` - Manganese weight percentage
  - `S (Wt%)` - Sulfur weight percentage
  - `Fe (Wt%)` - Iron weight percentage

#### Geometric Measurement Columns
- **Area**: `Area (sq. µm)` - Inclusion area in square micrometers
- **Length**: `Length (µm)` - Maximum length in micrometers
- **Perimeter**: `Perimeter (µm)` - Perimeter in micrometers
- **ECD**: `ECD (µm)` - Equivalent Circular Diameter in micrometers
- **Aspect Ratio**: `Aspect Ratio` - Length to width ratio
- **Circularity**: `Circularity` - Shape circularity measure

## Usage Examples

### Testing Basic Classification

```r
# Test the basic classification script
datoteka_pot <- "test_data/test_data.xlsx"
output_dir <- "test_output"

# Run classification
source("../basic_NMIs_classification/enostavna_klasifikacija.R")
```

### Testing Comparison Analysis

```r
# Test multi-steel comparison
# Copy test_data.xlsx to multiple files with different names
# Then run comparison analysis
source("../basic_NMIs_classification/comparison_analysis_multipleplots.r")
```

### Testing Ternary Analysis

```matlab
% Test MATLAB ternary analyzer
% Ensure Carl Sandrock's ternplot package is installed
run('../double_normalisation_for_ternary_plot/DYNAMIC_TERNARY_ANALYZER.m')
% Select test_data.xlsx when prompted
```

### Testing Hexagonal Plots

```r
# Test hexagonal ternary diagrams
source("../hexagonal_ternary_plot/create_joint_ternary_diagram.R")

create_joint_ternary_diagram(
    "test_data.xlsx",
    "Al.(Wt%)",
    "Si.(Wt%)+O.(Wt%)",
    "Ca.(Wt%)+Mg.(Wt%)",
    "Mn.(Wt%)+S.(Wt%)",
    "Fe.(Wt%)",
    "Ti.(Wt%)+Zr.(Wt%)",
    "P.(Wt%)+N.(Wt%)"
)
```

## Data Characteristics

### Sample Size
- **Number of inclusions**: Approximately 100-500 data points
- **Variety**: Different types of inclusions (oxides, sulfides, complex)
- **Size range**: Various inclusion sizes for comprehensive testing

### Inclusion Types
The test data includes examples of:

#### Chemical Classifications
- **Oxides**: Al2O3, SiO2, CaO, MgO, MnO
- **Sulfides**: MnS, CaS, FeS
- **Complex oxides**: Al-Mg-O, Ca-Al-O, Mn-Si-O
- **Alkali oxides**: Na2O, K2O
- **Microdefects**: Fe-matrix artifacts

#### Morphological Classifications
- **Spherical**: High circularity inclusions
- **Globular**: Moderate circularity inclusions
- **Elongated**: High aspect ratio inclusions
- **Angular**: Low circularity, high solidity inclusions
- **Irregular**: Complex shape inclusions

### Size Distribution
- **Small inclusions**: < 2 µm ECD
- **Medium inclusions**: 2-10 µm ECD
- **Large inclusions**: > 10 µm ECD
- **Deformable sulfides**: > 13 µm ECD

## Validation Checklist

Use this checklist to verify your installation:

### Basic Classification Test
- [ ] Script runs without errors
- [ ] Excel output file is created
- [ ] All classification sheets are present
- [ ] Chemical classifications are reasonable
- [ ] Morphological classifications are reasonable

### Comparison Analysis Test
- [ ] Multiple plots are generated
- [ ] Statistical summaries are calculated
- [ ] PNG files are saved
- [ ] Excel comparison file is created

### Ternary Analysis Test
- [ ] MATLAB script runs without errors
- [ ] File selection dialog works
- [ ] Column selection works
- [ ] Plot is generated
- [ ] PNG export works

### Hexagonal Plot Test
- [ ] R function runs without errors
- [ ] Hexagonal plots are generated
- [ ] Output files are created
- [ ] All 6 plots are present

## Troubleshooting

### Common Issues with Test Data

#### "File not found" errors
- **Solution**: Ensure you're in the correct working directory
- **Check**: File path is relative to your current location

#### "Column not found" errors
- **Solution**: Check column names match exactly
- **Note**: Column names are case-sensitive

#### "No data to analyze" errors
- **Solution**: Ensure Excel file has data in first sheet
- **Check**: Data starts from row 2 (row 1 should be headers)

#### Classification errors
- **Solution**: Check that chemical composition columns contain numerical data
- **Check**: No empty cells in data rows

### Data Format Issues

#### Missing columns
- **Solution**: Ensure all required columns are present
- **Reference**: See "Expected Column Format" section above

#### Non-numerical data
- **Solution**: Check for text in numerical columns
- **Fix**: Replace text with numerical values or remove rows

#### Empty cells
- **Solution**: Fill empty cells with 0 or remove rows
- **Note**: Some functions handle NaN values automatically

## Customizing Test Data

### Adding Your Own Data
1. **Copy the structure**: Use test_data.xlsx as a template
2. **Maintain column names**: Keep the exact column naming convention
3. **Add your data**: Replace sample data with your measurements
4. **Validate format**: Ensure all data is numerical

### Modifying Column Names
If you need different column names:
1. **Update scripts**: Modify the scripts to match your column names
2. **Use aliases**: Create column name mapping functions
3. **Standardize**: Consider standardizing your data format

## Data Sources

### Sample Data Origin
- **Synthetic data**: Generated for testing purposes
- **Realistic values**: Based on typical inclusion measurements
- **Comprehensive coverage**: Includes various inclusion types and sizes

### Data Quality
- **Validated**: Tested with all toolkit functions
- **Representative**: Covers typical analysis scenarios
- **Clean**: No missing values or formatting issues

## Version Information

- **Version**: 1.0.0
- **Author**: Vid Kuder Marušič
- **Email**: vidkm30@gmail.com
- **Repository**: https://github.com/vidkudermarusic/PROGRAM-TOOLS-FOR-THE-EVALUATION-OF-NON-METALLIC-INCLUSIONS-IN-STEELS
- **Last Updated**: 2025

## Support

### Getting Help
- **Check examples**: Review usage examples above
- **Validate data**: Use the validation checklist
- **Troubleshoot**: Follow troubleshooting steps
- **Contact**: Vid Kuder Marušič (vidkm30@gmail.com)

### Reporting Issues
If you encounter issues with the test data:
1. **Check format**: Ensure your data matches the expected format
2. **Verify installation**: Confirm all required packages are installed
3. **Test with sample**: Try with the provided test_data.xlsx first
4. **Contact support**: Email vidkm30@gmail.com for assistance

---

**Note**: This test data is provided for validation and learning purposes. For production use, replace with your actual measurement data following the same format structure.
