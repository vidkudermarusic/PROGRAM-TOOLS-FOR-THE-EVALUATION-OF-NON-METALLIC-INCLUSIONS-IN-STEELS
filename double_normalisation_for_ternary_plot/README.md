# Double Normalisation for Ternary Plot

## Overview

This folder contains MATLAB tools for creating ternary plots with dual normalization methods. The tools allow interactive analysis of Excel data with customizable plotting options and support for multiple normalization approaches.

## ⚠️ Important Dependency Notice

**The `DYNAMIC_TERNARY_ANALYZER.m` script requires Carl Sandrock's ternplot package to function properly.**

### Required Package Installation

```matlab
% Download and install Carl Sandrock's ternplot package
% Repository: https://github.com/alchemyst/ternplot
% Citation: Sandrock, C. (2025). alchemyst/ternplot. GitHub. Retrieved September 18, 2025.
```

**Without this package, the ternary plotting functions will not work correctly.**

## Files Description

### Main Scripts

#### `DYNAMIC_TERNARY_ANALYZER.m`
- **Purpose**: Interactive ternary plotting analyzer
- **Features**:
  - Dynamic column selection from Excel files
  - Support for multiple columns per component (automatically summed)
  - Dual normalization methods (fractions and fractionsA)
  - Customizable marker styles and sizes
  - High-resolution plot export (PNG, 600 DPI)
  - Interactive user interface
- **Usage**: Run the script and follow the interactive prompts
- **Dependencies**: Requires Carl Sandrock's ternplot package

#### `fractions.m`
- **Purpose**: Fractional normalization for ternary data
- **Function**: `[fA, fB, fC] = fractions(A, B, C)`
- **Method**: `fA = A/(A+B+C)`, `fB = B/(A+B+C)`, `fC = 1-(fA+fB)`
- **Use case**: Relative proportions where components sum to 1

#### `fractionsA.m`
- **Purpose**: Alternative fractional normalization
- **Function**: `[fA, fB, fC] = fractionsA(A, B, C)`
- **Method**: `fA = A/100`, `fB = B/100`, `fC = C/100`
- **Use case**: Percentage-based normalization

### Plotting Functions

#### `ternplot.m`
- **Purpose**: Ternary phase diagram plotting
- **Function**: `ternplot(A, B, C, varargin)`
- **Features**:
  - Automatic normalization if values are not fractions
  - Customizable line types and plot properties
  - Grid options (majors parameter)
  - Compatible with standard MATLAB plot functions
- **Dependencies**: Part of Carl Sandrock's ternplot package

#### `ternplotA.m`
- **Purpose**: Alternative ternary plotting function
- **Function**: `ternplotA(A, B, C, varargin)`
- **Features**: Similar to ternplot but with different normalization approach
- **Dependencies**: Part of Carl Sandrock's ternplot package

## Usage Examples

### Basic Usage

```matlab
% 1. Install required packages first
% Download ternplot package from: https://github.com/alchemyst/ternplot

% 2. Run the interactive analyzer
run('DYNAMIC_TERNARY_ANALYZER.m')

% 3. Follow the interactive prompts:
%    - Select Excel file
%    - Choose columns for components A, B, C
%    - Select marker style and size
%    - Choose normalization method
%    - Save plot if desired
```

### Programmatic Usage

```matlab
% Load your data
data = xlsread('your_data.xlsx');

% Extract components (example)
A = data(:, 1);  % First column
B = data(:, 2);  % Second column  
C = data(:, 3);  % Third column

% Create ternary plot with fractions normalization
ternplot(A, B, C, 'ro', 'MarkerSize', 8);

% Add labels
ternlabel('Component A', 'Component B', 'Component C');

% Add title
title('Ternary Analysis');
```

### Normalization Methods

#### Method 1: Fractions (Relative Proportions)
```matlab
[fA, fB, fC] = fractions(A, B, C);
% Results: fA + fB + fC = 1.0
```

#### Method 2: FractionsA (Percentage-based)
```matlab
[fA, fB, fC] = fractionsA(A, B, C);
% Results: fA, fB, fC are percentages (0-100)
```

## Input Data Requirements

### Excel File Format
- **File types**: .xlsx or .xls
- **Structure**: Headers in first row, numerical data below
- **Required**: At least 3 columns with numerical data
- **Optional**: Multiple columns can be combined per component

### Data Validation
- **Numerical data**: All selected columns must contain numbers
- **Missing values**: NaN values are automatically filtered out
- **Empty columns**: Cannot be selected (validation included)

## Output Features

### Plot Customization
- **Marker styles**: Point, Circle, Square, Triangle, Diamond, Plus, Hexagon
- **Marker sizes**: User-defined (default: 2)
- **Line widths**: User-defined (default: 1.5)
- **Colors**: Automatic or user-specified

### Export Options
- **Format**: PNG (high resolution, 600 DPI)
- **Naming**: Automatic based on component names
- **Location**: Current working directory
- **Quality**: Publication-ready

## Troubleshooting

### Common Issues

#### "Function not found" errors
- **Solution**: Install Carl Sandrock's ternplot package
- **Download**: https://github.com/alchemyst/ternplot
- **Installation**: Follow package installation instructions

#### Excel file reading errors
- **Check**: File format (.xlsx or .xls)
- **Check**: Headers in first row
- **Check**: Numerical data in selected columns

#### Plot saving issues
- **Check**: Write permissions in current directory
- **Check**: Available disk space
- **Check**: Figure handle validity

### Error Messages

#### "No file selected"
- **Cause**: User cancelled file selection dialog
- **Solution**: Select a valid Excel file

#### "Column numbers out of range"
- **Cause**: Invalid column selection
- **Solution**: Use valid column numbers (1 to number of columns)

#### "Invalid input"
- **Cause**: Non-numerical input for column selection
- **Solution**: Enter numbers separated by spaces

## Advanced Features

### Multiple Column Support
- **Feature**: Combine multiple columns per component
- **Usage**: Enter multiple column numbers (e.g., "5 6 7")
- **Result**: Columns are automatically summed
- **Example**: "SiO2 + Al2O3" for combined silica-alumina

### Dual Normalization Comparison
- **Option**: Select "Both methods" for comparison
- **Result**: Overlay plot showing both normalization approaches
- **Legend**: Automatic legend with method descriptions

### Interactive Configuration
- **Marker selection**: Visual examples provided
- **Parameter validation**: Input validation for all parameters
- **Real-time feedback**: Immediate confirmation of selections

## Dependencies

### Required MATLAB Packages
```matlab
pkg install -forge statistics
pkg install -forge io
```

### Required External Package
- **Carl Sandrock's ternplot package**
- **Repository**: https://github.com/alchemyst/ternplot
- **Citation**: Sandrock, C. (2025). alchemyst/ternplot. GitHub. Retrieved September 18, 2025.
- **License**: Check repository for license information

## Version Information

- **Version**: 1.0.0
- **Author**: Vid Kuder Marušič
- **Email**: vidkm30@gmail.com
- **Repository**: https://github.com/vidkudermarusic/PROGRAM-TOOLS-FOR-THE-EVALUATION-OF-NON-METALLIC-INCLUSIONS-IN-STEELS
- **Last Updated**: 2025
- **Compatibility**: MATLAB R2018b or later
- **Tested Platforms**: Windows, Linux, macOS

## Support

For issues related to:
- **This toolkit**: Contact Vid Kuder Marušič (vidkm30@gmail.com)
- **Carl Sandrock's ternplot package**: https://github.com/alchemyst/ternplot

---

**Important**: Always ensure Carl Sandrock's ternplot package is properly installed before using any ternary plotting functions in this folder.
