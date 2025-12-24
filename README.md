```
    __         __                 ____
   / /_  ___  / /___  ___  ____  / __ \___
  / __ \/ _ \/ / __ \/ _ \/ __ \/ /_/ / __|
 / / / /  __/ / /_/ /  __/ / / / _, _/\__ \
/_/ /_/\___/_/ .___/\___/_/ /_/_/ |_|/____/
            /_/
        ╔═══════════════════════════════════╗
        ║  Publication-Ready R Output       ║
        ║  Tables • Heat Maps • Statistics  ║
        ╚═══════════════════════════════════╝
```

![R](https://img.shields.io/badge/R-%3E%3D%203.3.3-blue)
![License](https://img.shields.io/badge/License-GPL--3-green)

## Overview

**helpeRs** is an R package for high-quality presentation of quantitative research. It provides tools for generating publication-ready regression tables in LaTeX format, with support for:

- Clustered standard errors (Arellano 1987)
- Heteroskedasticity-robust standard errors
- Bootstrap inference
- Instrumental variables diagnostics
- Heat map visualizations

## Installation

```r
# Install from GitHub
devtools::install_github("cjerzak/helpeRs-software/helpeRs")

# Load the package
library(helpeRs)
```

## Quick Start

### Generate a LaTeX Regression Table

```r
# Fit some models
fit1 <- lm(mpg ~ wt, data = mtcars)
fit2 <- lm(mpg ~ wt + hp, data = mtcars)
fit3 <- lm(mpg ~ wt + hp + cyl, data = mtcars)

# Generate publication-ready LaTeX tables
Tables2Tex(
  reg_list = list(fit1, fit2, fit3),
  clust_id = NULL,  # Use robust SEs (or specify cluster variable)
  saveFolder = "./tables/",
  nameTag = "MPG_Models",
  tabCaption = "Determinants of Fuel Efficiency",
  model.names = c("(1)", "(2)", "(3)")
)
```

This produces two files:
- `tabMPG_Models_SEanalytical.tex` — Condensed table with key coefficients
- `FULL_tabMPG_Models_SEanalytical.tex` — Full table in longtable format

### Quick Heat Map

```r
# Visualize a correlation matrix
cor_mat <- cor(mtcars[, 1:5])
heatmap2(cor_mat, use_gg = TRUE)

# Or interpolate scattered (x, y, z) data
heatMap(x = mtcars$wt, y = mtcars$hp, z = mtcars$mpg,
        N = 50, xlab = "Weight", ylab = "Horsepower")
```

## Features

### Regression Tables

| Function | Description |
|----------|-------------|
| `Tables2Tex()` | Main function: multiple models → LaTeX tables |
| `GetTableEntry()` | Extract formatted results from a single model |
| `vcovCluster()` | Clustered standard errors (Arellano method) |

**Standard Error Options:**
- `seType = "analytical"` — Sandwich estimators (clustered or HC-robust)
- `seType = "boot"` — Bootstrap standard errors from pre-computed replications

**Output Customization:**
- `inParens = "tstat"` or `"se"` — Display t-statistics or standard errors
- `NameConversionMat` — Rename/filter covariates in output
- `checkmark_list` — Add checkmark rows for model features

### Visualization

| Function | Description |
|----------|-------------|
| `heatMap()` | Interpolated heat map from scattered (x, y, z) data |
| `heatmap2()` | Quick matrix visualization (base R or ggplot2) |
| `MakeHeatMap()` | Two-way predictor effect surfaces from fitted models |
| `image2()` | Matrix plotting with intuitive orientation |

### Data Utilities

| Function | Description |
|----------|-------------|
| `f2n()` | Convert factors to numeric (via character) |
| `cols2numeric()` | Batch convert data frame columns to numeric |
| `colSummmary()` | Column-wise mean (numeric) or mode (categorical) |
| `fixZeroEndings()` | Ensure consistent decimal places |

## LaTeX Requirements

The generated tables require these LaTeX packages:

```latex
\usepackage{longtable}  % For full tables
\usepackage{amssymb}    % For checkmark symbols
\usepackage{ragged2e}   % If using WidenMargins()
```

## Example Output

```
┌─────────────────────────────────────────────────────────┐
│                  Determinants of MPG                    │
├─────────────────────────────────────────────────────────┤
│                     (1)        (2)        (3)           │
├─────────────────────────────────────────────────────────┤
│ Weight            -5.34*     -3.88*     -3.17*          │
│                   (-9.56)    (-5.32)    (-4.22)         │
│ Horsepower                   -0.03*     -0.02           │
│                              (-3.52)    (-1.45)         │
│ Cylinders                               -0.94*          │
│                                         (-2.26)         │
├─────────────────────────────────────────────────────────┤
│ Adj. R²            0.74       0.81       0.82           │
│ Observations        32         32         32            │
└─────────────────────────────────────────────────────────┘
```

## Citation

If you use **helpeRs** in your research, please cite:

```bibtex
@software{helpeRs,
  author = {Jerzak, Connor},
  title = {helpeRs: Helper Functions for High-Quality Presentation of Quantitative Research},
  url = {https://github.com/cjerzak/helpeRs-software},
  year = {2026}
}
```

## License

GPL-3

## Author

[**Connor Jerzak**](https://connorjerzak.com/), University of Texas at Austin
