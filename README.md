# myRfunc

A collection of R functions for customizing table themes and creating forest plots with tabular data.

## Features

- **Custom themes** for gt and gtsummary tables with consistent styling
- **Forest plots** combined with tabular data for visualizing effect measures
- **Flexible styling options** with three built-in themes: striped, black & white, and background color

## Installation

You can install the development version of myRfunc from GitHub with:

```r
# install.packages("devtools")
devtools::install_github("Jimmybbww/myRfunc")
```

Load the package:

```r
library(myRfunc)
```

## Usage

### tabular_forest

Create forest plots combined with tabular data, perfect for visualizing odds ratios, hazard ratios, or other effect measures with confidence intervals:

```r
# Create example data
example_data <- data.frame(
  Label = paste0("Variable", 1:6),
  OR = c(1.25, 1.50, 0.85, 1.75, 1.15, 1.5),
  LCL = c(1.10, 1.25, 0.2, 0.9, 0.95, 1.2),
  UCL = c(1.40, 1.75, 1.5, 2.6, 1.35, 2.0),
  grp = paste0('Group ', LETTERS[rep(1:3, each=2)])
)

# Basic forest plot
tabular_forest(example_data[, -5])

# Forest plot with grouping
tabular_forest(example_data, grp_col = 'grp')

# Customized forest plot with arrows and custom colors
tabular_forest(
  example_data,
  grp_col = 'grp',
  label_text = "Variables",
  label_table = "HR (95% CI)",
  label_axis = "\nHazard Ratio (95% CI)",
  color_map = c("red", "blue", "green"),
  arrows = TRUE,
  xlim = c(0.5, 2.2),
  ci_sep = ", ",
  point_size = 3,
  point_shape = 21
)
```

### theme_gtsummary_style

Customize the appearance of gtsummary tables:

```r
# Apply a striped theme globally
theme_gtsummary_style(
  style = "strip",
  title = "Clinical Trial Results",
  strip.bg.color = "#FFFAEE"
)

# Create a summary table (theme will be applied)
trial %>%
  tbl_summary()
```

### theme_gt_style

Customize the appearance of gt tables:

```r
# Create a gt table with background color theme
my_data %>%
  gt() %>%
  theme_gt_style(
    style = "bg",
    title = "Sales Report",
    bg.color = "#F2F1E9"
  )

# Striped theme
my_data %>%
  gt() %>%
  theme_gt_style(
    style = "strip",
    title = "Financial Overview"
  )
```

## Available Styles

Three different styles are available for table themes:

- `strip`: Striped rows with white background
- `bw`: Black and white theme (minimal styling)
- `bg`: Solid background color

## License

This project is licensed under the MIT License - see the LICENSE file for details.