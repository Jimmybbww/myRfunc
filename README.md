# myRfunc

A collection of R functions for customizing gt and gtsummary table themes

## Installation

You can install the development version of myRfunc from GitHub with:

```r
# install.packages("devtools")
devtools::install_github("Jimmybbww/myRfunc")
```

## Usage

### theme_gtsummary_style

Customize the appearance of gtsummary tables:

```r
library(myRfunc)
library(gtsummary)

# Apply a striped theme
theme_gtsummary_style(
  style = "strip",
  title = "My Table",
  strip.bg.color = "#FFFAEE"
)
```

### theme_gt_style

Customize the appearance of gt tables:

```r
library(myRfunc)
library(gt)

# Create a gt table with background color theme
my_data %>%
  gt() %>%
  theme_gt_style(
    style = "bg",
    title = "My Table",
    bg.color = "#F2F1E9"
  )
```

## Styles

Three different styles are available:

- `strip`: Striped rows with white background
- `bw`: Black and white theme
- `bg`: Solid background color

## License

This project is licensed under the MIT License - see the LICENSE file for details.