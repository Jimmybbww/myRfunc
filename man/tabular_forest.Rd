% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tabular_forest.R
\name{tabular_forest}
\alias{tabular_forest}
\title{Create a Forest Plot with Tabular Data}
\usage{
tabular_forest(
  data,
  label_col = 'Label',
  est_col = "OR",
  lcl_col = "LCL",
  ucl_col = "UCL",
  grp_col = NULL,
  seq_col = "seq",
  label_text = "Label",
  ci_sep = " to ",
  null_line_at = 1,
  arrows = FALSE,
  point_size = 2.5,
  point_shape = 22,
  table_font_size = 3.2,
  font_family = "Arial",
  table_theme = NULL,
  xlim = NULL
)
}
\arguments{
\item{data}{A data frame containing the data to plot}
\item{label_col}{Column name for labels (default: 'Label')}
\item{est_col}{Column name for point estimates (default: "OR")}
\item{lcl_col}{Column name for lower confidence limits (default: "LCL")}
\item{ucl_col}{Column name for upper confidence limits (default: "UCL")}
\item{grp_col}{Optional column name for grouping (default: NULL)}
\item{seq_col}{Column name for sequence/ordering (default: "seq")}
\item{label_text}{Text for empty row label (default: "Label")}
\item{ci_sep}{Separator for confidence intervals (default: " to ")}
\item{null_line_at}{Position of reference line (default: 1)}
\item{arrows}{Whether to show arrows for out-of-bounds values (default: FALSE)}
\item{point_size}{Size of the points (default: 2.5)}
\item{point_shape}{Shape of the points (default: 22)}
\item{table_font_size}{Font size for the table (default: 3.2)}
\item{font_family}{Font family (default: "Arial")}
\item{table_theme}{Custom theme for the plot (default: NULL)}
\item{xlim}{Limits for x-axis (default: NULL)}
}
\value{
A list with class 'forest_plot' containing three components:
  \itemize{
    \item final: The combined forest plot with table
    \item left: The forest plot component
    \item right: The table component
  }
}
\description{
This function creates a forest plot combined with a table of results. It is
particularly useful for visualizing odds ratios, risk ratios, or other
effect measures with confidence intervals.
}
\examples{
\dontrun{
# Create sample data
data <- data.frame(
  Label = c("Group A", "Group B", "Group C"),
  OR = c(1.5, 8.2, 0.3),
  LCL = c(0.9, 6.5, 0.1),
  UCL = c(2.1, 12.5, 0.8),
  seq = 1:3
)

# Basic forest plot
tabular_forest(data)

# Forest plot with arrows for out-of-bounds values
tabular_forest(data,
               xlim = c(0.5, 5),
               arrows = TRUE,
               null_line_at = 1)
}
}