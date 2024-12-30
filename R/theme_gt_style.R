#' Create a Custom Theme for gt Tables
#'
#' This function provides a customizable theme for gt tables with various styling options.
#' It allows for consistent table formatting with options for different background styles,
#' border configurations, and text formatting.
#'
#' @param gt_object A gt table object created with gt::gt()
#' @param style Character string specifying the style. One of:
#'   \itemize{
#'     \item "strip": Striped rows with white background
#'     \item "bw": Black and white theme
#'     \item "bg": Solid background color
#'   }
#' @param title Optional title for the table. Will be displayed in bold if provided.
#' @param subtitle Optional subtitle for the table
#' @param bg.color Background color for the table. Default is "#F2F1E9"
#' @param strip.bg.color Background color for striped rows. Default is "#FFFAEE"
#'
#' @return A modified gt table object with the specified theme applied
#' @export
#'
#' @examples
#' \dontrun{
#' # Create sample data
#' data <- data.frame(
#'   Category = c("A", "B", "C"),
#'   Value = c(100, 200, 300),
#'   Change = c("+10%", "-5%", "+15%")
#' )
#'
#' # Create a gt table with background color theme
#' data %>%
#'   gt() %>%
#'   theme_gt_style(
#'     style = "bg",
#'     title = "Sales Report",
#'     bg.color = "#F2F1E9"
#'   )
#'
#' # Create a table with striped theme
#' data %>%
#'   gt() %>%
#'   theme_gt_style(
#'     style = "strip",
#'     title = "Financial Overview",
#'     strip.bg.color = "#FFFAEE"
#'   )
#' }
theme_gt_style <- function(gt_object,
                          style,
                          title = NULL,
                          subtitle = NULL,
                          bg.color = "#F2F1E9",
                          strip.bg.color = "#FFFAEE") {

  if(is.null(title)) title = NULL else title = gt::md(paste0("**",title,"**"))

  if (style == 'strip') {strip = TRUE; bg.color = "white"; strip.bg.color = strip.bg.color; body.hlines = gt::px(0)}
  if (style == 'bw') {strip = FALSE; bg.color = "white"; body.hlines = gt::px(0)}
  if (style == 'bg') {strip = FALSE; bg.color = bg.color; body.hlines = gt::px(1)}

  gt_object |>
    gt::tab_options(
      row.striping.include_table_body = strip,
      row.striping.background_color = strip.bg.color,
      table.font.names = "Arial",
      heading.align = "left",
      heading.title.font.size = 14,
      table.font.size = 12,
      footnotes.font.size = 10,
      data_row.padding = gt::px(1),
      summary_row.padding = gt::px(1),
      grand_summary_row.padding = gt::px(1),
      footnotes.padding = gt::px(1),
      source_notes.padding = gt::px(1),
      row_group.padding = gt::px(1),
      column_labels.padding = gt::px(3),
      table.background.color = bg.color,
      heading.background.color = "white",
      footnotes.background.color = "white",
      source_notes.background.color = "white",
      table.border.top.width = gt::px(0),
      table.border.top.color = "gray30",
      column_labels.border.top.width = gt::px(2.5),
      column_labels.border.top.color = "gray30",
      column_labels.border.bottom.width = gt::px(2),
      column_labels.border.bottom.color = "gray30",
      table.border.bottom.width = gt::px(0),
      table_body.border.bottom.width = gt::px(2),
      table_body.border.bottom.color = "gray30",
      table_body.hlines.width = body.hlines,
      row_group.border.top.width = gt::px(2),
      row_group.border.top.color = "gray30",
      row_group.border.bottom.width = gt::px(3),
      summary_row.border.style = "dashed",
      summary_row.border.width = gt::px(2),
      grand_summary_row.border.color = "gray30",
      stub.border.width = gt::px(0)
    ) |>
    gt::tab_header(title = title, subtitle = subtitle) |>
    gt::tab_style(
      style = cell_text(align = "left"),
      locations = list(
        cells_column_spanners(),
        cells_column_labels(),
        cells_body(),
        cells_stub()
      )
    )
}