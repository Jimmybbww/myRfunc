#' Custom Theme for gtsummary Tables
#'
#' @param style Character string specifying the style ('strip', 'bw', or 'bg')
#' @param title Optional title for the table
#' @param subtitle Optional subtitle for the table
#' @param bg.color Background color for the table
#' @param strip.bg.color Background color for striped rows
#' @param set_theme Logical; whether to set the theme globally
#' @return A gtsummary theme list
#' @export
#'
#' @examples
#' \dontrun{
#' # Apply a striped theme
#' theme_gtsummary_style(
#'   style = "strip",
#'   title = "My Table",
#'   strip.bg.color = "#FFFAEE"
#' )
#' }
theme_gtsummary_style <- function(style,
                                 title = NULL,
                                 subtitle = NULL,
                                 bg.color = "#F2F1E9",
                                 strip.bg.color = "#FFFAEE",
                                 set_theme = TRUE) {

  if(is.null(title)) title = NULL else title = gt::md(paste0("**",title,"**"))

  if (style == 'strip') {strip = TRUE; bg.color = "white"; strip.bg.color = strip.bg.color; body.hlines = gt::px(0)}
  if (style == 'bw') {strip = FALSE; bg.color = "white"; body.hlines = gt::px(0)}
  if (style == 'bg') {strip = FALSE; bg.color = bg.color; body.hlines = gt::px(1)}

  my_theme = list(
    "as_gt-lst:addl_cmds" = list(
      "tab_spanner" = rlang::expr(
        gt::tab_options(
          row.striping.include_table_body = !!strip,
          row.striping.background_color = !!strip.bg.color,
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
          table.background.color = !!bg.color,
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
          table_body.hlines.width = !!body.hlines,
          row_group.border.top.width = gt::px(2),
          row_group.border.top.color = "gray30",
          row_group.border.bottom.width = gt::px(3),
          summary_row.border.style = "dashed",
          summary_row.border.width = gt::px(2),
          grand_summary_row.border.color = "gray30",
          stub.border.width = gt::px(0)
        )
      ),
      user_added1 = rlang::expr(gt::tab_header(
        title = !!title,
        subtitle = !!subtitle
      )),
      user_added2 = rlang::expr(gt::tab_style(
        style = cell_text(align = "left"),
        locations = list(
          cells_column_spanners(),
          cells_column_labels(),
          cells_body(),
          cells_stub()
        )
      ))
    )
  )

  if (set_theme == TRUE)
    set_gtsummary_theme(my_theme)

  return(invisible(my_theme))
}