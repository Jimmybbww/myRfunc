#' Create a Forest Plot with Tabular Data
#'
#' @param data A data frame containing the data to plot
#' @param label_col Column name for labels (default: 'Label')
#' @param est_col Column name for point estimates (default: "OR")
#' @param lcl_col Column name for lower confidence limits (default: "LCL")
#' @param ucl_col Column name for upper confidence limits (default: "UCL")
#' @param grp_col Optional column name for grouping (default: NULL)
#' @param label_text Text for empty row label (default: "Label")
#' @param label_table Text for table label (default: "OR (95% CI)")
#' @param label_axis Text for x-axis label (default: "OR (95% CI)")
#' @param label_axis_size Font size for x-axis label (default: 10)
#' @param ci_sep Separator for confidence intervals (default: " to ")
#' @param null_line_at Position of reference line (default: 1)
#' @param arrows Whether to show arrows for out-of-bounds values (default: FALSE)
#' @param xlim Limits for x-axis (default: NULL)
#' @param point_color Color of the points (default: 'black')
#' @param point_size Size of the points (default: 2.5)
#' @param point_shape Shape of the points (default: 22)
#' @param errorbar_size Size of errorbar (default: 0.35)
#' @param errorbar_width Width of errorbar (default: 0.15)
#' @param errorbar_color Color of errorbar (default: "grey20")
#' @param text_color Color of x-axis title and y-axis text (default: "black")
#' @param table_font_size Font size for the table (default: 3.2)
#' @param font_family Font family (default: "Arial")
#' @param color_map List of color map (default: NULL)
#' @param shape_map List of shape map (default: NULL)
#' @param plot_theme Custom theme for the plot (default: NULL)
#' @param insert_label Insert label column to the first row (default: TRUE)
#'
#' @return A list with class 'forest_plot' containing three components:
#'   \itemize{
#'     \item final: The combined forest plot with table
#'     \item left: The forest plot component
#'     \item right: The table component
#'   }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Create example data
#' example_data <- data.frame(
#'   Label = paste0("Variable", 1:6),
#'   OR = c(1.25, 1.50, 0.85, 1.75, 1.15, 1.5),
#'   LCL = c(1.10, 1.25, 0.2, 0.9, 0.95, 1.2),
#'   UCL = c(1.40, 1.75, 1.5, 2.6, 1.35, 2.0),
#'   grp = paste0('Group ', LETTERS[rep(1:3, each=2)])
#'   )
#'
#' # Basic forest plot
#' tabular_forest(example_data[,-5])
#' tabular_forest(example_data, grp_col = 'grp')
#'
#' # Other customized forest plot
#' tabular_forest(example_data, 
#'                grp_col = 'grp',
#'                label_text = "Variables",
#'                label_table = "HR (95% CI)",
#'                label_axis = "\nHazard Ratio (95% CI)",
#'                color_map = c("red", "blue", "green"), 
#'                arrow = T, 
#'                xlim = c(0.5, 2.2),
#'                ci_sep = ", ",
#'                point_size = 3, 
#'                point_shape = 21, 
#'                font_family = "mono",
#'                table_font_size = 4.2,
#'                plot_theme = theme(legend.text = element_text(size = 12), 
#'                                   axis.text = element_text(size = 12),
#'                                   axis.title.x = element_text(size = 14)
#'                                   )
#'                )
#' }
tabular_forest <- function(data,
                           label_col = 'Label',
                           est_col = "OR",
                           lcl_col = "LCL",
                           ucl_col = "UCL",
                           grp_col = NULL,
                           label_text = label_col,
                           label_table = "OR (95% CI)",
                           label_axis = "OR (95% CI)",
                           label_axis_size = 10,
                           ci_sep = " to ",
                           null_line_at = 1,
                           arrows = FALSE,
                           xlim = NULL,
                           point_color = 'black',
                           point_size = 2.5,
                           point_shape = 22,
                           errorbar_size = 0.35,
                           errorbar_width = 0.15,
                           errorbar_color = 'grey20',
                           text_color = 'black',
                           table_font_size = 3.2,
                           font_family = "Arial",
                           color_map = NULL,
                           shape_map = NULL,
                           plot_theme = NULL,
                           insert_label = TRUE) {
  
  # 檢查輸入數據
  required_cols <- c(label_col, est_col, lcl_col, ucl_col)
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop(sprintf("Missing required columns: %s", paste(missing_cols, collapse = ", ")))
  }
  
  if (!is.null(grp_col) && !(grp_col %in% names(data))) {
    stop(sprintf("Group column '%s' not found in data", grp_col))
  }
  
  # 確保數值欄位是數值型
  data[[est_col]] <- as.numeric(data[[est_col]])
  data[[lcl_col]] <- as.numeric(data[[lcl_col]])
  data[[ucl_col]] <- as.numeric(data[[ucl_col]])
  
  # 準備資料
  if (insert_label == TRUE){
    empty_row <- data.frame(matrix(NA, nrow = 1, ncol = ncol(data)))
    names(empty_row) <- names(data)
    empty_row[[label_col]] <- paste0("<b>", label_text)
    empty_row$header <- TRUE
    empty_row$seq <- nrow(data)+1
    if(!is.null(grp_col)) empty_row[[grp_col]] <- NA
  } else {empty_row = NULL}
  
  # 繪圖資料
  p_data <- rbind(
    empty_row, 
    mutate(data, header = FALSE, seq = nrow(data):1)
  ) 
  
  # 創建標籤
  p_data$text <- ifelse(
    p_data$header,
    label_table,
    sprintf("%.2f (%.2f%s%.2f)",
            p_data[[est_col]],
            p_data[[lcl_col]],
            ci_sep,
            p_data[[ucl_col]])
  )
  
  p_data$text <- ifelse(p_data$text == paste0("NA (NA", ci_sep, "NA)"), "", p_data$text)
  
  # 預設顏色映射
  if(!is.null(grp_col)) {
    grp_lv = levels(as.factor(p_data[[grp_col]]))
    pal = paletteer::paletteer_d("ggsci::default_igv", length(grp_lv)) |> as.character()
  } else {
    grp_lv = NA
    pal = NA
  }
  
  # 自訂顏色映射
  fcmap = function(color_map) {
    if (!is.null(color_map)) {
      color_map
    } else {
      setNames(pal, grp_lv)
    }
  }
  
  # 預設形狀映射
  if(!is.null(grp_col)) {
    grp_lv = levels(as.factor(p_data[[grp_col]]))
    shp = 21:(20 + length(grp_lv))
  } else {
    grp_lv = NA
    shp = NA
  }
  
  # 自訂形狀映射
  fhmap = function(shape_map) {
    if (!is.null(shape_map)) {
      shape_map
    } else {
      setNames(shp, grp_lv)
    }
  }
  
  # 基礎圖形審美設定
  base_aes <- list(
    x = quo(!!sym(est_col)),
    y = quo(!!sym("seq"))
  )
  
  # 如果有分組，加入填充審美
  if (!is.null(grp_col)) {
    base_aes$fill <- quo(!!sym(grp_col))
    base_aes$color <- quo(!!sym(grp_col))
    base_aes$shape <- quo(!!sym(grp_col))
  }
  
  # 左側forest plot
  p_left <- ggplot(aes(!!!base_aes), data = p_data) +
    theme_bw() +
    scale_y_continuous(breaks = p_data$seq, 
                       labels = p_data[[label_col]]
    ) + 
    geom_segment(x = null_line_at, xend = null_line_at,
                 y = 0,
                 yend = nrow(data)+.3,
                 linetype = "dashed", 
                 color = "gray50", 
                 size = 0.35
    ) +
    geom_errorbar(aes(xmin = .data[[lcl_col]],
                      xmax = .data[[ucl_col]]
    ),
    width = errorbar_width,
    alpha = .8,
    color = errorbar_color,
    size = errorbar_size)
  
  # 處理箭頭
  if (arrows && !is.null(xlim)) {
    arrow_data <- p_data[!is.na(p_data[[est_col]]), ]
    
    # 右側箭頭
    right_arrows <- arrow_data[arrow_data[[ucl_col]] > xlim[2], ]
    if (nrow(right_arrows) > 0) {
      p_left <- p_left +
        geom_segment(data = right_arrows,
                     aes(x = .data[[lcl_col]],
                         xend = xlim[2],
                         y = .data$seq,
                         yend = .data$seq),
                     alpha = .8,
                     color = errorbar_color,
                     size = errorbar_size,
                     arrow = arrow(length = unit(0.2, "cm")))
    }
    
    # 左側箭頭
    left_arrows <- arrow_data[arrow_data[[lcl_col]] < xlim[1], ]
    if (nrow(left_arrows) > 0) {
      p_left <- p_left +
        geom_segment(data = left_arrows,
                     aes(x = .data[[ucl_col]],
                         xend = xlim[1],
                         y = .data$seq,
                         yend = .data$seq),
                     alpha = .8,
                     color = errorbar_color,
                     size = errorbar_size,
                     arrow = arrow(length = unit(0.2, "cm")))
    }
    
    # 設定 x 軸範圍
    p_left <- p_left + 
      scale_x_continuous(limits = xlim)
  }
  
  # 添加點和主題設定
  p_left <- p_left +
    geom_point(size = point_size) +
    labs(x = label_axis, y = '', fill = NULL, color = NULL, shape = NULL) +
    guides(fill = guide_legend(override.aes = list(size = 4), position = "top"),
           color = guide_legend(override.aes = list(size = 4), position = "top"),
           shape = guide_legend(override.aes = list(size = 4), position = "top")
    ) +
    theme(
      text = element_text(family = font_family),
      axis.title = element_text(face = "bold"),
      axis.text.y = ggtext::element_markdown(hjust = 0, color = text_color),
      axis.title.x = element_text(color = text_color, size = label_axis_size),
      legend.title = element_text(face = "bold"),
      axis.ticks.y = element_blank(),
      panel.border = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      axis.line.x = element_line(color = "black")
    )
  
  if (!is.null(color_map)) {
    p_left <- p_left + 
      scale_fill_manual(values = fcmap(color_map), na.translate = F) + 
      scale_color_manual(values = fcmap(color_map), na.translate = F)
  } 
  if (is.null(grp_col)) {
    p_left <- p_left + 
      update_geom_defaults(geom = 'point', new = c(fill = point_color, color = point_color))
  } 
  
  if (!is.null(shape_map)) {
    p_left <- p_left + 
      scale_shape_manual(values = fhmap(shape_map), na.translate = F) 
  } 
  if (is.null(grp_col)) {
    p_left <- p_left + 
      aes(shape = NULL) + update_geom_defaults(geom = 'point', new = list(shape = point_shape))
  } 
  
  if (!is.null(plot_theme)) {
    p_left <- p_left + plot_theme + 
      theme(text = element_text(family = font_family),
            axis.title = element_text(face = "bold"),
            axis.text.y = ggtext::element_markdown(hjust = 0, color = text_color),
            axis.title.x = element_text(color = text_color, size = label_axis_size),
            axis.ticks.y = element_blank()
      )
  }
  
  # 右側數值標籤
  p_right <- ggplot(data = p_data) +
    scale_y_continuous(breaks = p_data$seq) +
    geom_text(
      aes(
        x = 0,
        y = .data$seq,
        label = text
      ),
      family = font_family,
      fontface = ifelse(is.na(p_data[[est_col]]), "bold", "plain"),
      size = table_font_size,
      hjust = 0,
      color = "grey20"
    ) +
    theme_void()
  
  # 定義佈局
  layout <- c(
    area(t = 0, l = 0, b = 30, r = 6),
    area(t = 0, l = 3, b = 30, r = 11)
  )
  
  # 組合圖形
  final_plot <- p_left + p_right + plot_layout(design = layout)
  
  # 創建結果列表並設定類別
  result <- structure(
    list(
      final = final_plot,
      left = p_left,
      right = p_right,
      data = p_data
    ),
    class = c("forest_plot", "meta_data")
  )
  
  # 定義打印方法
  print.forest_plot <<- function(x, ...) {
    print(x$final)
  }
  
  return(result)
}
