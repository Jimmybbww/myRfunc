#' Create a Forest Plot with Tabular Data
#'
#' This function creates a forest plot combined with a table of results. It is
#' particularly useful for visualizing odds ratios, risk ratios, or other
#' effect measures with confidence intervals.
#'
#' @param data A data frame containing the data to plot
#' @param label_col Column name for labels (default: 'Label')
#' @param est_col Column name for point estimates (default: "OR")
#' @param lcl_col Column name for lower confidence limits (default: "LCL")
#' @param ucl_col Column name for upper confidence limits (default: "UCL")
#' @param grp_col Optional column name for grouping (default: NULL)
#' @param seq_col Column name for sequence/ordering (default: "seq")
#' @param x_lab Label for x-axis (default: "OR (95% CI)")
#' @param vline Position of vertical reference line (default: 1)
#' @param fill_lab Label for fill legend (default: NULL)
#' @param color_map Named vector for custom color mapping (default: NULL)
#' @param save_path Path to save the plot (default: NULL)
#' @param width Plot width in inches when saving (default: 12)
#' @param height Plot height in inches when saving (default: 9)
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
#' # Create sample data
#' data <- data.frame(
#'   Label = c("Group A", "Group B", "Group C"),
#'   OR = c(1.5, 0.8, 2.1),
#'   LCL = c(0.9, 0.5, 1.4),
#'   UCL = c(2.1, 1.1, 2.8),
#'   seq = 1:3
#' )
#'
#' # Create basic forest plot
#' tabular_forest(data)
#'
#' # Add grouping and custom colors
#' data$Group <- c("X", "X", "Y")
#' tabular_forest(
#'   data,
#'   grp_col = "Group",
#'   color_map = c(X = "blue", Y = "red")
#' )
#' }
tabular_forest <- function(
    data,
    label_col = 'Label',
    est_col = "OR",
    lcl_col = "LCL",
    ucl_col = "UCL",
    grp_col = NULL,
    seq_col = "seq",
    x_lab = "OR (95% CI)",
    vline = 1,
    fill_lab = NULL,
    color_map = NULL,
    save_path = NULL,
    width = 12,
    height = 9
) {
    # 檢查輸入數據
    required_cols <- c(label_col, est_col, lcl_col, ucl_col, seq_col)
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
    empty_row <- data.frame(matrix(NA, nrow = 1, ncol = ncol(data)))
    names(empty_row) <- names(data)
    empty_row[[seq_col]] <- 0
    empty_row[[label_col]] <- ""
    if(!is.null(grp_col)) empty_row[[grp_col]] <- NA

    # 繪圖資料
    p_data <-
        rbind(empty_row, data) |>
        select(all_of(c(label_col, est_col, lcl_col, ucl_col, grp_col, seq_col)))

    p_data[[seq_col]] = factor(p_data[[seq_col]], labels=p_data[[label_col]])

    # 創建標籤
    p_data$text <- ifelse(
        is.na(p_data[[est_col]]),
        'OR (95% CI)',
        sprintf("%.2f (%.2f-%.2f)",
            p_data[[est_col]],
            p_data[[lcl_col]],
            p_data[[ucl_col]])
    )

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

    # 基礎圖形審美設定
    base_aes <- list(
        x = quo(!!sym(est_col)),
        y = quo(fct_rev(!!sym(seq_col)))
    )

    # 如果有分組，加入填充審美
    if (!is.null(grp_col)) {
        base_aes$fill <- quo(!!sym(grp_col))
    }

    # 左側forest plot
    p_left <- ggplot(aes(!!!base_aes), data = p_data) +
        theme_bw() +
        geom_errorbar(aes(xmin = .data[[lcl_col]],
                        xmax = .data[[ucl_col]]),
                    width = 0.15,
                    alpha = .8,
                    color = 'gray20',
                    size = .35) +
        geom_point(color = 'black', size = 2.5, shape = 22) +
        geom_segment(x = vline, xend = vline,
                    y = 0,
                    yend = nrow(data)+.3,
                    linetype = "dashed",
                    color = "gray30",
                    size = 0.5) +
        labs(x = x_lab, y = '', fill = fill_lab) +
        guides(fill = guide_legend(override.aes = list(size = 4),
                                position = "top")) +
        theme(
            axis.title = element_text(face = "bold"),
            axis.text = element_text(face = "bold"),
            axis.text.y = element_text(hjust = 0),
            legend.title = element_text(face = "bold"),
            axis.ticks.y = element_blank(),
            panel.border = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            axis.line.x = element_line(color = "black")
        ) +
        scale_fill_manual(values = fcmap(color_map), na.translate = F)

    if (is.null(grp_col)) {
        p_left <- p_left + update_geom_defaults(geom = 'point', new = c(fill = 'grey50'))
    }

    # 右側數值標籤
    p_right <- ggplot(data = p_data) +
        scale_y_discrete(limits = rev(p_data[[seq_col]]) ) +
        geom_text(
            aes(
                x = 0,
                y = .data[[seq_col]],
                label = text
            ),
            fontface = ifelse(is.na(p_data[[est_col]]), "bold", "plain"),
            size = 3.5,
            hjust = 0
        ) +
        theme_void()

    # 定義佈局
    layout <- c(
        area(t = 0, l = 0, b = 30, r = 6),
        area(t = 0, l = 3, b = 30, r = 11)
    )

    # 組合圖形
    final_plot <- p_left + p_right + plot_layout(design = layout)

    # 如果提供儲存路徑，則儲存圖片
    if (!is.null(save_path)) {
        tiff(save_path,
            width = width,
            height = height,
            units = "in",
            res = 300)
        print(final_plot)
        dev.off()
    }

    # 創建結果列表並設定類別
    result <- structure(
        list(
            final = final_plot,
            left = p_left,
            right = p_right
        ),
        class = c("forest_plot", "meta_plot")
    )

    # 定義打印方法為本地函數
    print.forest_plot <<- function(x, ...) {
        print(x$final)
    }

    return(result)
}