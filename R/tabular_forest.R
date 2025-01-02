#' Create a Forest Plot with Tabular Data
#'
#' [previous documentation remains the same...]
tabular_forest <- function(data,
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
                          xlim = NULL) {
    
    # [previous code remains the same until the plot section...]
    
    # 左側forest plot
    p_left <- ggplot(aes(!!!base_aes), data = p_data) +
        theme_bw() +
        geom_vline(xintercept = null_line_at,
                  linetype = "dashed",
                  color = "gray30",
                  size = 0.5) +
        geom_errorbar(aes(xmin = .data[[lcl_col]],
                        xmax = .data[[ucl_col]]),
                    width = 0.15,
                    alpha = .8,
                    color = 'gray20',
                    size = .35)

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
                               y = .data[[seq_col]],
                               yend = .data[[seq_col]]),
                           alpha = .8,
                           color = 'gray20',
                           size = .35,
                           arrow = arrow(length = unit(0.2, "cm")))
        }
        
        # 左側箭頭
        left_arrows <- arrow_data[arrow_data[[lcl_col]] < xlim[1], ]
        if (nrow(left_arrows) > 0) {
            p_left <- p_left +
                geom_segment(data = left_arrows,
                           aes(x = .data[[ucl_col]],
                               xend = xlim[1],
                               y = .data[[seq_col]],
                               yend = .data[[seq_col]]),
                           alpha = .8,
                           color = 'gray20',
                           size = .35,
                           arrow = arrow(length = unit(0.2, "cm")))
        }
        
        p_left <- p_left + 
            scale_x_continuous(limits = xlim)
    }

    # 添加點和主題設定
    p_left <- p_left +
        geom_point(color = 'black',
                  size = point_size,
                  shape = point_shape) +
        labs(x = "OR (95% CI)", y = '', fill = NULL) +
        guides(fill = guide_legend(override.aes = list(size = 4),
                                position = "top")) +
        theme(
            text = element_text(family = font_family),  # 修改：移到這裡
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
        )

    if (!is.null(grp_col)) {
        p_left <- p_left + scale_fill_manual(values = fcmap(NULL), na.translate = F)
    } else {
        p_left <- p_left + update_geom_defaults(geom = 'point', new = c(fill = 'grey50'))
    }

    if (!is.null(table_theme)) {
        p_left <- p_left + table_theme
    }

    # 右側數值標籤
    p_right <- ggplot(data = p_data) +
        scale_y_discrete(limits = rev(p_data[[seq_col]])) +
        geom_text(
            aes(
                x = 0,
                y = .data[[seq_col]],
                label = text
            ),
            family = font_family,  # 修改：移到這裡
            fontface = ifelse(is.na(p_data[[est_col]]), "bold", "plain"),
            size = table_font_size,
            hjust = 0,
            color = "grey20"
        ) +
        theme_void()

    # [remaining code stays the same...]