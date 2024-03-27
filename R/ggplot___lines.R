ggplot___lines = function(df,
                          col_names = NULL,
                          x_col = "Year",
                          x_axis_vals = NULL,
                          # options
                          point = T,
                          show.legend = T,
                          # labels
                          title = "Timeseries",
                          xlab = "Year",
                          ylab = "Value",
                          color_legend_title = "Category",
                          # export
                          path_Export = NULL,
                          file.name = NULL,
                          width = 20,
                          height = 5){
  # 🟥 Colnames ====================================================================
  ## 🟨 Check input ===============================================================
  if(is.null(col_names)){
    col_names = colnames(df)
    col_names = col_names[col_names != x_col]
  }


  ## 🟨 Subset ===============================================================
  df_selected = df[, c(x_col, col_names), drop = FALSE]







  # 🟥 x-axis ====================================================================
  if(is.null(x_axis_vals)){
    x_axis_vals = 1:nrow(df_selected)
  }
  if(nrow(df_selected)!=length(x_axis_vals)){
    stop("Compare the length of x_axis_vals and the rows of df")
  }
  x_axis_labs = df_selected[, x_col]






  # 🟥 Transform data into long format ============================================================
  df_selected$Year = df_selected$Year %>% as.numeric
  long_df = df_selected %>% pivot_longer(cols = -!!x_col,
                                         names_to = "Category",
                                         values_to = "Value") %>% dplyr::arrange(Category, !!x_col)




  # 🟥 Add x-axis vals ============================================================
  x.axis_df = cbind(x_axis_vals = x_axis_vals, long_df)
  x.axis_df$Value = as.numeric(x.axis_df$Value)
  x.axis_df$x_axis_vals = as.numeric(x.axis_df$x_axis_vals)



  # 🟥 plotting ====================================================================
  ## 🟨 Line ============================================================================
  p <- ggplot() +
    geom_line(data = x.axis_df, aes(x = x_axis_vals, y = Value, group = Category, color = Category),
              show.legend = show.legend)




  ## 🟨 Point ============================================================================
  if(point){

    p = p + geom_point(data = x.axis_df,
                       aes(x = x_axis_vals, y = Value, group = Category, color = Category),
                       show.legend = FALSE) # 선 위에 점 추가, 범례는 이미 geom_line에서 표시했으므로 여기서는 표시하지 않음

  }



  ## 🟨 Lables ============================================================================
  p = p + scale_x_discrete(limits = x_axis_labs) + # x축 라벨 지정
    theme_minimal() +
    labs(title = title, x = xlab, y = ylab, color = color_legend_title)




  ## 🟨 Theme ============================================================================
  p = p + theme(plot.title = element_text(hjust = 0.5, size = 25, face = "bold"), # 타이틀 가운데 정렬
                axis.title.x = element_text(size = 20, face = "bold"), # x 축 라벨 크기 및 굵기 조정
                axis.title.y = element_text(size = 20, face = "bold"), # y 축 라벨 크기 및 굵기 조정
                legend.title = element_text(size = 17, face = "bold") # 범례 제목 크기 및 굵기 조정
                )



  # 🟥 Exporting =================================================================================
  if(!is.null(path_Export)){
    ggsave(paste0(path_Export, "/", file.name, ".png"), p, bg = "white", width = width, height = height)
  }

  return(p)
}
