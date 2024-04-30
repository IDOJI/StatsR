ggplot___QQplot = function(df, x, group_var=NULL, path_save = NULL){
  # 🟥 colnames =========================================================================
  df = change_colnames(df, x, "x_var")
  df = change_colnames(df, group_var, "group_var")



  # 🟥 plot grouped =========================================================================
  if(is.null(group_var)){

    p <- ggplot(df, aes(sample = x_var)) +
      stat_qq(size = 3) +
      geom_qq_line(colour = "red", linetype = "dashed", size = 1.5) + # 기준선을 더 돋보이게 조정
      labs(title = paste("Q-Q Plot - ", x), x = "Theoretical Quantiles", y = "Sample Quantiles") +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
        axis.title.x = element_text(size = 16, face = "bold"),
        axis.title.y = element_text(size = 16, face = "bold")
      )

  }else{

    p <- ggplot(df, aes(sample = x_var)) +
      stat_qq(aes(color = group_var), size = 3) +
      geom_qq_line(colour = "black", linetype = "dashed", linewidth = 1.5) + # 기준선을 더 돋보이게 조정
      labs(title = paste("Q-Q Plot - ", x),
           x = "Theoretical Quantiles",
           y = "Sample Quantiles",
           color = group_var) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
        axis.title.x = element_text(size = 16, face = "bold"),
        axis.title.y = element_text(size = 16, face = "bold"),
        legend.title = element_text(size = 14, face = "bold")
      )

  }


  # 🟥 file name =========================================================================
  filename = paste0("[QQ plot] ", x)
  if(!is.null(group_var)){
    filename = paste0(filename, "___", group_var)
  }



  # 🟥 Explort =========================================================================
  if (!is.null(path_save)) {

    ggsave(plot = p, filename = paste0(path_save, "/", filename, ".png"), width = 10, height = 7.5, units = "in", dpi = 200, bg = "white")

  }


  return(p)
}
