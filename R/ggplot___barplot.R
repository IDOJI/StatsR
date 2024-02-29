ggplot___barplot <- function(input_vector, title = "Barplot", xlab = "Model", ylab = "Values", adding.values=T){
  # 🟥 packages ######################################################################################
  install_packages = function(packages, load=TRUE) {
    # load : load the packages after installation?
    for(pkg in packages) {
      if (!require(pkg, character.only = TRUE)) {
        install.packages(pkg)
      }

      if(load){
        library(pkg, character.only = TRUE)
      }
    }
  }
  visual = c("ggplot2", "rlang", "RColorBrewer")
  install_packages(visual)





  # 🟥 color palette #################################################################################
  palette = c("Pastel1", "Pastel2", "Set1", "Set2", "Set3")
  fill_color = brewer.pal(n = length(input_vector), sample(palette, 1)) %>% suppressWarnings()






  # 🟥 data frame #######################################################################################
  df = data.frame(Model = paste(xlab, seq_along(input_vector), sep = "_"),
                  Values = input_vector)
  df = change_colnames(df, "Model", xlab)
  df = change_colnames(df, "Values", ylab)



  # 🟥 barplot ########################################################################################################
  p <- ggplot(df, aes(x = !!sym(xlab), y = !!sym(ylab))) +
    geom_bar(stat = "identity", fill = fill_color) +
    labs(x = xlab, y = ylab,
         title = title) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),  # 타이틀 설정
      axis.title.x = element_text(size = 14, face = "bold"),  # x 축 제목 설정
      axis.title.y = element_text(size = 14, face = "bold"),  # y 축 제목 설정
      axis.text = element_text(size = 12)  # 축 텍스트 크기 설정
    )





  # 🟥 adding values on the barplot ################################################################################
  if(adding.values){

    p <- p + geom_text(aes(label = round(after_stat(y), digits = 3)),
                       vjust = -0.5,
                       size = 5,
                       color = "black")

  }


  return(p)
}
