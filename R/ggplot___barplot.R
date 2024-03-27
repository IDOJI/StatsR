ggplot___barplot <- function(input_vector,
                             log_y = T,
                             title = "Barplot",
                             xlab = NULL,
                             xlab_title = "Category",
                             ylab_title = "Values",
                             adding.values=T){
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




  # 🟥 Check input ######################################################################################
  if(!is.vector(input_vector)){
    stop("The input should be a vector")
  }





  # 🟥 color palette #################################################################################
  palette = c("Pastel1", "Pastel2", "Set1", "Set2", "Set3")
  fill_color = brewer.pal(n = length(input_vector), sample(palette, 1)) %>% suppressWarnings()






  # 🟥 as data frame #######################################################################################
  if(is.null(xlab)){
    df = data.frame(x = 1:length(input_vector), y = input_vector)
  }else{
    if(length(xlab) != length(input_vector)){
      stop("The length of `xlab` and `input_vector` should be same!")
    }else{
      df = data.frame(x = factor(xlab, levels = xlab), y = input_vector)
    }
  }

  # log
  df$log_y = log(df$y)





  # 🟥 barplot ########################################################################################################
  ## 🟨 plotting ====================================================================
  if(log_y){
    p = ggplot(df, aes(x = x, y = log_y)) +
      geom_bar(stat = "identity", fill = fill_color)
  }else{
    p = ggplot(df, aes(x = x, y = y)) +
      geom_bar(stat = "identity", fill = fill_color)
  }





  ## 🟨 Label ====================================================================
  p = p + labs(x = xlab_title, y = ylab_title, title = title)



  ## 🟨 Theme ====================================================================
  p = p + theme_minimal() +
    theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),  # 타이틀 설정
      axis.title.x = element_text(size = 14, face = "bold"),  # x 축 제목 설정
      axis.title.y = element_text(size = 14, face = "bold"),  # y 축 제목 설정
      axis.text = element_text(size = 12), # 축 텍스트 크기 설정
      axis.text.x = element_text(angle = 45, hjust = 1)  # 축 텍스트 크기 설정
    )




  # 🟥 adding values on the barplot ################################################################################
  if(adding.values){

    if(log_y){

      p = p + geom_text(aes(label = y, y = log_y), vjust = -0.5, color = "black") +
        scale_y_continuous(limits = c(0, max(df$log_y) * 1.2)) # Change the max scale of y axis

    }else{
      p <- p + geom_text(aes(label = round(after_stat(y), digits = 3)),
                         vjust = -0.5,
                         size = 5,
                         color = "black") +
        scale_y_continuous(limits = c(0, max(df$y) * 1.2)) # Change the max scale of y axis
    }



  }


  return(p)
}
