ggplot___correlation = function(df=NULL, y=NULL, x=NULL, method = "pearson", p.adj.method = "bonferroni"){
  # 🟥 Install and loading Packages ############################################################################################################
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
  install_packages(c("scales", "RColorBrewer"))





  # 🟥 Method ############################################################################################################
  if(method=="pearson"){
    type = "parametric"
  }






  # 🟥 x vs y ############################################################################################################
  if(!is.null(x) && !is.null(y) && is.null(df)){

    p = ggplot___scatterplot(df, x, y, method = method) # only pearson

  }



  # 🟥 whole df ############################################################################################################
  if(is.null(x) && is.null(y)){

    p = ggcorrmat(df, p.adjust.method = p.adj.method, type = type)

  }




  # 🟥 one var vs the others ############################################################################################################
  if(!is.null(df) && !is.null(y)){

    if(is.null(x)){
      x = names(df)[names(df)!=y]
    }

    # Correlation matrix on whole variables
    cor.mat <- cor(df, use = "complete.obs", method = method)

    # only target variable
    target_cor <- cor.mat[y, ]

    # corr to df
    # Create a data frame with Variable and Correlation columns
    cor.df <- data.frame(Variable = names(target_cor), Correlation = target_cor) %>%
      # Exclude y
      dplyr::filter(Variable != y) %>%
      # Create a new column with the absolute values of Correlation
      dplyr::mutate(Abs_Correlation = abs(Correlation)) %>%
      # Arrange the data frame based on the absolute values of Correlation
      dplyr::arrange(desc(Abs_Correlation)) %>%
      dplyr::select(c(1:2))
    rownames(cor.df) = NULL



    # Perform correlation test and extract p-values
    cor.df$p_values = sapply(cor.df$Variable, function(x){
      cor.test(x = df[,x], y = df[,y], method = method)$p.value
    })


    # adjust pvals
    cor.df$adjusted_p_values = p.adjust(cor.df$p_values, method = p.adj.method)



    # Significance
    cor.df$Signif = SUB___P.vals.Signif.Stars(cor.df$adjusted_p_values, F)



    # factorization for variables order in the plot
    cor.df$Variable <- factor(cor.df$Variable, levels = rev(cor.df$Variable))


    # signif for text on a plot
    cor.df$Signif_Text = paste0(round(cor.df$Correlation, 4), " ", cor.df$Signif)



    # 데이터에서 상관관계 값의 범위 확인
    min_cor <- min(cor.df$Correlation)
    max_cor <- max(cor.df$Correlation)

    # 색상 팔레트 조건부 설정
    library(RColorBrewer)
    if (min_cor >= 0) {
      # 양수 값만 있는 경우 녹색 계열 사용
      colors <- brewer.pal(n = 9, name = "Greens")[9:1]
    } else if (max_cor <= 0) {
      # 음수 값만 있는 경우 빨간 계열 사용
      colors <- brewer.pal(n = 9, name = "Reds")[9:1]
    } else {
      # 양수와 음수 값 모두 있는 경우 기본 팔레트 사용
      colors <- brewer.pal(n = 9, name = "RdYlGn")
    }



    # ggplot 코드 시작
    p <- ggplot(cor.df, aes(x = Variable, y = Correlation, fill = Correlation)) +
      geom_col() +
      coord_flip() +
      geom_text(aes(label = Signif_Text, hjust = ifelse(Correlation > 0, -0.2, 1.2)), position = position_dodge(width = 0.9), size = 4) +
      # 기존 Correlation 값에 대한 텍스트
      # geom_text(aes(label = sprintf("%.2f", Correlation), hjust = ifelse(Correlation > 0, -0.2, 1.2)), position = position_dodge(width = 0.9), size = 3.5) +
      # Signif 값을 추가하는 텍스트
      # geom_text(aes(label = Signif, hjust = ifelse(Correlation > 0, -0.3, 1.3)), position = position_dodge(width = 0.9), size = 3.5) +
      labs(title = paste0("Correlation of `", y, "` with Other Variables"),
           x = "Variables",
           y = "Correlation Coefficient") +
      theme_minimal() +
      scale_fill_gradientn(colours = colors) +
      scale_y_continuous(limits = c(min(cor.df$Correlation) - 0.1, max(cor.df$Correlation) + 0.1)) +
      theme(
        plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
        plot.title.position = "plot",
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 15, face = "bold")
      ) +
      xlab("Variables") + ylab("Correlation Coefficient")

  }


  return(p)

}
