ggplot___correlation = function(df=NULL, y=NULL, x=NULL, method = "pearson", p.adj.method = "bonferroni"){
  # 🟥 Method ############################################################################################################
  if(method=="pearson"){
    type = "parametric"
  }



  # 🟥 x vs y ############################################################################################################
  if(!is.null(x) && !is.null(y)){

    p = ggplot___scatterplot(df, x, y, method = method) # only pearson

  }
  # 🟥 whole df ############################################################################################################
  if(is.null(x) && is.null(y)){

    p = ggcorrmat(df, p.adjust.method = p.adj.method, type = type)

  }
  # 🟥 one var vs the others ############################################################################################################
  if(is.null(x) && !is.null(df) && !is.null(y)){

    # Correlation matrix on whole variables
    cor.mat <- cor(df, use = "complete.obs", method = method)

    # only target variable
    target_cor <- cor.mat[y, ]

    # corr to df
    cor.df <- data.frame(Variable = names(target_cor), Correlation = target_cor) %>%
      dplyr::filter(., Variable != y) # Exclude `y`


    # Perform correlation test and extract p-values
    p_values = sapply(cor.df$Variable, function(x){
      cor.test(x = df[,y], y = df[,x], method = method)$p.value
    })


    # adjust pvals
    adjusted_p_values = p.adjust(p_values, method = p.adj.method)



    # Significance
    Signif = SUB___P.vals.Signif.Stars(adjusted_p_values, F)


    # color palette
    library(RColorBrewer)
    colors <- brewer.pal(9, "RdYlGn")

    # visualization
    p <- ggplot(cor.df, aes(x = Variable, y = Correlation, fill = Correlation)) +
      geom_col() +
      geom_text(aes(label = round(Correlation, 2)),  # 각 bar 위에 텍스트 표시
                position = position_stack(vjust = 0.5),  # 텍스트 위치 조정
                size = 5,  # 텍스트 크기 설정
                color = "black") +  # 텍스트 색상 설정
      geom_text(aes(label = Signif),
                position = position_stack(vjust = 0.5),  # 텍스트 위치 조정
                size = 8,  # 텍스트 크기 설정
                color = "white", hjust = -0.6) +  # 텍스트 색상 설정
      coord_flip() +  # 변수 이름을 수평으로 표시
      labs(title = paste0("Correlation of `", y, "` with Other Variables"),
           x = "Variables",
           y = "Correlation Coefficient") +
      theme_minimal() +
      scale_fill_gradientn(colours = colors) +
      theme(
        plot.title = element_text(size = 20, face = "bold", hjust = 0.5),  # 제목의 크기와 굵기 변경 및 중앙 정렬
        plot.title.position = "plot",  # 제목을 중앙에 위치
        axis.text = element_text(size = 12),  # x축과 y축 눈금의 글자 크기 변경
        axis.title = element_text(size = 15, face = "bold")  # xlab과 ylab의 글자 크기 변경
      ) +
      xlab("Variables") + ylab("Correlation Coefficient")


  }


  return(p)

}
