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



    # Perform correlation test and extract p-values
    p_values = sapply(cor.df$Variable, function(x){
      cor.test(x = df[,x], y = df[,y], method = method)$p.value
    })


    # adjust pvals
    adjusted_p_values = p.adjust(p_values, method = p.adj.method)



    # Significance
    cor.df$Signif = SUB___P.vals.Signif.Stars(adjusted_p_values, F)



    # factorization for variables order in the plot
    cor.df$Variable <- factor(cor.df$Variable, levels = rev(cor.df$Variable))


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

    geom_text(aes(label = Signif),
              position = position_nudge(x = 0, y = 0),  # Adjust the position to align with the axis
              size = 8, color = "white", hjust = 0) +

      library(ggplot2)

    # ggplot 코드 시작
    p <- ggplot(cor.df, aes(x = Variable, y = Correlation, fill = Correlation)) +
      geom_col() +
      coord_flip() +
      # 기존 Correlation 값에 대한 텍스트
      geom_text(aes(label = sprintf("%.2f", Correlation), hjust = ifelse(Correlation > 0, -0.2, 1.2)), position = position_dodge(width = 0.9), size = 3.5) +
      # Signif 값을 추가하는 텍스트
      geom_text(aes(label = Signif, hjust = ifelse(Correlation > 0, -0.3, 1.3)), position = position_dodge(width = 0.9), size = 3.5) +
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

    # 플롯 출력
    print(p)
    library(ggplot2)

    # ggplot 코드 시작
    p <- ggplot(cor.df, aes(x = Variable, y = Correlation, fill = Correlation)) +
      geom_col() +
      coord_flip() +
      # Signif 값을 추가하는 텍스트
      geom_text(aes(label = Signif, vjust = ifelse(Correlation > 0, 0.5, 0.5)), position = position_dodge(width = 0.9), size = 3.5) +
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

    # 플롯 출력
    print(p)



    library(ggplot2)
    library(dplyr)

    # 예제 데이터 프레임
    cor.df <- data.frame(
      Variable = c("Var1", "Var2", "Var3", "Var4"),
      Correlation = c(0.2, -0.4, 0.6, -0.8),
      Signif = c("***", "*", "**", "***")
    )

    # 바의 안쪽에 텍스트를 위치시키기 위한 위치 조정 변수 추가
    cor.df <- cor.df %>%
      mutate(TextPosition = ifelse(Correlation > 0, Correlation - 0.05, Correlation + 0.05))

    # ggplot 코드 시작
    p <- ggplot(cor.df, aes(x = Variable, y = Correlation, fill = Correlation)) +
      geom_col() +
      coord_flip() +
      geom_text(aes(y = TextPosition, label = Signif), position = position_dodge(width = 0.9), size = 3.5) +
      labs(title = "Correlation with Other Variables",
           x = "Variables",
           y = "Correlation Coefficient") +
      theme_minimal() +
      scale_fill_gradientn(colours = c("red", "blue")) +
      scale_y_continuous(limits = c(min(cor.df$Correlation) - 0.1, max(cor.df$Correlation) + 0.1)) +
      theme(
        plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
        plot.title.position = "plot",
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 15, face = "bold")
      ) +
      xlab("Variables") + ylab("Correlation Coefficient")

    # 플롯 출력
    print(p)




    p
    library(ggplot2)
    library(dplyr)
    library(RColorBrewer)

    # 기존 데이터 프레임 생성 코드
    cor.df <- data.frame(Variable = names(target_cor), Correlation = target_cor) %>%
      filter(Variable != y) %>%
      mutate(Abs_Correlation = abs(Correlation)) %>%
      arrange(desc(Abs_Correlation)) %>%
      select(c(1:2))

    # Significance 문자열과 hjust 값 결정
    cor.df <- cor.df %>%
      mutate(Signif = SUB___P.vals.Signif.Stars(adjusted_p_values, F),
             hjust = ifelse(Correlation > 0, 1, 0)) # 양수면 1 (오른쪽), 음수면 0 (왼쪽)

    # factorization for variables order in the plot
    cor.df$Variable <- factor(cor.df$Variable, levels = rev(cor.df$Variable))


    cor.df <- data.frame(Variable = names(target_cor), Correlation = target_cor) %>%
      dplyr::filter(Variable != y) %>%
      dplyr::mutate(Abs_Correlation = abs(Correlation)) %>%
      dplyr::arrange(desc(Abs_Correlation)) %>%
      dplyr::select(Variable, Correlation) # 이 부분을 수정했습니다.
    cor.df$Correlation

    # 색상 팔레트 설정 코드 생략 (기존 코드 사용)

    # 수정된 시각화 코드
    p <- ggplot(cor.df, aes(x = Variable, y = Correlation, fill = Correlation)) +
      geom_col() +
      geom_text(aes(label = round(Correlation, 2)),
                position = position_stack(vjust = 0.5),
                size = 5, color = "black") +
      geom_text(aes(label = Signif, hjust = ifelse(cor.df$Correlation > 0, 0, 1)),
                position = position_nudge(x = ifelse(cor.df$Correlation > 0, -0.03, 0.03)), # x 위치를 미세 조정
                size = 5, color = "white") +
      coord_flip() +
      labs(title = paste0("Correlation of `", y, "` with Other Variables"),
           x = "Variables", y = "Correlation Coefficient") +
      theme_minimal() +
      scale_fill_gradientn(colours = colors) +
      theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
            plot.title.position = "plot",
            axis.text = element_text(size = 12),
            axis.title = element_text(size = 15, face = "bold")) +
      xlab("Variables") + ylab("Correlation Coefficient")

    p


    p



  }


  return(p)

}
