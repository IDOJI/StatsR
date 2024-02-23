Test___Normality = function(Data,
                            Group_Var=NULL,
                            Response_Vars=NULL,
                            outlier_method,
                            alpha = 0.05){
  # 🟥 Decision ############################################################################
  ## 🟧Single vector ===============================================================
  if(is.null(Group_Var) && is.null(Response_Vars)){

    Results = Test___Normality___Single.Vector(Data, outlier_method, alpha)

  ## 🟧Data.frame with group var =============================================================================
  }else if(!is.null(Response_Vars)){

    Results = lapply(Response_Vars, function(ith_Response, ...){

      Test___Normality___Data.Frame(Data = Data, Group_Var = Group_Var, Response_Var = ith_Response, outlier_method, alpha = alpha)

   })
    names(Results) = Response_Vars

  ## 🟧else ============================================================================
  }else{
    stop("Check input!")
  }




  # 🟥Density function #############################################################################
  for(ith_Response in Response_Vars){

    # Combined Group
    p1 = ggplot___histogram(df = Data,
                            x = ith_Response,
                            group_var = Group_Var,
                            group_combined = T,
                            density = T,
                            path_Export = path_save)

    # Each group differently
    p2 = ggplot___histogram(df = Data,
                            x = ith_Response,
                            group_var = Group_Var,
                            group_combined = F,
                            density = T,
                            path_Export = path_save)


  }




  cat("\n", crayon::green("Testing"), crayon::red("Normality"), crayon::green("is done!"),"\n")
  return(Results)

}

library(ggplot2)
library(ggpubr)  # ggplot2와 함께 사용할 수 있는 패키지

# 예제 데이터 생성
set.seed(123)
data <- data.frame(value = rnorm(100))

# 히스토그램과 QQ 플롯을 하나의 그림 위에 겹쳐서 그리는 함수
plot_combined <- function(data, x_var) {
  # 히스토그램과 QQ 플롯을 하나의 ggplot 객체에 추가
  combined_plot <- ggplot(data, aes(x = !!sym(x_var))) +
    geom_histogram(binwidth = 0.5, fill = "skyblue", color = "black", alpha = 0.7) +
    labs(x = x_var, y = "Frequency", title = "Histogram with QQ Plot") +
    geom_qq(aes(sample = !!sym(x_var)), color = "red") +
    geom_qq_line(aes(sample = !!sym(x_var)), color = "red")

  return(combined_plot)
}

# 함수를 사용하여 히스토그램과 QQ 플롯을 겹쳐서 그리기
plot_combined(data, "value")


