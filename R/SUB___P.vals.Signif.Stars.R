sub___p.vals.signif.stars <- function(p.vals, show.NS = TRUE){
  p.vals <- as.numeric(p.vals)  # p-values를 숫자로 변환

  if(show.NS){
    # show.NS가 TRUE일 때 NS와 HNS를 포함하여 출력
    ifelse(p.vals < 0.0001, "****",
           ifelse(p.vals < 0.001, "***",
                  ifelse(p.vals <= 0.01, "**",
                         ifelse(p.vals <= 0.05, "*",
                                ifelse(p.vals > 0.1, "HNS", "NS"))))) # 0.1 이상은 HNS로 표시
  } else {
    # show.NS가 FALSE일 때는 유의하지 않은 경우 빈 문자열 반환
    ifelse(p.vals < 0.0001, "****",
           ifelse(p.vals < 0.001, "***",
                  ifelse(p.vals <= 0.01, "**",
                         ifelse(p.vals <= 0.05, "*", ""))))
  }
}
