Text___Wordcloud <- function(text, path_save=NULL, filename=NULL) {
  pkgs <- c("wordcloud", "stringr", "tm", "RColorBrewer", "dplyr")
  for(ith in pkgs){
    if(!require(ith, character.only = TRUE)){
      install.packages(ith)
      library(ith, character.only = TRUE)
    }
  }


  # 텍스트 데이터를 Corpus 객체로 변환
  text = text %>% unlist
  docs <- Corpus(VectorSource(text))

  # 텍스트 정제: 소문자 변환, 불용어 제거, 특수 문자 제거, 숫자 제거
  docs <- suppressWarnings(tm_map(docs, content_transformer(tolower)))
  docs <- suppressWarnings(tm_map(docs, removePunctuation))
  docs <- suppressWarnings(tm_map(docs, removeNumbers))
  docs <- suppressWarnings(tm_map(docs, removeWords, stopwords("english")))

  # TermDocumentMatrix 생성
  tdm <- TermDocumentMatrix(docs)

  # 행렬로 변환하고 빈도수 계산
  m <- as.matrix(tdm)
  wordFreqs <- sort(rowSums(m), decreasing=TRUE)



  if(is.null(path_save) && is.null(filename)){
    # 워드 클라우드 생성
    wordcloud(names(wordFreqs), wordFreqs, min.freq=1, max.words=200,
              random.order=FALSE, scale=c(3, 0.5), rot.per=0.1,
              colors=brewer.pal(8, "Dark2"))
  }else{
    # 파일로 저장
    png(file=paste0(path_save, "/", filename, ".png"), width=800, height=600, res=120)

    # 워드 클라우드 생성
    wordcloud(names(wordFreqs), wordFreqs, min.freq=1,
              max.words=100, random.order=FALSE,
              colors=brewer.pal(8, "Dark2"))

    # warning 발생하는 경우
    # wordcloud(names(wordFreqs), wordFreqs, min.freq=2, max.words=100,
    #           random.order=FALSE, scale=c(3, 0.5), rot.per=0.1,
    #           colors=brewer.pal(8, "Dark2"))

    # 그래픽 장치 닫기
    dev.off()
  }

}
