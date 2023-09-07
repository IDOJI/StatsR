Export___xlsx___Highlighting = function(data.df,
                                        colors.list, # colors.list = "red"
                                        which_cols.list, # which_cols.list = 컬럼 위치
                                        coloring_index.list, # coloring_index.list : i번째 열에서 어떤 행에 coloring을 할 것인가
                                        save.path, # save.path = "C:/Users/IDO/OneDrive/github/Rpkgs/StatAnalysis"
                                        file.name,  # file.name = "test"
                                        sheet.name){
  #===========================================================================
  # replace NA with ""
  #===========================================================================
  data.df[is.na(data.df)] = ""





  #===========================================================================
  # length of colors
  #===========================================================================
  if(length(colors.list)==1 && !is.list(colors.list) && is.vector(colors.list)){
    colors.list = rep(colors.list, length(which_cols.list)) %>% as.list
  }



  #===========================================================================
  # cols as num
  #===========================================================================
  if(which_cols.list %>% unlist %>% class == "character"){
    cols_ind = list()
    for(j in 1:length(which_cols.list)){
      cols_ind[[j]] = which(names(data.df)==which_cols.list[[j]])
    }
    which_cols.list = cols_ind
  }



  #===========================================================================
  # Creating workbook
  #===========================================================================
  install_package("openxlsx")

  ### Create a new workbook
  wb = openxlsx::createWorkbook()

  ### Add a worksheet
  openxlsx::addWorksheet(wb, sheetName = sheet.name)

  ### write dataset
  openxlsx::writeData(wb, sheet=sheet.name, x=data.df, na.string = "") # error 발생시 df의 원소들을 character로 바꿔볼 것


  #===========================================================================
  # Highlighting
  #===========================================================================
  for(i in 1:length(which_cols.list)){
    #i=1
    ### define style
    style = openxlsx::createStyle(fgFill=colors.list[[i]])

    ### log2FoldChange
    y = which_cols.list[[i]]
    for(j in 1:length(coloring_index.list)){
      x = coloring_index.list[[j]]
      # +1 for header line
      openxlsx::addStyle(wb, sheet=sheet.name, style=style, rows=x+1, cols=y, gridExpand=TRUE)
    }
    print(i)
  }


  ### Save Results
  save.path = path_tail_slash(save.path)
  openxlsx::saveWorkbook(wb, paste0(save.path, file.name, ".xlsx"), overwrite=TRUE)
  #To save a data frame to an Excel file without "NA" values using the openxlsx package,
  # you'll first need to replace the NA values in your data frame with an empty string "".
  # After that, you can utilize the createWorkbook() and addWorksheet() functions from openxlsx to save the modified data frame as an Excel file.
  cat("\n", crayon::blue("Writing an xlsx is done!"),"\n")
}


