Export___xlsx___Highlighting = function(data.df,
                                        colors.list, # colors.list = "red"
                                        which_cols.list, # which_cols.list = 컬럼 위치
                                        coloring_index.list, # coloring_index.list : i번째 열에서 어떤 행에 coloring을 할 것인가
                                        save.path, # save.path = "C:/Users/IDO/OneDrive/github/Rpkgs/StatAnalysis"
                                        file.name,  # file.name = "test"
                                        sheet.name){

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
  ### Create a new workbook
  wb = openxlsx::createWorkbook()

  ### Add a worksheet
  openxlsx::addWorksheet(wb, sheetName = sheet.name)

  ### write dataset
  openxlsx::writeData(wb, sheet=sheet.name, x=data.df, na.string = "")



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
  cat("\n", crayon::blue("Writing an xlsx is done!"),"\n")
}


