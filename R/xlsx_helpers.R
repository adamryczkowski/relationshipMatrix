sheet_used_range<-function(sheet) {
  MaxRow<-sheet$getLastRowNum()
  MaxCol<-1
  #browser()
  for(i in seq(0, MaxRow)) {
    #if(i==15) browser()
    row<-sheet$getRow(as.integer(i))
    if(is.null(row)) break
    MaxCol<-max(MaxCol, row$getLastCellNum())
  }
  return(c(MaxRow=MaxRow+1, MaxCol=MaxCol))
}

read_xlsx_cells<-function(sheet, rows, cols, emptyval=NA, what_to_get=c('values', 'formulas', 'comments')) {
  if (missing(what_to_get)) {
    what_to_get<-'values'
  }
  if (is.null(what_to_get)) {
    what_to_get<-'values'
  }
  flag_get_formulas <- what_to_get=='formulas'
  flag_get_comments <- what_to_get=='comments'

  rw <- xlsx::getRows(sheet, rows)
  cl <- xlsx::getCells(rw, cols)
  ans<-matrix(data=emptyval, nrow = length(rows), ncol = length(cols) )
  rownames(ans)<-rows
  colnames(ans)<-cols
  for(i in seq_along(cl)) {
    cell_name<-names(cl)[[i]]
    a<-as.integer(stringr::str_split(cell_name, '\\.', simplify = TRUE))
    row<-match(a[[1]], rows)
    col<-match(a[[2]], cols)
    if (flag_get_comments) {
      cm<-xlsx::getCellComment(cell = cl[[cell_name]])
      if(!is.null(cm)){
        ans[[row, col]]<-cm$getString()$toString()
      }
    } else {
      ans[[row, col]]<-xlsx::getCellValue(cell = cl[[cell_name]], keepFormulas = flag_get_formulas)
    }
  }
  return(ans)
}

split_matrix_into_list<-function(m, flag_vertical) {
  ans<-if (flag_vertical) {
    plyr::alply(m, 2, identity)
  } else {
    plyr::alply(m, 1, identity)
  }
  attributes(ans)<-NULL
  return(ans)
}
