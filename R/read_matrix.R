
read_matrix<-function(filename='shared/macierze_analiz.xlsx', dt_structure=NULL)
{
  if(is.null(dt_structure)){
    stop("dt_structure is a compulsory argument")
  }
  wb <- xlsx::loadWorkbook(filename)
  sheets <- xlsx::getSheets(wb)
  tododf <- NULL
  for (i in seq_along(sheets))
  {
    sheet <- sheets[[i]]
    sheetname <- names(sheets)[[i]]
    ret <- read_sheet(sheet = sheet, sheetname = sheetname,  dt_structure=dt_structure)
    tododf <- rbind(ret, tododf)
  }
  # fn<-Vectorize(make_cell_hash)
  # hashes <- fn(filter=tododf$filter, indepvar = tododf$indepvar,
  #              depvar = tododf$depvar, groupby = tododf$groupvar,
  #              indepvartype = tododf$indepvartype, depvartype = tododf$depvartype)
  # return(cbind(tododf, hash = hashes))

  return(tododf)
}


read_sheet<-function(sheet, sheetname, dt_structure)
{
  all_col_names<-dt_structure$colname
  #The recognize_sheet_format reads the format and parses all default dictionaries
  format<-recognize_sheet_format(sheet = sheet)

  #  macierz <-read_xlsx_cells(sheet, rows = 5:rowcount, cols=4:colcount)
  macierz<-format$value_matrix[-1,-1]

  rekordy <- which((macierz==1), arr.ind=TRUE)

  #  macierz_kom <-read_xlsx_cells(sheet, rows = 5:rowcount, cols=4:colcount, what_to_get = 'comments')
  macierz_kom<-format$value_comments_matrix[-1,-1]


  #  col_names = read_xlsx_cells(sheet, rows=4, cols=4:colcount, emptyval='')
  #  col_filters = read_xlsx_cells(sheet, rows=3,cols=4:colcount, emptyval='')
  #  col_gr = read_xlsx_cells(sheet, rows=2,cols=4:colcount, emptyval='')
  #  row_names = read_xlsx_cells(sheet, rows=5:rowcount,cols=3, emptyval='')
  #  row_filters = read_xlsx_cells(sheet, rows=5:rowcount,cols=2, emptyval='')
  #  row_gr = read_xlsx_cells(sheet, rows=5:rowcount,cols=1, emptyval='')
  i=1


  dict<-rep(list(list()), nrow(rekordy))
  recnr<-1
  #  browser()

  for (i in seq_along(dict)){
#    cat(paste0(recnr,'\n'))
#    if(recnr==73) browser()
    row_nr <- rekordy[i,1]
    rec<-join_dicts(general_dict = format$global_properties, specific_dict =  format$row_properties[[row_nr]], conflict_action = 'ignore')
    col_nr <- rekordy[i,2]
    rec<-join_dicts(general_dict = rec, specific_dict = format$column_properties[[col_nr]], conflict_action = 'ignore')

    comments <- get_comment_info(macierz_kom = macierz_kom, row = row_nr, col = col_nr)
    rec<-join_dicts(general_dict = rec, specific_dict = comments, conflict_action = 'ignore')

    if(!is.na(rec$indepvar) && !is.na(rec$depvar)) {
      prop_iv<-expand_varnames(rec$indepvar, all_col_names)
      prop_dv<-expand_varnames(rec$depvar, all_col_names)

      prop_filter<-trimws(unlist(stringr::str_split(rec$filter, pattern = stringr::fixed(';'))))

      prop_gr <- trimws(unlist(stringr::str_split(rec$groupvar, pattern = stringr::fixed(';'))))
      prop_gr <- as.character(unlist(expand_varnames(prop_gr, all_col_names)))


      if(length(prop_filter)==0) prop_filter<-''
      if(length(prop_gr)==0) prop_gr<-''
      subrecords<-do.call(expand.grid, list(filter=prop_filter, groupvar=prop_gr, indepvar=prop_iv, depvar=prop_dv))

      for(j in seq(nrow(subrecords))) {
        rec_tmp<-rec
        rec_tmp$filter<-subrecords$filter[[j]]
        rec_tmp$groupvar<-subrecords$groupvar[[j]]
        rec_tmp$indepvar<-subrecords$indepvar[[j]]
        rec_tmp$depvar<-subrecords$depvar[[j]]
        dict[[recnr]]<-rec_tmp
        recnr<-recnr+1
      }

    }

  }

  all_names<-reduce(dict, function(x1,x2) unique(c(x1,names(x2))), .init=names(dict[[1]]))

  tododf<-data.table(prefix=rep(sheet$getSheetName(), length(dict)))
  for(myname in all_names) {
    vec<-rep('', length(dict))
    vec<-purrr::map_chr(dict, ~if(myname %in% names(.)) as.character(.[[myname]]) else NA_character_)
    tododf[,(myname):=vec]
  }

  dt_structure_clone<-data.table::copy(dt_structure)
  colnames(dt_structure_clone)<-paste0('depvar.', colnames(dt_structure))
  tododf<-dplyr::left_join(x=tododf,y=dt_structure_clone, by=c('depvar'='depvar.colname'), suffix=c('', 'depvar.'))
  dt_structure_clone<-data.table::copy(dt_structure)
  colnames(dt_structure_clone)<-paste0('indepvar.', colnames(dt_structure))
  tododf<-dplyr::left_join(x=tododf,y=dt_structure_clone, by=c('indepvar'='indepvar.colname'), suffix=c('', 'indepvar.'))

  return(tododf)
}

known_keys<-c('indepvar', 'depvar', 'groupvar', 'filter')

get_comment_info<-function(macierz_kom, row, col) {
  ytxt <- macierz_kom[row, col]
  if(!is.na(ytxt)) {
    ans<-tryCatch(
      yaml::yaml.load(ytxt),
      error=function(e) e
    )
    if('error' %in% class(ans)) {
      warning(paste0('Cell comment in row ',row, ', col ', col,
                     'is not proper yaml: "', ytxt, '". Ignoring.'))
    } else {

    }
    # if (length(setdiff(names(ans), as.character(known_keys)))>0) {
    #   warning(paste0('In cell at row ',row, ', col ', col, ' there are keys in comment, that are unknown: ',
    #                  paste0(setdiff(names(ans), as.character(known_keys)), collapse = ', ')))
    # }
    return(ans)
  } else {
    return(NULL)
  }
}

expand_varnames<-function(varstring, col_names) {
  row_names <- trimws(unlist(stringr::str_split(varstring, pattern = stringr::fixed(';'))))
  expand_varnames_prv <- Vectorize(expand_varname_prv, vectorize.args = 'varname')
  row_names <- as.character(unlist(expand_varnames_prv(row_names, col_names)))
  return(unique(row_names))
}
