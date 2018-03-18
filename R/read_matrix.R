#This function also makes a side-effect for the aggregates - it load the input variables' metadata
read_matrix<-function(filename='shared/macierze_analiz.xlsx', dt_structure=NULL, aggregate_types=list())
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
    ret <- read_sheet(sheet = sheet, sheetname = sheetname,  dt_structure=dt_structure, aggregate_types=aggregate_types)
    tododf <- rbind(ret, tododf)
  }


  tododf<-as.data.table(tododf)
  for(i in seq_len(ncol(tododf))) {
    varname<-colnames(tododf)[[i]]
    var<-tododf[[i]]
    if('factor' %in% class(var)) {
      tododf[,(varname):=as.character(var)]
    }
  }

  props<-getOption('relationshipMatrix.chunkdf_properties')
  prefixes<-c(getOption('relationshipMatrix.property_depvar_prefix'),
              getOption('relationshipMatrix.property_indepvar_prefix'),
              getOption('relationshipMatrix.property_groupvar_prefix'))
  varname<-paste0(prefixes[[1]], 'label')
  filter<-which(tododf[[varname]]=='')
  if(length(filter)>0) {
    data.table::set(tododf, filter, varname, tododf[[props$depvar]][filter])
  }
  varname<-paste0(prefixes[[2]], 'label')
  filter<-which(tododf[[varname]]=='')
  if(length(filter)>0) {
    data.table::set(tododf, filter, varname, tododf[[props$indepvar]][filter])
  }
  varname<-paste0(prefixes[[3]], 'label')
  filter<-which(tododf[[varname]]=='')
  if(length(filter)>0) {
    data.table::set(tododf, filter, varname, tododf[[props$groupvar]][filter])
  }

  for(a in aggregate_types) {
    a$.__enclos_env__$private$discover_metadata(dt_structure)
  }

  tododf$cellnr<-seq_len(nrow(tododf))

  return(tododf)
}


read_sheet<-function(sheet, sheetname, dt_structure, aggregate_types=list())
{
  dt_structure<-cbind(dt_structure, is_aggregate=FALSE)
  if(length(aggregate_types)>0) {
    validate_aggregate_types(dt_structure, aggregate_types)
    aggreg_df<-make_aggregateTypesDF(aggregate_types)
    aggreg_prop_name<-getOption('relationshipMatrix.is_aggregate')
    if(nrow(aggreg_df)>0) {
      aggreg_df[[aggreg_prop_name]]<-TRUE
    } else {
      aggreg_df[[aggreg_prop_name]]<-logical(0)
    }


    for(cn in colnames(dt_structure)) {
      if(! cn %in% colnames(aggreg_df)) {
        t<-dt_structure[[cn]]
        t[[1]]<-NA
        aggreg_df[[cn]]<-rep(t[[1]], nrow(aggreg_df))
      }
    }
    dt_structure<-rbind(dt_structure, aggreg_df)
  }

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
#    if(recnr==692) browser()
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
      subrecords<-do.call(expand.grid, list(filter=prop_filter, groupvar=prop_gr, indepvar=prop_iv, depvar=prop_dv, stringsAsFactors=FALSE))

      for(j in seq(nrow(subrecords))) {
        rec_tmp<-rec
        rec_tmp$filter<-subrecords$filter[[j]]
        rec_tmp$groupvar<-subrecords$groupvar[[j]]
        rec_tmp$indepvar<-subrecords$indepvar[[j]]
        rec_tmp$depvar<-subrecords$depvar[[j]]
        rec_tmp$colnr<-col_nr
        rec_tmp$rownr<-row_nr
        dict[[recnr]]<-rec_tmp
        recnr<-recnr+1
      }

    }
  }

  #all_names<-reduce(dict, function(x1,x2) unique(c(x1,names(x2))), .init=names(dict[[1]]))



#  browser()
  tododf<-objectstorage::lists_to_df(dict, list_columns = c('prefix', 'prefix1', 'prefix2', 'prefix3'))

  tododf[tododf$indepvar=='NULL','indepvar']<-''
  tododf[tododf$depvar=='NULL','depvar']<-''
  tododf[tododf$groupvar=='NULL','groupvar']<-''
  tododf[tododf$filter=='NULL','filter']<-''
  tododf<-tibble::as_tibble(tododf)

  dt_structure_clone<-data.table::copy(dt_structure)
  depvar_prefix<-getOption('relationshipMatrix.property_depvar_prefix')
  colnames(dt_structure_clone)<-paste0(depvar_prefix, colnames(dt_structure))
  tododf<-dplyr::left_join(x=tododf,y=dt_structure_clone, by=c('depvar'=paste0(depvar_prefix, 'colname')), suffix=c('', depvar_prefix))

  dt_structure_clone<-data.table::copy(dt_structure)
  indepvar_prefix<-getOption('relationshipMatrix.property_indepvar_prefix')
  colnames(dt_structure_clone)<-paste0(indepvar_prefix, colnames(dt_structure))
  tododf<-dplyr::left_join(x=tododf,y=dt_structure_clone, by=c('indepvar'=paste0(indepvar_prefix, 'colname')), suffix=c('', indepvar_prefix))

  dt_structure_clone<-data.table::copy(dt_structure)
  groupvar_prefix<-getOption('relationshipMatrix.property_groupvar_prefix')
  colnames(dt_structure_clone)<-paste0(groupvar_prefix, colnames(dt_structure))
  tododf<-dplyr::left_join(x=tododf,y=dt_structure_clone, by=c('groupvar'=paste0(groupvar_prefix, 'colname')), suffix=c('', groupvar_prefix))

  #browser()
  return(tododf)
}

known_keys<-as.character(getOption('relationshipMatrix.chunkdf_properties'))

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

read_chapters<-function(yaml_path) {
  #TODO. W przyszłości stąd będą odczytywane właściwości rozdziałów
  browser()
  yaml::read_yaml(yaml_path)
  props<-c('caption', 'name', 'chart_folder', 'cache_folder')
  for(i in seq_along(yaml)) {
    depwalker:::test_for_elements(colnames = names(yaml[[i]]), required_items = '', optional_items = 'props')
  }
  read_chapters<-1

}

