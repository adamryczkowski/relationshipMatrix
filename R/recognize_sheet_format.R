recognize_sheet_format<-function(sheet) {
  #browser()
  ans=list()
  xls_range<-sheet_used_range(sheet)
  all_values<-read_xlsx_cells(sheet, seq(xls_range[['MaxRow']]), seq(xls_range[['MaxCol']]))

  colnr<-1
  dict<-list()

  #Parse the sheet to get box for global properties, column properties and row properties

  #Get the global properties box
  repeat {
    key<-all_values[1,colnr]
    if(is.na(key)) {
      colnr<-colnr-1
      break
    }
    value<-all_values[2,colnr]
    match<-stringr::str_match(string = key, pattern = stringr::regex('^(.*):$'))
    if(is.na(match[[1]])) {
      break
    }
    colnr<-colnr+1
  }
  ans$global_properties_box<- list(left=1, right=colnr, top=1, bottom=2)
  ans$global_properties<-read_global_properties(all_values = take_box(all_values, ans$global_properties_box))
  #Get the column properties box
  rownr<-2
  colnr<-colnr+1
  #browser()
  repeat {
    key<-all_values[rownr,colnr]
    if(is.na(key)) {
      rownr<-rownr-1
      break
    }
    match<-stringr::str_match(string = key, pattern = stringr::regex('^(.*):$'))
    if(is.na(match[[1]])) {
      break
    }
    rownr<-rownr+1
  }
  ans$column_properties_box<-list(left=colnr, right = xls_range[['MaxCol']], top=2, bottom=rownr)
  tmp_val<-take_box(all_values, ans$column_properties_box)
  ans$column_properties<-read_properties_many(keys=tmp_val[,1],
                                              values_list = split_matrix_into_list(tmp_val[,-1], flag_vertical = TRUE),
                                              defaults = ans$global_properties)
  iv<-take_box(all_values, list(left=ans$column_properties_box$left+1,
                                right=ans$column_properties_box$right,
                                top=ans$column_properties_box$bottom+1,
                                bottom=ans$column_properties_box$bottom+1))
  if(length(iv)!=length(ans$column_properties)) {
    browser() #ERROR
  }
  for(i in seq(length(iv))) {
    ans$column_properties[[i]]<-c(ans$column_properties[[i]], list(indepvar=iv[[i]]))
  }


  #Get the row properties box
  ans$row_properties_box<-list(left=ans$global_properties_box$left, right=ans$global_properties_box$right, top=rownr+1, bottom=xls_range[['MaxRow']])
  tmp_val<-take_box(all_values, ans$row_properties_box)
  dv<-take_box(all_values, list(left=ans$row_properties_box$right+1,
                                right=ans$row_properties_box$right+1,
                                top=ans$row_properties_box$top+1,
                                bottom=ans$row_properties_box$bottom))
  ans$row_properties<-read_properties_many(keys=tmp_val[1,],
                                           values_list = split_matrix_into_list(tmp_val[-1,], flag_vertical = FALSE),
                                           defaults = ans$global_properties)
  if(length(dv)!=length(ans$row_properties)) {
    browser() #ERROR
  }
  for(i in seq(length(dv))) {
    ans$row_properties[[i]]<-c(ans$row_properties[[i]], list(depvar=dv[[i]]))
  }


  ans$value_matrix_box<-list(left=ans$row_properties_box$right+1, right=xls_range[['MaxCol']],
                             top=ans$column_properties_box$bottom+1, bottom=xls_range[['MaxRow']])
  ans$value_matrix<-take_box(all_values, ans$value_matrix_box)
  #  macierz_kom <-read_xlsx_cells(sheet, rows = 5:rowcount, cols=4:colcount, what_to_get = 'comments')

  ans$value_comments_matrix<-read_xlsx_cells(sheet,
                                             rows = seq(ans$value_matrix_box$top, ans$value_matrix_box$bottom),
                                             cols = seq(ans$value_matrix_box$left, ans$value_matrix_box$right),
                                             what_to_get = 'comments')
  return(ans)
}

read_global_properties<-function(all_values) {
  keys<-all_values[1,]
  values<-all_values[2,]
  ans<-read_properties(keys = keys, values = values)
  return(ans)
}

#Ta sama funkcja. co read_properties, ale czyta więcej niż jeden wektor values
read_properties_many<-function(keys, values_list, defaults=list()) {
  ans<-rep(list(list()), length(values_list))
  for(i in seq_along(values_list)) {
    ans[[i]]<-read_properties(keys=keys, values=values_list[[i]], defaults = defaults)
  }
  return(ans)
}

read_properties<-function(keys, values, defaults=list()) {
  dict<-list()
  for(i in seq_along(values))
  {
    key<-keys[[i]]
    value<-values[[i]]
    if(!is.na(value)) {
      if(!is.na(key)) {
        match<-stringr::str_match(string = key, pattern = stringr::regex('^(.*):$'))
        if(is.na(match[[1]])) {
          dict<-parse_comment(value, dict, 'error')
        } else {
          dict[[match[[2]] ]]<-value
        }
      } else {
        if(!is.na(value)) {
          dict<-parse_comment(value, dict, 'error')
        }
      }
    }
  }
  if(length(defaults)>0) {
    dict<-join_dicts(general_dict = defaults, specific_dict = dict, conflict_action = 'ignore')
  }
  return(dict)
}

parse_comment<-function(comment, existing_dict=dict(), conflict_action='warning') {
  if(is.na(comment)) {
    return(existing_dict)
  }
  if(stringr::str_trim(comment, side='both')=='') {
    return(existing_dict)
  }
  ans<-tryCatch(
    yaml::yaml.load(comment),
    error=function(e) e
  )
  if('error' %in% class(ans)) {
    stop(paste0("The comment ", comment, ' is not in the proper YAML'))
  }
  ans<-join_dicts(existing_dict, ans, conflict_action = conflict_action)
}

join_dicts<-function(general_dict, specific_dict, conflict_action=c('error', 'warning', 'ignore')) {
  if(length(conflict_action)>1) {
    stop("conflict_action has to be specified and needs to be a char vector of length 1")
  }
  if(!conflict_action %in% c('error', 'warning', 'ignore')) {
    stop(paste0("Unrecognized conflict action ", conflict_action, ". Possible values are error, warning and ignore"))
  }

  common_keys<-intersect(names(specific_dict),names(general_dict))
  if (length(common_keys)>0) {
    mismatches<-which(as.character(specific_dict[common_keys])!=as.character(general_dict[common_keys]))
    if(length(mismatches)>0) {
      if(conflict_action!='ignore') {
        df<-tibble(key=common_keys[mismatches],
                   default_value=as.character(general_dict[common_keys[mismatches] ]),
                   new_value=as.character(specific_dict[common_keys[mismatches] ]))
        setattr(df$default_value, 'label', 'stara wartość')
        setattr(df$new_value, 'label', 'nowa wartość')
        msg<-paste0("Nadpisane wartości atrybutów: ", danesurowe::format_item_list(df = df, txt_attribute_separator_last = '->'))

      }
      if(conflict_action=='warning') {
        warning(msg)
      } else if (conflict_action=='error') {
        stop(msg)
      }
      general_dict<-general_dict[setdiff(names(general_dict), common_keys)]
    }
  }
  return(c(general_dict, specific_dict))
}

take_box<-function(m, box) {
  return(m[seq(box$top, box$bottom), seq(box$left, box$right)])
}

expand_varname_prv<-function(varname, col_names) {
  if (stringr::str_detect(varname ,pattern='\\*')) {
    pattern <- utils::glob2rx(varname)
    lista <-col_names[stringr::str_detect(col_names, pattern)]
    return(lista)
  }
  return(varname)
}
