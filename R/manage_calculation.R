#TODO 1. Połączyć projekt z depwalkerem. Niech depwalker zajmie się cachowaniem
#TODO 2. Wprowadzam oddzielną funkcję na poziomie dispatchera, która oblicza statystyki i oddzielną dla decyzji, jakie raporty mają być
#policzone. Chodzi o to, aby zmiany w parametrach liczonych raportów nie wymagały ponownego liczenia statystyk.

#browser()
#Potrzebuję połączyć ten plik z depwalker i użyć depwalkera jako narzędzia cachującego.
#
#Cały proces liczenia komórki na 4 wywołania depwalkera:
#
#Najpierw wywoływany jest dispatcher w trybie discovery i tworzona jest lista istotnych dla niego parametrów.
#Z tych parametrów, parametrów ogólnych oraz z df tworzony jest chunkdf przy pomocy Wywołania 1:
#
#Wywołanie 1. calculate_chunkdf(df, rekord, filter)
#Funkcja liczy kawałek df zgodny z filtrem i istotnymi dla problemu zmiennymi.
#
#Wynik tego wywołania jest używany jako argument wywołania 2:
#Wywołanie 2. stats_dispatcher(chunkdf, properties) (stats_dispatcher to jest funkcja, którą podał użytkownik i która liczy statystyki)
#Ta funkcja tworzy wszystkie statystyczne obliczenia i zapisuje je w jakiejś swojej formie, specyficznej dla siebie
#i nie interpretownej przez relationshipMatrix
#
#Potem wywoływana jest funkcja
#Wywołanie 3. report_dispatcher(statistics, properties) (report_dispatcher to jest funkcja, którą podał użytkownik i która tworzy listę raportów.
#                                                        statistics to jest wynik pracy stats_dispatchera przekazany mam nadzieję byref.
#report_dispatcher zwraca tabelę z funkcjami oraz listą funkcji tworzącymi kolejne kroki raportu wraz z ich argumentami - funkcja rysująca wykres,
#           - funkcja tworząca tabelę
#           - funkcja dodająca jakiś nietrywialny tekst.
#Zwracana tabela zawiera pola, które będą dostępne dla funkcji w tym samym mechaniźmie, co report_dispatcher miał dostęp do properties.
#
#Wywołanie 4. report_chunk(statistics, properties) (argumenty o tym samym znaczeniu co w report_dispatcher, tyle że properties wzbogacone
#                                                   o wartości dodane przez report_dispatcher)
#Funkcja zwraca obiekt, który zawiera 100% informacji, które relationshipMatrix potrzebuje, aby wyrenderować jako raport.
#Jest to obiekt, który składa się z listy elementów dodawanych jeden po drugim (elementy to a) wykres, b) tabela, c) tekst) oraz
#metadane służące do wzbogacenia raportu, takie jak info o użytych metodach statystycznych.




#' This function generates the whole of the analysis, and it is the main entry point to the whole library.
#'
#' It is responsible for:
#'
#' \itemize{
#' \item generating the structure (sections) of the resulting document,
#' \item calling the \code{\function{do_cell}} to get the results of each cell (possibly by allocating computations to the
#'       remote nodes),
#' \item rendering the document
#' }
#'
#'
render_matrix<-function(cellsdf, author='Adam Ryczkowski', format='docx', title="Analiza statystyczna",
                        stats_dispatchers, report_dispatchers=list(), report_functions=list(),
                        aggregates=list(), filters=list(), df_task,
                        chart_foldername='chapter', cache_foldername='cache', flag_add_chapter_for_each_cell=TRUE
                        ){
  # Algorithm:
  # 1. Convert prefix1 from NA to '', like all other prefixes

  cellsdf<-enhance_tododf(cellsdf, filters)
#  browser()

  title_property<-getOption('relationshipMatrix.property_cell_title')
  prefix_column<-getOption('relationshipMatrix.prefix')
  dv_prefix<-getOption('relationshipMatrix.property_depvar_prefix')
  iv_prefix<-getOption('relationshipMatrix.property_indepvar_prefix')
  gv_prefix<-getOption('relationshipMatrix.property_groupvar_prefix')


  if(! paste0(prefix_column, 1) %in% colnames(cellsdf) ) {
    cellsdf[[paste0(prefix_column, 1)]]<-NA_character_
  }
  if(prefix_column %in% colnames(cellsdf)) {
    if(! paste0(prefix_column, 2) %in% colnames(cellsdf) ) {
      data.table::setnames(cellsdf, prefix_column, paste0(prefix_column, 2))
    } else {
      browser()
    }
  }
  if(! paste0(prefix_column, 2) %in% colnames(cellsdf) ) {
    cellsdf[[paste0(prefix_column, 2)]]<-NA_character_
  }
  if(! paste0(prefix_column, 3) %in% colnames(cellsdf) ) {
    cellsdf[[paste0(prefix_column, 3)]]<-NA_character_
  }



  # 2. Gather all chapters
  chapters<-new.env(parent=emptyenv())
  cellsdf$.chapter<-rep(list(list()), nrow(cellsdf))
  for(i in seq_len(nrow(cellsdf))) {
    paths<-c(cellsdf[[paste0(prefix_column, 1)]][[i]],
      cellsdf[[paste0(prefix_column, 2)]][[i]],
      cellsdf[[paste0(prefix_column, 3)]][[i]])

#    if(i==26) browser()
    base_chapter<-chapters
    chapter_path<-character(0)
    for(path in paths) {
      if(length(path)>1 || !is.na(path)) {
        if(length(path)==1) {
          ch_name<-path
          ch_prio<-NA_real_
        } else if (length(path)==2) {
          ch_name<-names(path)[[2]]
          ch_prio<-names(path)[[1]]
        } else {
          browser()
        }
        if(is.na(ch_name)) {
          browser()
        }
        ch_name<-stringr::str_replace(ch_name, pattern=stringr::fixed('<depvar>'), cellsdf[[paste0(dv_prefix, 'label')]][[i]])
        ch_name<-stringr::str_replace(ch_name, pattern=stringr::fixed('<indepvar>'), cellsdf[[paste0(iv_prefix, 'label')]][[i]])
        ch_name<-stringr::str_replace(ch_name, pattern=stringr::fixed('<groupvar>'), cellsdf[[paste0(gv_prefix, 'label')]][[i]])
        ch_name<-stringr::str_replace(ch_name, pattern=stringr::fixed('<filter>'), cellsdf$filter.label[[i]])

        if(ch_name %in% names(base_chapter)) {
          tmp_chapter<-base_chapter[[ch_name]]
          if(!is.na(ch_prio)) {
            if(is.na(tmp_chapter$priority)) {
              tmp_chapter$priority<-ch_prio
            } else if(tmp_chapter$priority != ch_prio) {
              browser() #We override the priority of the chapter
            }
            base_chapter[[ch_name]]<-tmp_chapter
          }
        } else {
          tmp_chapter<-list(priority=ch_prio,
                            env=new.env(parent = emptyenv()))
          base_chapter[[ch_name]]<-tmp_chapter
        }
        base_chapter<-tmp_chapter$env
        chapter_path<-c(chapter_path, ch_name)
      }
    }
    cell_title<-cellsdf[[title_property]][[i]]
    if(cell_title!='' && flag_add_chapter_for_each_cell) {
      chapter_path<-c(chapter_path, cell_title)
      tmp_chapter<-list(priority=NA_real_,
                        env=new.env(parent = emptyenv()))
      base_chapter[[cell_title]]<-tmp_chapter

    }
    cellsdf[['.chapter']][[i]]<-list(chapter_path)
  }
  #browser()
  #3. Now generate the document core chapters based on this layout
  doc<-doc_Document$new(chart_foldername = chart_foldername, cache_foldername = cache_foldername,
                        author = author, format = format, title = title)
  insert_chapters<-function(container, env_chapters, tags=character(0)) {
    df<-tibble::tibble(priorities=as.numeric(purrr::map_chr(as.list(env_chapters), 'priority')),
               names=names(env_chapters))
    myorder<-order(df$priorities) #For stable sort
    df_ref<-df[myorder,] #dplyr::arrange(df, priorities, names)


    p<-purrr::map_int(df_ref$names, ~which(df$names %in% .))

    for(i in p) {
      rec<-as.list(df[i,])
      n<-rec$names
      obj<-env_chapters[[n]]
      if('tags' %in% names(obj)) {
        tags<-obj$tags
      } else {
        tabs<-character(0)
      }
      if('tags' %in% names(obj)) {
        tags<-obj$tags
      } else {
        tabs<-character(0)
      }
      if('chart_foldername' %in% names(obj)) {
        chart_foldername<-obj$tags
      } else {
        chart_foldername<-''
      }
      if('cache_foldername' %in% names(obj)) {
        cache_foldername<-obj$tags
      } else {
        cache_foldername<-''
      }
      ch<-container$insert_section(n, tags=tags, chart_foldername=chart_foldername, cache_foldername=cache_foldername)
      insert_chapters(ch, obj$env)
    }
    env_chapters$doc<-container
  }
  insert_chapters(doc, chapters)

  #4. Iterate over render_matrix and build the anaylisis cell-by-cell. Each retreived piece is put into the corresponding chapter
  for(i in seq_len(nrow(cellsdf))) {
    cat(paste0('i=',i,'\n'))

    chapter_path<-cellsdf[['.chapter']][[i]]


    raw_chapters<-do_cell(cellsdf = cellsdf, cellnr = i, stats_dispatchers = stats_dispatchers, report_dispatchers = report_dispatchers,
                         report_functions = report_functions, aggregates = aggregates, filters = filters, df_task = df_task,
                         chapter_path=chapter_path,
                         chart_foldername=chart_foldername, cache_foldername=cache_foldername)
    raw_chapters$set_property('cellnr', i, indepth_level=1)
    raw_chapters$set_property('rownr', cellsdf$rownr[[i]], indepth_level=1)
    raw_chapters$set_property('colnr', cellsdf$colnr[[i]], indepth_level=1)
    raw_chapters$set_property('dv', cellsdf$depvar[[i]], indepth_level=1)
    raw_chapters$set_property('iv', cellsdf$indepvar[[i]], indepth_level=1)
    raw_chapters$set_property('gv', cellsdf$groupvar[[i]], indepth_level=1)
    raw_chapters$set_property('f', cellsdf$filter.filterstring[[i]], indepth_level=1)
    raw_chapters$set_property('dispatcher', cellsdf$dispatcher[[i]], indepth_level=1)
#    browser()
    target_chapter<-doc$get_chapter_by_path(chapter_path)
    raw_chapters$insert_into(target_chapter)
  }



  return(doc)
}


enhance_tododf<-function(tododf, filters) {
  #browser()
  dv_prefix<-getOption('relationshipMatrix.property_depvar_prefix')
  iv_prefix<-getOption('relationshipMatrix.property_indepvar_prefix')
  gv_prefix<-getOption('relationshipMatrix.property_groupvar_prefix')
  props<-getOption('relationshipMatrix.chunkdf_properties')

  varname<-paste0(dv_prefix, 'label')
  src_varname<-props[['depvar']]
  filter<-is.na(tododf[[varname]])
  tododf[filter, (varname):=tododf[[src_varname]][filter] ]

  varname<-paste0(iv_prefix, 'label')
  src_varname<-props[['indepvar']]
  filter<-is.na(tododf[[varname]])
  tododf[filter, (varname):=tododf[[src_varname]][filter] ]

  varname<-paste0(gv_prefix, 'label')
  src_varname<-props[['groupvar']]
  filter<-is.na(tododf[[varname]])
  tododf[filter, (varname):=tododf[[src_varname]][filter] ]

  filter_prefix<-'filter.'
  filters_list<-purrr::map(seq_along(filters), ~list(filterstring=filters[[.]]$filterstring,
                                                     label=filters[[.]]$label,
                                                     name = names(filters)[[.]]))
  filters_df<-objectstorage::lists_to_df(filters_list)
  colnames(filters_df)<-paste0(filter_prefix, colnames(filters_df))
  tododf<-tibble::as_tibble(tododf)
  tododf<-dplyr::left_join(x=tododf,y=filters_df, by=c('filter'=paste0(filter_prefix, 'name')), suffix=c('', filter_prefix))

  #Add cell title (if missing)
  cell_title_name<-getOption('relationshipMatrix.property_cell_title')
  if(!cell_title_name %in% colnames(tododf)) {
    tododf[[cell_title_name]]<-NA_character_
  }

  tododf<-data.table(tododf)
  #Setting title for all cells that have missing title cell
  for(i in seq_len(nrow(tododf))) {
    if(is.na(tododf[[cell_title_name]][[i]])) {
      indepvarname<-tododf[[paste0(iv_prefix, 'label')]][[i]]
      depvarname<-tododf[[paste0(dv_prefix, 'label')]][[i]]
      if(tododf$language[[i]]=='PL') {
        tododf[i, (cell_title_name):=paste0(indepvarname, ' a ', depvarname)]
      } else {
        tododf[i, (cell_title_name):=paste0(indepvarname, ' vs ', depvarname)]
      }
    }
  }

  #browser()
  tododf[, (paste0(dv_prefix, 'f.o.b')):=guess_fob(tododf=tododf, prefix=dv_prefix)]
  tododf[, (paste0(iv_prefix, 'f.o.b')):=guess_fob(tododf=tododf, prefix=iv_prefix)]
  tododf[, (paste0(gv_prefix, 'f.o.b')):=guess_fob(tododf=tododf, prefix=gv_prefix)]

  return(tododf)
}

count_levels<-function(str) {
  l<-stringr::str_split(str, pattern=stringr::regex('(?<=\\");(?=([^=]+=))'))
  return(length(l[[1]]))
}

guess_fob_1<-function(tododf, i, prefix='.dv') {
  fob<-tododf[[paste0(prefix, 'f.o.b')]][[i]]
  if(!is.na(fob)) {
    return(fob)
  }

  count<-count_levels(tododf[[paste0(prefix, 'labels_string')]][[i]])
  if(count==2) {
    return(3)
  } else if (count<=1) {
    return(-1)
  }

  typ <- tododf[[paste0(prefix, 'vartype')]][[i]]
  if(typ %in% c('F', 'L')) {
    if(count==2) {
      fob<-3
      #        data.table::setattr(var, 'f.o.b', 3)
    } else {
      if (tododf[[paste0(prefix, 'limit_to_labels')]][[i]]) {
        cl<-stringr::str_split(tododf[[paste0(prefix, 'class')]][[i]], ',')
        if('ordered' %in% cl) {
          fob<-2
        } else {
          fob<-1
        }
      } else {
        fob<-2
      }
    }
  } else if (typ %in% c('I', 'N', 'D') ) {
    fob<-0
  } else if (typ == '0') {
    fob<-3
  } else if (typ == 'S') {
    fob<-NA
  } else {
    browser()
  }
  return(fob)
}

guess_fob<-function(tododf, prefix='.dv') {
  purrr::map_int(seq_len(nrow(tododf)), ~as.integer(guess_fob_1(tododf=tododf, i=., prefix=prefix )))
}

#' Generates pair of priority, path from strings with format \code{4:`Chapter name`}.
#' Also contains a fallback mode that simply accepts strings without priority:
#' \code{`Chapter name`}, or even without backticks: \code{Chapter name}
#' @example
#' str_path=c("1:`Ala ma kota`", "2:`Zosia-samosia`", "Taki sobie katalog", "`Inny katalog`", NA, '4:`Rozdział`/`podrozdział 1`', "`Rozdział`/`podrozdział 2`", "`Rozdział`/`podrozdział 1`/`sekcja 1`", "10:`Rozdział`/`podrozdział 1`/`sekcja 2`", "10:`Rozdział`/4:`podrozdział 1`/11:`sekcja 3`")
parse_path<-function(str_path) {
  paths<-stringr::str_split(str_path, pattern = stringr::regex('(?<=`)/(?=([^:]+:)?`)'))

  match<-purrr::map(paths, function(path) {
    #browser()
    if(length(path)==1 && is.na(path)) {
      return(list(priority=NA, path=NA))
    } else {
      match<-stringr::str_match(path, stringr::regex('^(([^:]+):)?`(.+)`$'))
      match[is.na(match[,4]),4]<-path[is.na(match[,4])]
      return(list(priority=match[,3], path=match[,4]))
    }
  })
  paths<-stringr::str_split(match[,4], pattern = stringr::regex('`/([^:]+:)?`'))
  return(list(priority=match[,3], path=paths))
}

split_paths<-function(str_path) {
  stringr::str_split(str_path, pattern = stringr::regex('(?<=`)/(?=([^:]+:)?`)'))
}

#' This function translates the call to the cell into a call to get the reports. It parses the arguments,
#' calls the dispatcher in the discovery mode and returns the task that actually gets the results
#'
#'
#' On entry it gets the cells_df - a tododf with the dispatcher, that contains all the
#' information needed to do a given analysis.
#'
#' \strong{Algorithm}
#'
#' First it tries to create the chunkdf - a little subset of the database that contains only the needed variables
#' and only the filtered cases. To get there, it first need to run the dispatcher function in the
#' "discovery mode". In this mode the \code{popertyAccessor} will not give access to the chunkdf (which is not
#' ready yet), but it will trigger the error, which will be catched by the \code{do_cell()}.
#'
#' Then \code{do_cell} will call \code{do_cell_with_chunkdf} to get the rest of the work done.
#'
#' This function will be memoized or turned into the task later on. Its responsibility will be to
#' create the actual result.
#'
#' @param cellsdf A \code{data.frame} that contains at least the following columns:
#' \describe{
#' \item{depvar}{Name of the dependent variable. It can be either a column of the input data.frame, or
#'               an aggegate name.}
#' \item{indepvar}{Name of the independent variable. It can be either a column of the input data.frame, or
#'                 an aggegate name.}
#' \item{dispatcher}{Name of the dispatcher to handle this particular cell. Dispatcher must be present in the
#'                   \code{dispatchers} argument.}
#' }
#' The following columns have reserved names:
#' \item{groupvar}{Name of the grouping variable. The grouping variable must be present in the input data.frame,
#'                 and it must be a labelled variable. Analysis will be made in groups defined by this variable.}
#' \item{filter}{Name of the filter to apply before doing the analysis. Without it, the analysis will be done on
#'               the whole dataset.}
#' @param stats_dispatchers Named list of functions that build statistics for the cells. Missing function can be inserted as NA,
#'                          in this case the statistics itself will be an empty list.
#' @param report_dispatchers Named list of functions that list functions that do consecutive pieces of reports for the cells.
#'                           Missing function is an error/warning.
#' @param report_functions Named list of functions that produce a given piece of report. Missing function will be assumed to be
#'                         produced by the report_dispatcher. In case it also fails, it will generate an error.
#' @param aggregates Named list of objects of class \code{AggregateType} that define aggregate variables, i.e. variables
#'                   which are defined only on subsets of the database, in contrast to ordinary variables, that are defined
#'                   for each case separately.
#' @param filters Named list of objects of class \code{Filter}.
#' @param cellnr Which record of cellsdf to process. Must be a single integer.
#' @param df_task Either a dabase object or a depwalker::task.
#'
#' @return List of results gathered by running the cell
#'
do_cell<-function(cellsdf, stats_dispatchers, report_dispatchers=list(), report_functions=list(),
                  aggregates=list(), filters, cellnr, df_task, chapter_path, cache_foldername, chart_foldername){

  #Sanity checks

  checkmate::assert_class(cellsdf, classes = 'data.frame')
  dispatcher_propname<-getOption('relationshipMatrix.property_dispatcher')
  testthat::expect_true(dispatcher_propname %in% colnames(cellsdf))
  chunkdf_propnames<-getOption('relationshipMatrix.chunkdf_properties')
  testthat::expect_true(all(chunkdf_propnames[c('depvar', 'indepvar')] %in% colnames(cellsdf)))
  testthat::expect_gt(nrow(cellsdf), 0)

  checkmate::assertString(chart_foldername)
  checkmate::assertString(cache_foldername)


  checkmate::assert_class(stats_dispatchers, classes = 'list')
  for(fn in stats_dispatchers) {
    checkmate::assert_class(fn, 'function')
  }

  checkmate::assert_class(report_dispatchers, classes = 'list')
  for(fn in report_dispatchers) {
    checkmate::assert_class(fn, 'function')
  }

  checkmate::assert_class(report_functions, classes = 'list')
  for(fn in report_functions) {
    checkmate::assert_class(fn, 'function')
  }

  checkmate::assert_class(aggregates, classes = 'list')
  for(fn in report_functions) {
    checkmate::assert_class(fn, 'AggregateType')
  }

  checkmate::assert_class(filters, classes = 'list')
  for(fn in filters) {
    checkmate::assert_class(fn, 'Filter')
  }

  checkmate::assertInteger(cellnr)
  testthat::expect_gte(nrow(cellsdf), cellnr)
  checkmate::checkClass(chapter_path, 'list')

  #Getting the database. In future the database will only be allowed to be input in the depwalker-compatible way, either
  #as inputobject or parent(preferrably), so this function wouldn't need to juggle this big object at all.



  if('data.frame' %in% class(df_task)) {
    df<-df_task
  } else if('DepwalkerTask' %in% class(df)) {
    browser() #not implemented (yet)
  }

  for(cname in colnames(cellsdf)) {
    if('factor' %in% cellsdf[[cname]]) {
      cellsdf[[cname]]<-as.character(cellsdf[[cname]])
    }
  }

  # Getting the chunkdf objects

  #1. Get list of the variables
  #1a. From depvar
  propname<-chunkdf_propnames[['depvar']]
  dv<-cellsdf[[propname]][[cellnr]]

  propname<-paste0(getOption('relationshipMatrix.property_depvar_prefix'),
                   getOption('relationshipMatrix.is_aggregate'))
  if(cellsdf[[propname]][[cellnr]]) {
    if(! dv %in% names(aggregates)) {
      stop(paste0("Errors when processing cellsdf, row ", cellnr, ". Cannot find aggregate dependent variable", dv, "." ))
    }
    ag<-aggregates[[dv]]
    dv<-ag$all_vars
    depvar<-ag
  } else {
    depvar<-dv
  }

  #1b. From indepvar
  propname<-chunkdf_propnames[['indepvar']]
  iv<-cellsdf[[propname]][[cellnr]]

  propname<-paste0(getOption('relationshipMatrix.property_indepvar_prefix'),
                   getOption('relationshipMatrix.is_aggregate'))
  if(cellsdf[[propname]][[cellnr]]) {
    if(!iv %in% names(aggregates)) {
      stop(paste0("Errors when processing cellsdf, row ", cellnr, ". Cannot find aggregate independent variable", iv, "." ))
    }
    ag<-aggregates[[iv]]
    iv<-ag$all_vars
    indepvar<-ag
  } else {
    indepvar<-iv
  }

  #1c. From groupvar
  propname<-chunkdf_propnames[['groupvar']]
  gv<-cellsdf[[propname]][[cellnr]]
  if(is.na(gv)) {
    gv<-NULL
    groupvar<-''
  } else {
    groupvar<-gv
  }

  #1d. Todo? from custom-added auxiliary columns - another tododf property
#  browser()


  #2. Get filter
  propname<-chunkdf_propnames[['filter']]
  filter<-cellsdf[[propname]][[cellnr]]
  if(!is.na(filter)) {
    if(filter %in% names(filters)) {
      filter<-filters[[filter]]
      filterstring<-filter$filterstring
    } else {
      browser()
      stop(paste0("Cannot find filter ", filter, " in user-supplied list of filters"))
    }
  } else {
    filterstring<-NA
  }

  #3. Building the parameterAccessor with all the available parameters
  all_properties = as.list(cellsdf[cellnr,])

  #browser()
  chapter<-doc_Standalone_Chapter$new(chart_foldername = chart_foldername, cache_foldername = cache_foldername)
  dispatcher_name<-as.character(cellsdf[[dispatcher_propname]][[cellnr]])
  stats_dispatcher<-stats_dispatchers[[dispatcher_name]]
  report_dispatcher<-report_dispatchers[[dispatcher_name]]

  path<-system.file('00_get_report.R', package = 'relationshipMatrix')
  source(path, local = TRUE)

  return(reports)
}





#' High level that gets the chunkdf
#'
#' @param req_prop_list A record read from the relationship matrix. It is a list of all parameters, including the variable names
#' @param dt The database, specified either as the dataframe, a path to it, or a task describing the process of getting it.
#' @param aggregates List of all aggregates.
#' @param filters List of all filters
#' @param flag_use_depwalker Flag. If set, the function will use depwalker, that would cache the results or delegate it to remote
#'        servers
#' @return Returns the chunkdb
prepare_chunkdf<-function(req_prop_list, dt, aggregates, filters, flag_use_depwalker=FALSE) {
  ###### Generate the chunk of df

  #1a. From depvar
  propname<-getOption('property_depvar')
  dv<-req_prop_list[[propname]]

  propname<-paste0(getOption('property_depvar_prefix'),'is_aggregate')
  if(req_prop_list[[propname]]) {
    if(! dv %in% names(aggregates)) {
      stop(paste0("Errors when processing cellsdf. Cannot find aggregate for aggregate dependent variable ", propname))
    }
    ag<-aggregates[[dv]]
    dv<-ag$all_vars
  }

  #1b. From indepvar
  propname<-getOption('property_indepvar')
  iv<-req_prop_list[[propname]]

  propname<-paste0(getOption('property_indepvar_prefix'),'is_aggregate')
  if(req_prop_list[[propname]]) {
    if(! iv %in% names(aggregates)) {
      stop(paste0("Errors when processing cellsdf. Cannot find aggregate for aggregate independent variable ", propname))
    }
    ag<-aggregates[[iv]]
    iv<-ag$all_vars
  }

  #1c. From groupvar
  propname<-getOption('property_groupvar')
  gv<-req_prop_list[[propname]]
  if(is.na(gv)) {
    gv<-NULL
  }

  #1d. Todo? from custom-added auxiliary columns - another tododf property

  #2. Get filter
  propname<-getOption('property_filter')
  f<-req_prop_list[[propname]]

  #3. Prepare the database
  if(!is.na(f)) {
    if(!f %in% names(filters)) {
      stop(paste0("Cannot find filter ", f, " in filters dictionary."))
    }
    f_fn<-filters[[f]]
    if(! 'R6' %in% class(f_fn) || !'Filter' %in% class(f_fn)){
      stop(paste0("Wrong type of filter ", f, " in filters dictionary. ",
                  "Expected (R6, Filter), got: ", paste0(class(f_fn), collapse=', ')))
    }
    chunkdf<-filter_(df, f_fn$filterstring) %>% select_(c(dv, iv, gv))
  } else {
    chunkdf<-select_(df, c(dv, iv, gv))
  }

  return(chunkdf)
}


#This file contains function that manage calculation dispatching

prepare_tododf<-function(dt, matrix_file, dispatchers) {
  dt_structure<-danesurowe::create_df_from_df_structure(dt, flag_include_vartype = TRUE)
  tododf<-read_matrix(matrix_file, dt_structure)
  return(tododf)
}

