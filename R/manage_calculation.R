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
render_matrix<-function(cellsdf, author, title, format){
  # Algorithm:
  # 1. Convert prefix1 from NA to '', like all other prefixes

  prefix_column<-getOption('relationshipMatrix.prefix')

  if(! paste0(prefix_column, 1) %in% colnames(cellsdf) ) {
    cellsdf[[paste0(prefix_column, 1)]]<-NA_character_
  }
  if(! paste0(prefix_column, 2) %in% colnames(cellsdf) ) {
    cellsdf[[paste0(prefix_column, 2)]]<-NA_character_
  }
  if(! paste0(prefix_column, 3) %in% colnames(cellsdf) ) {
    cellsdf[[paste0(prefix_column, 3)]]<-NA_character_
  }



  # 2. Gather all chapters
  chapters<-new.env(parent=emptyenv())
  for(i in seq_len(nrow(cellsdf))) {
    paths<-c(cellsdf[[paste0(prefix_column, 1)]][[i]],
      cellsdf[[paste0(prefix_column, 2)]][[i]],
      cellsdf[[paste0(prefix_column, 3)]][[i]])

    base_chapter<-chapters
    for(path in paths) {
      if(!is.na(path)) {
        if(length(path)==1) {
          ch_name<-path[[1]]
          ch_prio<-NA_real_
        } else if (length(path)==2) {
          ch_name<-path[[2]]
          ch_prio<-path[[1]]
        } else {
          browser()
        }
        if(is.na(ch_name)) {
          browser()
        }
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
      }
    }
  }

  #3. Analyse the chapters to get their sort order

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
do_cell<-function(cellsdf, stats_dispatchers, report_dispatchers=list(), report_functions=list(), aggregates=list(), filters, cellnr, df_task){

  #Sanity checks

  checkmate::assert_class(cellsdf, classes = 'data.frame')
  dispatcher_propname<-getOption('relationshipMatrix.property_dispatcher')
  testthat::expect_true(dispatcher_propname %in% colnames(cellsdf))
  chunkdf_propnames<-getOption('relationshipMatrix.chunkdf_properties')
  testthat::expect_true(all(chunkdf_propnames[c('depvar', 'indepvar')] %in% colnames(cellsdf)))
  testthat::expect_gt(nrow(cellsdf), 0)


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
  for(fn in report_functions) {
    checkmate::assert_class(fn, 'Filter')
  }

  checkmate::assertInteger(cellnr)
  testthat::expect_gte(nrow(cellsdf), cellnr)

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
    if(! db %in% names(aggregates)) {
      stop(paste0("Errors when processing cellsdf, row ", cellnr, ". Cannot find aggregate for aggregate dependent variable ", propname))
    }
    if(!dv %in% names(aggregates)) {
      stop(paste0("Cannot find the aggregate in the aggregates argument"))
    }
    ag<-aggregates[[dv]]
    dv<-ag$all_vars
  }

  #1b. From indepvar
  propname<-chunkdf_propnames[['depvar']]
  iv<-cellsdf[[propname]][[cellnr]]

  propname<-paste0(getOption('relationshipMatrix.property_indepvar_prefix'),
                   getOption('relationshipMatrix.is_aggregate'))
  if(cellsdf[[propname]][[cellnr]]) {
    if(! db %in% names(aggregates)) {
      stop(paste0("Errors when processing cellsdf, row ", cellnr, ". Cannot find aggregate for aggregate independent variable ", propname))
    }
    if(!iv %in% names(aggregates)) {
      stop(paste0("Cannot find the aggregate in the aggregates argument"))
    }
    ag<-aggregates[[iv]]
    iv<-ag$all_vars
  }

  #1c. From groupvar
  propname<-chunkdf_propnames[['groupvar']]
  gv<-cellsdf[[propname]][[cellnr]]
  if(is.na(gv)) {
    gv<-NULL
  }

  #1d. Todo? from custom-added auxiliary columns - another tododf property
  browser()


  #2. Get filter
  propname<-chunkdf_propnames[['filter']]
  filter<-cellsdf[[propname]][[cellnr]]

  #3. Getting the parameters from the discovery mode.
  dbobj<-relationshipMatrix::ChunkDB$new(chunkdf = df, depvar = dv, indepvar = iv, groupvar = gv, filtr = filter, flag_never_serve_df = TRUE)
  pa<-discover_parameters(cellsdf = cellsdf, cellnr = cellnr, stats_dispatchers = stats_dispatchers, db=dbobj)


  #4. Prepare the database
  if(!is.na(f)) {
    chunkdf<-df %>% filter_() %>% select_(c(dv, iv, gv))
  } else {
    chunkdf<-df %>% select_(c(dv, iv, gv))
  }

  browser()
  #Calculate the hash using this chunkdf, record and source of the dispatcher function and
  #compare it again with the cache. If matches, then update the small cache object on disk and return.
  #If still doesn't match, it means that some of the inputs have changed and we need to run the dispatcher fully

}



#Function calculates all fast parameters to get as accurate hash as possible.
#It returns populated propertyAccessor and list of all properties required to get chunkdf
discover_parameters<-function(cellsdf, cellnr, stats_dispatchers, db) {
  checkmate::assert_class(cellsdf, classes = 'data.frame')
  dispatcher_propname<-getOption('relationshipMatrix.property_dispatcher')
  testthat::expect_true(dispatcher_propname %in% colnames(cellsdf))
  chunkdf_propnames<-as.character(getOption('relationshipMatrix.chunkdf_properties'))

  testthat::expect_gt(nrow(cellsdf), 0)

  checkmate::assert_class(stats_dispatchers, classes = 'list')
  for(fn in stats_dispatchers) {
    checkmate::assert_class(fn, 'function')
  }

  checkmate::assertInteger(cellnr)
  testthat::expect_gte(nrow(cellsdf), cellnr)


  #Now we generate property accessor
  properties<-as.list(cellsdf[cellnr,])
  existing_chunkdf_propnames<-intersect(names(properties), chunkdf_propnames)
  basic_properties<-properties[existing_chunkdf_propnames]

  pa<-propertyAccessor$new(db = db, properties = properties)
  paprivate<-pa$.__enclos_env__$private
  #paprivate$setup_serve_properties(properties=properties, db=data.frame(a=1))




  browser()
  #Get the dispatcher
  dname<-as.character(cellsdf[[dispatcher_propname]][[cellnr]])
  if(! dname %in% names(stats_dispatchers)) {
    stop(paste0("Errors when processing cellsdf, row ", cellnr, ". Cannot find definition of dispatcher ", dname, ". "))
  }
  disp<-stats_dispatchers[[dname]]

  #Execute the dispatcher in the discovery mode, to get the list of all relevant properties
  ans<-tryCatch(
    disp(pa),
    error = function(e) e
  )
  if(! 'error' %in% class(ans)) {
    stop(paste0("Dispatcher ", dname, " did not call serve_db() function."))
  }

  if(ans$message!='Done discovery mode') {
    stop(paste0("Error ", ans$message, " while calling the dispatcher ", dname, " in the discovery mode."))
  }

  #proplist is a list of all accessed properties together with the validation function for each of them
  #We remove basic chunkdf properties from the records, as they will be accessed anyway
  proplist<-setdiff(paprivate$property_validators_, chunkdf_propnames)
  record<-list()
  if(length(proplist)>0) {
    for(prop in sort(names(proplist))) {
      value<-properties[[prop]]
      validfn<-proplist[[prop]]
      ans<-tryCatch(
        validfn(value),
        error = function(e) {}
      )
      if( 'error' %in% class(ans)) {
        stop(paste0("Errors when processing cellsdf, row ", cellnr, ". Argument ", prop, " with value «", value, "» did not pass the validation function set by the dispatcher ", dname, ". "))
      }
      record[[prop]]<-value
    }
  }


  # We disallow the accessor to record basic properties
  paprivate$all_properties_<-record
  paprivate$property_validators_<-NULL

  parameterList<-paprivate$cannonize()

  return(list(pa=pa))
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
    chunkdf<-dt %>% filter_(f_fn$filterstring) %>% select_(c(dv, iv, gv))
  } else {
    chunkdf<-df %>% select_(c(dv, iv, gv))
  }

  return(chunkdf)
}


#This file contains function that manage calculation dispatching

prepare_tododf<-function(dt, matrix_file, dispatchers) {
  dt_structure<-danesurowe::create_df_from_df_structure(dt, flag_include_vartype = TRUE)
  tododf<-read_matrix(matrix_file, dt_structure)
  return(tododf)
}



#' Lowest level function that generates the cell.
#'
#' It accepts only the actual parameters that will be used for code generation.
#'
#' @varnames List of all
#'
#' @param properties Dictionary with all the custom properties needed for the cell, learned by the
#'        discovery step on the dispatcher.
do_cell_with_chunkdf<-function(varnames, properties, dispatcher, filter) {

}

#Function generates hash for the whole database together with the record needed by the cell
cellhash_fulldf<-function(dfhash, record) {

}

#Function generates hash for the whole database together with the record needed by the cell
cellhash_chunkdf<-function(dfhash, record) {

}

#Calculates hash of the whole database
calculate_hash_db<-function(df) {

}
