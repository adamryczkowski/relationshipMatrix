#This code gets called to generate the report list for the given cell.
#
#It only assumes the most basic parameters. It is supposed to be run on a access node, because it doesn't contain code that is
#expensive to execute.
#
#
#Parameters:
#
#df: The database. In future it will be a dependent task.
#all_properties: named list of all parameters possible parameters. The actual parameters will be discovered by the parameter discovery step for each external function run
#indepvar: name of the independent variable or list with the serialized arguments of the AggregateType
#depvar: name of the dependent variable or list with the serialized arguments of the AggregateType
#groupvar: name of the grouping variable or '' if none.
#filter: serialized object of the type Filter or NULL
#stats_dispatcher: function that will generate the statistics. That function was already run in the discovery phase.
#report_dispatcher: function that will generate the list of statistics. It should be returned during the discovery phase.
#report_functions: list of all functions that could be potentially called

get_chunkdf<-function(df, indepvar, depvar, groupvar, filter) {
  ###### Generate the chunk of df

  #1. From depvar and indepvar
  #browser()
  variables<-character(0)
  if('AggregateType' %in% class(indepvar)) {
    variables<-c(variables, indepvar$all_vars)
  } else {
    variables<-c(variables, indepvar)
  }
  if('AggregateType' %in% class(depvar)) {
    variables<-c(variables, depvar$all_vars)
  } else {
    variables<-c(variables, depvar)
  }

  #2. From groupvar
  if(groupvar!='') {
    variables<-c(variables, groupvar)
  }

  #3. Get filter
  if('Filter' %in% class(filter)) {
    filterstring<-filter$filterstring
  } else {
    filterstring<-NULL
  }

  source(system.file('01_chunkdf.R', package = 'relationshipMatrix'), local = TRUE) #This call would be cached, if we used depwalker
  return(chunkdf)
}

merge_properties<-function(all_properties, properties) {
  extra_names<-which(! names(properties) %in% names(all_properties))
  extra_names<-names(properties)[extra_names[stringr::str_detect(names(properties)[extra_names], pattern = stringr::regex('^[^\\.]'))]]
  return(c(all_properties, properties[extra_names]))
}

#all_properites contain all properties. That's why we need to do a discovery run for each user-supplied function beforehand
do_cell<-function(df, indepvar, depvar, groupvar, filter, all_properties,  stats_dispatcher, report_dispatcher, report_functions, chapter) {
  #  browser()
  language_prop<-getOption('relationshipMatrix.property_i18n_language')
  #1. Grabs the chunk db
  ans<-get_chunkdf(df=df, indepvar=indepvar, depvar=depvar, filter=filter, groupvar=groupvar)
  chunkdf<-ans$chunkdf
  filterNA<-ans$filterNA
  nrow_total<-nrow(df)
  dbobj<-relationshipMatrix::ChunkDB$new(chunkdf = df, depvar = dv, indepvar = iv, nrow_total=nrow_total,
                                         groupvar = gv, filter = filter, filterNA = filterNA, flag_never_serve_df = TRUE)


  #browser()
  #2. Run statistics function

  #a) Generate the propertyAccessor. It will enter the mode 1 to do discovery
  pAcc<-propertyAccessor$new(db=dbobj, properties = all_properties)

  #b) Getting the parameters from the discovery mode.
  #browser()
  discover_parameters(pa=pAcc, user_function=stats_dispatcher)

  #Uses chunkdf and propertyAccessor_cannonized
  #Outputs statistics and propertyAccessor

  #c) Run the function
  properties<-pAcc$.__enclos_env__$private$get_discovered_properties_list(flag_include_dbreversal=FALSE)
  all_properties<-merge_properties(all_properties, properties)
  source(system.file('02_stats_dispatch.R', package = 'relationshipMatrix'), local = TRUE)


  #3. Run the report function

  if(!is.null(pAcc$report_dispatcher)) {
    report_dispatcher<-pAcc$report_dispatcher
  } else {
    if(is.null(report_dispatcher)) {
      stop(paste0("Cannot find report_dispatcher for the cell"))
    }
  }

  dbreversed<-pAcc$is_reversed()

  #a) Generate again the propertyAccessor. It will enter the mode 1 to do discovery
  pAcc<-propertyAccessor$new(db=dbobj, properties = all_properties)
  if(dbreversed) {
    pAcc$reverse_vars()
  }

  #b) Getting the parameters from the discovery mode.
  discover_parameters(pa=pAcc, user_function=report_dispatcher, user_arguments=list(statistics=statistics))


  #c) Run the function
  properties<-pAcc$.__enclos_env__$private$get_discovered_properties_list()
  all_properties<-merge_properties(all_properties, properties)
  source(system.file('03_report_dispatch.R', package = 'relationshipMatrix'), local = TRUE)
  if(length(learned_properties)>0) {
    for(i in seq_along(learned_properties)) {
      lname<-names(learned_properties)[[i]]
      all_properties[[lname]]<-learned_properties[[i]]
    }
  }

  dbreversed<-pAcc$is_reversed()
  #  browser()
  #4. Insert NA report. Raport is assumed to be generated fast (if this is not true, we should move it to external task)
  language<-all_properties[[language_prop]] #Getting the language is important
  pAcc<-propertyAccessor$new(db=dbobj, properties = all_properties)
  discover_parameters(pa=pAcc, user_function=filter_info, user_arguments=list(language=language, chapter=chapter))
  properties<-pAcc$.__enclos_env__$private$get_discovered_properties_list(flag_include_dbreversal=FALSE)
  pAcc=propertyAccessor$new(properties=properties, db=dbobj, mode=3)
  filter_info(pAcc = pAcc, language = language, chapter = chapter)

  #5. Execute each report functions - the functions that generate the actual report
  for(i in seq_along(report_functions)) {
    fun<-report_functions[[i]]
    if('character' %in% class(fun)) {
      if(fun %in% names(report_functions)) {
        fun<-report_functions[[i]]
      } else {
        browser()
      }
    }
    #a) Generate the propertyAccessor. It will enter the mode 1 to do discovery
    pAcc<-propertyAccessor$new(db=dbobj, properties = all_properties)
    if(dbreversed) {
      pAcc$reverse_vars()
    }
    if(length(learned_properties)>0) {
      for(i in seq_along(learned_properties)) {
        lname<-names(learned_properties)[[i]]
        all_properties[[lname]]<-learned_properties[[i]]
      }
    }

    #    browser()
    if(language=='PL') {
      options(OutDec= ",")
    } else {
      options(OutDec= ".")
    }
    #b) Getting the parameters from the discovery mode.
    discover_parameters(pa=pAcc, user_function=fun, user_arguments=list(statistics=statistics, chapter=chapter))
    properties<-pAcc$.__enclos_env__$private$get_discovered_properties_list()
    source(system.file('04_report_gen.R', package = 'relationshipMatrix'), local = TRUE)
  }
  return(chapter)
}

#Function executes a given user function in the discovery mode. Generates a special tailored propertyAccessor as the output.
#It expects a db object, but only for its metadata. Accessing the object's actual data.frame will signal end of
#discovery phase.
discover_parameters<-function(pa, user_function, user_arguments=list()) {
  checkmate::assert_class(user_function, classes = 'function')
  checkmate::assert_class(pa, classes = 'propertyAccessor')
  checkmate::assert_class(user_arguments, classes = 'list')


  if('chapter' %in% names(user_arguments)) {
    user_arguments$chapter<-doc_Void_Document$new(chart_foldername = '', cache_foldername = '')
  }

  #Execute the dispatcher in the discovery mode, to get the list of all relevant properties
  ans<-tryCatch(
    do.call(user_function, args=c(pa, user_arguments)),
    error = function(e) e
  )

  if(! 'error' %in% class(ans)) {
    pa$.__enclos_env__$private$mode_<-2
    warning(paste0("Dispatcher did not call done_discovery() function."))
    return(pa)
  }

  if(ans$message!='Done discovery mode') {
    stop(paste0("Error ", ans$message, " while calling the dispatcher in the discovery mode."))
  }

  return(pa)
}


reports<-do_cell(df, indepvar, depvar, groupvar, filter,
                 all_properties,
                 stats_dispatcher, report_dispatcher, report_functions,
                 chapter=chapter)
