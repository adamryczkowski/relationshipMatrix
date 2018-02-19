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
  variables<-character(0)
  if('AggregateType%' %in% class(indepvar)) {
    variables<-c(variables, indepvar$all_vars)
  } else {
    variables<-c(variables, indepvar)
  }
  if('AggregateType%' %in% class(depvar)) {
    variables<-c(variables, indepvar$all_vars)
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

#all_properites contain all properties. That's why we need to do a discovery run for each user-supplied function beforehand
do_cell<-function(df, indepvar, depvar, groupvar, filter, all_properties,  stats_dispatcher, report_dispatcher, report_functions, chapter) {
  browser()
  #1. Grabs the chunk db
  chunkdf<-get_chunkdf(df=df, indepvar=indepvar, depvar=depvar, filter=filter, groupvar=groupvar)
  dbobj<-relationshipMatrix::ChunkDB$new(chunkdf = df, depvar = dv, indepvar = iv,
                                         groupvar = gv, filtr = filterstring)



  #2. Run statistics function

  #a) Generate the propertyAccessor. It will enter the mode 1 to do discovery
  pAcc<-propertyAccessor$new(db=chunkdf, properties = all_properties)

  #b) Getting the parameters from the discovery mode.
  discover_parameters(pa=pAcc, user_function=stats_dispatcher)

  #Uses chunkdf and propertyAccessor_cannonized
  #Outputs statistics and propertyAccessor

  #c) Run the function
  all_properties<-pAcc$.__enclos_env__$private$get_discovered_properties_list()
  source(system.file('02_stats_dispatch.R', package = 'relationshipMatrix'), local = TRUE)


  #3. Run the report function

  if(!is.null(pa$report_dispatcher)) {
    report_dispatcher<-pa$report_dispatcher
  } else {
    if(is.null(report_dispatcher)) {
      stop(paste0("Cannot find report_dispatcher for the cell"))
    }
  }

  #a) Generate the propertyAccessor. It will enter the mode 1 to do discovery
  pAcc<-propertyAccessor$new(db=chunkdf, properties = all_properties)

  #b) Getting the parameters from the discovery mode.
  discover_parameters(pa=pAcc, user_function=stats_dispatcher, user_arguments=list(statistics=statistics))


  #c) Run the function
  all_properties<-pAcc$.__enclos_env__$private$get_discovered_properties_list()
  source(system.file('03_report_dispatch.R', package = 'relationshipMatrix'), local = TRUE)



  reports<-list()
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
    pAcc<-propertyAccessor$new(db=chunkdf, properties = all_properties)

    #b) Getting the parameters from the discovery mode.
    discover_parameters(pa=pAcc, user_function=stats_dispatcher, user_arguments=list(statistics=statistics, chapter=chapter))
    all_properties<-pAcc$.__enclos_env__$private$get_discovered_properties_list()
    source(system.file('04_report_gen.R', package = 'relationshipMatrix'), local = TRUE)
    reports[[i]]<-report
  }
  return(reports)
}

#Function executes a given user function in the discovery mode. Generates a special tailored propertyAccessor as the output.
#It expects a db object, but only for its metadata. Accessing the object's actual data.frame will signal end of
#discovery phase.
discover_parameters<-function(pa, user_function, user_arguments=list()) {
  checkmate::assert_class(user_function, classes = 'function')
  checkmate::assert_class(user_function, classes = 'propertyAccessor')
  checkmate::assert_class(user_function, classes = 'list')



  #Execute the dispatcher in the discovery mode, to get the list of all relevant properties
  ans<-tryCatch(
    do.call(user_function, args=c(pa, user_arguments)),
    error = function(e) e
  )
  if(! 'error' %in% class(ans)) {
    stop(paste0("Dispatcher ", dname, " did not call serve_db() function."))
  }

  if(ans$message!='Done discovery mode') {
    stop(paste0("Error ", ans$message, " while calling the dispatcher ", dname, " in the discovery mode."))
  }

  return(pa)
}


reports<-do_cell(df, indepvar, depvar, groupvar, filter,
                 all_properties,
                 stats_dispatcher, report_dispatcher, report_functions,
                 chapter=chapter)
