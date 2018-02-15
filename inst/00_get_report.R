#This code gets called to generate the report list for the given cell.
#
#It only assumes the most basic parameters
#
#
#Parameters:
#
#df: The database. In future it will be a dependent task.
#parameterList: list of all parameters gathered by the parameter discovery.
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
    variables<-c(variables, indepvar)
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

  source('inst/01_chunkdf.R') #This call would be cached, if we used depwalker
  return(list(df=chunkdf, indepvar=indepvar, depvar=depvar, groupvar=groupvar, filter=filter)
}

do_cell<-function(df, indepvar, depvar, groupvar, filter, all_properties,  stats_dispatcher, report_dispatcher, report_functions) {
  #1. Grabs the chunk db
  chunkdf<-get_chunkdf(df=df, indepvar=indepvar, depvar=depvar, filter=filter, groupvar=groupvar)

  #2. Make statistics
  prperties_names<-getOption('relationshipMatrix.chunkdf_properties')[c('depvar', 'indepvar', 'groupvar', 'filter')]
  all_properties_extras<-setNames(c(depvar, indepvar, groupvar, filter), prperties_names)
  all_properties=c(all_properties_extras, all_properties)
  pAcc<-propertyAccessor$new(initlist=c(all_properties), db=chunkdb, indepvar=indepvar, depvar=depvar, groupvar=groupvar, filter=filter)
  propertyAccessor_cannonized<-pAcc$cannonize()

  #Uses chunkdf and propertyAccessor_cannonized
  #Outputs statistics and propertyAccessor
  source('inst/02_stats_dispatch.R')



  source('inst/03_report_dispatch.R')

  #TODO In future the following will be split into
  # 1. function that generates the tasks
  # 2. call to that tasks
  # 3. execution of the function that gathers all the results into the (properly ordered) list
  for(i in seq_along(report_functions)) {
    fun<-report_functions[[i]]
    if('character' %in% class(fun)) {
      if(fun %in% names(report_functions)) {
        fun<-report_functions[[i]]
      } else {
        browser()
      }
    }
    source('inst/04_report_gen.R')
    reports[[i]]<-report
  }
  return(reports)
}


reports<-do_cell(df, indepvar, depvar, groupvar, filter,
                 all_properties,
                 stats_dispatcher, report_dispatcher, report_functions)
