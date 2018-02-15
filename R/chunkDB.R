ChunkDB<-R6::R6Class(
  "ChunkDB",
  #DB accessor that can nicely format all labels
  public =
    list(
      initialize=function(chunkdf, depvar, indepvar, groupvar, filtr) {
        private$chunkdf_<-chunkdf
        private$depvar_<-depvar
        private$indepvar_<-indepvar
        private$groupvar_<-groupvar
        private$filtr_<-filtr
      },
      depvar_label = function(flag_md=FALSE) {
        get_labels_var(var=private$depvar_, dt=private$chunkdf_, flag_md=flag_md)
      },
      indepvar_label = function(flag_md=FALSE) {
        get_labels_var(var=private$indepvar_, dt=private$chunkdf_, flag_md=flag_md)
      },
      groupvar_label = function(flag_md=FALSE) {
        get_labels_var(var=private$groupvar_, dt=private$chunkdf_, flag_md=flag_md)
      },
      df_label = function(flag_md=FALSE) {
        browser() #TODO - należy dodać nazwę bazy danych, uwzględniającą filtr
      }
    ),
  active = list(
    chunkdf = function() {private$chunkdf_},
    filterstring = function() {
      if(!'Filter' %in% class(private$filtr_)){
        return('')
      } else {
        return(private$filtr_$filterstring)
      }
    }
  ),
  #Can be accessed with object$.__enclos_env__$private
  private = list(
    chunkdf_=data.frame(),
    depvar_=NA,
    indepvar_=NA,
    groupvar_=NA,
    filtr_=NA
  )
)

#' Gets variable label
get_labels_var<-function(var, dt, flag_md=FALSE) {
  if('character' %in% class(var))
  {
    if(flag_md) {
      ans<-paste0('_', Hmisc::label(dt[[var]]), '_')
    } else {
      ans<-Hmisc::label(dt[[var]])
    }
    if(!is.null(attr(dt[[var]], 'units'))) {
      ans<-paste0(ans, ' [', attr(dt[[var]], 'units'), ']' )
    }
  } else if('AggregateType' %in% class(var)) {
    if(flag_md) {
      ans<-paste0('_', var$label, '_')
    } else {
      ans<-var$label
    }
    if(!is.na(var$unit)) {
      ans<-paste0(ans, ' [', var$unit, ']' )
    }
  } else {
    browser() #Unknown class
  }
}

