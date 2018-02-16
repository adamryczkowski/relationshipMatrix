ChunkDB<-R6::R6Class(
  "ChunkDB",
  #DB accessor that can nicely format all labels
  public =
    list(
      initialize=function(chunkdf, depvar, indepvar, groupvar, filtr, flag_never_serve_df) {
        private$chunkdf_<-chunkdf
        private$depvar_<-depvar
        private$indepvar_<-indepvar
        private$groupvar_<-groupvar
        private$filtr_<-filtr
        private$flag_never_serve_df_<-flag_never_serve_df
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
      },
      reversed = function() {
        out<-ChunkDB$new(chunkdf=private$chunkdf_,
                         depvar=private$indepvar_,
                         indepvar=private$depvar_,
                         groupvar=private$groupvar_,
                         filtr=private$filtr_)
        return(out)
      },
      is_grouped = function() {!is.na(private$groupvar_)}
    ),
  active = list(
    chunkdf = function() {
      if(private$flag_never_serve_df_) {
        stop("Done discovery mode") #We trigger the error so we can grab the object
      }
      private$chunkdf_
    },
    filterstring = function() {
      if(!'Filter' %in% class(private$filtr_)){
        return('')
      } else {
        return(private$filtr_$filterstring)
      }
    },
    depvar_name = function() {private$depvar_},
    indepvar_name = function() {private$indepvar_},
    groupvar_name = function() {private$groupvar_},
    depvar = function() {
      if(private$flag_never_serve_df_) {
        stop("Done discovery mode") #We trigger the error so we can grab the object
      }
      if('character' %in% class(private$depvar_)) {
        return(private$chunkdf_[[private$depvar_]])
      } else if ('AggregateType' %in% class(private$depvar_)) {
        browser() #TODO
      } else {
        browser()
      }
    },
    indepvar = function() {
      if(private$flag_never_serve_df_) {
        stop("Done discovery mode") #We trigger the error so we can grab the object
      }
      if('character' %in% class(private$indepvar_)) {
        return(private$chunkdf_[[private$indepvar_]])
      } else if ('AggregateType' %in% class(private$indepvar_)) {
        browser() #TODO
      } else {
        browser()
      }
    }
  ),
  #Can be accessed with object$.__enclos_env__$private
  private = list(
    chunkdf_=data.frame(),
    depvar_=NA,
    indepvar_=NA,
    groupvar_=NA,
    filtr_=NA,
    flag_never_serve_df_=FALSE
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

