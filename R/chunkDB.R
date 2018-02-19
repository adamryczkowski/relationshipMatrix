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
        get_labels_var(var=private$depvar_, dt=private$chunkdf_, flag_md=flag_md, private$metaserver_)
      },
      indepvar_label = function(flag_md=FALSE) {
        get_labels_var(var=private$indepvar_, dt=private$chunkdf_, flag_md=flag_md, private$metaserver_)
      },
      groupvar_label = function(flag_md=FALSE) {
        get_labels_var(var=private$groupvar_, dt=private$chunkdf_, flag_md=flag_md, private$metaserver_)
      },
      df_label = function(flag_md=FALSE) {

        browser() #TODO - należy dodać nazwę bazy danych, uwzględniającą filtr
      },
      is_depvar_aggregate=function() {'AggregateType' %in% class(private$depvar_)},
      is_indepvar_aggregate=function() {'AggregateType' %in% class(private$indepvar_)},
      reversed = function() {
        out<-ChunkDB$new(chunkdf=private$chunkdf_,
                         depvar=private$indepvar_,
                         indepvar=private$depvar_,
                         groupvar=private$groupvar_,
                         filtr=private$filtr_,
                         flag_never_serve_df = private$flag_never_serve_df_)
        return(out)
      },
      is_grouped = function() {!is.na(private$groupvar_)},
      chunkdf_ivdvgv = function() {
        if(private$flag_never_serve_df_) {
          stop("Done discovery mode") #We trigger the error so we can grab the object
        }
        if(self$is_depvar_aggregate() || self$is_indepvar_aggregate()) {
          stop("Cannot cat ivdvgv format if either of variables is aggregate")
        }
        df<-data.table::copy(private$chunkdf_)
        data.table::setnames(x = df, old = c(private$depvar_, private$indepvar_, private$groupvar_), new = c('dv', 'iv', 'gv') )
        df
      },
      filter_label = function(flag_md=FALSE) {
        if(is.null(private$metaserver_)) {
          filtr<-private$filtr_$label
        } else {
          filtr<-private$metaserver_$get_property(paste0(getOption('relationshipMatrix.chunkdf_properties')$filter, '.label'))
        }
        if(flag_md) {
          ans<-paste0('_', filtr, '_')
        } else {
          ans<-filtr
        }
        return(ans)
      }
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
    flag_never_serve_df_=FALSE,
    metaserver_=NULL #Server that serves metadata
  )
)

#' Gets variable label
get_labels_var<-function(var, dt, flag_md=FALSE, pAcc=NULL, prefix='.') {

  if(is.null(pAcc)) {
    if('AggregateType' %in% class(var)) {

    } else if ('character' %in% class(var)) {
      varlab<-var$label
      varunit<-var$unit
      if(is.na(varunit)) {
        varunit<-''
      }
    } else {
      browser() #unknown class
    }
    if(is.null(varunit)) {
      varunit<-''
    }
  } else {
    varlab<-pAcc$get_property(paste0(prefix,'label'))
    varunit<-pAcc$get_property(paste0(prefix,'units'))
  }

  if(flag_md) {
    ans<-paste0('_', varlab, '_')
  } else {
    ans<-varlab
  }
  if(varunit!='') {
    ans<-paste0(ans, ' [', varunit, ']' )
  }
  ans
}

