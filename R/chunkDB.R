ChunkDB<-R6::R6Class(
  "ChunkDB",
  #DB accessor that can nicely format all labels
  public =
    list(
      initialize=function(chunkdf, depvar, indepvar, groupvar, filtr, flag_never_serve_df=FALSE) {
        private$chunkdf_<-chunkdf
        private$depvar_<-depvar
        private$indepvar_<-indepvar
        private$groupvar_<-groupvar
        private$filtr_<-filtr
        private$flag_never_serve_df_<-flag_never_serve_df
      },
      depvar_label = function(flag_md=FALSE) {
        prefix<-getOption('relationshipMatrix.property_depvar_prefix')
        get_labels_var(var=private$depvar_, dt=private$chunkdf_, flag_md=flag_md, private$metaserver_, prefix = prefix)
      },
      indepvar_label = function(flag_md=FALSE) {
        prefix<-getOption('relationshipMatrix.property_indepvar_prefix')
        get_labels_var(var=private$depvar_, dt=private$chunkdf_, flag_md=flag_md, private$metaserver_, prefix = prefix)
      },
      groupvar_label = function(flag_md=FALSE) {
        if(self$is_grouped()) {
          prefix<-getOption('relationshipMatrix.property_groupvar_prefix')
          get_labels_var(var=private$depvar_, dt=private$chunkdf_, flag_md=flag_md, private$metaserver_, prefix = prefix)
        } else {
          NA_character_
        }
      },
      df_label = function(flag_md=FALSE) {

        browser() #TODO - należy dodać nazwę bazy danych, uwzględniającą filtr
      },
      dvlevels = function(flag_recalculate=FALSE, flag_include_NA=FALSE) {
        checkmate::assertFALSE(self$is_depvar_aggregate())
        danesurowe::GetLevels(private$chunkdf_[[private$depvar_]], flag_recalculate = flag_recalculate, flag_include_NA = flag_include_NA)
      },
      ivlevels = function(flag_recalculate=FALSE, flag_include_NA=FALSE) {
        checkmate::assertFALSE(self$is_indepvar_aggregate())
        danesurowe::GetLevels(private$chunkdf_[[private$indepvar_]], flag_recalculate = flag_recalculate, flag_include_NA = flag_include_NA)
      },
      gvlevels = function(flag_recalculate=FALSE, flag_include_NA=FALSE) {
        danesurowe::GetLevels(private$chunkdf_[[private$groupvar_]], flag_recalculate = flag_recalculate, flag_include_NA = flag_include_NA)
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
        out$.__enclos_env__$private$metaserver_<-private$metaserver_
        return(out)
      },
      is_grouped = function() {
        if(!is.na(private$groupvar_)) {
          private$groupvar_!=''
        } else {
          FALSE
        }
      },
      chunkdf_ivdvgv = function() {
        if(private$flag_never_serve_df_) {
          private$metaserver_$done_discovery()
#          stop("Done discovery mode") #We trigger the error so we can grab the object
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
        private$metaserver_$done_discovery()
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
#      browser()
      if('character' %in% class(private$depvar_)) {
        private$metaserver_$done_discovery()
        return(private$chunkdf_[[private$depvar_]])
      } else if ('AggregateType' %in% class(private$depvar_)) {
        private$depvar_$.__enclos_env__$private$db_<-self
        return(private$depvar_)
      } else {
        browser()
      }
    },
    indepvar = function() {
      if('character' %in% class(private$indepvar_)) {
        private$metaserver_$done_discovery()
        return(private$chunkdf_[[private$indepvar_]])
      } else if ('AggregateType' %in% class(private$indepvar_)) {
        private$indepvar_$.__enclos_env__$private$db_<-self
        return(private$indepvar_)
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
    if(is.na(varunit)) {
      varunit<-''
    }
    if(is.na(varlab)) {
      browser()
    }
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

