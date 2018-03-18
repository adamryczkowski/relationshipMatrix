ChunkDB<-R6::R6Class(
  "ChunkDB",
  #DB accessor that can nicely format all labels
  public =
    list(
      initialize=function(chunkdf, depvar, indepvar, groupvar, filter, flag_never_serve_df=FALSE, filterNA, nrow_total) {
        checkmate::assertClass(chunkdf, 'data.frame')
        private$chunkdf_<-chunkdf
        private$depvar_<-depvar
        private$indepvar_<-indepvar
        checkmate::assertString(groupvar)
        private$groupvar_<-groupvar
        checkmate::assertClass(filter, "Filter")
        private$filter_<-filter
        checkmate::assert_int(filterNA)
        private$filterNA_<-filterNA
        checkmate::assertFlag(flag_never_serve_df)
        private$flag_never_serve_df_<-flag_never_serve_df
        checkmate::assertInt(nrow_total)
        private$nrow_total_<-nrow_total
      },
      depvar_label = function(flag_md=FALSE) {
        if(private$metaserver_reversed_) {
          prefix<-getOption('relationshipMatrix.property_indepvar_prefix')
        } else {
          prefix<-getOption('relationshipMatrix.property_depvar_prefix')
        }
        get_labels_var(var=private$depvar_, dt=private$chunkdf_, flag_md=flag_md, private$metaserver_, prefix = prefix)
      },
      indepvar_label = function(flag_md=FALSE) {
        if(private$metaserver_reversed_) {
          prefix<-getOption('relationshipMatrix.property_depvar_prefix')
        } else {
          prefix<-getOption('relationshipMatrix.property_indepvar_prefix')
        }
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
      filter_label = function(flag_md=FALSE) {
        if(is.null(private$metaserver_)) {
          filter<-private$filter_$label
        } else {
          filter<-private$metaserver_$get_property(paste0(getOption('relationshipMatrix.chunkdf_properties')$filter, '.label'))
        }
        if(flag_md) {
          ans<-paste0('_', filter, '_')
        } else {
          ans<-filter
        }
        return(ans)
      },
      #Returns named vector of all labels (keys are varnames). They include aggregates, if they exist
      all_labels = function() {
        vars<-character(0)
        labels<-character(0)
        if(self$is_depvar_aggregate()) {
          vars<-c(vars, private$depvar_$all_vars)
          labels<-c(labels, private$depvar_$var_labels())
        } else {
          vars<-c(vars, private$depvar_)
          labels<-c(labels, self$depvar_label())
        }
        if(self$is_indepvar_aggregate()) {
          vars<-c(vars, private$indepvar_$all_vars)
          labels<-c(labels, private$indepvar_$var_labels())
        } else {
          vars<-c(vars, private$indepvar_)
          labels<-c(labels, self$indepvar_label())
        }
        if(self$is_grouped()) {
          vars<-c(vars, private$groupvar_)
          labels<-c(labels, self$groupvar_label())
        }
        return(setNames(labels, vars))
      },
      depvar_property = function(property_name) {
        if(private$metaserver_reversed_) {
          prefix<-getOption('relationshipMatrix.property_depvar_prefix')
        } else {
          prefix<-getOption('relationshipMatrix.property_indepvar_prefix')
        }
        private$metaserver_$get_property(paste0(prefix, property_name))
      },
      indepvar_property = function(property_name) {
        if(private$metaserver_reversed_) {
          prefix<-getOption('relationshipMatrix.property_indepvar_prefix')
        } else {
          prefix<-getOption('relationshipMatrix.property_depvar_prefix')
        }
        private$metaserver_$get_property(paste0(prefix, property_name))
      },
      groupvar_property = function(property_name) {
        prefix<-getOption('relationshipMatrix.property_groupvar_prefix')
        private$metaserver_$get_property(paste0(prefix, property_name))
      },
      df_label = function(flag_md=FALSE) {

        browser() #TODO - należy dodać nazwę bazy danych, uwzględniającą filtr
      },
      #Returns data.frame that lists resons of NA in the filtered data set.
      NA_report = function() {
        NA_pattern<-mice::md.pattern(private$chunkdf_)
        return(list(NA_pattern=NA_pattern, NA_filter=private$filterNA_, nrow_total=private$nrow_total_))
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
                         filter=private$filter_,
                         filterNA=private$filterNA_,
                         nrow_total=private$nrow_total_,
                         flag_never_serve_df = private$flag_never_serve_df_)
        out$.__enclos_env__$private$metaserver_<-private$metaserver_
        out$.__enclos_env__$private$metaserver_reversed_<-! private$metaserver_reversed_
        return(out)
      },
      is_grouped = function() {
        if(!is.na(private$groupvar_)) {
          private$groupvar_!=''
        } else {
          FALSE
        }
      },
      chunkdf = function(flag_include_NA=FALSE) {
        checkmate::assertFlag(flag_include_NA)
        if(private$flag_never_serve_df_) {
          private$metaserver_$done_discovery()
          stop("Done discovery mode") #We trigger the error so we can grab the object
        }
        if(!flag_include_NA) {
          db<-na.omit(private$chunkdf_)
        } else {
          db<-private$chunkdf_
        }
        return(db)
      },
      chunkdf_ivdvgv = function(flag_include_NA=FALSE) {
        #browser()

        if(self$is_depvar_aggregate() || self$is_indepvar_aggregate()) {
          stop("Cannot cat ivdvgv format if either of variables is aggregate")
        }
        df<-data.table::copy(self$chunkdf(flag_include_NA))

        if(self$is_grouped()) {
          all_names<-c(private$depvar_, private$indepvar_, private$groupvar_)
          if(sum(duplicated(all_names))>0) {
            oldnames<-setdiff(colnames(df), c('dv', 'iv', 'gv') )
            df$dv<-private$chunkdf_[[private$depvar_]]
            df$iv<-private$chunkdf_[[private$indepvar_]]
            df$gv<-private$chunkdf_[[private$groupvar_]]
            for(n in oldnames) {
              df[[n]]<-NULL
            }
          } else {
            data.table::setnames(x = df, old = c(private$depvar_, private$indepvar_, private$groupvar_), new = c('dv', 'iv', 'gv') )
          }
        } else {
          all_names<-c(private$depvar_, private$indepvar_)
          if(sum(duplicated(all_names))>0) {
            oldnames<-setdiff(colnames(df), c('dv', 'iv') )
            df$dv<-private$chunkdf_[[private$depvar_]]
            df$iv<-private$chunkdf_[[private$indepvar_]]
            for(n in oldnames) {
              df[[n]]<-NULL
            }
          } else {
            data.table::setnames(x = df, old = c(private$depvar_, private$indepvar_), new = c('dv', 'iv') )
          }
        }
        return(df)
      }
    ),
  active = list(
    filterstring = function() {
      if(!'Filter' %in% class(private$filter_)){
        return('')
      } else {
        return(private$filter_$filterstring)
      }
    },
    depvar_name = function() {private$depvar_},
    indepvar_name = function() {private$indepvar_},
    groupvar_name = function() {private$groupvar_},
    nrow = function() {nrow(private$chunkdf_)},
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
    filter_=NA,
    filterNA_=NA_integer_,
    nrow_total_=NA_integer_,
    flag_never_serve_df_=FALSE,
    metaserver_=NULL, #Server that serves metadata
    metaserver_reversed_=FALSE #True means that access to the var's properties via metaserver must be reversed
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

