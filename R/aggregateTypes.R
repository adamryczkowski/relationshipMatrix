check_aggregateFn<-function(fn) {
  fmls<-formals(fn)
  typy<-plyr::laply(fmls, is.symbol)
  testthat::expect_gte(length(typy),2, "Aggregate function should have at least two parameters")
  if(length(typy)>2) {
    testthat::expect_true(all(!typy[-(1:2)]), "All arguments past the first two must have default values")
  }
}

AggregateType<-R6::R6Class(
  "AggregateType",
  #Aggregate type is an aggregation that is treated as a numeric variable
  public =
    list(
      initialize=function(name, label, fn, fn2, index_var=character(0), vars=character(0),
                          theoretical_min=NA, theoretical_max=NA, unit=NA) {
        private$name_<-name
        check_aggregateFn(fn)
        check_aggregateFn(fn2)
        private$fn_<-fn
        private$fn2_<-fn2
        private$label_<-label
        private$index_var_<-index_var
        private$vars_<-vars
        private$theoretical_min_<-as.numeric(theoretical_min)
        private$theoretical_max_<-as.numeric(theoretical_max)
        private$unit_<-unit
      },
      boot=function(bootstrap_n=NA, ncpus=4) {
 #       browser()
        if(is.na(bootstrap_n)) {
          stop("Missing bootstrap_n argument")
        }
        if(is.null(private$db_)) {
          stop("Missing database. This function can be used only on properly initialized object (inside ChunkDB)")
        }
        dt <- data.table::copy(private$db_$chunkdf)
        do_boot <- function(dt, bootstrap_n ) {
          if (nrow(dt)>0) {
            b<-simpleError('')
            while('error' %in% class(b)){
              b<-tryCatch(
                boot::boot(dt, private$fn2_, R = bootstrap_n, ncpus=4, parallel = 'multicore'),
                error=function(e) {e}
              )
              if(ceiling(bootstrap_n/2)<bootstrap_n){
                bootstrap_n <- ceiling(bootstrap_n/2)
              } else {
                out <- private$fn_(dt,seq(nrow(dt)))
                return(list(m=as.numeric(out$value), sd=as.numeric(out$error), q05=as.numeric(NA), q25=as.numeric(NA), q50=as.numeric(NA), q75=as.numeric(NA), q95=as.numeric(NA)))
                #          stop("Failed to get correct number of bootstrap repetitions")
              }
            }
            qs<-quantile(b$t, probs=c(0.025, 0.25, 0.5, 0.75, 0.975), na.rm=TRUE)
            return(list(m=b$t0, sd=sd(b$t), q05=qs[[1]], q25=qs[['25%']], q50=qs[['50%']], q75=qs[['75%']], q95=qs[[5]]))
          } else {
            return(list(m=as.numeric(NA), sd=as.numeric(NA), q05=as.numeric(NA), q25=as.numeric(NA), q50=as.numeric(NA), q75=as.numeric(NA), q95=as.numeric(NA)))
          }
        }
        #  if (length(c(groupby, zn))>0){
        dt<-as.data.table(dt)
        if(private$db_$is_grouped()) {
          b<-dt[, as.data.table(do_boot(.SD, bootstrap_n=bootstrap_n)), by = c(private$db_$groupvar_name, private$db_$indepvar_name)]
        } else {
          b<-dt[, as.data.table(do_boot(.SD, bootstrap_n=bootstrap_n)), by = c(private$db_$indepvar_name)]
        }
        return(b)
      },
      boot_ivgv=function(bootstrap_n=NA, ncpus=4) {
#        browser()
        b<-self$boot(bootstrap_n = bootstrap_n, ncpus=ncpus)
        if(private$db_$is_grouped()) {
          data.table::setnames(b, c(private$db_$groupvar_name, private$db_$indepvar_name), c('gv', 'iv'))
        } else {
          data.table::setnames(b, private$db_$indepvar_name, 'iv')
        }
      }
    ),
  active = list(
    name = function() {private$name_},
    fn = function() {private$fn_},
    fn2 = function() {private$fn2_},
    label = function() {private$label_},
    index_var = function() {private$index_var_},
    vars = function() {private$vars_},
    all_vars = function() {c(private$index_var_, private$vars_)},
    theoretical_min = function() {as.numeric(private$theoretical_min_)},
    theoretical_max = function() {as.numeric(private$theoretical_max_)},
    unit = function() {if(is.na(private$unit_)) {
      ''
    } else {
      private$unit_
    }}
  ),

  #Can be accessed with object$.__enclos_env__$private
  private = list(
    name_=NA_character_, #colname of the variable. This is how the variable is going to be identified programmatically.
    fn_=NA, # Function that takes database and index variable of valid rows (just like in bootstrap),
    # and returns value of the aggregate and standard error estimates.
    fn2_=NA, # Function that takes database and index variable of valid rows (just like in bootstrap),
    # and returns value of the aggregate. Standard errors are computed with bootstrap
    label_=NA_character_, #Nice label
    index_var_=character(0), # Variable that will benefit if is indexed
    vars_=character(0), # Other variables that will be used in the aggregate and need to be in the input data frame
    theoretical_min_=NA_real_,
    theoretical_max_=NA_real_,
    unit_=NA_character_,
    db_=NULL
  )
)

make_aggregateTypesDF<-function(aggregate_types) {
  if(length(aggregate_types)==0) {
    return(tibble::tibble(colname=character(0), label=character(0), class=character(0), vartype=character(0),
                  theoretical_min_numeric=numeric(0), theoretical_max_numeric=numeric(0),
                  force_integers=logical(0), unit=character(0)))
  }
  df<-tibble::tibble(colname=names(aggregate_types))
  df$label<-purrr::map_chr(aggregate_types, ~.$label)
  df$class<-'numeric'
  df$vartype<-'N'
  df$theoretical_min_numeric<-purrr::map_dbl(aggregate_types, ~as.numeric(.$theoretical_min))
  df$theoretical_max_numeric<-purrr::map_dbl(aggregate_types, ~as.numeric(.$theoretical_max))
  df$force_integers<-FALSE
  df$units<-purrr::map_chr(aggregate_types, ~.$unit)
  return(df)
}

validate_aggregate_types<-function(dt_structure, aggregate_types) {
  for(aggrt in aggregate_types) {
    var_present<-aggrt$all_vars %in% dt_structure$colname
    if(sum(!var_present)>0) {
      stop(paste0("There are ", sum(!var_present), " missing variables in aggregate type ", aggrt$label, " (", aggrt$name, "): ",
                  paste0(aggrt$all_vars[!var_present], collapse=", ")))
    }
  }
}
