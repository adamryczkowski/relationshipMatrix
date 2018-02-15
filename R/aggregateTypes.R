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
      initialize=function(name, label, fn, index_var=character(0), vars=character(0),
                          theoretical_min=NA, theoretical_max=NA, unit=NA) {
        private$name_<-name
        check_aggregateFn(fn)
        private$fn_<-fn
        private$label_<-label
        private$index_var_<-index_var
        private$vars_<-vars
        private$theoretical_min_<-as.numeric(theoretical_min)
        private$theoretical_max_<-as.numeric(theoretical_max)
        private$unit_<-unit
      }
    ),
  active = list(
    name = function() {private$name_},
    fn = function() {private$fn_},
    label = function() {private$label_},
    index_var = function() {private$index_var_},
    vars = function() {private$vars_},
    all_vars = function() {c(private$index_var_, private$vars_)},
    theoretical_min = function() {as.numeric(private$theoretical_min_)},
    theoretical_max = function() {as.numeric(private$theoretical_max_)},
    unit = function() {private$unit_}
  ),

  #Can be accessed with object$.__enclos_env__$private
  private = list(
    name_=NA_character_, #colname of the variable. This is how the variable is going to be identified programmatically.
    fn_=NA, # Function that takes database and index variable of valid rows (just like in bootstrap),
    # and returns value of the aggregate. Standard errors are computed with bootstrap
    label_=NA_character_, #Nice label
    index_var_=character(0), # Variable that will benefit if is indexed
    vars_=character(0), # Other variables that will be used in the aggregate and need to be in the input data frame
    theoretical_min_=NA_real_,
    theoretical_max_=NA_real_,
    unit_=NA_character_
  )
)

make_aggregateTypesDF<-function(aggregate_types) {
  if(length(aggregate_types)==0) {
    return(tibble(colname=character(0), label=character(0), class=character(0), vartype=character(0),
                  theoretical_min_numeric=numeric(0), theoretical_max_numeric=numeric(0),
                  force_integers=logical(0), unit=character(0)))
  }
  df<-tibble(colname=names(aggregate_types))
  df$label<-map_chr(aggregate_types, ~.$label)
  df$class<-'numeric'
  df$vartype<-'N'
  df$theoretical_min_numeric<-map_dbl(aggregate_types, ~as.numeric(.$theoretical_min))
  df$theoretical_max_numeric<-map_dbl(aggregate_types, ~as.numeric(.$theoretical_max))
  df$force_integers<-FALSE
  df$unit<-map_chr(aggregate_types, ~.$unit)
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
