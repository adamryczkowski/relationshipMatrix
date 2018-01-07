AggregateType<-R6::R6Class("AggregateType",
  #Aggregate type is an aggregation that is treated as a numeric variable
  public = list(
    initialize=function(name, label, fn, index_var=character(0), vars=character(0),
                        theoretical_min=NA, theoretical_max=NA, unit=NA) {
      private$name_<-name
      private$fn_<-fn
      private$label_<-label
      private$index_var_<-index_var
      private$vars_<-vars
      private$theoretical_min_<-theoretical_min
      private$theoretical_max_<-theoretical_max
      private$unit_<-unit
    }
  ),
  active = list(
    name = function() {private$name_},
    fn = function() {private$fn_},
    label = function() {private$label_},
    index_var = function() {private$index_var_},
    vars = function() {private$vars_},
    theoretical_min = function() {private$theoretical_min_},
    theoretical_max = function() {private$theoretical_max_},
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
  df$theoretical_min_numeric<-map_dbl(aggregate_types, ~.$theoretical_min)
  df$theoretical_max_numeric<-map_dbl(aggregate_types, ~.$theoretical_max)
  df$force_integers<-FALSE
  df$unit<-map_chr(aggregate_types, ~.$unit)
  return(df)
}
