Filter<-R6::R6Class("Filter",
  #Class that encapuslates filter and all its properties
  public = list(
    initialize=function(filterstring, label) {
      private$filterstring_<-filterstring
      private$label_<-label
    }
  ),
  active = list(
    filterstring = function() {private$filterstring_},
    label = function() {private$label_}
  ),
  private = list(
    filterstring_=NA_character_,
    label_=NA_character_
  )
)
