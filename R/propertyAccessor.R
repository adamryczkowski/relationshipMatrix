propertyAccessor<-R6::R6Class("propertyAccessor",
  #By default initializes validation learning mode. To serve properties you need to call a private function
  #setup_serve_properties()
  public = list(
    initialize=function() {
      private$mode_=1
      private$property_validators_<-list()
      private$all_properties_<-list()
    },
    get_property=function(property_name, validator) {
      if(private$mode_==1) {
        private$property_validators_[[property_name]]<-validator
        return(NA)
      } else if (private$mode_==2) {
        browser()
        stop("Cannot accept validators and serve properties after accessing the database")
      } else if (private$mode_==3) {
        return(private$all_properties_[[property_name]])
      } else if (private$mode_==4) {
        browser()
        stop("Cannot serve properties after accessing the database")
      } else {
        browser()
        stop("Wrong mode")
      }
    },
    #After calling this functon, no more properties can be accepted
    serve_db=function() {
      if(private$mode_==1) {
        stop("Done") #We trigger the error so we can grab the object
      } else if (private$mode_==2) {
        browser()
        stop("You can't call the serve_db here")
      } else if (private$mode_==3) {
        private$mode_<-4
        return(private$db_)
      } else if (private$mode_==4) {
        return(private$db_)
      } else {
        browser()
        stop("Wrong mode")
      }
    }
  ),
  #Can be accessed with object$.__enclos_env__$private
  private = list(
    all_properties_=NA, #List of all properties
    property_validators_=NA, #Named list with validator object for each property.
    mode_=NA, # 1 - learning the validators, 2 - done learning, exception was already thrown, 3 - serving the properties, 4 - done serving.
    db_=NA, # Database to serve,
    setup_serve_properties=function(db, properties) {
      private$mode_<-3
      testthat::expect_equal(class(properties), 'list')
      private$all_properties_<-properties
      testthat::expect_true('data.frame' %in% class(db))
      private$db_<-db
      private$property_validators_<-NA
    }
  )
)
