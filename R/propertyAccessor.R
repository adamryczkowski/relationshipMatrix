#mode=1: discovery mode. Only records property access.
#mode=2: discovery mode finished. Only proprty list can be retrieved
#mode=3: runtime mode with setting custom properties. Serves the properties and allows setting them
#mode=4: runtime mode with setting custom properties after accessing the database. Disallows getting the properties, but allows setting them
#mode=5: runtime mode readonly. Only serves the properties and database. No discovery possible.

propertyAccessor<-R6::R6Class("propertyAccessor",
  #By default initializes validation learning mode. To serve properties you need to call a private function
  #setup_serve_properties()
  public = list(
    initialize=function(db, properties) {
      checkmate::checkClass(properties, 'list')
      private$mode_<-1
      private$property_validators_<-list()
      private$all_properties_<-properties
      private$reverse_vars_<-FALSE
      private$db_<-db
    },
    reverse_vars=function() {
      private$reverse_vars_<-TRUE
    },
    is_reversed=function() {
      private$reverse_vars_
    },
    get_property=function(property_name, validator=identity) {
      if(private$mode_==1) {
        private$property_validators_[[property_name]]<-validator
        return(private$all_properties_[[property_name]])
        return(NA)
      } else if (private$mode_==2) {
        browser()
        stop("Cannot accept validators and serve properties after accessing the database")
      } else if (private$mode_%in% c(3,5)) {
        return(private$all_properties_[[property_name]])
      } else if (private$mode_==4) {
        browser()
        stop("Cannot serve properties after accessing the database")
      } else {
        browser()
        stop("Wrong mode")
      }
    },
    put_property=function(property_name, value) {
      if(private$mode_ %in% c(1,3) ) {
        private$all_properties_[[property_name]]<-value
        return(TRUE)
      } else if (private$mode_==2) {
        browser()
        stop("Cannot accept validators and serve properties after accessing the database")
      } else if (private$mode_==4) {
        browser()
        stop("Cannot serve properties after accessing the database")
      } else if (private$mode_==5) {
        stop("Cannot set the property in this mode")
      } else {
        browser()
        stop("Wrong mode")
      }
    },
    #After calling this functon, no more properties can be accepted
    serve_db=function() {
      db<-private$db_
      if(private$reverse_vars_) {
        db<-db$reversed()
      }
      if(private$mode_==1) {
        return(db)
#        stop("Done") #We trigger the error so we can grab the object
      } else if (private$mode_==2) {
        browser()
        stop("You can't call the serve_db here")
      } else if (private$mode_==3) {
        private$mode_<-4
        return(db)
      } else if (private$mode_ %in% c(4,5)) {
        return(db)
      } else {
        browser()
        stop("Wrong mode")
      }
    }
  ),
  #Can be accessed with object$.__enclos_env__$private
  private = list(
    all_properties_=NA, #List of all properties
    reverse_vars_=FALSE,
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
    },
    cannonize=function() {
      ans<-list()
      if(length(private$all_properties_)>0) {
        cnames<-order(names(private$all_properties_))
        props<-private$all_properties_[cnames]
      } else {
        props<-list()
      }
      ans<-list(properties=props, reversed=private$reverse_vars_)
    },
    reinit=function(initlist, db, newmode=3) {
      if(!'list' %in% class(initlist)) {
        browser()
      }
      if(is.null(db)) {
        browser()
      }
      if(!all(c('properties', 'reversed') %in% names(initlist))) {
        browser()
        stop("Missing some properties in the cannonized object")
      }
      private$mode_<-newmode
      private$property_validators_<-list()
      private$all_properties_<-initlist$properties
      private$reverse_vars_<-initlist$reversed
      private$db_<-db
      dbprivate<-private$db_$.__enclos_env__$private
      if(newmode==1) {
        dbprivate$flag_never_serve_df_<-TRUE
      } else {
        dbprivate$flag_never_serve_df_<-FALSE
      }
    }
  )
)
