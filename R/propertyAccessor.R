#mode=1: discovery mode. Records and sets property access.
#mode=2: discovery mode finished. Only proprty list can be retrieved
#mode=3: runtime mode readonly. Only serves the properties and database.
#mode=4: runtime mode readonly, after passing the "done_discovery" mark. Will trigger any attempt to set any property

propertyAccessor<-R6::R6Class("propertyAccessor",
  #By default initializes validation learning mode. To serve properties you need to call a private function
  #setup_serve_properties()
  public = list(
    initialize=function(db, properties=list(), mode=1) {
      checkmate::checkClass(properties, 'list')
      checkmate::checkClass(db, 'ChunkDB')
      dbprivate<-db$.__enclos_env__$private
      if(mode==1) {
        private$property_validators_<-list()
        private$all_properties_<-properties
        private$report_dispatcher_<-NULL
        dbprivate$flag_never_serve_df_<-TRUE
      } else if (mode==3) {
        dbprivate$flag_never_serve_df_<-FALSE
        private$all_properties_<-properties
        if('.reversed' %in% names(properties)) {
          private$reverse_vars_<-properties$.reversed
          properties$.reversed<-NULL
        }
        if('.report_dispatcher' %in% names(properties)) {
          private$report_dispatcher_<-properties$.report_dispatcher
          properties$.report_dispatcher<-NULL
        }
      } else {
        stop("Illegal mode. Valid values: 1 and 3.")
      }
      dbprivate$metaserver_<-self
      private$mode_<-mode
      private$db_<-db
    },
    reverse_vars=function() {
      private$reverse_vars_<-TRUE
    },
    is_reversed=function() {
      private$reverse_vars_
    },
    get_property=function(property_name, validator=identity, default_value=NA) {
      if(private$mode_==1) {
        private$property_validators_[[property_name]]<-validator
        if(!property_name %in% names(private$all_properties_)) {
          if(!is.na(default_value)) {
            private$all_properties_[[property_name]]<-default_value
          } else {
            warning(paste0("There is no property '", property_name, "'. Will use return NA."))
            private$all_properties_[[property_name]]<-NA
          }
        }
        return(private$all_properties_[[property_name]])
      } else {
        if(!property_name %in% names(private$all_properties_)) {
          stop(paste0("There is no property '", property_name, "'. Ask for the property during the discovery mode."))
        }
        if (private$mode_==2) {
          browser()
          stop("Cannot accept validators and serve properties after accessing the database")
        } else if (private$mode_%in%c(3,4)) {
          return(private$all_properties_[[property_name]])
        } else {
          browser()
          stop("Wrong mode")
        }
      }
    },
    set_report_dispatcher=function(report_dispatcher) {
      checkmate::checkClass(report_dispatcher, 'function')
      if('character' %in% class(private$report_dispatcher_)) {
        browser()
        stop("Cannot set report dispatcher here. Use it only in statistics dispatcher.")
      }
      if(private$mode_==1) {
        private$report_dispatcher_<-report_dispatcher
        return(invisible(NULL))
      } else if (private$mode_%in%c(2,4)) {
        browser()
        stop("Cannot accept report_dispatcher function after discovery")
      } else if (private$mode_==3) {
        #do nothing. Report dispatcher already set.
      } else {
        browser()
        stop("Wrong mode")
      }
    },
    put_property=function(property_name, value) {
      if(private$mode_ %in% c(1,3) ) {
        private$all_properties_[[property_name]]<-value
        private$property_validators_[[property_name]]<-NA
        return(TRUE)
      } else if (private$mode_ %in% c(2,4)) {
        browser()
        stop("Cannot accept validators and serve properties after accessing the database")
      } else {
        browser()
        stop("Wrong mode")
      }
    },
    done_discovery=function() {
      if(private$mode_==1) {
        private$mode_<-2
        stop("Done discovery mode")
      } else if(private$mode_ %in% c(2,4) ) {
        #Do nothing. Exiting the discovery mode many times is legal.
      } else if (private$mode_ == 3) {
        private$mode_<-4
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
      return(db)
    }
  ),
  active = list(
    report_dispatcher = function() {private$report_dispatcher_}
  ),
  #Can be accessed with object$.__enclos_env__$private
  private = list(
    all_properties_=NA, #List of all properties
    reverse_vars_=FALSE,
    property_validators_=NA, #Named list with validator object for each property.
    mode_=NA, # 1 - learning the validators, 2 - done learning, exception was already thrown, 3 - serving the properties, 4 - done serving.
    db_=NA, # Database to serve,
    report_dispatcher_=NULL,
    #Returns all information specified by the user during the discovery phase. Needed after the discovery,
    #to get list that will get pushed to the depwalker to build propertyAccessor in mode 3
    get_discovered_properties_list=function(flag_include_dbreversal=TRUE) {
      if(private$mode_!=2) {
        browser()
      }

      #Insert all accessed properties
      proplist<-names(private$property_validators_)
      record<-list()
      if(length(private$property_validators_)>0) {
        for(prop in sort(names(private$property_validators_))) {
          value<-private$all_properties_[[prop]]
          validfn<-private$property_validators_[[prop]]
          ans<-tryCatch(
            validfn(value),
            error = function(e) {}
          )
          if( 'error' %in% class(ans)) {
            stop(paste0("Errors when processing cellsdf, row ", cellnr, ". Argument ", prop, " with value «", value, "» did not pass the validation function set by the dispatcher ", dname, ". "))
          }
          record[[prop]]<-value
        }
      }
      if(flag_include_dbreversal) {
        record<-c(record, list(.reversed=private$reverse_vars_, .report_dispatcher=private$report_dispatcher_))
      } else {
        record<-c(record, list(.reversed=FALSE, .report_dispatcher=private$report_dispatcher_))
      }
      cnames<-order(names(record))
      props<-record[cnames]
      return(record)
    }
  )
)

validate_bool<-function(value) {
  as.boolean(value)
}

validate_int<-function(value) {
  as.integer(value)
}
