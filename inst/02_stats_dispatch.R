#This code only calls dispatcher to get the statistics.
#library(R6) #Not needed actually; set by the task for us
#browser()
dbobj<-relationshipMatrix::ChunkDB$new(chunkdf = chunkdf, depvar = depvar, indepvar = indepvar,
                                       groupvar = groupvar, filtr = filter)
pAcc=propertyAccessor$new(properties=properties, db=dbobj, mode=3)
statistics<-stats_dispatcher(pAcc)
learned_properties<-pAcc$.__enclos_env__$private$get_learned_properties_list()
