#This code only calls dispatcher to get the statistics.
#library(R6) #Not needed actually; set by the task for us

paprivate<-pAcc$.__enclos_env__$private
paprivate$reinit(propertyAccessor_cannonized, chunkdf)
statistics<-stats_dispatcher(pAcc)
propertyAccessor_cannonized<-paprivate$cannonize()
