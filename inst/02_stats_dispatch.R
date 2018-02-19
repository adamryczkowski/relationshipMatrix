#This code only calls dispatcher to get the statistics.
#library(R6) #Not needed actually; set by the task for us
#browser()
pAcc=propertyAccessor$new(properties=all_properties, db=chunkdf, mode=3)
statistics<-stats_dispatcher(pAcc)
