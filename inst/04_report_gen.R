#Code that calls report generating function
#
#Arguments: function name
#browser()
pAcc=propertyAccessor$new(properties=all_properties, db=chunkdf, mode=3)
report<-fun(pAcc, statistics, chapter)
