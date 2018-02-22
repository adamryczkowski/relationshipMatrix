#Code that calls report generating function
#
#Arguments: function name
#browser()
dbobj<-relationshipMatrix::ChunkDB$new(chunkdf = chunkdf, depvar = depvar, indepvar = indepvar,
                                       groupvar = groupvar, filtr = filter)
pAcc=propertyAccessor$new(properties=properties, db=dbobj, mode=3)
report<-fun(pAcc, statistics, chapter)
