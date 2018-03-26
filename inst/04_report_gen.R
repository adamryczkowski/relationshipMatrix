#Code that calls report generating function
#
#Arguments: function name
#browser()
dbobj<-relationshipMatrix::ChunkDB$new(chunkdf = chunkdf, depvar = depvar, indepvar = indepvar,
                                       groupvar = groupvar, filter = filter, filterNA = filterNA,
                                       nrow_total=nrow_total)
pAcc=propertyAccessor$new(properties=properties, db=dbobj, mode=3)
chapter$set_property('cell_hash', pAcc$hash(statistics))
report<-fun(pAcc, statistics, chapter)
