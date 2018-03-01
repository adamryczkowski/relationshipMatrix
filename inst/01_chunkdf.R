#This code generates object of type chunkdf. This object contains the database chunk together with all the necessary metadata.

#TODO: Nie zamierzam rozdzielać metadanych zmiennych bazy danych od samych cyferek. Mogę jednak oddzielić nazwę bazy danych i nazwę filtra.
#Muszę też zastanowić się nad formą przekazania bazy danych do obiektu, aby uzyskać maksymalnie kanoniczną bazę danych. Prawdopodobnie powinienem posortować
#nazwy zmiennych. Muszę też jakoś przekazać metadane filtra.

#Parameters: variable list, filter, dt (dt jako oddzielny plik lub inny task)
#Returns: chunkdf

get_chunkdf<-function(variables, filterstring=NULL, df) {
  #browser()
  if(!is.null(filterstring) && filterstring!='') {
    #    if('character' %in% class(filterstring)) {
    #      filterstring<-parse(text = filterstring)
    #    }
    chunkdf<-dplyr::filter_(df, filterstring)
    #    chunkdf<-dplyr::filter(df, !! filterstring)
  } else {
    chunkdf<-df
  }
  return(as_tibble(chunkdf)[variables])
}

chunkdf<-get_chunkdf(variables, filterstring, df)
