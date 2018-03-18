#This code generates object of type chunkdf. This object contains the database chunk together with all the necessary metadata.

#TODO: Nie zamierzam rozdzielać metadanych zmiennych bazy danych od samych cyferek. Mogę jednak oddzielić nazwę bazy danych i nazwę filtra.
#Muszę też zastanowić się nad formą przekazania bazy danych do obiektu, aby uzyskać maksymalnie kanoniczną bazę danych. Prawdopodobnie powinienem posortować
#nazwy zmiennych. Muszę też jakoś przekazać metadane filtra.

#Parameters: variable list, filter, dt (dt jako oddzielny plik lub inny task)
#Returns: chunkdf

get_chunkdf<-function(variables, filterstring=NULL, df) {
#  browser()
  if(!is.null(filterstring) && filterstring!='') {
    #    if('character' %in% class(filterstring)) {
    #      filterstring<-parse(text = filterstring)
    #    }
    filterobj<-rlang::parse_quo(filterstring, env = parent.frame())
    filterNA<-sum(is.na(dplyr::transmute(df, filter=!!filterobj)[[1]]))

    chunkdf<-dplyr::filter(df, !!filterobj)
    #    chunkdf<-dplyr::filter(df, !! filterstring)
  } else {
    chunkdf<-df
    filterNA<-0
  }

  chunkdf<-tibble::as_tibble(chunkdf)[variables]

  return(list(chunkdf=tibble::as_tibble(chunkdf), filterNA=filterNA))
}

chunkdf<-get_chunkdf(variables, filterstring, df)
