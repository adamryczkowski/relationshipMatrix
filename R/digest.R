df_digest<-function(df)
{
  checkmate::checkClass(df, 'data.frame')

  get_attribs<-function(var) {
    type<-
      danesurowe::class2vartype(var = var)
    if(type=='F') {
      return('levels')
    } else if(type%in%c('S', 'L' )) {
      return('labels')
    } else {
      return(character(0))
    }
  }
  calc_digest<-function(var) {
    browser()
    attrs<-c(get_attribs(var), 'class')
    all_a<-attributes(var)
    an<-intersect(names(all_a), attrs)
    attributes(var)<-all_a[an]
    digest::digest(var, algo="md5")
  }

  d<-tryCatch(parallel::mclapply(df , calc_digest),
              error=function(e) e)
  if ('error' %in% class(d))
  {
    browser()
  }
  d<-digest::digest(d[order(names(d))])
  return(d)
}
