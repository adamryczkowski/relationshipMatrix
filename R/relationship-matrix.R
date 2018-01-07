#' relationshipMatrix:  Reads the Excel-compatible relationship matrix, that contain list of all pairwise analysis to do.
#'
#'It doesn't perform any analysis. It is just a scaffolding
#'
#'
#' @docType package
#' @name relationshipMatrix
NULL


# nocov start
.onLoad	<-	function(libname,	pkgname)	{
  op	<-	options()
  op.relationshipMatrix	<-	list(
    property_depvar	=	'depvar',
    property_indepvar = 'indepvar',
    property_groupvar = 'groupvar',
    property_filter = 'filter',
    property_depvar_prefix = 'dv.',
    property_indepvar_prefix = 'iv.'
  )
  toset	<-	!(names(op.relationshipMatrix)	%in%	names(op))
  if(any(toset))	options(op.relationshipMatrix[toset])
  invisible()
}
# nocov end

