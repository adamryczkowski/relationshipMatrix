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
    relationshipMatrix.chunkdf_properties=list(
      depvar	=	'depvar',
      indepvar = 'indepvar',
      groupvar = 'groupvar',
      filter = 'filter'),
    relationshipMatrix.property_dispatcher = 'dispatcher',
    relationshipMatrix.property_depvar_prefix = 'dv.',
    relationshipMatrix.property_indepvar_prefix = 'iv.',
    relationshipMatrix.is_aggregate = 'is_aggregate',
    relationshipMatrix.reversed_vars = 'reversed_vars',
    relationshipMatrix.prefix = 'prefix'
  )
  toset	<-	!(names(op.relationshipMatrix)	%in%	names(op))
  if(any(toset))	options(op.relationshipMatrix[toset])
  invisible()
}
# nocov end

