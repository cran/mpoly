#' Define a collection of multivariate polynomials.
#'
#' Combine a series of mpoly objects into a mpolyList.
#' 
#' @param ... a series of mpoly objects.
#' @return An object of class mpolyList.
#' @export
#' @examples
#' ( p1 <- mp('t^4 - x') )
#' ( p2 <- mp('t^3 - y') )
#' ( p3 <- mp('t^2 - z') )
#' ( ms <- mpolyList(p1, p2, p3) )
#' is.mpolyList( ms )
mpolyList <- function(...){
  arguments <- as.list(match.call()[-1])  
  out <- lapply(arguments, function(l) get(as.character(l)))
  if(!all(sapply(out, is.mpoly))){
  	stop('each argument must be of class mpoly.', call. = FALSE)
  }
  class(out) <- 'mpolyList'
  out
}
