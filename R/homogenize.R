#' Homogenize a polynomial
#'
#' Homogenize a polynomial.
#'
#' @param x an mpoly object, see [mpoly()]
#' @param indeterminate name of homogenization
#' @return a (de/homogenized) mpoly or an mpolyList
#' @name homogenize
#' @examples
#'
#' x <- mp("x^4 + y + 2 x y^2 - 3 z")
#' is.homogeneous(x)
#' (xh <- homogenize(x))
#' is.homogeneous(xh)
#'
#' homogeneous_components(x)
#'
#' homogenize(x, "o")
#'
#' xh <- homogenize(x)
#' dehomogenize(xh) # assumes indeterminate = "t"
#' plug(xh, "t", 1) # same effect, but dehomogenize is faster
#'
#'
#'
#' # the functions are vectorized
#' (ps <- mp(c("x + y^2", "x + y^3")))
#' (psh <- homogenize(ps))
#' dehomogenize(psh)
#'
#'
#' # demonstrating a leading property of homogeneous polynomials
#' library(magrittr)
#' p  <- mp("x^2 + 2 x + 3")
#' (ph <- homogenize(p, "y"))
#' lambda <- 3
#' (d <- totaldeg(p))
#' ph %>%
#'   plug("x", lambda*mp("x")) %>%
#'   plug("y", lambda*mp("y"))
#' lambda^d * ph
#' 



#' @rdname homogenize
#' @export
homogenize <- function(x, indeterminate = "t"){
  
  if (is.mpolyList(x)) {
    hs <- lapply(x, homogenize, indeterminate = indeterminate)
    class(hs) <- "mpolyList"
    return(hs)
  }
  
  if(!is.mpoly(x)) stop("homogenize requires mpoly objects.", call. = FALSE)
  
  term_exps <- exponents(x)
  term_degs <- vapply(term_exps, sum, numeric(1))
  max_deg   <- max(term_degs)
  
  vars <- vars(x)
  p <- length(vars)
  
  homo_deg_per_term <- max_deg - term_degs

  for(k in 1:length(x)){
    if(homo_deg_per_term[[k]] == 0) next
    term <- x[[k]]
    coef_ndx <- which(names(term) == "coef")
    homo_piece <- homo_deg_per_term[[k]]
    names(homo_piece) <- indeterminate
    x[[k]] <- c(term[-coef_ndx], homo_piece, term[coef_ndx])
  }
  
  x
  
}







#' @rdname homogenize
#' @export
dehomogenize <- function(x, indeterminate = "t"){
  
  if (is.mpolyList(x)) {
    hs <- lapply(x, dehomogenize, indeterminate = indeterminate)
    class(hs) <- "mpolyList"
    return(hs)
  }
  
  if(!is.mpoly(x)) stop("dehomogenize requires mpoly objects.", call. = FALSE)
  # if(missing(indeterminate)) stop("dehomogenize requires a variable to dehomogenize.", call. = FALSE)
  
  # check for inhomogeneous
  if (!is.homogeneous(x)) {
    stop("polynomial ", print.mpoly(x, silent = TRUE), " is not homogeneous.", call. = FALSE)
  }
  
  # dehomogenize
  # plug(x, indeterminate, 1) # works, but is not optimized, so 
   xdh <- lapply(x, function(term){
    var_ndx <- which(names(term) == indeterminate)
    if (length(var_ndx) == 0) {
      return(term) # indeterminate not in term  
    } else {
      return(term[-var_ndx]) 
    }
  })
  
  class(xdh) <- "mpoly"
  xdh
}




#' @rdname homogenize
#' @export
is.homogeneous <- function(x) {
  if (is.mpolyList(x)) return(vapply(x, is.homogeneous, logical(1)))
  term_total_degs <- vapply(exponents(x), sum, numeric(1))
  all(term_total_degs == term_total_degs[1])
}



#' @rdname homogenize
#' @export
homogeneous_components <- function(x){
  exps <- exponents(x)
  exps_mat <- t(simplify2array(exps))
  term_total_degs <- rowSums(exps_mat)
  n_terms <- length(x)
  term_ndcs <- split(1:n_terms, term_total_degs)
  homo_comps <- lapply(term_ndcs, function(ndcs){
    x[ndcs]
  })
  homo_comps <- unname(homo_comps)
  class(homo_comps) <- "mpolyList"
  homo_comps
}
