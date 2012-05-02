#' Enumerate integer r-vectors summing to n
#'
#' Determine all r-vectors with nonnegative integer entries summing to n
#' 
#' @param n integer to sum to
#' @param r number of cells (dimension of vectors)
#' @return a matrix whose rows are the n-tuples
#' @export
#' @examples
#' burst(4)
#' apply(burst(4), 1, sum) # all sum to 4
#' 
#' burst(4, 4)
#' burst(4, 3)
#' burst(4, 2)
#' burst(4, 1)
#' apply(burst(4,3), 1, sum) # all sum to 4
#' apply(burst(4,2), 1, sum) # all sum to 4
#' apply(burst(4,1), 1, sum) # all sum to 4
#'
#' burst(10, 4) # all possible 2x2 contingency tables with n=10
#' burst(10, 4) / 10 # all possible empirical relative frequencies
#'
burst <- function(n, r = n){
  stopifnot(is.wholenumber(n))
  stopifnot(n > 0)
  stopifnot(is.wholenumber(r))
  stopifnot(r > 0)
  
  parts <- partitions(n)

  partsWOzeros <- apply(parts, 1, function(row) row[row != 0] )
  
  partsWOzeros <- partsWOzeros[sapply(partsWOzeros, length) <= r]
  rvectors <- lapply(partsWOzeros, function(v){
    if(length(v) < r) return(c(v, rep(0, r - length(v))))
    v
  })
  
  unname(as.matrix(plyr:::list_to_dataframe(
    lapply(rvectors, function(x) as.data.frame(permutations(x)))
  )))
}