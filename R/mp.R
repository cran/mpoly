#' Define a multivariate polynomial.
#'
#' mp is a smart function which attempts to create a formal mpoly object from a character string containing the usual representation  of a multivariate polynomial.
#' 
#' @param string a character string containing a polynomial, see examples
#' @param varorder (optional) order of variables in string
#' @return An object of class mpoly.
#' @author David Kahle \email{david.kahle@@gmail.com}
#' @seealso \code{\link{mpoly}}
#' @export
#' @examples
#' ( m <- mp('x + y + x y') )
#' is.mpoly( m )
#' unclass(m)
#'
#' mp('x - y')
#' mp('x - 1')
#' mp('x +      y')
#' mp('x -      5')
#' mp('x - -5')
#' mp('10 x 6 x') # -> 60 x^2
#' mp('10 x 6 x + 10 x 6 x y y 2') # -> 60 x^2  +  120 x^2 y^2
#'
#' mp('x^2 + x^2 y') # -> x^2  +  x^2 y
#'
#' mp('x - x') # -> 0
#' mp('x - 4 x') # -> -3 x
#' mp('x y^2 - y^2 x') # -> 0
#' 
#' mp('5^2') # -> 25
#' mp('2^2 x + 5^2 + 3^2') # -> 4 x  +  34
#' mp('1 1') # -> 1
#' mp('1  3 5^2 + 2 3^4 x') # -> 75  + 162 x
#'
#' ( ms <- mp(c('x + y', '2 x')) )
#' is.mpolyList(ms)
#'
#' mp('10 x + 2 y 3 + x^2 5 y') # -> 10 x  +  6 y  +  5 x^2 y
#' mp('x + 2 y + x^2 y + x y z') # -> x  +  2 y  +  x^2 y  +  x y z
#' mp('x + 2 y + x^2 y + x y z', varorder = c('y', 'z', 'x')) # -> x  +  2 y  +  y  +  y z x
#' #mp('x + 2 y + x^2 y', varorder = c('q', 'p')) # -> error
#'
#' mp('p111 + p121 2 p112^2')
#' unclass(mp('p111 + p121 2 p112^2'))
#'
#' mp('0')
#' mp('2')
#' mp('-5')
#' mp('-4 x')
#' mp('y + -1 x')
#' mp('-1 + x')
#'
#' gradient( mp('x + 2 y + x^2 y + x y z') ) 
#'
#' # mp and the print methods are kinds of inverses of each other
#' ( polys <- mp(c('x + y', 'x - y')) )
#' strings <- print(polys)
#' strings
#' mp(strings)
#'
#' # possible specification syntax issues -
#' mp('x + 4x') # note the 4x as opposed to 4 x
#' mp('x - 4x') # same
#' mp('x -1') # -> x  -  11
#'
#' # future -
#' #mp('(x-2)^2')
mp <- function(string, varorder){

  stopifnot(is.character(string))
  
  # if string is a vector of polys, return mpolyList
  if(length(string) > 1){
  	if(missing(varorder)){
  	  mpolyList <- lapply(
    	  as.list(string),
    	  mp
      )
    } else {
  	  mpolyList <- lapply(
    	  as.list(string),
    	  mp,
    	  varorder = varorder
      )    	
    }
    class(mpolyList) <- 'mpolyList'
    return(mpolyList)
  }
  
  # trim string
  string <- str_trim(string)

  # constants - no variables, allows mp('-5') mp('5') mp('3^2') mp('2 1')
  if(!str_detect(string, "[a-zA-Z]")){
  	if(str_detect(string, "\\+")){
  	  string <- str_trim(strsplit(string, "\\+")[[1]])
  	}
    string <- gsub('[ ]{2,}', ' ', string)
    string <- gsub('[ ]{1}', '*', string)
    string <- paste(string, collapse = "+")  	
    return(mpoly( list(c(coef = eval(parse(text = string)))) ))
  }
 
  # fix negatives
  if(substr(string, 1, 1) == '-'){ # starting with a negative coefficient
    string <- paste('0 + ', string, sep = '')
  }
  string <- gsub('\\+ \\-', '- ', string) # negative coefficients
  string <- gsub('\\- \\-', '+ ', string) # minus negative
  string <- gsub('\\-', '+ -1', string) # subtraction
  
  # fix white space (shrink x +   y to x + y)
  string <- gsub('[ ]{2,}', ' ', string)
  
  # situations such as (x + y) (x - y)
  if(str_detect(string, '\\(') || str_detect(string, '\\)')){
    stop('parenthetical expressions not currently handled.')
  }
  
  # division
  if(str_detect(string, '/')){
    stop('expressions with division are not currently handled (use decimals).')
  }  
  
  
  ## begin real work
   
  # separate terms
  string <- paste(' ', string, ' ', sep = '') # prep for lapply to come
  l <- as.list(strsplit(string, '\\+')[[1]])  
  terms <- lapply(l, function(s){
    n <- nchar(s)
    substr(s, 2, n - 1)	
  })
  
  # burst terms
  elements <- lapply(terms, function(string){
    strsplit(string, ' ')[[1]]
  })
  if(length(elements[[1]]) == 1 && elements[[1]] == '0'){ # fix - first coef
  	elements <- elements[2:length(elements)]
  }

  # fix exponents, division, multiplication of constants
  parseNumberElem <- function(x) as.character(eval(parse(text = x)))
  
  elements <- lapply(elements, function(x){

  	varElems <- str_detect(x, "[a-zA-Z]")
  	
  	# parse only number elements e.g. c("1", "3", "5^2")
  	if(!any(varElems)){
  	  x <- paste(x, collapse = "*")
  	  return(parseNumberElem(x))
  	}
  	
  	# combine coefficient elements
  	xCoefs <- x[!varElems]
  	xCoef <- parseNumberElem(paste(xCoefs, collapse = "*"))
  	
  	#
  	c(xCoef, x[varElems])
  })

  
  # determine variables
  pre_vars <- unlist(elements)
  var_ndxs <- substr(pre_vars, 1, 1) %in% c(letters, LETTERS)
  pre_vars <- unique(pre_vars[var_ndxs])
  pre_vars <- gsub('\\^[0-9]+', '', pre_vars) # remove exponents
  vars <- unique(pre_vars)

  
  # set number of variables in string
  p <- length(vars)
  
  
  # populate data frame for mpoly
  l <- lapply(elements, function(v){
  	
    # get coefficient (the code takes care of missing coefs using defaults)
    asNum <- suppressWarnings(as.numeric(v))
    coef <- prod(asNum, na.rm = TRUE)
    if(sum(is.na(asNum)) > 0){ 
      v <- v[is.na(asNum)]
    } else {
      return(c(coef = coef))	
    }

    # parse degrees
    v <- sapply(strsplit(v, '\\^'), function(z){
      if(length(z) == 1) z <- c(z, 1)
      out <- as.numeric( z[2] )
      names(out) <- z[1]
      out
    })

    c(v, coef = coef)
  })
  

  # mpoly 
  out <- mpoly(l)
  
  # check varorder argument
  if(!missing(varorder)){
  	
    if(!all(vars %in% varorder)){
      error <- paste(
        'if specified, varorder must contain all computed vars - ',
        paste(vars, collapse = ', '),
        sep = ''
      )
      stop(error, call. = FALSE)
    }
    
    # order vars appropriately
    vars <- intersect(varorder, vars)
    out <- reorder.mpoly(out, varorder = vars)
  }  
  
  # return
  out
}



