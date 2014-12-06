##' This is a wrapper for the normal sum function.
##'
##'
##' The function sums a vector with missing values, but when the whole
##' vector is missing it returns NA rather than zero.
##'
##' @param x The vector to be summed.
##'
##' @export
##' 

sum = function(..., na.rm = !all(is.na(x)){
    base::sum(..., na.rm = na.rm))
}
