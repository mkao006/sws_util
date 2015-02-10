##' Null to NA
##' 
##' This function takes a vector which may contain NULL values and converts
##' any NULLs to NAs.
##' 
##' @param nullList The input list of data which is to be checked for NULLs.
##' 
##' @return A vector of the same length as nullList, but with NULL replaced
##' with NA.
##' 
##' @export
##' 

NullToNa = function(nullList) {
  if (is.null(nullList) || !is.list(nullList)) {
    warning("nullList is not a list, returning the argument.")
    nullList
  } else {
    if (length(nullList) == 0) {
      vector(mode = "character")
    } else {
      listType =
        unique(na.omit(sapply(nullList,
            FUN = function(x){
              ifelse(is.null(x), NA, typeof(x))
            }
        )))
      ## If no type is available, assume character
      if(length(listType) == 0)
        listType = "character"
      vector = as.vector(rep(NA, length = length(nullList)),
                         mode = listType)
      validEntry = which(sapply(nullList, FUN = function(x) !is.null(x)))
      vector[validEntry] =
        unlist(nullList[validEntry])
      vector
    }
  }
}
