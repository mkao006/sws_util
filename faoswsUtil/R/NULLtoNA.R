##' Turn list of NULL to vector of NA
##'
##' This is a temporary solution where empty cells are handled as list
##' of NULL.
##'
##' @param nullList The list of NULL
##'
##' @export
NULLtoNA = function(nullList){
    vector = rep(NA, length = length(nullList))
    validEntry = which(sapply(nullList, FUN = function(x) !is.null(x)))
    vector[validEntry] =
        unlist(nullList[validEntry])
    vector
}
