##' Turn list of NULL to vector of NA
##'
##' This is a temporary solution where empty cells are handled as list
##' of NULL.
##'
##' @param nullList The list of NULL
##'
##' @export

NULLtoNA = function(nullList){
    ## Assuming the list contains only one type and NULL
    listType =
        unique(na.omit(sapply(nullList,
                              FUN = function(x){
                                  ifelse(is.null(x), NA, typeof(x))
                              }
                              )
                       )
               )
    ## NOTE (Michael): Default the column to character, the reasoning
    ##                 is because most of the column which has all of
    ##                 its value as NULL usually belong to the flag
    ##                 cloumn.
    if(length(listType) == 0)
        listType = "character"
    vector = as.vector(rep(NA, length = length(nullList)),
        mode = listType)
    validEntry = which(sapply(nullList, FUN = function(x) !is.null(x)))
    vector[validEntry] =
        unlist(nullList[validEntry])
    vector
}
