#' Compute sum of one column of a data frame by the values of the other columns in the data frame
#'
#' @description This is a general function that computes the sum of the last (rightmost) column of a data frame
#' indexed by the combinations of values of all of the preding columns of that data frame.
#'
#' @author Sara Hertog
#'
#' @param indata data frame. last (rightmost) column must be numeric.
#' 
#' @return a data frame with same names as indata for which the last column is the computed sum of values in the last
#' column of indata
#' @export
sum_last_column <- function(indata) {
  
  bylist <- list()
  for (i in 1:(ncol(indata)-1)) {
    bylist[[i]] <- indata[, i]
  }
    
  outdata <- aggregate(indata[,ncol(indata)],
                       by = bylist,
                       FUN = sum)
  names(outdata) <- names(indata)
  return(outdata)
}




