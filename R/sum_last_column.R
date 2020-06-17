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

sum_five_year_age_groups <- function(indata, byvar) {
  
  # deaths
  age_start                  <- floor(indata$age_start/5)*5
  df5    <- sum_last_column(cbind(age_start,
                                  indata[,c(byvar, "value")]))
  df5$age_span <- ifelse(df5$age_start == max(df5$age_start),
                                             1000,
                                             5)
  df5    <- df5[, c(byvar, "age_start", "age_span", "value")]
  
  return(df5)
  
}


