
# take output that is organized by year in ccmpp_output and reorganize by indicator
data_gather_ccmpp_output <- function(ccmpp_output, object, 
                                     time_start = time_start, 
                                     time_end = time_end, 
                                     time_span = time_span, 
                                     ids) {
  
  df       <- data.frame(sapply(ccmpp_output, "[[", object))
  df       <- reshape(df, 
                      varying=list(1:ncol(df)), 
                      ids = ids,
                      times = seq(time_start, time_end, time_span),
                      timevar = "time_start",
                      v.names="value", direction = "long")
  names(df)[ncol(df)] <- deparse(substitute(ids))
  
  return(df)
}

