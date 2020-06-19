# this function takes the list output by the cmmppWPP_workflow_one_country_variant and reshapes
# it into a single long data frame 
# This is the preferred way to transfer the output data from R into Eagle

data_reshape_wpp_output_long <- function(wpp_output, locid, variant) {
  
  wpp_output <- wpp_output[1:26]

  out_list <- list()
  for (i in 1:length(wpp_output)) {
    df <- wpp_output[[i]]
    nm <- names(df)
    if (!("indicator" %in% nm)) {
      df$indicator <- names(wpp_output)[i]
    }
    if (!("sex" %in% nm)) {
      df$sex <- "both"
    }
    if (!("age_start" %in% nm)) {
      df$age_start <- 0
      df$age_span  <- 1000
    }
    out_list[[i]] <- df
  }
  wpp_output_long <- do.call(rbind, out_list)
  wpp_output_long$locid <- locid
  wpp_output_long$variant <- variant
  
  wpp_output_long <- wpp_output_long[, c("locid", "variant", "indicator", "time_start", "time_span", "sex",
                                         "age_start", "age_span", "value")]
  
}

# wpp_output_long_eagle <- data_reshape_wpp_output_long(wpp_output = wpp_output_example,
#                                                       locid = 124,
#                                                       variant = "Estimates")
# save(wpp_output_long_eagle, file="ccmppWPP/data/wpp_output_long_eagle.rda")
