# this function takes the list output by the cmmppWPP_workflow_one_country_variant and reshapes
# it into a single long data frame 
# This is the preferred way to transfer the output data from R into Eagle

data_reshape_wpp_output_long <- function(wpp_output) {
  
  # identify global parameters from attributes
  atr <- attributes(wpp_output)
  atr <- atr[!(names(atr) == "names")]
  
  global <- data.frame(indicator = rep("global_parameters", length(atr)),
                       subindicator = names(atr),
                       time_start = "",
                       time_span = "",
                       sex = "",
                       age_start = "",
                       age_span = "",
                       value = do.call(rbind, atr))
  
  wpp_output <- wpp_output[!(names(wpp_output) %in% c("mig_parameter", "mig_net_count_age_sex_override"))]
  
  out_list <- list()
  
  for (i in 1:length(wpp_output)) {
    df <- wpp_output[[i]]
    nm <- names(df)
    if ("indicator" %in% nm) {
      names(df)[names(df) == "indicator"] <- "subindicator"
    } else if (!("indicator" %in% nm)) {
      df$subindicator <- ""
    }
    
    df$indicator <- names(wpp_output)[i]
    
    if (!("sex" %in% nm)) {
      df$sex <- "both"
    }
    if (!("age_start" %in% nm)) {
      df$age_start <- 0
      df$age_span  <- -1
    }
    
    out_list[[i]] <- df
  }
  wpp_output_long <- rbind(global, do.call(rbind, out_list))
  
  # reduce size of file by shortening some fields
  wpp_output_long$sex <- substr(wpp_output_long$sex,1,1)
  wpp_output_long$age_span[which(wpp_output_long$age_span == 1000)] <- -1

  return(wpp_output_long)
  
}

# wpp_output_example <- ccmppWPP_workflow_one_country_variant(wpp_input = wpp_input_example)
# 
# wpp_output_long_eagle <- data_reshape_wpp_output_long(wpp_output = wpp_output_example)




