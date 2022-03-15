#' life table helper functions for WPP
#' 
#' #' Take life tables output from lt functions in DemoTools and reshape to long data frame
#'
#' @description This function reshapes the wide life table data frame returned by the DemoTools functions 
#' lt_abridged() or lt_single_mx() and retuns a long data frame with the "indicator", "age_start", "age_span", 
#' and value" columns used for WPP.
#'
#' @author Sara Hertog
#'
#' @param life_table_df data frame. life table output by the lt_abridged or lt_single_mx functions of DemoTools.
#'
#' @return a long data frame with columns "indicator" containing the life table column label, "age_start" and 
#' "age_span", and "value"
#' containing the numeric value associated with the life table indicator
#' @export
lt_long <- function(life_table_df) {
  
  lt_long <- reshape(life_table_df, idvar="Age", 
                     varying=list(3:ncol(life_table_df)), 
                     times=names(life_table_df)[3:ncol(life_table_df)],
                     timevar = "indicator",
                     v.names="value", direction = "long")
  lt_long$age_start <- lt_long$Age
  lt_long$age_span  <- c(diff(lt_long$age_start), NA)
  lt_long$age_span[which(lt_long$age_start == max(lt_long$age_start))] <- 1000
  lt_long$indicator <- paste0("lt_", lt_long$indicator)
  
  lt_long <- lt_long[, c("indicator", "age_start", "age_span", "value")]
  
  return(lt_long)
  
}


#' Compute summary life table values
#'
#' @description This function computes selected summary life table values from the output of DemoTools life table
#' function lt_abridged() or lt_single_mx() that has been transformed to a long dataset with the lt_long() function.
#'
#' @author Sara Hertog
#'
#' @param lt_data data frame. column "indicator" is the label for the life table column, age" is age group 
#' and "value" is is the value of the given life table indicator. 
#' @param byvar character. string of column names in lt_data by which the life table values are computed.
#'
#' @details age/period can be 1x1 or 5x5.
#'
#' @return a data frame with "indicator" labeling the life table summary measure and "value" containing the value of 
#' that measure, computed by byvar columns.
#' @export
lt_summary <- function(lt_data, byvar) {
  
  # compute summary qx from lx
  lt_lx           <- lt_data[lt_data$indicator=="lt_lx", c(byvar, "age_start", "value")]
  lt_lx           <- reshape(lt_lx, idvar=byvar, v.names="value", timevar="age_start", direction="wide")
  lt_lx$lt_1q0    <- 1-(lt_lx$value.1 / lt_lx$value.0)
  lt_lx$lt_4q1    <- 1-(lt_lx$value.5 / lt_lx$value.1)
  lt_lx$lt_5q0    <- 1-(lt_lx$value.5 / lt_lx$value.0)
  lt_lx$lt_10q15  <- 1-(lt_lx$value.25 / lt_lx$value.15)
  lt_lx$lt_10q25  <- 1-(lt_lx$value.35 / lt_lx$value.25)
  lt_lx$lt_15q35  <- 1-(lt_lx$value.50 / lt_lx$value.35)
  lt_lx$lt_35q15  <- 1-(lt_lx$value.50 / lt_lx$value.15)
  lt_lx$lt_45q15  <- 1-(lt_lx$value.60 / lt_lx$value.15)
  
  lt_lx           <- lt_lx[, c(byvar, "lt_1q0", "lt_4q1", "lt_5q0", "lt_10q15", "lt_10q25", 
                               "lt_15q35", "lt_35q15", "lt_45q15")]
  lt_lx           <- reshape(lt_lx, 
                             idvar=byvar,  
                             varying=list((length(byvar)+1):ncol(lt_lx)), 
                             times=names(lt_lx)[(length(byvar)+1):ncol(lt_lx)],
                             timevar = "indicator",
                             v.names="value", direction = "long")
  
  label <- stringr::str_split_fixed(lt_lx$indicator, "_", 2)[,2]
  label <- stringr::str_split_fixed(label, "q", 2)
  lt_lx$age_start <- as.numeric(label[,2])
  lt_lx$age_span  <- as.numeric(label[,1])
  
  # extract summary ex
  lt_ex           <- lt_data[lt_data$indicator=="lt_ex" & lt_data$age_start %in% c(0,15,50,60,65,80,85,100,110), 
                             c(byvar, "age_start", "value")]
  lt_ex$indicator <- paste0("lt_e", lt_ex$age_start)
  lt_ex$age_span  <- 1000
  lt_ex           <- lt_ex[, c(byvar, "indicator", "age_start", "age_span", "value")]
  
  lt_summary      <- rbind(lt_lx, lt_ex)
  
  return(lt_summary)
  
}

#' Compute complete life tables for multiple time periods
#' 
#' @description This function computes complete life table values, looping over time periods and using the 
#' lt_single_mx() function in DemoTools.
#'
#' @author Sara Hertog
#'
#' @param mx data frame. "value" column contains age-specific mortality rates by time_start and age_start for one sex. 
#' @param sex character. string indicating "m" male, "f" female, or "b" both sexes.
#' @param a0rule character. string "ak" for andreev-kinkaid and "cd" for coale-demeny estimation of 1a0.
#' @param OAnew numeric. The starting age for the new open age group.
#'
#' @return a data frame with "indicator" labeling the life table column name and "value" containing the value of 
#' that measure, by single year of age_start and time_start
#' @export
lt_complete_loop_over_time <- function(mx, sex, a0rule = "ak", OAnew = 130) {
  
  # initialize output list
  lt_output_list <- list()
  n <- 0
  
  time_span              <- 1
  time_start             <- min(mx$time_start)
  time_end               <- max(mx$time_start + time_span)
  age_start              <- unique(mx$age_start) 
  
  for (time in seq(time_start, time_end-time_span, time_span)) {
    
    n   <- n+1
    
    maxage <- max(mx$age_start[which(mx$time_start == time)])
    lt <- DemoTools::lt_single_mx(nMx = mx$value[which(mx$time_start == time)],
                                  Age = mx$age_start[which(mx$time_start == time)],
                                  sex = substr(sex,1,1),
                                  a0rule = a0rule,
                                  OAnew = OAnew,
                                  extrapLaw = "kannisto", extrapFit = 80:(maxage-1), parS = c(A = 0.005, B = 0.13)) # here we set starting parameters for kannisto
    
    # The above gives a warning message and I'm not sure why.  Still seems to work though:
    # Warning message:
    #   In if (as.character(match.call()[[1]]) == "lt_single_simple") { :
    #       the condition has length > 1 and only the first element will be used
        
    lt <- lt_long(lt)
    lt$time_start <- time
    
    lt_output_list[[n]] <- lt
    
  }
  life_table <- do.call(rbind, lt_output_list)
  life_table$sex <- sex
  life_table$time_span <- 1
  life_table <- life_table[, c("indicator", "time_start", "time_span", "sex", "age_start", "age_span", "value")]
  
  return(life_table)
  
}


lt_single2abridged_loop_over_time <- function(lx_single, nLx_single, ex_single, sex) {
  
  # initialize output list
  lt_output_list <- list()
  n <- 0
  
  times                  <- unique(lx_single$time_start)
  time_span              <- diff(times)

  for (time in times) {

    n   <- n+1
    lt <- DemoTools::lt_single2abridged(lx = lx_single$value[which(lx_single$time_start == time & lx_single$sex == sex)],
                             nLx = nLx_single$value[which(nLx_single$time_start == time & nLx_single$sex == sex)],
                             ex = ex_single$value[which(ex_single$time_start == time & ex_single$sex == sex)])
    lt <- lt_long(lt)
    lt$time_start <- time
    lt$time_span  <- 1
    lt$sex        <- sex
    lt_output_list[[n]] <- lt
    
  }
  life_table <- do.call(rbind, lt_output_list)
  life_table <- life_table[, c("indicator", "time_start", "time_span", "sex", "age_start", "age_span", "value")]
  
  return(life_table)
  
}