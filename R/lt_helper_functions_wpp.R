#' life table helper functions for WPP
#' 
#' #' Take life tables output from lt functions in DemoTools and reshape to long data frame
#'
#' @description This function reshapes the wide life table data frame returned by the DemoTools functions 
#' lt_abridged() or lt_single_mx() and retuns a long data frame with the "indicator", "age", "value" columns
#' used for WPP.
#'
#' @author Sara Hertog
#'
#' @param life_table_df data frame. life table output by the lt_abridged or lt_single_mx functions of DemoTools.
#'
#' @return a long data frame with columns "indicator" containing the life table column label, "age", and "value"
#' containing the numeric value associated with the life table indicator
#' @export
lt_long <- function(life_table_df) {
  
  lt_long <- reshape(life_table_df, idvar="Age", 
                     varying=list(3:ncol(life_table_df)), 
                     times=names(life_table_df)[3:ncol(life_table_df)],
                     timevar = "indicator",
                     v.names="value", direction = "long")
  lt_long$age <- lt_long$Age
  lt_long$indicator <- paste0("lt_", lt_long$indicator)
  
  lt_long <- lt_long[, c("indicator", "age", "value")]
  
  return(lt_long)
  
}

#' Compute an abridged life table from a complete life table
#'
#' @description This function computes an abridged life table from the lx column of a complete life table (single 
#' year of age).
#'
#' @author Sara Hertog
#'
#' @param age_complete numeric. a vector of single-year ages from the complete life table.
#' @param lx_complete numeric. a vector of lx (life table survivors) values from the complete life table.
#' @param sex character. sex associated to the life table, can be "female", "male", "both", or "f", "m", "b"
#'
#' @return a long data frame with columns "indicator" containing the life table column label, "age" containing
#' the starting age of abridged life table age groups, and "value" containing the numeric value associated with 
#' the life table indicator
#' @export
lt_abridged_from_complete <- function(age_complete, lx_complete, sex) {
  
  lx     <- lx_complete[which(age_complete %in% c(0,1,seq(5,150,5)))]
  age    <- c(0,1,seq(5,150,5))[1:length(lx)]
  lt_abr <- lt_abridged(Age = age, 
                        lx=lx, 
                        sex=substr(sex,1,1))
  lt_abr <- lt_long(lt_abr)
  
  return(lt_abr)
  
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
  lt_lx           <- lt_data[lt_data$indicator=="lt_lx", c(byvar, "age", "value")]
  lt_lx           <- reshape(lt_lx, idvar=byvar, v.names="value", timevar="age", direction="wide")
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
  
  # extract summary ex
  lt_ex           <- lt_data[lt_data$indicator=="lt_ex" & lt_data$age %in% c(0,15,50,60,65,80,85,100,110), 
                             c(byvar, "age", "value")]
  lt_ex$indicator <- paste0("lt_e", lt_ex$age)
  lt_ex           <- lt_ex[, c(byvar, "indicator", "value")]
  
  lt_summary      <- rbind(lt_lx, lt_ex)
  
  return(lt_summary)
  
}