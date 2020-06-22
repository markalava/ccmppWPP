#------------------------------------------------------------

#' Parse the objects from ccmpp_input file needed by the project_ccmpp_loop_over_time function
#'
#' @description This function takes a wpp_input file and parses the objects needed to perform the 
#' full cohort-component population projection over time. Returns the list of inputs required for 
#' project_ccmpp_loop_over_time function
#'
#' @author Sara Hertog
#'
#' @param indata name of the standard wpp_input file
#'
#' @return a list of data.frames including base population by time and age and sex, survival ratios by time and age and sex,
#' fertility rates by time and age of mother, the sex ratio at birth by time, net migration counts by time and age and sex,
#' net migration rates by time and age and sex, total net migrant counts by time, migration parameters that control treatment
#' of net migration in the ccmpp
#' 
#' @export
#' 
data_parse_ccmpp_input <- function(indata) {
  
  ccmpp_input <- list(pop_count_age_sex_base = indata$pop_count_age_sex_base,
                      survival_ratio_age_sex = indata$life_table_age_sex[indata$life_table_age_sex$indicator=="lt_Sx", 
                                                                         c("time_start", "time_span", "sex", 
                                                                           "age_start", "age_span", "value")], 
                      fert_rate_age_f        = indata$fert_rate_age_f, 
                      srb                    = indata$srb,
                      mig_net_count_age_sex  = indata$mig_net_count_age_sex, 
                      mig_net_rate_age_sex   = indata$mig_net_rate_age_sex,
                      mig_net_count_tot_b    = indata$mig_net_count_tot_b,
                      mig_parameter          = indata$mig_parameter)
  
  return(ccmpp_input)
  
}

