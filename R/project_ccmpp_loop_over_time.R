#------------------------------------------------------------

#' Loop over periods for cohort component population projection
#'
#' @description This function takes a set of base population and time series of demographic inputs
#' and carries out the cohort component population projection over the specified time periods
#'
#' @author Sara Hertog
#'
#' @param indata list. ccmpp_input list of data frames
#'
#' @details This function takes a full set of base population and demographic inputs for a country over time and
#' implements a cohort-component population projection with steps of length z by looping through the
#' project_ccmpp_z_by_z() function.
#'
#' @return a list of objects with step by step cohort-component population projection results, including population
#' by age and sex, period deaths by cohort and sex, births by age of the mother, total period births by sex, and net
#' migrant counts by age and sex
#'
#' @export

project_ccmpp_loop_over_time <- function(indata) {

  # initialize output
  projection_output_list <- list()

  # read some attributes (these are temporarily read from example input file while Mark works on setting up attributes)
    time_span              <- time_span(indata)
    time_start             <- min(times(indata))
    time_end               <- max(times(indata)) + time_span
    survival_ratio_age_sex <- survival_ratio_component(indata)

  # define projection steps
  for (time in time_start:(time_end-time_span)) {

    # starting populations
    if (time == time_start) {

      pop_f_start     <- indata$pop_count_age_sex_base[which(indata$pop_count_age_sex_base$sex == "female"), "value"]
      pop_m_start     <- indata$pop_count_age_sex_base[which(indata$pop_count_age_sex_base$sex == "male"), "value"]

    }

    # subset demographic rates for the one-step projection
    # fertility
    asfr              <- indata$fert_rate_age_f[which(indata$fert_rate_age_f$time_start == time), "value"]
    srb               <- indata$srb[which(indata$srb$time_start == time), "value"]

    # mortality
    mort_f            <- survival_ratio_age_sex[which(survival_ratio_age_sex$time_start == time &
                                                            survival_ratio_age_sex$sex == "female"), "value"]
    mort_m            <- survival_ratio_age_sex[which(survival_ratio_age_sex$time_start == time &
                                                            survival_ratio_age_sex$sex == "male"), "value"]

    mig_type          <- indata$mig_parameter[which(indata$mig_parameter$time_start == time &
                                                   indata$mig_parameter$indicator == "mig_type"), "value"]

    mig_assumption    <- indata$mig_parameter[which(indata$mig_parameter$time_start == time &
                                                   indata$mig_parameter$indicator == "mig_assumption"), "value"]

    # net international migration
    if (mig_type == "counts") {

      mig_f           <- indata$mig_net_count_age_sex[which(indata$mig_net_count_age_sex$time_start == time &
                                                             indata$mig_net_count_age_sex$sex == "female"), "value"]
      mig_m           <- indata$mig_net_count_age_sex[which(indata$mig_net_count_age_sex$time_start == time &
                                                             indata$mig_net_count_age_sex$sex == "male"), "value"]

    } else if (mig_type =="rates") {

      # multiply age and sex specific rates by age and sex specific population counts
      mig_f           <- indata$mig_net_rate_age_sex[which(indata$mig_net_rate_age_sex$time_start == time &
                                                              indata$mig_net_rate_age_sex$sex == "female"), "value"]
      mig_f           <- mig_f * pop_m_start
      mig_m           <- indata$mig_net_rate_age_sex[which(indata$mig_net_rate_age_sex$time_start == time &
                                                             indata$mig_net_rate_age_sex$sex == "male"), "value"]
      mig_m           <- mig_m * pop_m_start

    } else if (mig_type == "totals") {
      # distribute total net migrants by age sex distribution of population
      mig_tot         <- indata$mig_net_count_tot_b[which(indata$mig_net_count_tot_b$time_start == time), "value"]
      pop_tot         <- sum(pop_f_start) + sum(pop_m_start)
      mig_f           <- mig_tot * pop_f_start / pop_tot
      mig_m           <- mig_tot * pop_m_start / pop_tot

    }

    # if net migration counts would produce negative population, adjust such that it would leave negligible population
    mig_f[which(pop_f_start + mig_f <= 0)] <- 0.00001 - pop_f_start[which(pop_f_start + mig_f <= 0)]
    mig_m[which(pop_m_start + mig_m <= 0)] <- 0.00001 - pop_m_start[which(pop_m_start + mig_m <= 0)]

    # project population forward one step
    fwd_1_step <- project_ccmpp_z_by_z(z                     = time_span,
                                       pop_count_age_m_start = pop_m_start,
                                       pop_count_age_f_start = pop_f_start,
                                       survival_ratio_age_m  = mort_m,
                                       survival_ratio_age_f  = mort_f,
                                       fert_rate_age_f       = asfr,
                                       srb                   = srb,
                                       mig_net_count_age_m   = mig_m,
                                       mig_net_count_age_f   = mig_f,
                                       mig_assumption        = mig_assumption)



    # append net migrant count to output
    # necessary to retain values in case "rates" option was used and counts were computed within loop function
    fwd_1_step$mig_net_count_age_m   <- mig_m
    fwd_1_step$mig_net_count_age_f   <- mig_f

    # append indentifying information about the projection interval
    fwd_1_step$time_start            <- as.numeric(time)
    fwd_1_step$time_span             <- as.numeric(time_span)

    # set end population as starting population for next projection step
    pop_f_start <- as.numeric(fwd_1_step$pop_count_age_f_end, drop=TRUE)
    pop_m_start <- as.numeric(fwd_1_step$pop_count_age_m_end, drop=TRUE)

    projection_output_list[[time-time_start+time_span]] <- fwd_1_step

  }
  return(projection_output_list)
}

