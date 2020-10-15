
#------------------------------------------------------------

#' One-step backwards cohort component population projection with flexible migration assumption
#'
#' @description This function takes a set of starting population and demographic inputs for the period time_start-z 
#' to time_start with age groups of width z and projects population backwards z units of time.
#'
#' @author Sara Hertog
#'
#' @param z numeric. width of age groups = length of one-step projection horizon
#' @param pop_count_age_m_start numeric. a vector of male population counts at the start of the projection step by age
#' @param pop_count_age_f_start numeric. a vector of female population counts at the start of the projection step by age
#' @param survival_ratio_age_m numeric. a vector of survival ratios by age for males from a life table referencing the 
#' period time_start-z to time_start
#' @param survival_ratio_age_f numeric. a vector of survival ratios by age for females from a life table referencing the 
#' period time_start-z to time_start
#' @param nLx_age_m numeric. a vector of life table person-years for males from a life table referencing period 
#' time_start-z to time_start
#' @param nLx_age_f numeric. a vector of life table person-years for females from a life table referencing period 
#' time_start-z to time_start
#' @param mig_net_count_age_m numeric. a vector of net migration counts for males by age referencing the period 
#' time_start-z to time_start
#' @param mig_net_count_age_f numeric. a vector of net migration counts for females by age referencing the period 
#' time_start-z to time_start
#' @param mig_assumption charater. a control for the migration assumption. "end" means migration is accounted at end of projection 
#' period and has no influence on deaths during the period. "even" accounts for migration evenly distributed 
#' over the period and thus does influence deaths
#'  
#' @details This function will accept any width of age group and projection horizon as long as those are equal
#' e.g., use z=1 for a 1x1 projection of population by single year of age and 1 year projection horizon
#' or use z=5 for a 5x5 projection of population by 5-year age groups and 5-year projection horizon
#'
#' @return a list of objects including projected population by age and sex and deaths by cohort and sex
#' 
#' @export
#' 
#' 

# # # first do forward projection with end period assumption
# 
#  test_input <- wpp_input_example
#  test_input$mig_parameter$value[test_input$mig_parameter$indicator=="mig_assumption"] <- "even"
# 
#  test_output <- ccmppWPP_workflow_one_country_variant(wpp_input = test_input)
# 
# 
# # # then test backwards projection to see if it replicates
# #
# 
#  start_yr <- 1951
#  end_yr   <- 1950
# 
#  z = start_yr-end_yr
# 
# pop_count_age_m_start <- test_output$pop_count_age_sex_1x1$value[test_output$pop_count_age_sex_1x1$time_start == start_yr &
#                                                                    test_output$pop_count_age_sex_1x1$sex == "male"]
# pop_count_age_f_start <- test_output$pop_count_age_sex_1x1$value[test_output$pop_count_age_sex_1x1$time_start == start_yr &
#                                                                    test_output$pop_count_age_sex_1x1$sex == "female"]
# survival_ratio_age_m <- test_output$lt_complete_age_sex$value[test_output$lt_complete_age_sex$time_start == end_yr &
#                                                                 test_output$lt_complete_age_sex$indicator == "lt_Sx" &
#                                                                 test_output$lt_complete_age_sex$sex == "male"]
# survival_ratio_age_f <- test_output$lt_complete_age_sex$value[test_output$lt_complete_age_sex$time_start == end_yr &
#                                                                 test_output$lt_complete_age_sex$indicator == "lt_Sx" &
#                                                                 test_output$lt_complete_age_sex$sex == "female"]
# nLx_age_m <- test_output$lt_complete_age_sex$value[test_output$lt_complete_age_sex$time_start == end_yr &
#                                                                 test_output$lt_complete_age_sex$indicator == "lt_nLx" &
#                                                                 test_output$lt_complete_age_sex$sex == "male"]
# nLx_age_f <- test_output$lt_complete_age_sex$value[test_output$lt_complete_age_sex$time_start == end_yr &
#                                                      test_output$lt_complete_age_sex$indicator == "lt_nLx" &
#                                                      test_output$lt_complete_age_sex$sex == "female"]
# mig_net_count_age_m <- test_output$mig_net_count_age_sex_1x1$value[test_output$mig_net_count_age_sex_1x1$time_start == end_yr &
#                                                                      test_output$mig_net_count_age_sex_1x1$sex == "male"]
# mig_net_count_age_f <- test_output$mig_net_count_age_sex_1x1$value[test_output$mig_net_count_age_sex_1x1$time_start == end_yr &
#                                                                      test_output$mig_net_count_age_sex_1x1$sex == "female"]
# 
# mig_assumption <- "even"


project_ccmpp_z_by_z_backwards <- function(z=1,
                                 pop_count_age_m_start,
                                 pop_count_age_f_start,
                                 survival_ratio_age_m,
                                 survival_ratio_age_f,
                                 nLx_age_m,
                                 nLx_age_f,
                                 mig_net_count_age_m,
                                 mig_net_count_age_f,
                                 mig_assumption = c("end", "even")) {

  # check that lengths of inputs agree
  check_length <- min(length(pop_count_age_m_start),length(pop_count_age_f_start),
                      length(survival_ratio_age_m),length(survival_ratio_age_f),
                      length(mig_net_count_age_m),length(mig_net_count_age_f)) ==
    max(length(pop_count_age_m_start),length(pop_count_age_f_start),
        length(survival_ratio_age_m),length(survival_ratio_age_f),
        length(mig_net_count_age_m),length(mig_net_count_age_f))
  if (isFALSE(check_length)) { stop("Input columns are not all the same length")}

  nage <- length(pop_count_age_m_start) # number of age groups

### Two possible migration assumptions: end of period or evenly over period
  # here we set up some interim vectors according to the migration assumptions
  # pxm and pxf are population at time 0 unaltered from the input if mig_assumption is end of period
  # or with half of net migration added if mig_assumption is evenly over period
  # migm_end and migf_end is the net migration to be added at the end of the period *before*
  # births are computed using mid-year female population and asfr.  These are set to 0 if
  # mig_assumption is end of period and to half of net migration if mig_assumption is evenly over period.
  # When mig_assumption is end of period, net migration is added only after all deaths and births
  # have been computed.

  if (mig_assumption == "end") {

    # all migrants are added/removed at end of period and thus have no effect on deaths
    # migration-adjusted beginning of period population by age and sex
    pop_count_age_m_start_mig_adj <- pop_count_age_m_start - mig_net_count_age_m
    pop_count_age_f_start_mig_adj <- pop_count_age_f_start - mig_net_count_age_f
    # net migration to be subtracted at end of period to reflect migrant exposures to mortality
    mig_net_count_age_m_end <- rep(0,nage)
    mig_net_count_age_f_end <- rep(0,nage)

  } else if (mig_assumption == "even") { # add/remove half of migrants at beginning of period

    # half of increments between age x and x+1 are subtracted at end of period and do not affect deaths in period
    # half of increments between age x-1 and x are subtracted at beginning of period and survived to age x to x+1
    # migration-adjusted beginning of period population by age and sex
    pop_count_age_m_start_mig_adj <- pop_count_age_m_start - mig_net_count_age_m/2
    pop_count_age_f_start_mig_adj <- pop_count_age_f_start - mig_net_count_age_f/2
    # net migration to be subtracted at end of period to reflect migrant exposures to mortality
    mig_net_count_age_m_end <- c(NA, mig_net_count_age_m[1:(nage-1)]/2)
    mig_net_count_age_f_end <- c(NA, mig_net_count_age_f[1:(nage-1)]/2)

  }

  # compute deaths from year 0 population and survival ratios 
    death_count_cohort_m <- pop_count_age_m_start_mig_adj * (1-survival_ratio_age_m) / survival_ratio_age_m
    death_count_cohort_f <- pop_count_age_f_start_mig_adj * (1-survival_ratio_age_f) / survival_ratio_age_f
    
  # project population by age at year +z from year 0 population and deaths
    pop_count_age_m_end <- pop_count_age_m_start_mig_adj + death_count_cohort_m - mig_net_count_age_m_end
    # need to split last age group. use same proportion of nLx for last two age groups
    pop_count_age_m_end_alt <- c(pop_count_age_m_end[2:(nage-1)], 
                             pop_count_age_m_end[nage] * (nLx_age_m[nage-1]/(sum(nLx_age_m[(nage-1):nage]))),
                             pop_count_age_m_end[nage] * (nLx_age_m[nage]/(sum(nLx_age_m[(nage-1):nage]))))
    
    
    pop_count_age_f_end <- pop_count_age_f_start_mig_adj + death_count_cohort_f - mig_net_count_age_f_end
    # need to split last age group. use same proportion of nLx for last two age groups
    pop_count_age_f_end <- c(pop_count_age_f_end[2:(nage-1)], 
                             pop_count_age_f_end[nage] * (nLx_age_f[nage-1]/(sum(nLx_age_f[(nage-1):nage]))),
                             pop_count_age_f_end[nage] * (nLx_age_f[nage]/(sum(nLx_age_f[(nage-1):nage]))))
    
    
  # ensure no negative population by age and sex (0.0005 is same as Abacus)
    pop_count_age_m_end[which(pop_count_age_m_end<0)] <- 0.0005
    pop_count_age_f_end[which(pop_count_age_f_end<0)] <- 0.0005

  # assemble list of outputs and return

    proj_list <- list(age_start            = c(seq(0,(nage-1)*z, z)),
                      age_span             = c(rep(z, nage-1), 1000), # age_span = 1000 identifies open age group
                      pop_count_age_m_end  = pop_count_age_m_end,
                      pop_count_age_f_end  = pop_count_age_f_end,
                      death_count_cohort_m = death_count_cohort_m,
                      death_count_cohort_f = death_count_cohort_f)

    return(proj_list)

    }


    

