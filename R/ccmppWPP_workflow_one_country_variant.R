
#------------------------------------------------------------

#' Full ccmppWPP implementation for one country and variant
#'
#' @description This function takes the wpp input list of objects for a given country and variant
#' and walks through the entire ccmppWPP workflow to produce all of the outputs published in the WPP.
#'
#' @author Sara Hertog
#'
#' @param wpp_input list of input objects required for one country-variant
#'
#' @details This function will accept any width of age group and projection horizon as long as those are equal
#' e.g., use z=1 for a 1x1 projection of population by single year of age and 1 year projection horizon
#' or use z=5 for a 5x5 projection of population by 5-year age groups and 5-year projection horizon
#'
#' @return a list of objects including projected population, deaths, exposures, life tables, births, fertility
#' rates and proportions, and net migrant counts
#'
#' @export
#'


ccmppWPP_workflow_one_country_variant <- function(wpp_input) {

    wpp_input <- as_ccmpp_input_list(wpp_input)

# extract objects needed for ccmpp
ccmpp_input <- data_parse_ccmpp_input(indata = wpp_input)

# run ccmpp, looping over time steps for the full projection period
ccmpp_output <- project_ccmpp_loop_over_time(indata = ccmpp_input)

# reshape the ccmpp output into a list of data frames by indicator
ccmpp_output <- data_reshape_ccmpp_output(ccmpp_output = ccmpp_output)

# splice base population with ccmpp output populations by age and sex

  # compute both sexes population by age at base year
  pop_count_age_sex_b <- sum_last_column(ccmpp_input$pop_count_age_sex_base[, c("time_start", "time_span",
                                                                                "age_start", "age_span", "value")])
  pop_count_age_sex_b$sex <- "both"
  # rbind both sexes base population, with sex-specific base population by age and projected population by age and sex
  pop_count_age_sex <- rbind(ccmpp_output$pop_count_age_sex,
                             ccmpp_input$pop_count_age_sex_base,
                             pop_count_age_sex_b)
  pop_count_age_sex <- pop_count_age_sex[with(pop_count_age_sex, order(time_start,
                                                                       sex,
                                                                       age_start)),]
  rm(pop_count_age_sex_b)

# compute exposures (mid-period population) by time and age and sex

  exposure_count_age_sex <- exposure_age_sex_loop_over_time(pop = pop_count_age_sex,
                                                            mig_assumption = ccmpp_input$mig_parameter[which(ccmpp_input$mig_parameter$indicator == "mig_assumption"),],
                                                            mig = ccmpp_output$mig_net_count_age_sex)

  # aggregate to both sexes
  exposure_count_age_b   <- sum_last_column(exposure_count_age_sex[,c("time_start", "time_span",
                                                                      "age_start", "age_span", "value")])
  exposure_count_age_b$sex <- "both"
  # r bind both sexes exposures with exposures by sex
  exposure_count_age_sex <- rbind(exposure_count_age_sex,
                                  exposure_count_age_b)
  exposure_count_age_sex <- exposure_count_age_sex[with(exposure_count_age_sex, order(time_start,
                                                                                      sex,
                                                                                      age_start)),]


# compute period deaths by age and sex from mx and exposures
  death_count_age_sex <- death_age_sex_loop_over_time(mx = wpp_input$life_table_age_sex[which(wpp_input$life_table_age_sex$indicator == "lt_nMx"),],
                                                      exp = exposure_count_age_sex,
                                                      distribute_residual = TRUE,
                                                      dth_cohort = ccmpp_output$death_count_cohort_sex)
  # aggregate to both sexes
  death_count_age_b   <- sum_last_column(death_count_age_sex[,c("time_start", "time_span",
                                                                      "age_start", "age_span", "value")])
  death_count_age_b$sex <- "both"
  # rbind both sexes deaths by age with deaths by sex
  death_count_age_sex <- rbind(death_count_age_sex,
                               death_count_age_b)
  death_count_age_sex <- death_count_age_sex[with(death_count_age_sex, order(time_start,
                                                                             sex,
                                                                             age_start)),]

# compute both sexes life tables by single year of age ("complete" life tables)

  # compute age-specific mortality rates from age-specific deaths and exposures
  mx_b <- cbind(death_count_age_b[, 1:4],
                value = death_count_age_b$value / exposure_count_age_b$value)
  # compute all life table columns from single year mx
  life_table_age_b <- lt_complete_loop_over_time(mx = mx_b, sex="both")

  # rbind both sexes life tables with life tables by sex
  lt_complete_age_sex   <- rbind(wpp_input$life_table_age_sex, life_table_age_b)

# compute abridged life tables
  lt_abridged_age_f   <- lt_abridged_from_complete_loop_over_time(lx = lt_complete_age_sex[which(lt_complete_age_sex$indicator == "lt_lx" &
                                                                                                   lt_complete_age_sex$sex == "female"),],
                                                                  sex = "female")
  lt_abridged_age_m   <- lt_abridged_from_complete_loop_over_time(lx = lt_complete_age_sex[which(lt_complete_age_sex$indicator == "lt_lx" &
                                                                                                   lt_complete_age_sex$sex == "male"),],
                                                                  sex = "male")
  lt_abridged_age_b   <- lt_abridged_from_complete_loop_over_time(lx = lt_complete_age_sex[which(lt_complete_age_sex$indicator == "lt_lx" &
                                                                                                   lt_complete_age_sex$sex == "both"),],
                                                                  sex = "both")
  lt_abridged_age_sex <- rbind(lt_abridged_age_b, lt_abridged_age_f, lt_abridged_age_m)

  rm(lt_abridged_age_b, lt_abridged_age_f, lt_abridged_age_m)

# extract summary life table values
  lt_summary            <- lt_summary(lt_data = lt_abridged_age_sex,
                                           byvar = c("time_start","time_span", "sex"))

# sum to five-year age groups
  # population
  pop_count_age_sex_5x1      <- sum_five_year_age_groups(indata = pop_count_age_sex,
                                                         byvar = c("time_start","time_span","sex"))

  # births
  birth_count_age_b_5x1      <- sum_five_year_age_groups(indata = ccmpp_output$birth_count_age_b,
                                                         byvar = c("time_start","time_span"))

  # exposures
  exposure_count_age_sex_5x1 <- sum_five_year_age_groups(indata = exposure_count_age_sex,
                                                         byvar = c("time_start","time_span","sex"))

  # deaths by age and sex

  death_count_age_sex_5x1 <- sum_five_year_age_groups(indata = death_count_age_sex,
                                                      byvar = c("time_start","time_span","sex"))

  # deaths by cohort and sex

  death_count_cohort_sex_5x1 <- sum_five_year_age_groups(indata = ccmpp_output$death_count_cohort_sex,
                                                      byvar = c("time_start","time_span","sex"))

  # net migrants

  mig_net_count_age_sex_5x1  <- sum_five_year_age_groups(indata = ccmpp_output$mig_net_count_age_sex,
                                                         byvar = c("time_start","time_span","sex"))

# sum to totals
  # population
  pop_count_tot_sex        <- sum_last_column(pop_count_age_sex[, c("time_start", "time_span", "sex", "value")])
  # deaths
  death_count_tot_sex      <- sum_last_column(death_count_age_sex[, c("time_start", "time_span", "sex", "value")])
  # net migrants
  mig_net_count_tot_sex    <- sum_last_column(ccmpp_output$mig_net_count_age_sex[, c("time_start", "time_span", "sex", "value")])

# compute some fertility indicators
  # fertility rates by 5-year age group of mother
  fert_rate_age_f_5x1      <- data.frame(time_start  = birth_count_age_b_5x1$time_start,
                                         time_span   = birth_count_age_b_5x1$time_span,
                                         age_start   = birth_count_age_b_5x1$age_start,
                                         age_span    = birth_count_age_b_5x1$age_span,
                                         value = birth_count_age_b_5x1$value/
                                           exposure_count_age_sex_5x1$value[which(exposure_count_age_sex_5x1$sex == "female")])
  # total fertility rate
  fert_rate_tot_f          <- sum_last_column(ccmpp_input$fert_rate_age_f[, c("time_start", "time_span", "value")])


  # percentage age-specific fertility rates
  fert_pct_age_f_1x1       <- fert_pasfr(fert_data_age = ccmpp_input$fert_rate_age_f,
                                         byvar         = c("time_start", "time_span"))


  fert_pct_age_f_5x1       <- fert_pasfr(fert_data_age = fert_rate_age_f_5x1,
                                         byvar         = c("time_start", "time_span"))

  # mean age of childbearing
  fert_mean_age_f          <- fert_mac(fert_data_age = ccmpp_input$fert_rate_age_f,
                                       byvar         = c("time_start", "time_span"))


# compile output list and return
  # assemble all estimates to send to Eagle
  ccmppWPP_output <- list(pop_count_age_sex_1x1      = pop_count_age_sex,
                          pop_count_age_sex_5x1      = pop_count_age_sex_5x1,
                          pop_count_tot_sex          = pop_count_tot_sex,
                          birth_count_age_b_1x1      = ccmpp_output$birth_count_age_b,
                          birth_count_age_b_5x1      = birth_count_age_b_5x1,
                          birth_count_tot_sex        = ccmpp_output$birth_count_tot_sex,
                          fert_rate_age_f_1x1        = ccmpp_input$fert_rate_age_f,
                          fert_rate_age_f_5x1        = fert_rate_age_f_5x1,
                          fert_pct_age_f_1x1         = fert_pct_age_f_1x1,
                          fert_pct_age_f_5x1         = fert_pct_age_f_5x1,
                          fert_rate_tot_f            = fert_rate_tot_f,
                          fert_mean_age_f            = fert_mean_age_f,
                          srb                        = ccmpp_input$srb,
                          death_count_age_sex_1x1    = death_count_age_sex,
                          death_count_age_sex_5x1    = death_count_age_sex_5x1,
                          death_count_tot_sex        = death_count_tot_sex,
                          death_count_cohort_sex_1x1 = ccmpp_output$death_count_cohort_sex,
                          death_count_cohort_sex_5x1 = death_count_cohort_sex_5x1,
                          exposure_count_age_sex_1x1 = exposure_count_age_sex,
                          exposure_count_age_sex_5x1 = exposure_count_age_sex_5x1,
                          lt_complete_age_sex        = lt_complete_age_sex,
                          lt_abridged_age_sex        = lt_abridged_age_sex,
                          lt_summary                 = lt_summary,
                          mig_net_count_age_sex_1x1  = ccmpp_output$mig_net_count_age_sex,
                          mig_net_count_age_sex_5x1  = mig_net_count_age_sex_5x1,
                          mig_net_count_tot_sex      = mig_net_count_tot_sex,
                          mig_assumption             = ccmpp_input$mig_parameter[which(ccmpp_input$mig_parameter$indicator == "mig_assumption"),]
  )

  return(ccmppWPP_output)

#

}
