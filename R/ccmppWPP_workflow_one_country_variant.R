
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
#' @details This function calls all of the various functions required to complete the WPP workflow.  It currently
#' works only for a 1x1 population projection and returns outputs summarised by 1-year and 5-year age groups.
#'
#' @return a list of objects including projected population, deaths, exposures, life tables, births, fertility
#' rates and proportions, and net migrant counts, needed for Eagle display
#'
#' @export
#'


ccmppWPP_workflow_one_country_variant <- function(wpp_input,
                                                  intermediate_output_folder) {

  # pull out input file attributes
  atr <- attributes(wpp_input)
  
  # validate inputs with S3 classes
  ccmpp_input <- as_ccmpp_input_list(wpp_input)
  
  # run the projection and assemble key indicators on population, births, deaths, exposures, migration and life tables
  # 1x1 for ages 0 to 130+
  ccmpp_output <- ccmppWPP_project_one_country_variant(ccmpp_input = ccmpp_input, atr = atr)
  
  # store projection outputs for all ages 0 to 130
  save(ccmpp_output, file = paste0(intermediate_output_folder,atr$locid,"_ccmpp_output.RData"))

  # truncate back to open age group 100+
  ccmpp_output_100 <- ccmppWPP_truncate_OAG(ccmpp_output = ccmpp_output, OAnew = 100)
  
  # compute all of the population and demographic indicators needed for WPP
  wpp_output <- ccmppWPP_compute_WPP_outputs(ccmpp_output = ccmpp_output_100, atr = atr)
  
  # carry over the migration parameters from the input
  wpp_output$mig_parameter <- ccmpp_input$mig_parameter
  
  # generate warning messages to alert analysts to when migration counts have been modified from inputs to avoid non-negative population
  wpp_output$mig_net_count_age_sex_override <- ccmppWPP_migration_override(ccmpp_input = ccmpp_input, 
                                                                           ccmpp_output = ccmpp_output_100)


  return(wpp_output)


}


ccmppWPP_project_one_country_variant <- function(ccmpp_input, atr) {
  
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
  
  
  # compute period deaths by age and sex from cohort deaths and separation factors computed with lx and nLx from input life table
  death_count_age_sex <- death_age_sex_loop_over_time(dth_cohort = ccmpp_output$death_count_cohort_sex,
                                                      lx = ccmpp_input$life_table_age_sex[which(ccmpp_input$life_table_age_sex$indicator == "lt_lx"),],
                                                      nLx = ccmpp_input$life_table_age_sex[which(ccmpp_input$life_table_age_sex$indicator == "lt_nLx"),])
  
  # derive exposures from input nMx and period deaths by age and sex 
  exposure_count_age_sex <- exposure_age_sex_loop_over_time(dth_age = death_count_age_sex,
                                                            nmx = ccmpp_input$life_table_age_sex[which(ccmpp_input$life_table_age_sex$indicator == "lt_nMx"),])
  
  
  # aggregate exposures to both sexes
  exposure_count_age_b   <- sum_last_column(exposure_count_age_sex[,c("time_start", "time_span",
                                                                      "age_start", "age_span", "value")])
  exposure_count_age_b <- exposure_count_age_b[with(exposure_count_age_b, order(time_start,
                                                                                age_start)),]
  exposure_count_age_b$sex <- "both"
  # r bind both sexes exposures with exposures by sex
  exposure_count_age_sex <- rbind(exposure_count_age_sex,
                                  exposure_count_age_b)
  exposure_count_age_sex <- exposure_count_age_sex[with(exposure_count_age_sex, order(time_start,
                                                                                      sex,
                                                                                      age_start)),]
  
  # aggregate age-period deaths to both sexes
  death_count_age_b   <- sum_last_column(death_count_age_sex[,c("time_start", "time_span",
                                                                "age_start", "age_span", "value")])
  death_count_age_b <- death_count_age_b[with(death_count_age_b, order(time_start,
                                                                       age_start)),]
  death_count_age_b$sex <- "both"
  # rbind both sexes deaths by age with deaths by sex
  death_count_age_sex <- rbind(death_count_age_sex,
                               death_count_age_b)
  death_count_age_sex <- death_count_age_sex[with(death_count_age_sex, order(time_start,
                                                                             sex,
                                                                             age_start)),]
  
  # compute both sexes life tables by single year of age ("complete" life tables)
  
  # compute age-specific mortality rates from age-specific deaths and exposures
  exp_b <- ifelse(exposure_count_age_b$value == 0, 0.00000001, exposure_count_age_b$value) # don't allow division by zero
  mx_b <- cbind(death_count_age_b[, 1:4],
                value = death_count_age_b$value / exp_b) 
  # compute all life table columns from single year mx
  life_table_age_b <- lt_complete_loop_over_time(mx = mx_b, sex="both", a0rule = atr$a0rule, OAnew = 130)
  
  # rbind both sexes life tables with life tables by sex
  lt_complete_age_sex   <- rbind(ccmpp_input$life_table_age_sex, life_table_age_b)
  
  intermediate_output <- list(pop_count_age_sex = pop_count_age_sex,
                              death_count_cohort_sex = ccmpp_output$death_count_cohort_sex,
                              death_count_age_sex = death_count_age_sex,
                              exposure_count_age_sex = exposure_count_age_sex,
                              lt_complete_age_sex = lt_complete_age_sex,
                              fert_rate_age_f = ccmpp_input$fert_rate_age_f,
                              srb = ccmpp_input$srb,
                              birth_count_age_b = ccmpp_output$birth_count_age_b,
                              birth_count_tot_sex = ccmpp_output$birth_count_tot_sex,
                              mig_net_count_age_sex = ccmpp_output$mig_net_count_age_sex)
  
  attr(intermediate_output, "revision") <- atr$revision
  attr(intermediate_output, "locid")    <- atr$locid
  attr(intermediate_output, "locname")  <- atr$locname
  attr(intermediate_output, "variant")  <- atr$variant
  attr(intermediate_output, "a0rule")   <- atr$a0rule
  
  return(intermediate_output)
  
}

ccmppWPP_truncate_OAG <- function(ccmpp_output, OAnew = 100) {
  
  atr <- attributes(ccmpp_output)
  
  # population by age and sex
  pop_OAnew <- sum_last_column(ccmpp_output$pop_count_age_sex[ccmpp_output$pop_count_age_sex$age_start >= OAnew, 
                                                              !(names(ccmpp_output$pop_count_age_sex) %in% c("age_start","age_span"))])
  pop_OAnew$age_start <- OAnew
  pop_OAnew$age_span <- 1000
  pop_count_age_sex <- rbind(ccmpp_output$pop_count_age_sex[ccmpp_output$pop_count_age_sex$age_start < OAnew,], pop_OAnew)
  pop_count_age_sex <- pop_count_age_sex[order(pop_count_age_sex$time_start, pop_count_age_sex$sex, pop_count_age_sex$age_start),]
  
  # deaths by cohort and sex
  death_OAnew <- sum_last_column(ccmpp_output$death_count_cohort_sex[ccmpp_output$death_count_cohort_sex$age_start >= OAnew, 
                                                                  !(names(ccmpp_output$death_count_cohort_sex) %in% c("age_start","age_span"))])
  death_OAnew$age_start <- OAnew
  death_OAnew$age_span <- 1000
  death_count_cohort_sex <- rbind(ccmpp_output$death_count_cohort_sex[ccmpp_output$death_count_cohort_sex$age_start < OAnew,], death_OAnew)
  death_count_cohort_sex <- death_count_cohort_sex[order(death_count_cohort_sex$time_start, death_count_cohort_sex$sex, death_count_cohort_sex$age_start),]
  rm(death_OAnew)
  
  # deaths by age and sex
  death_OAnew <- sum_last_column(ccmpp_output$death_count_age_sex[ccmpp_output$death_count_age_sex$age_start >= OAnew, 
                                                                  !(names(ccmpp_output$death_count_age_sex) %in% c("age_start","age_span"))])
  death_OAnew$age_start <- OAnew
  death_OAnew$age_span <- 1000
  death_count_age_sex <- rbind(ccmpp_output$death_count_age_sex[ccmpp_output$death_count_age_sex$age_start < OAnew,], death_OAnew)
  death_count_age_sex <- death_count_age_sex[order(death_count_age_sex$time_start, death_count_age_sex$sex, death_count_age_sex$age_start),]
  
  # exposures by age and sex
  exposure_OAnew <- sum_last_column(ccmpp_output$exposure_count_age_sex[ccmpp_output$exposure_count_age_sex$age_start >= OAnew, 
                                                                  !(names(ccmpp_output$exposure_count_age_sex) %in% c("age_start","age_span"))])
  exposure_OAnew$age_start <- OAnew
  exposure_OAnew$age_span <- 1000
  exposure_count_age_sex <- rbind(ccmpp_output$exposure_count_age_sex[ccmpp_output$exposure_count_age_sex$age_start < OAnew,], exposure_OAnew)
  exposure_count_age_sex <- exposure_count_age_sex[order(exposure_count_age_sex$time_start, exposure_count_age_sex$sex, exposure_count_age_sex$age_start),]
  
  # life tables by age and sex
  mx <- ccmpp_output$lt_complete_age_sex[ccmpp_output$lt_complete_age_sex$indicator == "lt_nMx",]
  lt_f <- lt_complete_loop_over_time(mx = mx[mx$sex == "female",], sex = "female", a0rule = atr$a0rule, OAnew = 100)
  lt_m <- lt_complete_loop_over_time(mx = mx[mx$sex == "male",], sex = "male", a0rule = atr$a0rule, OAnew = 100)
  lt_b <- lt_complete_loop_over_time(mx = mx[mx$sex == "both",], sex = "both", a0rule = atr$a0rule, OAnew = 100)
  
  lt_complete_age_sex <- rbind(lt_f, lt_m, lt_b)
  
  # births by age and sex
  birth_OAnew <- sum_last_column(ccmpp_output$birth_count_age_b[ccmpp_output$birth_count_age_b$age_start >= OAnew, 
                                                                  !(names(ccmpp_output$birth_count_age_b) %in% c("age_start","age_span"))])
  birth_OAnew$age_start <- OAnew
  birth_OAnew$age_span <- 1000
  birth_count_age_b <- rbind(ccmpp_output$birth_count_age_b[ccmpp_output$birth_count_age_b$age_start < OAnew,], birth_OAnew)
  birth_count_age_b <- birth_count_age_b[order(birth_count_age_b$time_start, birth_count_age_b$age_start),]
  
  # net migration by age and sex
  mig_net_OAnew <- sum_last_column(ccmpp_output$mig_net_count_age_sex[ccmpp_output$mig_net_count_age_sex$age_start >= OAnew, 
                                                                  !(names(ccmpp_output$mig_net_count_age_sex) %in% c("age_start","age_span"))])
  mig_net_OAnew$age_start <- OAnew
  mig_net_OAnew$age_span <- 1000
  mig_net_count_age_sex <- rbind(ccmpp_output$mig_net_count_age_sex[ccmpp_output$mig_net_count_age_sex$age_start < OAnew,], mig_net_OAnew)
  mig_net_count_age_sex <- mig_net_count_age_sex[order(mig_net_count_age_sex$time_start, mig_net_count_age_sex$sex, mig_net_count_age_sex$age_start),]
  
  # fertility rates by age
  fert_rate_age_f <- ccmpp_output$fert_rate_age_f[ccmpp_output$fert_rate_age_f$age_start <= OAnew,]
  fert_rate_age_f$age_span[fert_rate_age_f$age_start == OAnew] <- 1000
  
  truncated_output <- list(pop_count_age_sex = pop_count_age_sex,
                           death_count_cohort_sex = death_count_cohort_sex,
                           death_count_age_sex = death_count_age_sex,
                           exposure_count_age_sex = exposure_count_age_sex,
                           lt_complete_age_sex = lt_complete_age_sex,
                           fert_rate_age_f = fert_rate_age_f,
                           srb = ccmpp_output$srb,
                           birth_count_age_b = birth_count_age_b,
                           birth_count_tot_sex = ccmpp_output$birth_count_tot_sex,
                           mig_net_count_age_sex = mig_net_count_age_sex)
  
  return(truncated_output)
  
  
}

ccmppWPP_compute_WPP_outputs <- function(ccmpp_output, atr) {
  
  # compute abridged life tables
  lt_complete_age_sex <- ccmpp_output$lt_complete_age_sex
  lt_abridged_age_f   <- lt_single2abridged_loop_over_time(lx_single = lt_complete_age_sex[which(lt_complete_age_sex$indicator == "lt_lx" &
                                                                                                   lt_complete_age_sex$sex == "female"),],
                                                           nLx_single = lt_complete_age_sex[which(lt_complete_age_sex$indicator == "lt_nLx" &
                                                                                                    lt_complete_age_sex$sex == "female"),],
                                                           ex_single = lt_complete_age_sex[which(lt_complete_age_sex$indicator == "lt_ex" &
                                                                                                   lt_complete_age_sex$sex == "female"),],
                                                           sex = "female")
  lt_abridged_age_m   <- lt_single2abridged_loop_over_time(lx_single = lt_complete_age_sex[which(lt_complete_age_sex$indicator == "lt_lx" &
                                                                                                   lt_complete_age_sex$sex == "male"),],
                                                           nLx_single = lt_complete_age_sex[which(lt_complete_age_sex$indicator == "lt_nLx" &
                                                                                                    lt_complete_age_sex$sex == "male"),],
                                                           ex_single = lt_complete_age_sex[which(lt_complete_age_sex$indicator == "lt_ex" &
                                                                                                   lt_complete_age_sex$sex == "male"),],
                                                           sex = "male")
  lt_abridged_age_b   <- lt_single2abridged_loop_over_time(lx_single = lt_complete_age_sex[which(lt_complete_age_sex$indicator == "lt_lx" &
                                                                                                   lt_complete_age_sex$sex == "both"),],
                                                           nLx_single = lt_complete_age_sex[which(lt_complete_age_sex$indicator == "lt_nLx" &
                                                                                                    lt_complete_age_sex$sex == "both"),],
                                                           ex_single = lt_complete_age_sex[which(lt_complete_age_sex$indicator == "lt_ex" &
                                                                                                   lt_complete_age_sex$sex == "both"),],
                                                           sex = "both")
  lt_abridged_age_sex <- rbind(lt_abridged_age_b, lt_abridged_age_f, lt_abridged_age_m)
  
  rm(lt_abridged_age_b, lt_abridged_age_f, lt_abridged_age_m)
  
  # extract summary life table values
  lt_summary            <- lt_summary(lt_data = lt_abridged_age_sex,
                                      byvar = c("time_start","time_span", "sex"))
  
  # add sex field to births data frame
  birth_count_age_1x1        <- ccmpp_output$birth_count_age_b
  birth_count_age_1x1$sex    <- "both"
  
  # sum to five-year age groups
  # population
  pop_count_age_sex_5x1      <- sum_five_year_age_groups(indata = ccmpp_output$pop_count_age_sex,
                                                         byvar = c("time_start","time_span","sex"))
  
  # births
  birth_count_age_5x1        <- sum_five_year_age_groups(indata = birth_count_age_1x1,
                                                         byvar = c("time_start","time_span","sex"))
  
  # exposures
  exposure_count_age_sex_5x1 <- sum_five_year_age_groups(indata = ccmpp_output$exposure_count_age_sex,
                                                         byvar = c("time_start","time_span","sex"))
  
  # deaths by age and sex
  
  death_count_age_sex_5x1 <- sum_five_year_age_groups(indata = ccmpp_output$death_count_age_sex,
                                                      byvar = c("time_start","time_span","sex"))
  
  # deaths by cohort and sex
  
  death_count_cohort_sex_5x1 <- sum_five_year_age_groups(indata = ccmpp_output$death_count_cohort_sex,
                                                         byvar = c("time_start","time_span","sex"))
  
  # net migrants
  
  mig_net_count_age_sex_5x1  <- sum_five_year_age_groups(indata = ccmpp_output$mig_net_count_age_sex,
                                                         byvar = c("time_start","time_span","sex"))
  
  # sum to totals
  # population
  pop_count_tot_sex        <- sum_last_column(ccmpp_output$pop_count_age_sex[, c("time_start", "time_span", "sex", "value")])
  # exposures
  exposure_count_tot_sex   <- sum_last_column(ccmpp_output$exposure_count_age_sex[, c("time_start", "time_span", "sex", "value")])
  # births
  birth_count_tot_sex      <- ccmpp_output$birth_count_tot_sex
  # deaths
  death_count_tot_sex      <- sum_last_column(ccmpp_output$death_count_age_sex[, c("time_start", "time_span", "sex", "value")])
  # net migrants
  mig_net_count_tot_sex    <- sum_last_column(ccmpp_output$mig_net_count_age_sex[, c("time_start", "time_span", "sex", "value")])
  
  # compute population percentage distributions by age
  
  # single year of age
  pop_pct_age_sex <- merge(ccmpp_output$pop_count_age_sex, pop_count_tot_sex, by=c("time_start", "time_span", "sex"))
  pop_pct_age_sex$value <- pop_pct_age_sex$value.x/pop_pct_age_sex$value.y * 100
  pop_pct_age_sex       <- pop_pct_age_sex[, !(names(pop_pct_age_sex) %in% c("value.x", "value.y"))]
  
  # five year age groups
  pop_pct_age_sex_5x1   <- merge(pop_count_age_sex_5x1, pop_count_tot_sex, by=c("time_start", "time_span", "sex"))
  pop_pct_age_sex_5x1 $value <- pop_pct_age_sex_5x1 $value.x/pop_pct_age_sex_5x1 $value.y * 100
  pop_pct_age_sex_5x1   <- pop_pct_age_sex_5x1 [, !(names(pop_pct_age_sex_5x1 ) %in% c("value.x", "value.y"))]
  
  
  
  # compute crude birth, death and net migration rates, as well as rate of natural increase
  birth_rate_crude        <- data.frame(time_start = birth_count_tot_sex$time_start[birth_count_tot_sex$sex == "both"],
                                        time_span  = birth_count_tot_sex$time_span[birth_count_tot_sex$sex == "both"],
                                        value = birth_count_tot_sex$value[birth_count_tot_sex$sex == "both"] / 
                                          exposure_count_tot_sex$value[exposure_count_tot_sex$sex == "both"] * 1000)
  
  death_rate_crude        <- data.frame(time_start = death_count_tot_sex$time_start[death_count_tot_sex$sex == "both"],
                                        time_span  = death_count_tot_sex$time_span[death_count_tot_sex$sex == "both"],
                                        value = death_count_tot_sex$value[death_count_tot_sex$sex == "both"] / 
                                          exposure_count_tot_sex$value[exposure_count_tot_sex$sex == "both"] * 1000)
  
  mig_net_rate_crude      <- data.frame(time_start = mig_net_count_tot_sex$time_start[mig_net_count_tot_sex$sex == "both"],
                                        time_span  = mig_net_count_tot_sex$time_span[mig_net_count_tot_sex$sex == "both"],
                                        value = mig_net_count_tot_sex$value[mig_net_count_tot_sex$sex == "both"] / 
                                          exposure_count_tot_sex$value[exposure_count_tot_sex$sex == "both"] * 1000)
  
  pop_change_rate_natural <- data.frame(time_start = birth_rate_crude$time_start,
                                        time_span  = birth_rate_crude$time_span,
                                        value      = birth_rate_crude$value - death_rate_crude$value)
  
  # compute average annual growth rate
  pop_count_tot_b         <- pop_count_tot_sex[pop_count_tot_sex$sex == "both",]
  pop_change_rate_tot     <- data.frame(time_start = pop_count_tot_b$time_start[1:(nrow(pop_count_tot_b)-1)],
                                        time_span  = pop_count_tot_b$time_span[1:(nrow(pop_count_tot_b)-1)],
                                        value = log(pop_count_tot_b$value[2:nrow(pop_count_tot_b)]/
                                                      pop_count_tot_b$value[1:(nrow(pop_count_tot_b)-1)])*100)
  pop_change_rate_tot$time_span     <- 1
  
  # compute population sex ratio
  pop_sex_ratio           <- data.frame(time_start = pop_count_tot_b$time_start,
                                        time_span  = pop_count_tot_b$time_span,
                                        value      = pop_count_tot_sex$value[pop_count_tot_sex$sex == "male"] / 
                                          pop_count_tot_sex$value[pop_count_tot_sex$sex == "female"] * 100)
  pop_sex_ratio_age_1x1   <- data.frame(time_start = ccmpp_output$pop_count_age_sex$time_start[ccmpp_output$pop_count_age_sex$sex=="male"],
                                        time_span  = ccmpp_output$pop_count_age_sex$time_span[ccmpp_output$pop_count_age_sex$sex=="male"],
                                        age_start  = ccmpp_output$pop_count_age_sex$age_start[ccmpp_output$pop_count_age_sex$sex=="male"],
                                        age_span   = ccmpp_output$pop_count_age_sex$age_span[ccmpp_output$pop_count_age_sex$sex=="male"],
                                        value      = ccmpp_output$pop_count_age_sex$value[ccmpp_output$pop_count_age_sex$sex == "male"] / 
                                          ccmpp_output$pop_count_age_sex$value[ccmpp_output$pop_count_age_sex$sex == "female"] * 100)
  pop_sex_ratio_age_5x1   <- data.frame(time_start = pop_count_age_sex_5x1$time_start[pop_count_age_sex_5x1$sex=="male"],
                                        time_span  = pop_count_age_sex_5x1$time_span[pop_count_age_sex_5x1$sex=="male"],
                                        age_start  = pop_count_age_sex_5x1$age_start[pop_count_age_sex_5x1$sex=="male"],
                                        age_span   = pop_count_age_sex_5x1$age_span[pop_count_age_sex_5x1$sex=="male"],
                                        value      = pop_count_age_sex_5x1$value[pop_count_age_sex_5x1$sex == "male"] / 
                                          pop_count_age_sex_5x1$value[pop_count_age_sex_5x1$sex == "female"] * 100)
  
  
  # compute some fertility indicators
  # fertility rates by 5-year age group of mother
  fert_rate_age_5x1      <- data.frame(time_start  = birth_count_age_5x1$time_start,
                                       time_span   = birth_count_age_5x1$time_span,
                                       age_start   = birth_count_age_5x1$age_start,
                                       age_span    = birth_count_age_5x1$age_span,
                                       value = birth_count_age_5x1$value/
                                         exposure_count_age_sex_5x1$value[which(exposure_count_age_sex_5x1$sex == "female")])
  # total fertility rate
  fert_rate_tot          <- sum_last_column(ccmpp_output$fert_rate_age_f[, c("time_start", "time_span", "value")])
  
  # gross reproduction rate
  fert_rate_gross        <- fert_gross(fert_data_age = ccmpp_output$fert_rate_age_f,
                                       srb           = ccmpp_output$srb,
                                       byvar         = c("time_start", "time_span"))
  
  # net reproduction rate
  fert_rate_net          <- fert_net(fert_data_age = ccmpp_output$fert_rate_age_f,
                                     srb           = ccmpp_output$srb,
                                     nLx           = ccmpp_output$lt_complete_age_sex[ccmpp_output$lt_complete_age_sex$sex == "female" &
                                                                                        ccmpp_output$lt_complete_age_sex$indicator == "lt_nLx",],
                                     byvar         = c("time_start", "time_span"))
  
  # percentage age-specific fertility rates
  fert_pct_age_1x1       <- fert_pasfr(fert_data_age = ccmpp_output$fert_rate_age_f,
                                       byvar         = c("time_start", "time_span"))
  
  
  fert_pct_age_5x1       <- fert_pasfr(fert_data_age = fert_rate_age_5x1,
                                       byvar         = c("time_start", "time_span"))
  
  # mean age of childbearing
  fert_mean_age          <- fert_mac(fert_data_age = ccmpp_output$fert_rate_age_f,
                                     byvar         = c("time_start", "time_span"))
  
  # compile output list and return
  # assemble all estimates to send to Eagle
  ccmppWPP_output <- list(pop_count_age_sex_1x1      = ccmpp_output$pop_count_age_sex,
                          pop_count_age_sex_5x1      = pop_count_age_sex_5x1,
                          pop_count_tot_sex          = pop_count_tot_sex,
                          pop_pct_age_sex_1x1        = pop_pct_age_sex,
                          pop_pct_age_sex_5x1        = pop_pct_age_sex_5x1,
                          pop_change_rate_tot        = pop_change_rate_tot,
                          pop_change_rate_natural    = pop_change_rate_natural,
                          pop_sex_ratio              = pop_sex_ratio,
                          pop_sex_ratio_age_1x1      = pop_sex_ratio_age_1x1,
                          pop_sex_ratio_age_5x1      = pop_sex_ratio_age_5x1,
                          birth_count_age_1x1        = birth_count_age_1x1,
                          birth_count_age_5x1        = birth_count_age_5x1,
                          birth_count_tot_sex        = ccmpp_output$birth_count_tot_sex,
                          birth_rate_crude           = birth_rate_crude,
                          fert_rate_age_1x1          = ccmpp_output$fert_rate_age_f,
                          fert_rate_age_5x1          = fert_rate_age_5x1,
                          fert_pct_age_1x1           = fert_pct_age_1x1,
                          fert_pct_age_5x1           = fert_pct_age_5x1,
                          fert_rate_tot              = fert_rate_tot,
                          fert_rate_gross            = fert_rate_gross,
                          fert_rate_net              = fert_rate_net,
                          fert_mean_age              = fert_mean_age,
                          srb                        = ccmpp_output$srb,
                          death_count_age_sex_1x1    = ccmpp_output$death_count_age_sex,
                          death_count_age_sex_5x1    = death_count_age_sex_5x1,
                          death_count_tot_sex        = death_count_tot_sex,
                          death_rate_crude           = death_rate_crude,
                          death_count_cohort_sex_1x1 = ccmpp_output$death_count_cohort_sex,
                          death_count_cohort_sex_5x1 = death_count_cohort_sex_5x1,
                          exposure_count_age_sex_1x1 = ccmpp_output$exposure_count_age_sex,
                          exposure_count_age_sex_5x1 = exposure_count_age_sex_5x1,
                          lt_complete_age_sex        = lt_complete_age_sex,
                          lt_abridged_age_sex        = lt_abridged_age_sex,
                          lt_summary                 = lt_summary,
                          mig_net_count_age_sex_1x1  = ccmpp_output$mig_net_count_age_sex,
                          mig_net_count_age_sex_5x1  = mig_net_count_age_sex_5x1,
                          mig_net_count_tot_sex      = mig_net_count_tot_sex,
                          mig_net_rate_crude         = mig_net_rate_crude)
  
  attr(ccmppWPP_output, "revision") <- atr$revision
  attr(ccmppWPP_output, "locid")   <- atr$locid
  attr(ccmppWPP_output, "locname")<- atr$locname
  attr(ccmppWPP_output, "variant") <- atr$variant
  attr(ccmppWPP_output, "a0rule") <- atr$a0rule
  
  return(ccmppWPP_output)
  
}


ccmppWPP_migration_override <- function(ccmpp_input, ccmpp_output) {
  
  # compile warning messages to be communicated to analysts when mig counts have been modified to avoid negative population
  # migration counts 
  
  # vector of time_start for which input mig_type == "counts"
  mig_count_times <- ccmpp_input$mig_parameter$time_start[which(ccmpp_input$mig_parameter$indicator == "mig_type" & 
                                                                  ccmpp_input$mig_parameter$value == "counts")]
  # get inputs total for age group 100+
  total_100 <- sum_last_column(ccmpp_input$mig_net_count_age_sex[ccmpp_input$mig_net_count_age_sex$time_start %in% mig_count_times &
                                                                   ccmpp_input$mig_net_count_age_sex$age_start >= 100 ,
                                                                 c("time_start", "time_span", "sex", "value")])
  total_100$age_start <- 100
  total_100$age_span <- 1000
  mig_net_count_age_sex_compare <- rbind(ccmpp_input$mig_net_count_age_sex[ccmpp_input$mig_net_count_age_sex$time_start %in% mig_count_times &
                                                                             ccmpp_input$mig_net_count_age_sex$age_start < 100 ,],
                                         total_100)
  
  
  mig_net_count_age_sex_compare <- merge(mig_net_count_age_sex_compare, 
                                         ccmpp_output$mig_net_count_age_sex[, c("time_start", "age_start", "sex", "value")],
                                         by = c("time_start", "age_start",  "sex"),
                                         all.x = TRUE, all.y = FALSE)
  mig_net_count_age_sex_override <- mig_net_count_age_sex_compare[mig_net_count_age_sex_compare$value.x != 
                                                                    mig_net_count_age_sex_compare$value.y,]
  names(mig_net_count_age_sex_override)[c(6,7)] <- c("value.input", "value.output")
  
  if (nrow(mig_net_count_age_sex_override) > 0) {
    attr(mig_net_count_age_sex_override, "warning") <- paste0("Warning: input net migrant count has been overridden for ", 
                                                              nrow(mig_net_count_age_sex_override), " time-age-sex combinations in order to avoid negative population.")
    
  } else if (nrow(mig_net_count_age_sex_override) == 0) {
    mig_net_count_age_sex_override <- NULL
  }
  
  return(mig_net_count_age_sex_override)
}
