#test <- project_ccmppWPP_one_country_variant(ccmpp_input = canada_wpp_1950_2020_ccmpp_inputs_1x1)

# works for canada and mexico inputs. Fails for Kuwait, I think because of weird life table values.

project_ccmppWPP_one_country_variant <- function(ccmpp_input){

options(stringsAsFactors=FALSE)

z <- ccmpp_input$age_width
year_start <- ccmpp_input$year_base
year_stop <- ccmpp_input$year_stop
oag <- max(ccmpp_input$age)

# run cohort component population projection from base year to end of estimation period
ccmpp_output <- 
  project_ccmpp_loop_over_periods(z = z,
                                  year_start       = year_start,
                                  year_stop        = year_stop,
                                  pop_base_input_m = ccmpp_input$pop_count_age_sex_base$value[which(ccmpp_input$pop_count_age_sex_base$sex=="male")],
                                  pop_base_input_f = ccmpp_input$pop_count_age_sex_base$value[which(ccmpp_input$pop_count_age_sex_base$sex=="female")],
                                  fert_input       = ccmpp_input$fert_rate_age_f,
                                  srb_input        = ccmpp_input$srb,
                                  mort_input_m     = ccmpp_input$life_table_age_sex[with(ccmpp_input$life_table_age_sex, sex=="male" & indicator=="lt_Sx"),],
                                  mort_input_f     = ccmpp_input$life_table_age_sex[with(ccmpp_input$life_table_age_sex, sex=="female" & indicator=="lt_Sx"),],
                                  migration_type   = "counts",
                                  mig_input_m      = ccmpp_input$mig_net_count_age_sex[which(ccmpp_input$mig_net_count_age_sex$sex=="male"),],
                                  mig_input_f      = ccmpp_input$mig_net_count_age_sex[which(ccmpp_input$mig_net_count_age_sex$sex=="female"),],
                                  migration_assumption = ccmpp_input$mig_assumption)

# compute period exposures by age and sex
  exposure_output_list <- list()
    n <- 0
  for (yr in seq(year_start, year_stop-z, z)) {
    for (sx in c("female", "male")) {
      n   <- n+1
      exp <- exposure_age_period(pop_age_begin        = ccmpp_output$pop_count_age_sex$value[with(ccmpp_output$pop_count_age_sex, year == yr & sex == sx)],
                                 pop_age_end          = ccmpp_output$pop_count_age_sex$value[with(ccmpp_output$pop_count_age_sex, year == yr+z & sex == sx)],
                                 migration_assumption = ccmpp_input$mig_assumption$value[with(ccmpp_input$mig_assumption, year == yr)],
                                 mig_net_count        = ccmpp_output$mig_net_count_age_sex$value[with(ccmpp_output$mig_net_count_age_sex, year == yr & sex == sx)])
      
      exposure_output_list[[n]] <- data.frame(year = yr,
                                              sex = sx,
                                              age = ccmpp_output$pop_count_age_sex$age[with(ccmpp_output$pop_count_age_sex, year == yr & sex == sx)],
                                              value = exp)
    }
  }
  exposure_count_age_sex <- do.call(rbind, exposure_output_list)
  exp_both               <- sum_last_column(exposure_count_age_sex[, c("year", "age", "value")])
  exp_both$sex           <- "both"
  exposure_count_age_sex <- rbind(exposure_count_age_sex, exp_both)


# compute period deaths by age and sex
  death_output_list <- list()
    n <- 0
  for (yr in seq(year_start, year_stop-z, z)) {
    for (sx in c("female", "male")) {
      n   <-n+1
      dth <- death_cohort_period_to_age_period(mort_rate_age_period = ccmpp_input$life_table_age_sex$value[with(ccmpp_input$life_table_age_sex, 
                                                                                                                indicator == "lt_nMx" & year == yr & sex == sx)],
                                                exposure_age_period = exposure_count_age_sex$value[with(exposure_count_age_sex, 
                                                                                                        year==yr & sex==sx)],
                                                distribute_residual = TRUE,
                                                death_cohort_period = ccmpp_output$death_count_cohort_sex$value[with(ccmpp_output$death_count_cohort_sex, 
                                                                                                                     year == yr & sex == sx)])
      
      
      death_output_list[[n]] <- data.frame(year = yr,
                                           sex = sx,
                                           age = unique(ccmpp_input$life_table_age_sex$age[with(ccmpp_input$life_table_age_sex, year == yr & sex == sx)]),
                                           value = dth)
    }
  }
  death_count_age_sex   <- do.call(rbind, death_output_list)
  dth_both              <- sum_last_column(death_count_age_sex[, c("year", "age", "value")])
  dth_both$sex          <- "both"
  death_count_age_sex   <- rbind(death_count_age_sex, dth_both)
  
  # check that period deaths by age are equal to period deaths by cohort
  # sum(death_count_age_sex$value[which(death_count_age_sex$sex=="female" & death_count_age_sex$year==2019)])
  # sum(ccmpp_output$death_count_cohort_sex$value[which(ccmpp_output$death_count_cohort_sex$sex=="female" &
  #                                                       ccmpp_output$death_count_cohort_sex$year==2019)])
  
# compute both sexes combined life table 
  lt_output_list <- list()
  
  for (yr in seq(year_start, year_stop-z, z)) {
    
    deaths <- death_count_age_sex[with(death_count_age_sex, year==yr & sex=="both"), "value"]
    exposures <- exposure_count_age_sex[with(exposure_count_age_sex, year==yr & sex=="both"), "value"]
    
    lt_both <- lt_single_mx(nMx = deaths/exposures, sex="b")
    lt_both <- lt_long(lt_both)
    lt_both$year <- yr
    
    lt_output_list[[yr-year_start+1]] <- lt_both
  
  }
  life_table_age_b      <- do.call(rbind, lt_output_list)
  life_table_age_b$sex  <- "both"

# assemble all complete life tables
  lt_complete_age_sex   <- rbind(ccmpp_input$life_table_age_sex, life_table_age_b)

# compute abridged life tables from complete life tables
  lt_output_list <- list()
      n=0
  for (yr in seq(year_start, year_stop-z, z)) {
    for (sx in c("female", "male", "both")) {
      n<-n+1
  
    lt_complete         <- lt_complete_age_sex[with(lt_complete_age_sex, year==yr & sex==sx & indicator=="lt_lx"),]
    lt_abridged         <- lt_abridged_from_complete(age_complete = lt_complete$age,
                                                     lx_complete = lt_complete$value,
                                                     sex=sx)
    lt_abridged$year    <- yr
    lt_abridged$sex     <- sx
  
    lt_abridged         <- lt_abridged[, c("indicator","year","sex","age","value")]
    
    lt_output_list[[n]] <- lt_abridged
    }
  }
  
  lt_abridged_age_sex        <- do.call(rbind, lt_output_list)


# compute summary life table values
  lt_summary                 <- lt_summary(lt_data = lt_abridged_age_sex, 
                                           byvar = c("year","sex"))


# aggregate select indicators by five year age group
  

  # population
  age                        <- floor(ccmpp_output$pop_count_age_sex$age/5)*5
  pop_count_age_sex_5x1      <- sum_last_column(cbind(age,
                                                      ccmpp_output$pop_count_age_sex[,c("year","sex","value")]))
  pop_count_age_sex_5x1      <- pop_count_age_sex_5x1[, c("year", "sex", "age", "value")]

  # births
  age                        <- floor(ccmpp_output$birth_count_age_b$age/5)*5
  birth_count_age_b_5x1      <- sum_last_column(cbind(age,
                                                      ccmpp_output$birth_count_age_b[,c("year","value")]))
  birth_count_age_b_5x1      <- birth_count_age_b_5x1[, c("year", "age", "value")]
  
  # exposures
  age                        <- floor(exposure_count_age_sex$age/5)*5
  exposure_count_age_sex_5x1 <- sum_last_column(cbind(age,
                                                      exposure_count_age_sex[,c("year","sex","value")]))
  exposure_count_age_sex_5x1 <- exposure_count_age_sex_5x1[, c("year", "sex", "age", "value")]
  
  # deaths
  age                        <- floor(death_count_age_sex$age/5)*5
  death_count_age_sex_5x1    <- sum_last_column(cbind(age,
                                                      death_count_age_sex[,c("year","sex","value")]))
  death_count_age_sex_5x1    <- death_count_age_sex_5x1[, c("year", "sex", "age", "value")]

  # net migrants
  age                        <- floor(ccmpp_output$mig_net_count_age_sex$age/5)*5
  mig_net_count_age_sex_5x1  <- sum_last_column(cbind(age,
                                                      ccmpp_output$mig_net_count_age_sex[,c("year","sex","value")]))

# aggregate select indicators to totals over age, by sex
  
  # population
  pop_count_tot_sex        <- sum_last_column(ccmpp_output$pop_count_age_sex[, c("year", "sex", "value")])
  # deaths 
  death_count_tot_sex      <- sum_last_column(death_count_age_sex[, c("year", "sex", "value")])
  # net migrants
  mig_net_count_tot_sex    <- sum_last_column(ccmpp_output$mig_net_count_age_sex[, c("year", "sex", "value")])
  

# compute some fertility indicators
  
  # fertility rates by 5-year age group of mother
  fert_rate_age_f_5x1      <- data.frame(year  = birth_count_age_b_5x1$year,
                                         age   = birth_count_age_b_5x1$age,
                                         value = birth_count_age_b_5x1$value/
                                           exposure_count_age_sex_5x1$value[which(exposure_count_age_sex_5x1$sex == "female")])
  # total fertility rate
  fert_rate_tot_f          <- sum_last_column(ccmpp_input$fert_rate_age_f[, c("year", "value")])

  
  # percentage age-specific fertility rates
  fert_pct_age_f_1x1       <- fert_pasfr(fert_data_age = ccmpp_input$fert_rate_age_f,
                                         byvar         = "year")

  
  fert_pct_age_f_5x1       <- fert_pasfr(fert_data_age = fert_rate_age_f_5x1,
                                         byvar         = "year")

  # mean age of childbearing
  fert_mean_age_f          <- fert_mac(fert_data_age = ccmpp_input$fert_rate_age_f,
                                       byvar         = "year")

  
# assemble all estimates to send to Eagle
ccmppWPP_output <- list(LocID                      = ccmpp_input$LocID,
                        variant                    = ccmpp_input$variant,
                        pop_count_age_sex_1x1      = ccmpp_output$pop_count_age_sex,
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
                        exposure_count_age_sex_1x1 = exposure_count_age_sex,
                        exposure_count_age_sex_5x1 = exposure_count_age_sex_5x1,
                        lt_complete_age_sex        = lt_complete_age_sex,
                        lt_abridged_age_sex        = lt_abridged_age_sex,
                        lt_summary                 = lt_summary,
                        mig_net_count_age_sex_1x1  = ccmpp_output$mig_net_count_age_sex,
                        mig_net_count_age_sex_5x1  = mig_net_count_age_sex_5x1,
                        mig_net_count_tot_sex      = mig_net_count_tot_sex,
                        mig_assumption             = ccmpp_input$mig_assumption
                      )
  
return(ccmppWPP_output)
}
#            