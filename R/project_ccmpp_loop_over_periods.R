#------------------------------------------------------------

#' Loop over periods for cohort component population projection
#'
#' @description This function takes a set of base population and time series of demographic inputs 
#' and carries out the cohort component population projection over the specified periods
#'
#' @author Sara Hertog
#'
#' @param z numeric. width of age groups = length of one-step projection horizon 
#' @param year_start numeric. base year of projection
#' @param year_stop numeric. end year for projection
#' @param pop_base_input_m numeric. vector of male population counts at year=year_start
#' @param pop_base_input_f numeric. vector of female population counts at year=year_start
#' @param fert_input data frame. with columns "year", "age" and "value" with value containing age-specific 
#' fertility rates
#' @param srb data frame. with columns "year" and "value" with value containing the sex-ratio at birth (M/F)
#' @param mort_input_m data frame. with columns "year", "age" and "value" with value containing time and 
#' age-specific life table survival ratios for males
#' @param mort_input_f data frame. with columns "year", "age" and "value" with value containing time and 
#' age-specific life table survival ratios for females
#' @param migration_type character. "counts" if migration inputs are counts of net migrants; "rates" if migration 
#' inputs are as a proportion of population by age and sex.
#' @param mig_input_m data frame. with columns "year", "age" and "value" with value containing time and 
#' age-specific net migration values for males. Can be expressed as net counts or as a proportion of population (rates).
#' @param mig_input_f data frame. with columns "year", "age" and "value" with value containing time and 
#' age-specific net migration values for femles. Can be expressed as net counts or as a proportion of population (rates).
#' @param migration_assumption data frame. with columns "year" and "value" where value is a character string 
#' indicating whether the project_ccmpp_z_by_z function should implement net migration "even"ly over period or at
#' "end" of period. 
#' #'
#' @details This function takes a full set of base population and demographic inputs for a country over time and 
#' implements a cohort-component population projection with steps of length z by looping through the 
#' project_ccmpp_z_by_z() function.
#'
#' @return a list of objects with step by step cohort-component population projection results, including population
#' by age and sex, period deaths by cohort and sex, births by age of the mother, total period births by sex, and net 
#' migrant counts by age and sex
#' 
#' @export

project_ccmpp_loop_over_periods <- function(z=1, year_start, year_stop, pop_base_input_m, pop_base_input_f, 
                                            fert_input, srb_input, mort_input_m, mort_input_f, 
                                            migration_type, mig_input_m, mig_input_f, migration_assumption) {
  # initialize output
  projection_output_list <- list()

  # define projection steps
  for (year in year_start:(year_stop-z)) {

    # starting populations    
    if (year == year_start) {
      
      pop_f_start <- pop_base_input_f
      pop_m_start <- pop_base_input_m
      
    } 
    
    # subset demographic rates for the one-step projection
    # fertility
    asfr           <- fert_input[which(fert_input$year == year), "value"]
    srb            <- as.numeric(srb_input[which(srb_input$year == year), "value"], drop=TRUE)
    
    # mortality
    mort_f         <- mort_input_f[which(mort_input_f$year == year), "value"]
    mort_m         <- mort_input_m[which(mort_input_m$year == year), "value"]

    # net international migration
    mig_f          <- mig_input_f[which(mig_input_f$year == year), "value"]
    mig_m          <- mig_input_m[which(mig_input_m$year == year), "value"]
    mig_assumption <- migration_assumption[which(migration_assumption$year == year), "value"]
    
    if (migration_type == "rates") {
      
      mig_f <- mig_f * pop_f_start
      mig_m <- mig_m * pop_m_start
      
    }
    
    # project population forward one step
    fwd_1_step <- project_ccmpp_z_by_z(z=z, 
                                       pop_count_age_m_begin = pop_m_start, 
                                       pop_count_age_f_begin = pop_f_start, 
                                       survival_ratio_age_m = mort_m, 
                                       survival_ratio_age_f = mort_f,
                                       fert_rate_age_f = asfr, 
                                       srb = srb,
                                       mig_net_count_age_m = mig_m,
                                       mig_net_count_age_f = mig_f, 
                                       mig_assumption = mig_assumption)
    
    # append net migrant count to output
    # necessary to retain values in case "rates" option was used and counts were computed within loop function
    fwd_1_step$mig_net_count_age_m   <- mig_m
    fwd_1_step$mig_net_count_age_f   <- mig_f
    
    # append indentifying information about the projection interval
    fwd_1_step$year_start            <- year
    fwd_1_step$year_end              <- year+z
    
    # set end population as starting population for next projection step
    pop_f_start <- as.numeric(fwd_1_step$pop_count_age_f_end, drop=TRUE)
    pop_m_start <- as.numeric(fwd_1_step$pop_count_age_m_end, drop=TRUE)
    
    projection_output_list[[year-year_start+1]] <- fwd_1_step
      
  }
  
  # organize the resulting output by indicator for easier parsing later
  
  # population by age and sex
  pop_f   <- data.frame(age = c(0:100), 
                        base=pop_base_input_f, 
                        sapply(projection_output_list, "[[", "pop_count_age_f_end"))
  pop_f    <- reshape(pop_f, idvar="age", 
                      varying=list(2:ncol(pop_f)), 
                      times=seq(year_start, year_stop, z),
                      timevar = "year",
                      v.names="female", direction = "long")
  
  pop_m    <- data.frame(age = c(0:100), 
                         base=pop_base_input_m, 
                         sapply(projection_output_list, "[[", "pop_count_age_m_end"))
  pop_m    <- reshape(pop_m, idvar="age", 
                      varying=list(2:ncol(pop_m)), 
                      times=seq(year_start, year_stop, z),
                      timevar = "year",
                      v.names="male", direction = "long")
  
  pop               <- data.frame(pop_f, male = pop_m$male)
  pop$both          <- pop$female + pop$male
  pop_count_age_sex <- reshape(pop, idvar=c("year","age"),
                               varying = list(3:5),
                               times = names(pop)[3:5],
                               timevar = "sex",
                               v.names = "value",
                               direction = "long")
  pop_count_age_sex <- pop_count_age_sex[,c("year", "sex", "age", "value")]
  
  
  # cohort deaths by age and sex
  dth_f <- data.frame(age = c(0:100), 
                      sapply(projection_output_list, "[[", "death_count_cohort_f"))
  dth_f <- reshape(dth_f, idvar="age", 
                   varying=list(2:ncol(dth_f)), 
                   times=seq(year_start, year_stop-z, z),
                   timevar = "year",
                   v.names="female", direction = "long")
  
  dth_m <- data.frame(age = c(0:100),
                      sapply(projection_output_list, "[[", "death_count_cohort_m"))
  dth_m <- reshape(dth_m, idvar="age", 
                   varying=list(2:ncol(dth_m)), 
                   times=seq(year_start, year_stop-z, z),
                   timevar = "year",
                   v.names="male", direction = "long")
  
  dth                    <- data.frame(dth_f, male = dth_m$male)
  dth$both               <- dth$female + dth$male
  death_count_cohort_sex <- reshape(dth, idvar=c("year","age"),
                               varying = list(3:5),
                               times = names(dth)[3:5],
                               timevar = "sex",
                               v.names = "value",
                               direction = "long")
  death_count_cohort_sex <- death_count_cohort_sex[,c("year", "sex", "age", "value")]
  
  # births by age of mother
  bth_age           <- data.frame(age = c(0:100), 
                                  sapply(projection_output_list, "[[", "birth_count_age_b"))
  birth_count_age_b <- reshape(bth_age, idvar="age", 
                               varying=list(2:ncol(bth_age)), 
                               times=seq(year_start, year_stop-z, z),
                               timevar = "year",
                               v.names="value", direction = "long")
  birth_count_age_b <- birth_count_age_b[, c("year", "age", "value")]
  
  # total births by sex
  bth_f <- data.frame(year = seq(year_start, year_stop-z, z),
                      sex = "female", 
                      value = sapply(projection_output_list, "[[", "birth_count_tot_f"))
  bth_m <- data.frame(year = seq(year_start, year_stop-z, z),
                      sex = "male", 
                      value = sapply(projection_output_list, "[[", "birth_count_tot_m"))
  bth_b <- data.frame(year = seq(year_start, year_stop-z, z),
                      sex = "both", 
                      value = sapply(projection_output_list, "[[", "birth_count_tot_b"))
  
  birth_count_tot_sex     <- rbind(bth_f, bth_m, bth_b)
  birth_count_tot_sex$sex <- as.character(birth_count_tot_sex$sex)
  
  # net migrants by age and sex
  mig_f <- data.frame(age = c(0:100), 
                      sapply(projection_output_list, "[[", "mig_net_count_age_f"))
  mig_f <- reshape(mig_f, idvar="age", 
                   varying=list(2:ncol(mig_f)), 
                   times=seq(year_start, year_stop-z, z),
                   timevar = "year",
                   v.names="female", direction = "long")
  
  mig_m <- data.frame(age = c(0:100),
                      sapply(projection_output_list, "[[", "mig_net_count_age_m"))
  mig_m <- reshape(mig_m, idvar="age", 
                   varying=list(2:ncol(mig_m)), 
                   times=seq(year_start, year_stop-z, z),
                   timevar = "year",
                   v.names="male", direction = "long")
  
  mig                   <- data.frame(mig_f, male = mig_m$male)
  mig$both              <- mig$female + mig$male
  mig_net_count_age_sex <- reshape(mig, idvar=c("year","age"),
                                    varying = list(3:5),
                                    times = names(mig)[3:5],
                                    timevar = "sex",
                                    v.names = "value",
                                    direction = "long")
  mig_net_count_age_sex <- mig_net_count_age_sex[,c("year", "sex", "age", "value")]
  
  

  ccmpp_output <- list(pop_count_age_sex = pop_count_age_sex,
                       death_count_cohort_sex = death_count_cohort_sex,
                       birth_count_age_b = birth_count_age_b,
                       birth_count_tot_sex = birth_count_tot_sex,
                       mig_net_count_age_sex = mig_net_count_age_sex)
  return(ccmpp_output)
}


