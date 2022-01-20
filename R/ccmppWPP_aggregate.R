# 
# # 
# library(tidyverse)
# locationIDs <- c(124, 414, 4)
# base_year <- 1950
# last_year <- 2021
# intermediate_output_folder <- paste0( "C:/Users/SARAH/OneDrive - United Nations/WPP2021/ccmppWPP/ccmppWPP_outputs/Intermediate/Estimates/")
# 
# test <- ccmppWPP_aggregate(locationIDs, base_year, last_year, intermediate_output_folder, LocIDnew = 8888, LocNamenew = "Pretend Location")

## 
## 

#------------------------------------------------------------

#' aggregate ccmppWPP outputs across multiple location ids
#'
#' @description This function takes a vector of locids, compiles the ccmppWPP intermediate outputs (from ages 0:130) for
#' those locations, and computes the aggregate values for each demographic component 
#'
#' @author Sara Hertog
#'
#' @param wpp_input list of input objects required for one country-variant
#'
#' @details xx
#'
#' @return returns an "intermediate_output" list of dataframes with all ccmpp components by sex and ages 0 to 130
#'
#' @export
#'
#'
ccmppWPP_aggregate <- function(locationIDs, base_year, last_year, intermediate_output_folder, LocIDnew, LocNamenew) {
  
  location_outputs <- ccmppWPP_compile(locationIDs, base_year, last_year, intermediate_output_folder)
  
  pop_count_age_sex <- do.call(rbind, lapply(location_outputs, "[[", "pop_count_age_sex")) 
  pop_count_age_sex <- sum_last_column(pop_count_age_sex)
  
  death_count_cohort_sex <- do.call(rbind, lapply(location_outputs, "[[", "death_count_cohort_sex")) 
  death_count_cohort_sex <- sum_last_column(death_count_cohort_sex)
  
  death_count_age_sex <- do.call(rbind, lapply(location_outputs, "[[", "death_count_age_sex")) 
  death_count_age_sex <- sum_last_column(death_count_age_sex)
  
  exposure_count_age_sex <- do.call(rbind, lapply(location_outputs, "[[", "exposure_count_age_sex")) 
  exposure_count_age_sex <- sum_last_column(exposure_count_age_sex)
  
  birth_count_age_b <- do.call(rbind, lapply(location_outputs, "[[", "birth_count_age_b")) 
  birth_count_age_b <- sum_last_column(birth_count_age_b)
  
  birth_count_tot_sex <- do.call(rbind, lapply(location_outputs, "[[", "birth_count_tot_sex")) 
  birth_count_tot_sex <- sum_last_column(birth_count_tot_sex)
  
  mig_net_count_age_sex <- do.call(rbind, lapply(location_outputs, "[[", "mig_net_count_age_sex")) 
  mig_net_count_age_sex <- sum_last_column(mig_net_count_age_sex)
  
  # compute sex ratio at birth from total birth by sex
  srb <- birth_count_tot_sex %>% 
    dplyr::filter(sex %in% c("male", "female")) %>% 
    pivot_wider(names_from = "sex", values_from = "value") %>% 
    mutate(value = male/female) %>% 
    select(time_start, time_span, value)

  # compute age-specific fertility rates from births by age of mother and female exposures
  fert_rate_age_f <- birth_count_age_b %>% 
    rename(births = value) %>% 
    left_join(exposure_count_age_sex %>% dplyr::filter(sex == "female") %>% rename(exposures = value) %>% select(time_start, time_span, age_start, age_span, exposures),
              by = c("time_start", "time_span", "age_start", "age_span")) %>%
    mutate(value = births/exposures) %>% 
    arrange(time_start, age_start) %>% 
    select(-births, -exposures)

  # compute age-specific mortality rates from deaths by sex and age and exposures by sex and age
  lt_nMx <- merge(death_count_age_sex, exposure_count_age_sex, 
                  by = c("time_start", "time_span", "sex", "age_start", "age_span"), all.x = TRUE, all.y = TRUE ) 
  lt_nMx$value <- lt_nMx$value.x/lt_nMx$value.y
  lt_nMx <- lt_nMx[order(lt_nMx$time_start, lt_nMx$sex, lt_nMx$age_start),  
                   c("time_start", "time_span", "sex", "age_start", "age_span", "value")]
  
  years <- base_year:(last_year-1)
  
  life_table_age_sex <- lapply(1:length(years), function(x){
    
    nMx <- lt_nMx %>% dplyr::filter(time_start == years[x]) %>% 
      arrange(time_start, sex, age_start)
    lt_m <- DemoTools::lt_single_mx(nMx = nMx$value[nMx$sex == "male"], Age = nMx$age_start[nMx$sex == "male"], Sex = "m", a0rule = "ak")
    lt_f <- DemoTools::lt_single_mx(nMx = nMx$value[nMx$sex == "female"], Age = nMx$age_start[nMx$sex == "female"], Sex = "f", a0rule = "ak")
    lt_b <- DemoTools::lt_single_mx(nMx = nMx$value[nMx$sex == "both"], Age = nMx$age_start[nMx$sex == "both"], Sex = "b", a0rule = "ak")
    
    lts_one_year <- lt_m %>% 
      mutate(sex = "male") %>% 
      bind_rows(lt_f %>% mutate(sex = "female")) %>% 
      bind_rows(lt_b %>% mutate(sex = "both")) %>% 
      mutate(time_start = years[x],
             AgeInt = replace(AgeInt, is.na(AgeInt), 1000)) %>%
      rename(age_start = Age,
             age_span = AgeInt) %>% 
      pivot_longer(cols = 3:11, names_to = "indicator", values_to = "value") %>% 
      mutate(indicator = paste0("lt_", indicator),
             time_span = 1) %>% 
      arrange(indicator, time_start, sex, age_start) %>% 
      select(indicator, time_start, time_span, sex, age_start, age_span, value)
    
    return(lts_one_year)
  }) 
  lt_complete_age_sex <- do.call(rbind, life_table_age_sex)
  
  intermediate_output <- list(pop_count_age_sex = pop_count_age_sex,
                              death_count_cohort_sex = death_count_cohort_sex,
                              death_count_age_sex = death_count_age_sex,
                              exposure_count_age_sex = exposure_count_age_sex,
                              lt_complete_age_sex = lt_complete_age_sex,
                              fert_rate_age_f = fert_rate_age_f,
                              srb = srb,
                              birth_count_age_b = birth_count_age_b,
                              birth_count_tot_sex = birth_count_tot_sex,
                              mig_net_count_age_sex = mig_net_count_age_sex)
  
  attr(intermediate_output, "locid")    <- LocIDnew
  attr(intermediate_output, "locname")  <- LocNamenew
  attr(intermediate_output, "variant")  <- attributes(location_outputs)$variant
  attr(intermediate_output, "a0rule")   <- "ak"
  
  return(intermediate_output)
  
}

# compile the intermediate output files into a list with one record per location so that the demographic components can be aggregated
ccmppWPP_compile <- function(locationIDs, base_year, last_year, intermediate_output_folder) {
  
  location_outputs <- list()
  for (i in 1:length(locationIDs)) {
    
    # load ccmpp intermediate outputs for one country
    ccmpp_output <- NULL
    has_outputs <- file.exists(paste0(intermediate_output_folder, locationIDs[i], "_ccmpp_output.RData"))
    if (has_outputs) {
      load(paste0(intermediate_output_folder, locationIDs[i], "_ccmpp_output.RData")) # need to verify whether we changed ccmpp intermediates to rda
    } else {
      print(paste0("Error: No CCMPP outputs available for LocID = ", locationIDs[i]))
      stop()
    }
    
    # check whether base year agrees
    check_base_year <- min(ccmpp_output$pop_count_age_sex$time_start) == base_year
    if (!check_base_year) {
      print(paste0("Error: Base year for LocID = ", locationIDs[i], " not equal to ", base_year))
      stop()
    }
    
    # check whether records for all years between base_year and last_year are present
    check_all_years <- all(base_year:last_year %in% unique(ccmpp_output$pop_count_age_sex$time_start))
    if (!check_all_years) {
      missing_years <- c(base_year:last_year)[!(base_year:last_year %in% unique(ccmpp_output$pop_count_age_sex$time_start))]
      print(paste0("Error: CCMPP outputs for LocID = ", locationIDs[i], " missing records for time_start = ", missing_years))
      stop()
    }
    
    if (check_base_year & check_all_years) {
      location_outputs[[i]] <- ccmpp_output 
    } 
  }

  attr(location_outputs, "variant")  <- attributes(ccmpp_output)$variant
  
  return(location_outputs)
}