# 
# # # 
# library(tidyverse)
# locationIDs <- c(124, 414, 4, 2222)
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
#'

ccmppWPP_aggregate <- function(locationIDs, base_year, last_year, intermediate_output_folder, LocIDnew, LocNamenew) {
  
  location_outputs <- ccmppWPP_compile(locationIDs, base_year, last_year, intermediate_output_folder)
  
  pop_count_age_sex <- location_outputs[[1]]$pop_count_age_sex %>% mutate(value = 0)
  for (i in 1:length(location_outputs)) {
    pop_count_age_sex <- pop_count_age_sex %>% 
      left_join(location_outputs[[i]]$pop_count_age_sex, by = c("time_start", "time_span", "sex", "age_start", "age_span")) %>% 
      mutate(value = value.x + value.y) %>% 
      select(-value.x, -value.y)
  }
  
  death_count_cohort_sex <- location_outputs[[1]]$death_count_cohort_sex %>% mutate(value = 0)
  for (i in 1:length(location_outputs)) {
    death_count_cohort_sex <- death_count_cohort_sex %>% 
      left_join(location_outputs[[i]]$death_count_cohort_sex, by = c("time_start", "time_span", "sex", "age_start", "age_span")) %>% 
      mutate(value = value.x + value.y) %>% 
      select(-value.x, -value.y)
  }
  
  death_count_age_sex <- location_outputs[[1]]$death_count_age_sex %>% mutate(value = 0)
  for (i in 1:length(location_outputs)) {
    death_count_age_sex <- death_count_age_sex %>% 
      left_join(location_outputs[[i]]$death_count_age_sex, by = c("time_start", "time_span", "sex", "age_start", "age_span")) %>% 
      mutate(value = value.x + value.y) %>% 
      select(-value.x, -value.y)
  }
  
  exposure_count_age_sex <- location_outputs[[1]]$exposure_count_age_sex %>% mutate(value = 0)
  for (i in 1:length(location_outputs)) {
    exposure_count_age_sex <- exposure_count_age_sex %>% 
      left_join(location_outputs[[i]]$exposure_count_age_sex, by = c("time_start", "time_span", "sex", "age_start", "age_span")) %>% 
      mutate(value = value.x + value.y) %>% 
      select(-value.x, -value.y)
  }
  
  # nax for first age group and last age group
  deaths_1A0_all <- do.call(rbind, lapply(1:length(location_outputs), function(x) {
    nAx <- location_outputs[[x]]$lt_1A0 %>% 
      dplyr::select(time_start, sex, age_start, value) %>% 
      rename(nAx = value)
    deaths_1A0 <- location_outputs[[x]]$death_count_age_sex %>%  dplyr::filter(age_start == 0) %>% 
      rename(deaths = value) %>% 
      left_join(nAx, by = c("time_start", "sex", "age_start")) 
  }))
  # weight aggregate nAx series for each period and sex by deaths and input nAx
  lt_1A0 <- deaths_1A0_all %>% 
    dplyr::group_by(time_start, sex, age_start) %>% 
    summarise(value = sum(deaths*nAx)/sum(deaths)) 
  
  birth_count_age_b <- location_outputs[[1]]$birth_count_age_b %>% mutate(value = 0)
  for (i in 1:length(location_outputs)) {
    birth_count_age_b <- birth_count_age_b %>% 
      left_join(location_outputs[[i]]$birth_count_age_b, by = c("time_start", "time_span", "age_start", "age_span")) %>% 
      mutate(value = value.x + value.y) %>% 
      select(-value.x, -value.y)
  }
  
  birth_count_tot_sex <- location_outputs[[1]]$birth_count_tot_sex %>% mutate(value = 0)
  for (i in 1:length(location_outputs)) {
    birth_count_tot_sex <- birth_count_tot_sex %>% 
      left_join(location_outputs[[i]]$birth_count_tot_sex, by = c("time_start", "time_span", "sex")) %>% 
      mutate(value = value.x + value.y) %>% 
      select(-value.x, -value.y)
  }
  
  
  mig_net_count_age_sex <- location_outputs[[1]]$mig_net_count_age_sex %>% mutate(value = 0)
  for (i in 1:length(location_outputs)) {
    mig_net_count_age_sex <- mig_net_count_age_sex %>% 
      left_join(location_outputs[[i]]$mig_net_count_age_sex, by = c("time_start", "time_span", "sex", "age_start", "age_span")) %>% 
      mutate(value = value.x + value.y) %>% 
      select(-value.x, -value.y)
  }
  
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
    
    nMx <- lt_nMx %>% dplyr::filter(time_start == years[x]) %>% arrange(time_start, sex, age_start)
    lt_m <- lt_single_custom_ax(nMx = nMx$value[nMx$sex == "male"], nAx = c(lt_1A0$value[lt_1A0$time_start == years[x] & lt_1A0$sex == "male"], rep(0.5,130))) 
    lt_f <- lt_single_custom_ax(nMx = nMx$value[nMx$sex == "female"], nAx = c(lt_1A0$value[lt_1A0$time_start == years[x] & lt_1A0$sex == "female"], rep(0.5,130))) 
    lt_b <- lt_single_custom_ax(nMx = nMx$value[nMx$sex == "both"], nAx = c(lt_1A0$value[lt_1A0$time_start == years[x] & lt_1A0$sex == "both"], rep(0.5,130))) 
      
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
  attr(intermediate_output, "a0rule")   <- "aggregate"
  
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
      log_print(paste0("Warning: No CCMPP outputs available for LocID = ", locationIDs[i],". This location is excluded from aggregate."))
      next()
    }
    
    # check whether base year agrees
    check_base_year <- min(ccmpp_output$pop_count_age_sex$time_start) == base_year
    if (!check_base_year) {
      log_print(paste0("Warning: Base year for LocID = ", locationIDs[i], " not equal to ", base_year,". This location is excluded from aggregate."))
      next()
    }
    
    # check whether records for all years between base_year and last_year are present
    check_all_years <- all(base_year:last_year %in% unique(ccmpp_output$pop_count_age_sex$time_start))
    if (!check_all_years) {
      missing_years <- c(base_year:last_year)[!(base_year:last_year %in% unique(ccmpp_output$pop_count_age_sex$time_start))]
      log_print(paste0("Warning: CCMPP population outputs for LocID = ", locationIDs[i], " missing records for time_start = ", missing_years,". This location is excluded from aggregate."))
      next()
    }
    
    if (check_base_year & check_all_years) {
      ccmpp_output$lt_1A0 <- ccmpp_output$lt_complete_age_sex %>% dplyr::filter(indicator == "lt_nAx" & age_start == 0)
      ccmpp_output  <- within(ccmpp_output, rm(lt_complete_age_sex, fert_rate_age_f, srb))
      location_outputs[[i]] <- ccmpp_output 
    } 
  }

  attr(location_outputs, "variant")  <- attributes(ccmpp_output)$variant
  # remove any null elements
  location_outputs <- location_outputs[!sapply(location_outputs, is.null)]
  
  return(location_outputs)
}

# compute a life table with a custom defined nAx series
# we use this for aggregating life tables with different ax rules
lt_single_custom_ax <- function(nMx, nAx, radix = 1e5) {
  
  nage <- length(nMx)
  Age <- 0:(nage-1)
  AgeInt <- c(rep(1,(nage-1)), NA)
  nMx <- nMx
  nAx <- nAx
  nqx <- nMx/(1+((1-nAx) * nMx))
  nqx[nage] <- 1
  npx <- 1-nqx
  lx <- c(radix, (radix * cumprod(npx))[1:(nage-1)])
  ndx <- c(-diff(lx),lx[nage])
  nLx <- c((nAx[1:(nage-1)] * ndx[1:(nage-1)]) + lx[2:nage], NA)
  nLx[nage] <- lx[nage]/nMx[nage]
  Sx <- c(nLx/c(radix,nLx[1:(nage-1)]))
  Sx[nage] <- nLx[nage]/(nLx[nage - 1] + nLx[nage])
  Tx <- rev(cumsum(rev(nLx)))
  ex <- Tx/lx
  ex[nage] <- 0.5
  
  lt_out <- data.frame(Age = Age,
                       AgeInt = AgeInt,
                       nMx = nMx,
                       nAx = nAx,
                       nqx = nqx, 
                       lx = lx,
                       ndx = ndx,
                       nLx = nLx,
                       Sx = Sx,
                       Tx = Tx,
                       ex = ex)
  
  return(lt_out)
  
  
}

