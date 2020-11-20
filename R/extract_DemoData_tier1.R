
#' Extract data needed for probabilistic population reconstruction with ccmppWPP
#'
#' @description This function extracts from the DemoData SQL database population counts by
#' age and sex, annual births by sex to compute sex ratio at birth, 1x1 age specific 
#' fertility rates from the Human Fertility Database and mortality rates from the Human 
#' Mortality Database.  It compiles those data into the input file required by ccmppWPP.  It 
#' also compiles a refernce dataset of population counts by age and sex for various reference
#' periods
#'
#' @author Sara Hertog
#'
#' @param LocID numeric. Location id of country.
#' @param times numeric. Vector of years for which data should be extracted
#' @param time_reference_base character. Reference date of the base year for population.
#' @param data_source_base character. DataSourceShortName of the data source for base year population.
#' @param revision character. Revision attribute for output ccmppWPP_inputs list.
#' @param variant character. Variant attribute for output ccmppWPP_inputs list.
#'
#' @details currently extracts from paperspace development server.
#'
#' @return a list of two objects. 1) the ccmppWPP_inputs list required to run the population projection; and
#' 2) a pop_counts_age_sex_reference data frame for reconciliation
#' @export
#' 


# Examples:
# canada_test <- DDextract_ccmppWPPinputs_tier1(LocID = 124,
#                                           times = 1950:2020,
#                                           time_reference_base = "01/01/1950",
#                                           data_source_base = "HMD")
# 
# usa_test <- DDextract_ccmppWPPinputs_tier1(LocID = 840,
#                                               times = 1950:2020,
#                                               time_reference_base = "01/01/1950",
#                                               data_source_base = "HMD")
# 
# sweden_test <- DDextract_ccmppWPPinputs_tier1(LocID = 752,
#                                            times = 1950:2020,
#                                            time_reference_base = "01/01/1950",
#                                            data_source_base = "HMD")

DDextract_ccmppWPPinputs_tier1 <- function(LocID, 
                                           times = 1950:2020, 
                                           time_reference_base = "01/01/1950",
                                           data_source_base = "HMD",
                                           revision = "test",
                                           variant = "estimates") {

  library(tidyverse)
  library(DDSQLtools)
  library(DemoTools)
  
  ## development server for UNPD testing (paperspace) DemoData api
  options(unpd_server = "http://74.82.31.177/DemoData/api/")
  
# extract population by single year of age and sex from HMD

  pop_est <- get_recorddata(locIds = LocID,
                           dataProcessIds = 6, # Estimates
                           indicatorIds = 60, # Population by single year of age and sex
                           startYear = times[1],
                           endYear = times[length(times)] + 1,
                           locAreaTypeIds = 2,
                           subGroupIds = 2)
  
  # use the latest upload of the data
  DSYear <- max(pop_est$DataSourceYear[pop_est$DataSourceShortName == data_source_base])
  
  pop_count_age_sex_reference <- pop_est %>% 
    # restrict to latest import of HMD, males and females, removing total pop
    filter(DataSourceShortName == "HMD" & DataSourceYear == DSYear & SexID %in% c(1,2) &
             !(AgeStart == 0 & AgeSpan == -1)) %>%
    mutate(data_source = DataSourceShortName,
           time_reference = TimeStart,
           time_start = floor(TimeMid),
           time_span = 1,
           age_start = AgeStart,
           age_span = AgeSpan,
           sex = ifelse(SexID == 1, "male", "female"),
           value = DataValue) %>% 
    select(data_source, time_reference, time_start, time_span, age_start, age_span, sex, value) %>% 
    arrange(data_source, time_start, time_reference, sex, age_start)
  
  # sum ages 100-110 to create open age group 100+
  pop100 <- pop_count_age_sex_reference %>% 
    filter(age_start >= 100) %>% 
    group_by(data_source, time_start, time_span, time_reference, sex) %>% 
    summarise(value = sum(value)) %>% 
    mutate(age_start = 100,
           age_span = 1)
  
  # append open age group 100+ to single year of age from 0-99
  pop_count_age_sex_reference <- pop_count_age_sex_reference %>% 
    filter(age_start < 100) %>% 
    bind_rows(pop100) %>% 
    arrange(data_source, time_start, time_reference, sex, age_start)

# 1 January 1950 population as base
  pop_count_age_sex_base <- pop_count_age_sex_reference %>% 
    filter(time_reference == time_reference_base & data_source == data_source_base) %>% 
    select(-data_source, -time_reference)

# extract total births by sex from DemoData
  
  birth_reg <- get_recorddata(locIds = LocID,
                              dataProcessTypeIds = 9, # Register
                              indicatorIds = 159, # Total births by sex
                              startYear = times[1],
                              endYear = times[length(times)],
                              locAreaTypeIds = 2,
                              subGroupIds = 2)
  
  # compute srb from birth registration
  srb <- birth_reg %>% 
    # keep sex-specific births direct from VR (no adjustment)
    filter(SexID %in% c(1,2) & DataTypeName == "Direct") %>% 
    select(DataSourceName,DataSourceYear, DataStatusName,DataTypeName,TimeStart,TimeMid,SexName,DataValue) %>% 
    # if there is more than one record per sex and TimeMid, then keep the one with the latest DataSourceYear
    group_by(DataSourceName,DataStatusName,DataTypeName,TimeStart,TimeMid,SexName) %>% 
    mutate(nrecords = length(DataValue),
           latestDataSourceYear = max(DataSourceYear)) %>% 
    ungroup() %>% 
    filter(nrecords == 1 | DataSourceYear == latestDataSourceYear) %>% 
    # transform into standard srb data frame needed for inputs
    spread(SexName, DataValue) %>% 
    mutate(time_start = floor(TimeMid),
           time_span  = 1,
           value = Male/Female) %>% 
    select(time_start, time_span, value) %>% 
    arrange(time_start)
  
  
  # fill in any missing srbs with average of observed values within five-year period
  times_missing <- data.frame(time_start = times[!(times %in% srb$time_start)],
                              time_span = 1,
                              value = NA)
  
  srb <- rbind(srb, times_missing) %>% 
    arrange(time_start) %>% 
    mutate(time5 = floor(time_start/5)) %>% 
    group_by(time5) %>% 
    mutate(value5 = mean(value, na.rm=TRUE)) %>% 
    ungroup() %>% 
    mutate(value = replace(value, is.na(value), value5[is.na(value)])) %>% 
    select(-time5, -value5)
  
  earliest_obs <- min(srb$time_start[!is.na(srb$value)])
  latest_obs   <- max(srb$time_start[!is.na(srb$value)])
  
  srb <- srb %>% 
    mutate(value = replace(value, 
                           is.na(value) & time_start < earliest_obs,
                           srb$value[time_start == earliest_obs]),
           value = replace(value,
                           is.na(value) & time_start > latest_obs,
                           srb$value[time_start == latest_obs]))
  srb <- unique(srb)
  

  
# extract ferility rates by single year of age (extract one year at a time as this seems to work faster)

  fert_est <- get_recorddata(locIds = LocID,
                         dataProcessIds = 6, # Estimate
                         indicatorIds = 362, #  Age specific fertility rate (complete)
                         startYear = times[1],
                         endYear = times[length(times)] +1,
                         locAreaTypeIds = 2,
                         subGroupIds = 2)
  
  # use the latest upload of the data
  DSYear <- max(fert_est$DataSourceYear[fert_est$DataSourceShortName == "HFD"])
  
  fert_rate_age_f <- fert_est %>% 
    filter(DataSourceShortName == "HFD" & DataSourceYear == DSYear) %>% 
    mutate(time_start = floor(TimeMid),
           time_span = 1,
           age_start = AgeStart,
           age_span = 1,
           value = DataValue/1000) %>% 
    select(time_start, time_span, age_start, age_span, value) %>% 
    arrange(time_start, age_start)
  
  # add zero fertility rate for ages outside range
  yrage <- data.frame(time_start = rep(unique(fert_rate_age_f$time_start), length(pop_count_age_sex_base$age_start[which(pop_count_age_sex_base$sex=="female")])),
                      age_start = rep(seq(0,max(pop_count_age_sex_base$age_start),1),length(unique(fert_rate_age_f$time_start)))) %>% 
    arrange(time_start, age_start) %>% 
    filter(!(age_start %in% fert_rate_age_f$age_start)) %>% 
    mutate(value = 0.0,
           time_span = 1,
           age_span = 1)
  
  fert_rate_age_f <- fert_rate_age_f %>% 
    bind_rows(yrage) %>%
    arrange(time_start, age_start) 
  

# extract HMD nmx mortality rates by single year of age 
    mort_est <- get_recorddata(locIds = LocID,
                               dataProcessIds = 6, # Estimate
                               indicatorIds = 246, # m(x,n) - complete
                               startYear = times[1],
                               endYear = times[length(times)] +1,
                               locAreaTypeIds = 2,
                               subGroupIds = 2)
    
    # use the latest upload of the data
    DSYear <- max(mort_est$DataSourceYear[mort_est$DataSourceShortName == "HMD"])

    mx <- mort_est %>% 
      filter(DataSourceShortName == "HMD" & DataSourceYear == DSYear & SexID %in% c(1,2)) %>% 
      mutate(sex = ifelse(SexID == 1, "male", "female"),
             time_start = floor(TimeMid),
             time_span = 1,
             age_start = AgeStart,
             age_span = 1,
             value = DataValue) %>% 
      select(time_start, time_span, age_start, age_span, sex, value) %>% 
      arrange(time_start, sex, age_start)

  mx_m <- mx[mx$sex == "male",]
  lt_m <- NULL
  for (i in unique(mx_m$time_start)) {
    lt <- lt_single_mx(nMx = mx_m$value[mx_m$time_start==i], Age = mx_m$age_start[mx_m$time_start==i], OAnew = max(pop_count_age_sex_base$age_start), Sex = "m") %>% 
      mutate(time_start = i,
             time_span = 1,
             age_span = 1)
    lt_m <- rbind(lt_m,lt)
  }
  lt_m$sex <- "male"

  mx_f <- mx[mx$sex == "female",]
  lt_f <- NULL
  for (i in unique(mx_f$time_start)) {
    lt <- lt_single_mx(nMx = mx_f$value[mx_f$time_start==i], Age = mx_f$age_start[mx_f$time_start==i], OAnew = max(pop_count_age_sex_base$age_start), Sex = "f") %>% 
      mutate(time_start = i,
             time_span = 1,
             age_span = 1)
    lt_f <- rbind(lt_f,lt)
  }
  lt_f$sex <- "female"

  lt <- rbind(lt_m, lt_f) %>% 
    gather(key = "indicator", value = "value", c(3:11)) %>% 
    mutate(indicator = paste0("lt_", indicator),
           age_start = as.numeric(Age),
           age_span = 1) %>% 
    select(indicator, time_start, time_span, sex, age_start, age_span, value)

# restrict input data series to min of max years from HMD and HFD
  last_year <- min(max(lt$time_start), max(fert_rate_age_f$time_start))
  
  lt <- lt[lt$time_start <= last_year,]
  fert_rate_age_f <- fert_rate_age_f[fert_rate_age_f$time_start <= last_year,]
  srb <- srb[srb$time_start <= last_year,]


# create dummy inputs for migration -- right now all migration inputs = 0
    mig_net_count_age_sex <- lt[lt$indicator=="lt_nMx",] %>% 
      select(-indicator) %>% 
      mutate(value = 0)

    mig_net_rate_age_sex <- mig_net_count_age_sex 

    mig_net_count_tot_b <- mig_net_count_age_sex %>% 
      group_by(time_start, time_span) %>% 
      summarise(value = sum(value)) %>% 
      ungroup()

    mig_net_count_tot_b <- as.data.frame(mig_net_count_tot_b)

    mig_parameter <- data.frame(indicator = c(rep("mig_type", length(c(1950:last_year))),rep("mig_assumption", length(c(1950:last_year)))),
                                time_start = as.numeric(c(1950:last_year)),
                                time_span  = as.numeric(1),
                                value = c(rep("counts", length(c(1950:last_year))), rep("end", length(c(1950:last_year)))))


ccmppWPP_inputs <- list(pop_count_age_sex_base = pop_count_age_sex_base,
                    life_table_age_sex     = lt, 
                    fert_rate_age_f        = fert_rate_age_f, 
                    srb                    = srb,
                    mig_net_count_age_sex  = mig_net_count_age_sex, 
                    mig_net_rate_age_sex   = mig_net_rate_age_sex,
                    mig_net_count_tot_b    = mig_net_count_tot_b,
                    mig_parameter          = mig_parameter)

attr(ccmppWPP_inputs, "revision") <- revision
attr(ccmppWPP_inputs, "locid") <- LocID
attr(ccmppWPP_inputs, "variant") <- variant


tier1_wpp_inputs <- list(ccmppWPP_inputs = ccmppWPP_inputs,
                         pop_count_age_sex_reference = pop_count_age_sex_reference)

return(tier1_wpp_inputs)

}


