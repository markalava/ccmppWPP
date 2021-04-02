
#' Extract data needed for probabilistic population reconstruction with ccmppWPP
#'
#' @description This function extracts from the DemoData SQL database population counts by
#' age and sex, annual births by sex to compute sex ratio at birth, 1x1 age specific
#' fertility rates from the Human Fertility Database and EuroStat and mortality rates from the Human
#' Mortality Database and EuroStat.  It compiles those data into the input file required by ccmppWPP.  It
#' also compiles a refernce dataset of population counts by age and sex for various reference
#' periods
#'
#' @author Sara Hertog
#'
#' @param LocID numeric. Location id of country.
#' @param times numeric. Vector of years for which data should be extracted
#' @param times_censored_common logical.  If true, the time period covered by each series is censored
#' such that ouput times are only those for which population counts, asfr and mortality rates are all available.
#' If false, then all available data are output, but additional processing may be required (e.g., interpolation)
#' to harmonize the times across indicators before ccmppWPP workflow can be executed successfully.
#' @param data_source_pop character. Vector of data sources for population counts (HMD and/or EuroStat)
#' @param data_source_fert character. Vector of data sources for age-specific fertility rates (HFD and/or EuroStat)
#' @param data_source_mort character. Vector of data sources for age-specific mortality rates (HMD and/or EuroStat)
#' @param data_source_year_hmd numeric. DataSourceYear for the HMD series to use.
#' @param data_source_year_hfd numeric. DataSourceYear for the HFD series to use.
#' @param data_source_year_eurostat numeric. DataSourceYear for the EuroStat series to use.
#' @param data_source_year_hfc numeric. DataSourceYear for the HFC series to use.
#' @param revision character. Revision attribute for output ccmppWPP_inputs list.
#' @param variant character. Variant attribute for output ccmppWPP_inputs list.
#'
#' @details currently extracts from Valencia server. Data source name vectors should be in order of preference.
#' For example data_source_mort = c("HMD", "EuroStat") will prioritize HMD, but fill in any missing years with EuroStat
#'
#' @return a list of two objects. 1) the ccmppWPP_inputs list required to run the population projection; and
#' 2) a pop_counts_age_sex_reference data frame for reconciliation
#'
#' @importFrom magrittr %>%
#' @export
#'


# Examples:

# australia_test <- DDextract_ccmppWPPinputs_tier1(LocID = 36,
#                                                  times = 1950:2020,
#                                                  times_censored_common = FALSE,
#                                                  data_source_pop = c("HMD", "EuroStat"),
#                                                  data_source_mort = c("HMD", "EuroStat"),
#                                                  data_source_fert = c("HFD", "EuroStat","HFC-STAT"),
#                                                  data_source_year_hmd = 2020,
#                                                  data_source_year_hfd = 2020,
#                                                  data_source_year_eurostat = 2020,
#                                                  data_source_year_hfc = c(2012,2014,2015),
#                                                  revision = "test",
#                                                  variant = "estimates")
#
# italy_test <- DDextract_ccmppWPPinputs_tier1(LocID = 380,
#                                               times = 1950:2020,
#                                               times_censored_common = FALSE,
#                                               data_source_pop = c("HMD", "EuroStat"),
#                                               data_source_mort = c("HMD", "EuroStat"),
#                                               data_source_fert = c("HFD", "EuroStat","HFC-STAT"),
#                                               data_source_year_hmd = 2020,
#                                               data_source_year_hfd = 2020,
#                                               data_source_year_eurostat = 2020,
#                                               data_source_year_hfc = c(2012,2014,2015),
#                                               revision = "test",
#                                               variant = "estimates")
#
# ireland_test <- DDextract_ccmppWPPinputs_tier1(LocID = 372,
#                                              times = 1950:2020,
#                                              times_censored_common = TRUE,
#                                              data_source_pop = c("HMD", "EuroStat"),
#                                              data_source_mort = c("HMD", "EuroStat"),
#                                              data_source_fert = c("HFD", "EuroStat","HFC-STAT"),
#                                              data_source_year_hmd = 2020,
#                                              data_source_year_hfd = 2020,
#                                              data_source_year_eurostat = 2020,
#                                              data_source_year_hfc = c(2012,2014,2015),
#                                              revision = "test",
#                                              variant = "estimates")
#
# canada_test <- DDextract_ccmppWPPinputs_tier1(LocID = 124,
#                                               times = 1950:2020,
#                                               times_censored_common = TRUE,
#                                               data_source_pop = c("HMD"),
#                                               data_source_mort = c("HMD"),
#                                               data_source_fert = c("HFD","HFC-STAT"),
#                                               data_source_year_hmd = 2020,
#                                               data_source_year_hfd = 2020,
#                                               data_source_year_eurostat = NA,
#                                               data_source_year_hfc = c(2012,2014,2015),
#                                               revision = "test",
#                                               variant = "estimates")
#
# usa_test <- DDextract_ccmppWPPinputs_tier1(LocID = 840,
#                                            times = 1950:2020,
#                                            times_censored_common = TRUE,
#                                            data_source_pop = c("HMD"),
#                                            data_source_mort = c("HMD"),
#                                            data_source_fert = c("HFD","HFC-STAT"),
#                                            data_source_year_hmd = 2020,
#                                            data_source_year_hfd = 2020,
#                                            data_source_year_eurostat = NA,
#                                            data_source_year_hfc = c(2012,2014,2015),
#                                            revision = "test",
#                                            variant = "estimates")
#
# sweden_test <- DDextract_ccmppWPPinputs_tier1(LocID = 752,
#                                             times = 1950:2020,
#                                             times_censored_common = TRUE,
#                                             data_source_pop = c("HMD", "EuroStat"),
#                                             data_source_mort = c("HMD", "EuroStat"),
#                                             data_source_fert = c("HFD", "EuroStat","HFC-STAT"),
#                                             data_source_year_hmd = 2020,
#                                             data_source_year_hfd = 2020,
#                                             data_source_year_eurostat = 2020,
#                                             data_source_year_hfc = c(2012,2014,2015),
#                                             revision = "test",
#                                             variant = "estimates")

DDextract_ccmppWPPinputs_tier1 <- function(LocID,
                                           times = 1950:2020,
                                           times_censored_common = FALSE,
                                           data_source_pop = c("HMD", "EuroStat"),
                                           data_source_mort = c("HMD", "EuroStat"),
                                           data_source_fert = c("HFD", "EuroStat", "HFC-STAT"),
                                           data_source_year_hmd = 2020,
                                           data_source_year_hfd = 2020,
                                           data_source_year_eurostat = 2020,
                                           data_source_year_hfc = c(2012,2014,2015),
                                           revision = "test",
                                           variant = "estimates") {

  ## Valencia server
  options(unpd_server = "https://popdiv.dfs.un.org/DemoData/api/")
  ## Paperspace server
   #options(unpd_server = "http://74.82.31.177/DemoData/api/")

# extract population by single year of age and sex from HMD

  if ("HMD" %in% data_source_pop) {

    tryCatch({

    pop_hmd <- DDSQLtools::get_recorddata(locIds = LocID,
                             dataProcessIds = 6, # Estimates
                             indicatorIds = 60, # Population by single year of age and sex
                             startYear = times[1],
                             endYear = times[length(times)] + 1,
                             locAreaTypeIds = 2,
                             subGroupIds = 2,
                             dataSourceShortNames = "HMD",
                             dataSourceYears = data_source_year_hmd)

    }, error=function(e){cat("Error in file", conditionMessage(e), "\n")})

  if (exists('pop_hmd')) {

    pop_hmd <- pop_hmd %>%
      dplyr::filter(SexID %in% c(1,2) & !(AgeStart == 0 & AgeSpan == -1)) %>%
      dplyr::mutate(data_source = DataSourceShortName,
             time_reference = TimeStart,
             time_start = floor(TimeMid),
             time_span = 1,
             age_start = AgeStart,
             age_span = AgeSpan,
             sex = ifelse(SexID == 1, "male", "female"),
             value = DataValue) %>%
      dplyr::select(data_source, time_reference, time_start, time_span, age_start, age_span, sex, value) %>%
      dplyr::arrange(data_source, time_start, time_reference, sex, age_start)

    # sum ages 100-110 to create open age group 100+
    pop100 <- pop_hmd %>%
      dplyr::filter(age_start >= 100) %>%
      dplyr::group_by(data_source, time_start, time_span, time_reference, sex) %>%
      dplyr::summarise(value = sum(value)) %>%
      dplyr::mutate(age_start = 100,
             age_span = 1)

    # append open age group 100+ to single year of age from 0-99
    pop_hmd <- pop_hmd %>%
      dplyr::filter(age_start < 100) %>%
      dplyr::bind_rows(pop100) %>%
      dplyr::arrange(data_source, time_start, time_reference, sex, age_start)
  } else {

    pop_hmd <- NULL
    print("No HMD population by single year of age and sex available for selected times and data source year.")

  }
  } else { pop_hmd <- NULL }

# extract population by single year of age and sex from EuroStat

  if ("EuroStat" %in% data_source_pop) {

    tryCatch({

    pop_eur <- DDSQLtools::get_recorddata(locIds = LocID,
                              dataProcessIds = c(6,10), # Estimates and registers
                              indicatorIds = 60, # Population by single year of age and sex
                              startYear = times[1],
                              endYear = times[length(times)] + 1,
                              locAreaTypeIds = 2,
                              subGroupIds = 2,
                              dataSourceShortNames = "EuroStat",
                              dataSourceYears = data_source_year_eurostat)

    }, error=function(e){cat("Error in file", conditionMessage(e), "\n")})

  if (exists('pop_eur')) {

    # keep only series with open age group 100+
    pop_eur <- pop_eur %>%
      dplyr::filter(SexID %in% c(1,2) & !(AgeStart == 0 & AgeSpan == -1) & AgeStart != -2) %>%
      dplyr::group_by(TimeStart) %>%
      dplyr::mutate(oag100 = "100+" %in% AgeLabel) %>%
      dplyr::ungroup() %>%
      dplyr::filter(oag100 == TRUE) %>%
      dplyr::mutate(data_source = DataSourceShortName,
             time_reference = TimeStart,
             time_start = floor(TimeMid),
             time_span = 1,
             age_start = AgeStart,
             age_span = AgeSpan,
             sex = ifelse(SexID == 1, "male", "female"),
             value = DataValue) %>%
      dplyr::select(data_source, time_reference, time_start, time_span, age_start, age_span, sex, value) %>%
      dplyr::arrange(data_source, time_start, time_reference, sex, age_start)

  } else {

    pop_eur <- NULL
    print("No EuroStat population by single year of age and sex available for selected times and data source year.")

  }
  } else { pop_eur <- NULL }

  pop_count_age_sex_reference <- rbind(pop_hmd, pop_eur) %>%
    dplyr::mutate(prefer = ifelse(data_source == data_source_pop[1], 1, 2)) %>%
    dplyr::group_by(time_start) %>%
    dplyr::mutate(prefer_min = min(prefer)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(prefer == prefer_min) %>%
    dplyr::select(-prefer, -prefer_min)

# population as base for projection, earliest in times
  pop_count_age_sex_base <- pop_count_age_sex_reference %>%
    dplyr::filter(time_start == min(pop_count_age_sex_reference$time_start)) %>%
    dplyr::select(-data_source, -time_reference)

# extract total births by sex from DemoData

  birth_reg <- DDSQLtools::get_recorddata(locIds = LocID,
                              dataProcessTypeIds = 9, # Register
                              indicatorIds = 159, # Total births by sex
                              startYear = times[1],
                              endYear = times[length(times)],
                              locAreaTypeIds = 2,
                              subGroupIds = 2)

  # compute srb from birth registration
  srb <- birth_reg %>%
    # keep sex-specific births direct from VR (no adjustment)
    dplyr::filter(SexID %in% c(1,2) & DataTypeName == "Direct") %>%
    dplyr::select(DataSourceYear,TimeStart,TimeMid,SexName,DataValue) %>%
    # if there is more than one record per sex and TimeMid, then keep the one with the latest DataSourceYear
    dplyr::group_by(TimeStart,TimeMid,SexName) %>%
    dplyr::mutate(latestDataSourceYear = max(DataSourceYear)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(DataSourceYear == latestDataSourceYear) %>%
    dplyr::distinct() %>%
    # transform into standard srb data frame needed for inputs
    dplyr::spread(SexName, DataValue) %>%
    dplyr::mutate(time_start = floor(TimeMid),
           time_span  = 1,
           value = Male/Female) %>%
    dplyr::select(time_start, time_span, value) %>%
    dplyr::arrange(time_start)


  # fill in any missing srbs with average of observed values within five-year period
  times_missing <- data.frame(time_start = times[!(times %in% srb$time_start)],
                              time_span = 1,
                              value = NA)

  srb <- rbind(srb, times_missing) %>%
    dplyr::arrange(time_start) %>%
    dplyr::mutate(time5 = floor(time_start/5)) %>%
    dplyr::group_by(time5) %>%
    dplyr::mutate(value5 = mean(value, na.rm=TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(value = replace(value, is.na(value), value5[is.na(value)])) %>%
    dplyr::select(-time5, -value5)

  earliest_obs <- min(srb$time_start[!is.na(srb$value)])
  latest_obs   <- max(srb$time_start[!is.na(srb$value)])

  srb <- srb %>%
    dplyr::mutate(value = replace(value,
                           is.na(value) & time_start < earliest_obs,
                           srb$value[time_start == earliest_obs]),
           value = replace(value,
                           is.na(value) & time_start > latest_obs,
                           srb$value[time_start == latest_obs]))
 # if still more than one record per year, take the average
   srb <- unique(srb) %>%
    dplyr::group_by(time_start, time_span) %>%
    dplyr::summarise(value = mean(value))



# extract ferility rates by single year of age from HFD

  if ("HFD" %in% data_source_fert) {

    tryCatch({

      fert_hfd <- DDSQLtools::get_recorddata(locIds = LocID,
                                dataProcessIds = 6, # Estimates
                                indicatorIds = 362, #  Age specific fertility rate (complete)
                                startYear = times[1],
                                endYear = times[length(times)] + 1,
                                locAreaTypeIds = 2,
                                subGroupIds = 2,
                                dataSourceShortNames = "HFD",
                                dataSourceYears = data_source_year_hfd)

    }, error=function(e){cat("Error in file", conditionMessage(e), "\n")})

 if (exists('fert_hfd')) {

   fert_hfd <- fert_hfd %>%
     dplyr::mutate(data_source = DataSourceShortName,
            time_start = floor(TimeMid),
            time_span = 1,
            age_start = AgeStart,
            age_span = 1,
            value = DataValue/1000) %>%
     dplyr::select(data_source, time_start, time_span, age_start, age_span, value) %>%
     dplyr::arrange(data_source, time_start, age_start)

   # add zero fertility rate for ages outside range
   yrage <- data.frame(time_start = rep(unique(fert_hfd$time_start), length(pop_count_age_sex_base$age_start[which(pop_count_age_sex_base$sex=="female")])),
                       age_start = rep(seq(0,max(pop_count_age_sex_base$age_start),1),length(unique(fert_hfd$time_start)))) %>%
     dplyr::arrange(time_start, age_start) %>%
     dplyr::filter(!(age_start %in% fert_hfd$age_start)) %>%
     dplyr::mutate(value = 0.0,
            time_span = 1,
            age_span = 1,
            data_source = fert_hfd$data_source[1])

   fert_hfd <- fert_hfd %>%
     dplyr::bind_rows(yrage) %>%
     dplyr::arrange(time_start, age_start)

  } else {

    print("No HFD fertility rates by single year of age available for selected times and data source year.")
    fert_hfd <- NULL
  }

  } else { fert_hfd <- NULL }


  # extract ferility rates by single year of age from EuroStat

  if ("EuroStat" %in% data_source_fert) {

    tryCatch({

      fert_eur <- DDSQLtools::get_recorddata(locIds = LocID,
                                 dataProcessTypeIds = c(6,9), # Estimates and registers
                                 indicatorIds = 362, #  Age specific fertility rate (complete)
                                 startYear = times[1],
                                 endYear = times[length(times)] + 1,
                                 locAreaTypeIds = 2,
                                 subGroupIds = 2,
                                 dataSourceShortNames = "EuroStat",
                                 dataSourceYears = data_source_year_eurostat)

    }, error=function(e){cat("Error in file", conditionMessage(e), "\n")})

  if (exists('fert_eur')) {

    fert_eur <- fert_eur %>%
      dplyr::mutate(data_source = DataSourceShortName,
             time_start = floor(TimeMid),
             time_span = 1,
             age_start = AgeStart,
             age_span = 1,
             value = DataValue/1000) %>%
      dplyr::select(data_source, time_start, time_span, age_start, age_span, value) %>%
      dplyr::arrange(data_source, time_start, age_start)

    # add zero fertility rate for ages outside range
    yrage <- data.frame(time_start = rep(unique(fert_eur$time_start), length(pop_count_age_sex_base$age_start[which(pop_count_age_sex_base$sex=="female")])),
                        age_start = rep(seq(0,max(pop_count_age_sex_base$age_start),1),length(unique(fert_eur$time_start)))) %>%
      dplyr::arrange(time_start, age_start) %>%
      dplyr::filter(!(age_start %in% fert_eur$age_start)) %>%
      dplyr::mutate(value = 0.0,
             time_span = 1,
             age_span = 1,
             data_source = fert_eur$data_source[1])

    fert_eur <- fert_eur %>%
      dplyr::bind_rows(yrage) %>%
      dplyr::arrange(time_start, age_start)

   } else {

    fert_eur <- NULL
    print("No EuroStat fertility rates by single year of age available for selected times and data source year.")

   }

  } else { fert_eur <- NULL }

   # extract ferility rates by single year of age from the Human Fertility Collection

   if ("HFC-STAT" %in% data_source_fert) {

     tryCatch({

       fert_hfc <- DDSQLtools::get_recorddata(locIds = LocID,
                                  dataProcessTypeIds = c(6,9), # Estimates and registers
                                  indicatorIds = 362, #  Age specific fertility rate (complete)
                                  startYear = times[1],
                                  endYear = times[length(times)] + 1,
                                  locAreaTypeIds = 2,
                                  subGroupIds = 2,
                                  dataSourceShortNames = "HFC-STAT",
                                  dataSourceYears = data_source_year_hfc)

     }, error=function(e){cat("Error in file", conditionMessage(e), "\n")})

   if (exists('fert_hfc')) {

     fert_hfc <- fert_hfc %>%
       dplyr::mutate(data_source = DataSourceShortName,
              time_start = floor(TimeMid),
              time_span = 1,
              age_start = AgeStart,
              age_span = 1,
              value = DataValue/1000) %>%
       dplyr::select(data_source, time_start, time_span, age_start, age_span, value) %>%
       dplyr::arrange(data_source, time_start, age_start)

     # add zero fertility rate for ages outside range
     yrage <- data.frame(time_start = rep(unique(fert_hfc$time_start), length(pop_count_age_sex_base$age_start[which(pop_count_age_sex_base$sex=="female")])),
                         age_start = rep(seq(0,max(pop_count_age_sex_base$age_start),1),length(unique(fert_hfc$time_start)))) %>%
       dplyr::arrange(time_start, age_start) %>%
       dplyr::filter(!(age_start %in% fert_hfc$age_start)) %>%
       dplyr::mutate(value = 0.0,
              time_span = 1,
              age_span = 1,
              data_source = fert_hfc$data_source[1])

     fert_hfc <- fert_hfc %>%
       dplyr::bind_rows(yrage) %>%
       dplyr::arrange(time_start, age_start)

   } else {

     fert_hfc <- NULL
     print("No HFC fertility rates by single year of age available for selected times and data source year.")

   }
   } else { fert_hfc <- NULL }

  dsrank <- data.frame(rank = c(1:length(data_source_fert)), data_source = data_source_fert)

  fert_rate_age_f <- rbind(fert_hfd, fert_eur, fert_hfc) %>%
    dplyr::left_join(dsrank, by = "data_source") %>%
    dplyr::group_by(time_start) %>%
    dplyr::mutate(prefer_min = min(rank)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(rank == prefer_min) %>%
    dplyr::select(-rank, -prefer_min, -data_source) %>%
    dplyr::mutate(value = replace(value, age_start < 10, 0.000))

  rm(dsrank)



# extract HMD nmx mortality rates by single year of age

  if ("HMD" %in% data_source_mort) {

    tryCatch({

    mort_hmd <- DDSQLtools::get_recorddata(locIds = LocID,
                               dataProcessIds = 6, # Estimate
                               indicatorIds = 246, # m(x,n) - complete
                               startYear = times[1],
                               endYear = times[length(times)] +1,
                               locAreaTypeIds = 2,
                               subGroupIds = 2,
                               dataSourceShortNames = "HMD",
                               dataSourceYears = data_source_year_hmd)

    }, error=function(e){cat("Error in file", conditionMessage(e), "\n")})

  if (exists('mort_hmd')) {

    mort_hmd <- mort_hmd %>%
      dplyr::filter(SexID %in% c(1,2)) %>%
      dplyr::mutate(data_source = DataSourceShortName,
             sex = ifelse(SexID == 1, "male", "female"),
             time_start = floor(TimeMid),
             time_span = 1,
             age_start = AgeStart,
             age_span = 1,
             value = DataValue) %>%
      dplyr::select(data_source, time_start, time_span, age_start, age_span, sex, value) %>%
      dplyr::arrange(data_source, time_start, sex, age_start)

  } else {

    mort_hmd <- NULL
    print("No HMD mortality rates by single year of age available for selected times and data source year.")

  }
  }  else { mort_hmd <- NULL }

  # extract EuroStat nmx mortality rates by single year of age

  if ("EuroStat" %in% data_source_mort) {

    tryCatch({

      mort_eur <- DDSQLtools::get_recorddata(locIds = LocID,
                                 dataProcessTypeIds = c(6,9), # Estimate and registers
                                 indicatorIds = 246, # m(x,n) - complete
                                 startYear = times[1],
                                 endYear = times[length(times)] +1,
                                 locAreaTypeIds = 2,
                                 subGroupIds = 2,
                                 dataSourceShortNames = "EuroStat",
                                 dataSourceYears = data_source_year_eurostat)

    }, error=function(e){cat("Error in file", conditionMessage(e), "\n")})

  if (exists('mort_eur')) {

    mort_eur <- mort_eur %>%
      dplyr::filter(SexID %in% c(1,2)) %>%
      dplyr::mutate(data_source = DataSourceShortName,
             sex = ifelse(SexID == 1, "male", "female"),
             time_start = floor(TimeMid),
             time_span = 1,
             age_start = AgeStart,
             age_span = 1,
             value = DataValue) %>%
      dplyr::select(data_source, time_start, time_span, age_start, age_span, sex, value) %>%
      dplyr::arrange(data_source, time_start, sex, age_start)

  } else {

    mort_eur <- NULL
    print("No EuroStat mortality rates by single year of age available for selected times and data source year.")

  }
  }  else { mort_eur <- NULL }

  mx <- rbind(mort_hmd, mort_eur) %>%
    dplyr::mutate(prefer = ifelse(data_source == data_source_mort[1], 1, 2)) %>%
    dplyr::group_by(time_start) %>%
    dplyr::mutate(prefer_min = min(prefer)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(prefer == prefer_min) %>%
    dplyr::select(-prefer, -prefer_min)

  mx_m <- mx[mx$sex == "male",]
  lt_m <- NULL
  for (i in unique(mx_m$time_start)) {
    lt <- lt_single_mx(nMx = mx_m$value[mx_m$time_start==i], Age = mx_m$age_start[mx_m$time_start==i], OAnew = max(pop_count_age_sex_base$age_start), Sex = "m") %>%
      dplyr::mutate(time_start = i,
             time_span = 1,
             age_span = 1)
    lt_m <- rbind(lt_m,lt)
  }
  lt_m$sex <- "male"

  mx_f <- mx[mx$sex == "female",]
  lt_f <- NULL
  for (i in unique(mx_f$time_start)) {
    lt <- lt_single_mx(nMx = mx_f$value[mx_f$time_start==i], Age = mx_f$age_start[mx_f$time_start==i], OAnew = max(pop_count_age_sex_base$age_start), Sex = "f") %>%
      dplyr::mutate(time_start = i,
             time_span = 1,
             age_span = 1)
    lt_f <- rbind(lt_f,lt)
  }
  lt_f$sex <- "female"

  lt <- rbind(lt_m, lt_f) %>%
    gather(key = "indicator", value = "value", c(3:11)) %>%
    dplyr::mutate(indicator = paste0("lt_", indicator),
           age_start = as.numeric(Age),
           age_span = 1) %>%
    dplyr::select(indicator, time_start, time_span, sex, age_start, age_span, value)

if (times_censored_common == TRUE) {


  # identify earliest and latest years within times for which all indicators are available
  first_year <- max(min(lt$time_start), min(fert_rate_age_f$time_start))
  last_year <- min(max(lt$time_start), max(fert_rate_age_f$time_start))

  # replace base pop with pop counts for earliest year
  pop_count_age_sex_base <- pop_count_age_sex_reference %>%
    dplyr::filter(time_start == first_year) %>%
    dplyr::select(-data_source, -time_reference)

  lt <- lt[lt$time_start >= first_year & lt$time_start <= last_year,]
  fert_rate_age_f <- fert_rate_age_f[fert_rate_age_f$time_start >= first_year & fert_rate_age_f$time_start <= last_year,]
  srb <- srb[srb$time_start >= first_year & srb$time_start <= last_year,]

}


# create dummy inputs for migration -- right now all migration inputs = 0
    mig_net_count_age_sex <- lt[lt$indicator=="lt_nMx",] %>%
      dplyr::select(-indicator) %>%
      dplyr::mutate(value = 0)

    mig_net_rate_age_sex <- mig_net_count_age_sex

    mig_net_count_tot_b <- mig_net_count_age_sex %>%
      dplyr::group_by(time_start, time_span) %>%
      dplyr::summarise(value = sum(value)) %>%
      dplyr::ungroup()

    mig_net_count_tot_b <- as.data.frame(mig_net_count_tot_b)

    first_year <- min(mig_net_count_age_sex$time_start)
    last_year <- max(mig_net_count_age_sex$time_start)
    mig_parameter <- data.frame(indicator = c(rep("mig_type", length(c(first_year:last_year))),rep("mig_assumption", length(c(first_year:last_year)))),
                                time_start = as.numeric(c(first_year:last_year)),
                                time_span  = as.numeric(1),
                                value = c(rep("counts", length(c(first_year:last_year))), rep("end", length(c(first_year:last_year)))))


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


