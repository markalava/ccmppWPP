
#' read ccmppWPP inputs from country-specific excel input files
#'
#' @description This function reads the ccmppWPP inputs from the country specific excel files.  Specifically,
#' it reads from parameters, pop_count_age_sex_base, fert_rate_age_f, srb, mort_params, life_table_age_sex,
#' mig_net_count_age_sex, mig_net_count_tot_b, and mig_parameters sheets of the country input files.  It selects only
#' years bounded by the estimation period.
#'
#' @author Sara Hertog
#'
#' @param input_file_path character. string pointing to the file path for the Excel input file for one country.
#'
#' @return a list of data frames -- one for each type of input needed to produce the estimates variant with the ccmpp.
#' attributes assigned to the list describe the location id and name, a0 rule and variant
#' @export

ccmppWPP_input_file_estimates <- function(input_file_path) {

  # read metadata from parameters sheet of excel input file
  meta <-   readxl::read_xlsx(path = input_file_path,
                              sheet = "parameters")

  meta <- meta %>%
    dplyr::select(parameter, value) %>%
    dplyr::filter(!is.na(parameter))

  meta.list <- list()
  for (i in 1:nrow(meta)) {
    meta.list[[i]] <- ifelse(!is.na(suppressWarnings(as.numeric(meta$value[i]))), as.numeric(meta$value[i]), meta$value[i])
    names(meta.list)[i] <- gsub(" ", "_", meta$parameter[i])
  }
  rm(meta)

  base_year <- as.numeric(meta.list$Base_Year)
  begin_proj_year <- as.numeric(meta.list$Projection_First_Year)

  # base year population by sex and single year of age
  pop_count_age_sex_base <- readxl::read_xlsx(path = input_file_path,
                                              sheet = "pop_count_age_sex_base",
                                              n_max = 1048576) %>%
    mutate(value = replace(value, is.na(value), 0)) # replace any NA values with 0


  # asfr by single year of mother's age single year of time (jan 1 to dec 31)
  fert_rate_age_f <- readxl::read_xlsx(path = input_file_path,
                                       sheet = "fert_rate_age_f",
                                       n_max = 1048576) %>%
    dplyr::filter(time_start >= base_year & time_start < begin_proj_year) %>%
    arrange(time_start, age_start)

  # srb estimates by single year of time (jan 1 to dec 31)
  srb <- readxl::read_xlsx(path = input_file_path,
                           sheet = "srb",
                           n_max = 1048576) %>%
    dplyr::filter(time_start >= base_year & time_start < begin_proj_year) %>%
    arrange(time_start)

  # mortality estimation parameters
  mp <- readxl::read_xlsx(path = input_file_path, sheet = "MORT_PARAMS",
                          n_max = 1048576) %>%
    dplyr::filter(tolower(type) == "estimation") %>%
    dplyr::select(parameter, value)

  # for classic model life table families, we use the Coale-Demeny a0 rule, otherwise we use Andreev-Kinkaid
  a0rule <- ifelse(mp$value[mp$parameter == "Age_Specific_Mortality_Type"] == "Model-based" &
                     !is.na(mp$value[mp$parameter == "Age_Specific_MLT_Region"]) &
                     mp$value[mp$parameter == "Age_Specific_MLT_Region"] %in%
                     c("CD_West","CD_East","CD_North","CD_South", "UN_Chilean","UN_Far_Eastern",
                       "UN_General","UN_Latin_American","UN_South_Asian"), "cd", "ak")

  # life tables by sex and single year of age and single year of time (jan 1 to dec 31)
  life_table_age_sex <- readxl::read_xlsx(path = input_file_path,
                                       sheet = "life_table_age_sex",
                                       n_max = 1048576) %>%
    dplyr::filter(time_start >= base_year & time_start < begin_proj_year) %>%
    arrange(time_start, sex, age_start)

  # net international migration

    # estimates of counts by sex and single year of age and single year of time (jan 1 to dec 31)
    mig_net_count_age_sex <- readxl::read_xlsx(path = input_file_path,
                                               sheet = "mig_net_count_age_sex",
                                               n_max = 1048576) %>%
      mutate(value = replace(value, is.na(value), 0)) %>% # replace any NA values with 0
      dplyr::filter(time_start >= base_year & time_start < begin_proj_year) %>%
      arrange(time_start, sex, age_start)

    # estimates of totals (in case need to apply age distribution) by single year of time (jan 1 to dec 31)
    mig_net_count_tot_b <- readxl::read_xlsx(path = input_file_path,
                                             sheet = "mig_net_count_tot_b",
                                             n_max = 1048576) %>%
      mutate(value = replace(value, is.na(value), 0)) %>% # replace any NA values with 0
      dplyr::filter(time_start >= base_year & time_start < begin_proj_year) %>%
      arrange(time_start)

    # rates -- THIS IS NOT AN OPTION FOR THE 2022 or 2024 REVISIONS SO WE CREATE A PLACE HOLDER FOR FUTURE REVISIONS
    mig_net_rate_age_sex <- mig_net_count_age_sex
    mig_net_rate_age_sex$value <- 0

  # migration parameters
  mig_parameter <- readxl::read_xlsx(path = input_file_path,
                                     sheet = "mig_parameter",
                                     n_max = 1048576) %>%
    arrange(time_start)


  ccmppWPP_inputs_estimates <- list(pop_count_age_sex_base = pop_count_age_sex_base,
                                    life_table_age_sex     = life_table_age_sex,
                                    fert_rate_age_f        = fert_rate_age_f,
                                    srb                    = srb,
                                    mig_net_count_age_sex  = mig_net_count_age_sex,
                                    mig_net_rate_age_sex   = mig_net_rate_age_sex,
                                    mig_net_count_tot_b    = mig_net_count_tot_b,
                                    mig_parameter          = mig_parameter)

  # set attributes
  attr(ccmppWPP_inputs_estimates, "locid")    <- meta.list$LocationID
  attr(ccmppWPP_inputs_estimates, "locname")    <- meta.list$Location
  attr(ccmppWPP_inputs_estimates, "variant")  <- "estimates"
  attr(ccmppWPP_inputs_estimates, "a0rule")  <- a0rule

  return(ccmppWPP_inputs_estimates)

}

# this function takes 1x1 inputs to the ccmppWPP and extends them to a new open age group
# we use this to extend all inputs to OAG = 130+, which then ensures consistency in life tables across 3 sex categories (female, male, both)
# This will not _reduce_ the input to OAnew if it already has a higher open age interval.

#' @export
ccmppWPP_input_file_extend <- function(ccmppWPP_inputs, OAnew = 130, a0rule = "ak") {

  ############
  # life_table
  ############
  lt_in <- ccmppWPP_inputs$life_table_age_sex %>%
    arrange(time_start, sex, indicator, age_start)

  ages  <- unique(lt_in$age_start)
  maxage <- max(ages)

  lts <- NULL
  # if the OA is less than OAnew, then extend the life tables to OAnew
  if (maxage < OAnew) {
    for (i in unique(lt_in$time_start)) {
      mxM <- lt_in$value[lt_in$indicator == "lt_nMx" & lt_in$sex == "male" & lt_in$time_start == i]
      names(mxM) <- ages
      mxF <- lt_in$value[lt_in$indicator == "lt_nMx" & lt_in$sex == "female" & lt_in$time_start == i]
      names(mxF) <- ages

      # extend to new OA using two sex coherent kannisto
      # use sex coherent kannisto to extend rather than sex-independent extension in lt_single_mx
      ext <- MortCast::cokannisto(mxM, mxF, est.ages = 80:(maxage-1), proj.ages = maxage:OAnew)
      mxM <- ext$male
      mxF <- ext$female

      lt_m <- DemoTools::lt_single_mx(nMx = mxM, Age = 0:OAnew, a0rule = a0rule, Sex = "m", OAG = TRUE, OAnew = OAnew)
      lt_m$sex <- "male"
      lt_f <- DemoTools::lt_single_mx(nMx = mxF, Age = 0:OAnew, a0rule = a0rule, Sex = "f", OAG = TRUE, OAnew = OAnew)
      lt_f$sex <- "female"

      lt <- rbind(lt_m, lt_f)
      lt$time_start <- i

      lts <- rbind(lts,lt)
    }
    }

    # if the OA is greater than OAnew, then truncate life tables to OAnew
    if (maxage > OAnew) {
      for (i in unique(lt_in$time_start)) {
        mxM <- lt_in$value[lt_in$indicator == "lt_nMx" & lt_in$sex == "male" & lt_in$time_start == i]
        names(mxM) <- ages
        mxF <- lt_in$value[lt_in$indicator == "lt_nMx" & lt_in$sex == "female" & lt_in$time_start == i]
        names(mxF) <- ages

        lt_m <- DemoTools::lt_single_mx(nMx = mxM, Age = 0:maxage, a0rule = a0rule, Sex = "m", OAG = TRUE, OAnew = OAnew)
        lt_m$sex <- "male"
        lt_f <- DemoTools::lt_single_mx(nMx = mxF, Age = 0:maxage, a0rule = a0rule, Sex = "f", OAG = TRUE, OAnew = OAnew)
        lt_f$sex <- "female"

        lt <- rbind(lt_m, lt_f)
        lt$time_start <- i

        lts <- rbind(lts,lt)
      }

    lts$AgeInt[is.na(lts$AgeInt)] <- 1000

    life_table_age_sex <-reshape(lts,
                                 direction = "long",
                                 times = paste0("lt_", names(lts)[3:11]),
                                 timevar = "indicator",
                                 idvar = c("time_start", "sex", "Age", "AgeInt"),
                                 varying = list(names(lts)[3:11]),
                                 v.names = "value")
    names(life_table_age_sex)[c(1,2)] <- c("age_start", "age_span")
    life_table_age_sex$time_span <- 1

    }

  if (maxage == OAnew) {

    # check to see that all of the life table columns are present
    ltnames <- c("lt_nMx","lt_nqx","lt_lx","lt_nAx","lt_ndx","lt_nLx","lt_Tx","lt_Sx","lt_ex")

    ispresent <- function(x) {
      result <- x %in% lt_in$indicator
    }
    lt_indicators_present <- sapply(ltnames, FUN = ispresent )

    # if they are present, then just return the original input life tables
    if (all(lt_indicators_present)) {

      life_table_age_sex <- lt_in

    } else { # otherwise, re-compute the life table columns

      lt_f <- lt_complete_loop_over_time(lt_in[lt_in$indicator == "lt_nMx" & lt_in$sex == "female",],
                                         sex = "female", a0rule = a0rule, OAnew = 130)
      lt_m <- lt_complete_loop_over_time(lt_in[lt_in$indicator == "lt_nMx" & lt_in$sex == "male",],
                                         sex = "male", a0rule = a0rule, OAnew = 130)

      life_table_age_sex <- rbind(lt_f, lt_m)

    }
  }
  rm(ages,maxage)

  ############
  # base population
  ############
  pop_in <- ccmppWPP_inputs$pop_count_age_sex_base
  pop_in <- pop_in[order(pop_in$sex, pop_in$age_start),]
  ages <- unique(pop_in$age_start)
  maxage <- max(ages)

  if (maxage < OAnew) {
    ext_f <- DemoTools::OPAG(Pop = DemoTools::groupAges(pop_in$value[pop_in$sex == "female"]),
                             Age_Pop = seq(0,maxage,5),
                             nLx = DemoTools::groupAges(life_table_age_sex$value[life_table_age_sex$indicator=="lt_nLx" &
                                                                                   life_table_age_sex$sex == "female" &
                                                                                   life_table_age_sex$time_start == pop_in$time_start[1]]),
                             Age_nLx = seq(0,max(life_table_age_sex$age_start),5),
                             Redistribute_from = maxage,
                             OAnew = OAnew,
                             method = "mono")
    ext_f$Pop_out <- ifelse(is.na(ext_f$Pop_out), 0, ext_f$Pop_out)
    ext_f <- DemoTools::graduate_mono(ext_f$Pop_out, Age = ext_f$Age_out, AgeInt = DemoTools::age2int(ext_f$Age_out), OAG = TRUE)

    pop_count_age_sex_f <- c(pop_in$value[pop_in$sex == "female" & pop_in$age_start < maxage], ext_f[as.numeric(names(ext_f)) >= maxage])
    names(pop_count_age_sex_f) <- 0:OAnew

    ext_m <- DemoTools::OPAG(Pop = DemoTools::groupAges(pop_in$value[pop_in$sex == "male"]),
                             Age_Pop = seq(0,maxage,5),
                             nLx = DemoTools::groupAges(life_table_age_sex$value[life_table_age_sex$indicator=="lt_nLx" &
                                                                                   life_table_age_sex$sex == "male" &
                                                                                   life_table_age_sex$time_start == pop_in$time_start[1]]),
                             Age_nLx = seq(0,max(life_table_age_sex$age_start),5),
                             Redistribute_from = maxage,
                             OAnew = OAnew,
                             method = "mono")
    ext_m$Pop_out <- ifelse(is.na(ext_m$Pop_out), 0, ext_m$Pop_out)
    ext_m <- DemoTools::graduate_mono(ext_m$Pop_out, Age = ext_m$Age_out, AgeInt = DemoTools::age2int(ext_m$Age_out), OAG = TRUE)

    pop_count_age_sex_m <- c(pop_in$value[pop_in$sex == "male" & pop_in$age_start < maxage], ext_m[as.numeric(names(ext_m)) >= maxage])
    names(pop_count_age_sex_m) <- 0:OAnew

    pop_count_age_sex_base <- data.frame(time_start = rep(pop_in$time_start[1], (OAnew + 1) * 2),
                                         time_span = rep(0,(OAnew + 1) * 2),
                                         age_start = rep(0:OAnew,2),
                                         age_span = rep(1,(OAnew + 1) * 2),
                                         sex = c(rep("female", OAnew + 1),rep("male", OAnew + 1)),
                                         value = c(pop_count_age_sex_f, pop_count_age_sex_m))
    pop_count_age_sex_base$age_span[pop_count_age_sex_base$age_start == OAnew] <- 1000
  }

  if (maxage > OAnew) {
    # truncate back to OAnew, aggregating OAG
    pop_count_age_sex_m <- DemoTools::groupOAG(Value = pop_in$value[pop_in$sex == "male"], Age = pop_in$age_start[pop_in$sex == "male"], OAnew = OAnew)
    pop_count_age_sex_f <- DemoTools::groupOAG(Value = pop_in$value[pop_in$sex == "female"], Age = pop_in$age_start[pop_in$sex == "female"], OAnew = OAnew)
    pop_count_age_sex_base <- data.frame(time_start = rep(pop_in$time_start[1], (OAnew + 1) * 2),
                                         time_span = rep(0,(OAnew + 1) * 2),
                                         age_start = rep(0:OAnew,2),
                                         age_span = rep(1,(OAnew + 1) * 2),
                                         sex = c(rep("female", OAnew + 1),rep("male", OAnew + 1)),
                                         value = c(pop_count_age_sex_f, pop_count_age_sex_m))
    pop_count_age_sex_base$age_span[pop_count_age_sex_base$age_start == OAnew] <- 1000

  }

  if (maxage == OAnew) {
    pop_count_age_sex_base <- pop_in
  }
  rm(ages,maxage)

  # replace any zero values with a very small number
  pop_count_age_sex_base$value[pop_count_age_sex_base$value == 0] <- 0.00001

  ############
  # age-specific fertility rates
  ############
  fert_in <- ccmppWPP_inputs$fert_rate_age_f
  fert_in <- fert_in [order(fert_in$time_start, fert_in$age_start),]
  ages <- unique(fert_in$age_start)
  maxage <- max(ages)

  # here we simply add zeros to the extra age groups
  if (maxage < OAnew) {

    asfr_add <- as.data.frame(matrix(0.0, nrow=OAnew - maxage, ncol = length(unique(fert_in$time_start)),
                                     dimnames = list((maxage+1):OAnew, unique(fert_in$time_start))))
    asfr_add$age_start = as.numeric(row.names(asfr_add))
    asfr_add <- reshape(asfr_add,
                        direction = "long",
                        times = as.numeric(names(asfr_add[1:(ncol(asfr_add)-1)])),
                        timevar = "time_start",
                        idvar = "age_start",
                        varying = list(names(asfr_add)[1:(ncol(asfr_add)-1)]),
                        v.names = "value")
    asfr_add$age_span <- 1
    asfr_add$time_span <- 1

    fert_rate_age_f <- rbind(fert_in, asfr_add)
    fert_rate_age_f$age_span <- ifelse(fert_rate_age_f$age_start == OAnew, 1000, 1)
    fert_rate_age_f <- fert_rate_age_f[order(fert_rate_age_f$time_start, fert_rate_age_f$age_start),]

  }

  if (maxage > OAnew) {

    fert_rate_age_f <- fert_in[fert_in$age_start <= OAnew,]

  }
  if (maxage == OAnew) {

    fert_rate_age_f <- fert_in
  }
  rm(ages,maxage)

  ############
  # migration
  ############
  mig_in <- ccmppWPP_inputs$mig_net_count_age_sex
  mig_in <- mig_in [order(mig_in$time_start, mig_in$sex, mig_in$age_start),]
  ages <- unique(mig_in$age_start)
  maxage <- max(ages)

  if(maxage < OAnew) {
    mig_add <- as.data.frame(matrix(0.0, nrow=OAnew - maxage, ncol = length(unique(mig_in$time_start)),
                                    dimnames = list((maxage+1):OAnew, unique(mig_in$time_start))))

    mig_add$age_start = as.numeric(row.names(mig_add))
    mig_add <- reshape(mig_add,
                       direction = "long",
                       times = as.numeric(names(mig_add[1:(ncol(mig_add)-1)])),
                       timevar = "time_start",
                       idvar = "age_start",
                       varying = list(names(mig_add)[1:(ncol(mig_add)-1)]),
                       v.names = "value")
    mig_add$age_span <- 1
    mig_add$time_span <- 1

    mig_add_m <- mig_add
    mig_add_m$sex <- "male"
    mig_add_f <- mig_add
    mig_add_f$sex <- "female"

    mig_net_count_age_sex <- rbind(mig_in, mig_add_m, mig_add_f)
    mig_net_count_age_sex$age_span <- ifelse(mig_net_count_age_sex$age_start == OAnew, 1000, 1)
    mig_net_count_age_sex <- mig_net_count_age_sex[order(mig_net_count_age_sex$time_start, mig_net_count_age_sex$sex, mig_net_count_age_sex$age_start),]

    # for now set rates to 0 since we are not using these for 2022 revision
    mig_net_rate_age_sex <- mig_net_count_age_sex
    mig_net_rate_age_sex$value <- 0.0

  }
  if (maxage > OAnew) {
    mig_net_count_age_sex <- mig_in[mig_in$age_start <= OAnew,]
    mig_net_rate_age_sex <- ccmppWPP_inputs$mig_net_rate_age_sex[ccmppWPP_inputs$mig_net_rate_age_sex$age_start <= OAnew,]
  }

  if (maxage == OAnew){
    mig_net_count_age_sex <- mig_in
    mig_net_rate_age_sex <- ccmppWPP_inputs$mig_net_rate_age_sex
  }
  rm(ages,maxage)


  ccmppWPP_inputs_extended <- list(pop_count_age_sex_base = pop_count_age_sex_base,
                                   life_table_age_sex = life_table_age_sex,
                                   fert_rate_age_f = fert_rate_age_f,
                                   srb = ccmppWPP_inputs$srb,
                                   mig_net_count_age_sex = mig_net_count_age_sex,
                                   mig_net_rate_age_sex = mig_net_rate_age_sex,
                                   mig_net_count_tot_b = ccmppWPP_inputs$mig_net_count_tot_b,
                                   mig_parameter = ccmppWPP_inputs$mig_parameter)

  attr(ccmppWPP_inputs_extended, "revision") <- attributes(ccmppWPP_inputs)$revision
  attr(ccmppWPP_inputs_extended, "locid")    <- attributes(ccmppWPP_inputs)$locid
  attr(ccmppWPP_inputs_extended, "locname")    <- attributes(ccmppWPP_inputs)$locname
  attr(ccmppWPP_inputs_extended, "variant")  <- attributes(ccmppWPP_inputs)$variant
  attr(ccmppWPP_inputs_extended, "a0rule")  <- attributes(ccmppWPP_inputs)$a0rule


  return(ccmppWPP_inputs_extended)

}


# THIS FUNCTION COMPILES THE INPUT FILE NEEDED FOR MEDIUM VARIANT PROJECTIONS

#' @export
ccmppWPP_input_file_medium <- function(tfr_median_all_locs, # medium tfr from bayesian model
                                       srb_median_all_locs, # projected srb
                                       e0_median_all_locs, # medium e0 from bayesian model
                                       mx_median_all_locs = NULL,
                                       mig_net_count_proj_all_locs, # projected net migration by age and sex
                                       PasfrGlobalNorm, # pasfr global norm from pasfr_global_model() function
                                       PasfrNorm, # use Global Model or ConstantPasfr for this country
                                       input_file_path, # file path name for Excel input file
                                       ccmpp_estimates_130_folder) { # file path to folder where ccmpp intermediate outputs for ages 0 to 130 are stored

  # read metadata from parameters sheet of excel input file
  meta <-   readxl::read_xlsx(path = input_file_path,
                              sheet = "parameters",
                              n_max = 1048576)

  meta <- meta %>%
    dplyr::select(parameter, value) %>%
    dplyr::filter(!is.na(parameter))

  meta.list <- list()
  for (i in 1:nrow(meta)) {
    meta.list[[i]] <- ifelse(!is.na(suppressWarnings(as.numeric(meta$value[i]))), as.numeric(meta$value[i]), meta$value[i])
    names(meta.list)[i] <- gsub(" ", "_", meta$parameter[i])
  }
  rm(meta)

  locid <- meta.list$LocationID
  projection_years <- meta.list$Projection_First_Year:meta.list$Projection_Last_Year

  # load the intermediate outputs file for estimates that contains population by single year of age from 0 to 130+
  load(paste0(ccmpp_estimates_130_folder,locid,"_ccmpp_output.RData"))
  a0rule <- attributes(ccmpp_output)$a0rule

  # base year population by sex and single year of age from 0:100
  pop_count_age_sex_base <- ccmpp_output$pop_count_age_sex[ccmpp_output$pop_count_age_sex$time_start == meta.list$Projection_First_Year &
                                                             ccmpp_output$pop_count_age_sex$sex %in% c("female","male"),]

  ############################
  ############################
  # compute asfr for the medium variant TFR

  # get asfr observed from estimates
  asfr_observed <- ccmpp_output$fert_rate_age_f[ccmpp_output$fert_rate_age_f$age_start %in% c(10:54), # bayesPop requires single year of age from 10 to 54
                                                c("time_start", "age_start", "value")]
  # compute observed tfr from asfr
  tfr_observed <- sum_last_column(asfr_observed[, c("time_start", "value")])

  # compute observed pasfr
  pasfr_observed <- merge(asfr_observed, tfr_observed, by = "time_start")
  pasfr_observed$value <- pasfr_observed$value.x/pasfr_observed$value.y

  # transform to matrix
  pasfr_observed <- reshape(pasfr_observed[,c("time_start","age_start","value")],
                            direction = "wide", idvar = "age_start", timevar = "time_start")
  pasfr_observed_matrix <- as.matrix(pasfr_observed[,2:ncol(pasfr_observed)])
  rownames(pasfr_observed_matrix) <- unique(pasfr_observed$age_start)
  colnames(pasfr_observed_matrix) <- tfr_observed$time_start

  # get location-specific vectors of tfr estimates and projections, names are years
  tfr_median_all_locs <- tfr_median_all_locs[order(tfr_median_all_locs$time_start),]
  tfr_observed_projected <- tfr_median_all_locs$value[tfr_median_all_locs$LocID == locid]
  names(tfr_observed_projected) <- unique(tfr_median_all_locs$time_start)

  # project pasfr given projected tfr and historical observed pasfr
  pasfr_medium <- pasfr_given_tfr(PasfrGlobalNorm,
                                  PasfrNorm = PasfrNorm,
                                  pasfr_observed = pasfr_observed,
                                  tfr_observed_projected = tfr_observed_projected,
                                  years_projection = projection_years)

  # multiply projected pasfr by projected tfr to get projected asfr
  tfr_projected <- tfr_observed_projected[names(tfr_observed_projected) %in% projection_years]
  asfr_medium <- t(tfr_projected  * (t(pasfr_medium /100)))
  rownames(asfr_medium) <- 10:54

  # fill-in zero fertility for ages < 10 and > 54
  asfr_0_9 <- matrix(0.0, nrow=length(0:9), ncol = ncol(asfr_medium), dimnames = list(0:9, colnames(asfr_medium)))
  asfr_55_130 <- matrix(0.0, nrow=length(55:130), ncol = ncol(asfr_medium), dimnames = list(55:130, colnames(asfr_medium)))
  asfr_medium <- rbind(asfr_0_9,  asfr_medium, asfr_55_130)

  # transform to long data frame
  asfr_medium <- as.data.frame(asfr_medium)
  asfr_medium$age_start <- as.numeric(row.names(asfr_medium))
  asfr_medium <- reshape(asfr_medium,
                         idvar = "age_start",
                         direction = "long",
                         varying = list(names(asfr_medium)[1:(ncol(asfr_medium)-1)]),
                         times = names(asfr_medium)[1:(ncol(asfr_medium)-1)],
                         timevar = "time_start",
                         v.names = "value")
  asfr_medium$age_span <- ifelse(asfr_medium$age_start < 130, 1, 1000)
  asfr_medium$time_span <- 1

  ############################
  ############################
  # get projected srb

  srb <-   srb_median_all_locs[srb_median_all_locs$LocID == locid & srb_median_all_locs$time_start %in% projection_years,]

  if (is.null(mx_median_all_locs)) {
    ############################
    ############################
    # get projected 1mx from projected e0

    # get mx estimates by single year of age from 0 to 130
    life_table_age_sex_mx <- ccmpp_output$lt_complete_age_sex[ccmpp_output$lt_complete_age_sex$indicator=="lt_nMx" &
                                                                ccmpp_output$lt_complete_age_sex$sex %in% c("female", "male"),]

    # transform life_table_age_sex_mx into the matrices needed by the MortCast functions
    mxM <- life_table_age_sex_mx[life_table_age_sex_mx$sex == "male", c("age_start", "time_start", "value")]
    mxMr <- reshape(mxM, idvar = "age_start", timevar = "time_start", direction = "wide")
    mx_mat_m <- as.matrix(mxMr[,2:ncol(mxMr)])
    rownames(mx_mat_m) <- unique(mxM$age_start)
    colnames(mx_mat_m) <- unique(mxM$time_start)

    mxF <- life_table_age_sex_mx[life_table_age_sex_mx$sex == "female", c("age_start", "time_start", "value")]
    mxFr <- reshape(mxF, idvar = "age_start", timevar = "time_start", direction = "wide")
    mx_mat_f <- as.matrix(mxFr[,2:ncol(mxFr)])
    rownames(mx_mat_f) <- unique(mxF$age_start)
    colnames(mx_mat_f) <- unique(mxF$time_start)

    # parse median projected e0f and e0m
    e0_median_all_locs <- e0_median_all_locs[order(e0_median_all_locs$time_start),]
    e0f_projected <- e0_median_all_locs$value[e0_median_all_locs$LocID == locid &
                                               e0_median_all_locs$sex == "female" &
                                                e0_median_all_locs$time_start %in% projection_years]
    names(e0f_projected) <- projection_years

    e0m_projected <- e0_median_all_locs$value[e0_median_all_locs$LocID == locid &
                                                e0_median_all_locs$sex == "male" &
                                                e0_median_all_locs$time_start %in% projection_years]
    names(e0m_projected) <- projection_years

    # extract mortality parameters from Excel input file
    MORT_PARAMS <- readxl::read_xlsx(path = input_file_path, sheet = "MORT_PARAMS")

    Age_Mort_Proj_arguments <- list(Age_Mort_Proj_Method1 = MORT_PARAMS$value[MORT_PARAMS$parameter=="Age_Mort_Proj_Method1"],
                                    Age_Mort_Proj_Method2 = MORT_PARAMS$value[MORT_PARAMS$parameter=="Age_Mort_Proj_Method2"],
                                    Age_Mort_Proj_Pattern = MORT_PARAMS$value[MORT_PARAMS$parameter=="Age_Mort_Proj_Pattern"],
                                    Age_Mort_Proj_Method_Weights = eval(parse(text = MORT_PARAMS$value[MORT_PARAMS$parameter=="Age_Mort_Proj_Method_Weights"])),
                                    Age_Mort_Proj_Adj_SR = eval(parse(text = MORT_PARAMS$value[MORT_PARAMS$parameter=="Age_Mort_Proj_Adj_SR"])),
                                    Latest_Age_Mortality_Pattern = eval(parse(text = MORT_PARAMS$value[MORT_PARAMS$parameter=="Latest_Age_Mortality_Pattern"])),
                                    Latest_Age_Mortality_Pattern_Years = eval(parse(text = MORT_PARAMS$value[MORT_PARAMS$parameter=="Latest_Age_Mortality_Pattern_Years"])),
                                    Smooth_Latest_Age_Mortality_Pattern = eval(parse(text = MORT_PARAMS$value[MORT_PARAMS$parameter=="Smooth_Latest_Age_Mortality_Pattern"])),
                                    Smooth_Latest_Age_Mortality_Pattern_Degree = eval(parse(text = MORT_PARAMS$value[MORT_PARAMS$parameter=="Smooth_Latest_Age_Mortality_Pattern_Degree"])))

     mx_medium <- mx_given_e0(mx_mat_m = mx_mat_m, # matrix of mx estimates (age in rows, years in columns)
                             mx_mat_f = mx_mat_f,
                             e0m = e0m_projected, # vector of projected e0 (named with years)
                             e0f = e0f_projected,
                             Age_Mort_Proj_Method1 = tolower(Age_Mort_Proj_arguments$Age_Mort_Proj_Method1),
                             Age_Mort_Proj_Method2 = tolower(Age_Mort_Proj_arguments$Age_Mort_Proj_Method2),  # only used if first method is "pmd"
                             Age_Mort_Proj_Pattern = Age_Mort_Proj_arguments$Age_Mort_Proj_Pattern,
                             Age_Mort_Proj_Method_Weights = Age_Mort_Proj_arguments$Age_Mort_Proj_Method_Weights,
                             Age_Mort_Proj_Adj_SR = Age_Mort_Proj_arguments$Age_Mort_Proj_Adj_SR,
                             Latest_Age_Mortality_Pattern = Age_Mort_Proj_arguments$Latest_Age_Mortality_Pattern,
                             Latest_Age_Mortality_Pattern_Years = Age_Mort_Proj_arguments$Latest_Age_Mortality_Pattern_Years,
                             Smooth_Latest_Age_Mortality_Pattern = Age_Mort_Proj_arguments$Smooth_Latest_Age_Mortality_Pattern,
                             Smooth_Latest_Age_Mortality_Pattern_Degree = Age_Mort_Proj_arguments$Smooth_Latest_Age_Mortality_Pattern_Degree) # a number between 1 and nrow(mx_mat). Higher numbers give less smoothing

    # # old
    #  mx_medium <- mx_given_e0(mx_mat_m = mx_mat_m, # matrix of mx estimates (age in rows, years in columns)
    #                           mx_mat_f = mx_mat_f,
    #                           e0m = e0m_projected, # vector of projected e0 (named with years)
    #                           e0f = e0f_projected,
    #                           Age_Mort_Proj_Method1 = tolower(Age_Mort_Proj_arguments$Age_Mort_Proj_Method1),
    #                           Age_Mort_Proj_Method2 = tolower(Age_Mort_Proj_arguments$Age_Mort_Proj_Method2),  # only used if first method is "pmd"
    #                           Age_Mort_Proj_Pattern = Age_Mort_Proj_arguments$Age_Mort_Proj_Pattern,
    #                           Age_Mort_Proj_Method_Weights = Age_Mort_Proj_arguments$Age_Mort_Proj_Method_Weights,
    #                           Age_Mort_Proj_Adj_SR = Age_Mort_Proj_arguments$Age_Mort_Proj_Adj_SR,
    #                           Latest_Age_Mortality_Pattern = Age_Mort_Proj_arguments$Latest_Age_Mortality_Pattern,
    #                           Smooth_Latest_Age_Mortality_Pattern = Age_Mort_Proj_arguments$Smooth_Latest_Age_Mortality_Pattern) # a number between 1 and nrow(mx_mat). Higher numbers give less smoothing
    #
    # organize into a long file required by ccmppWPP

    mx_f <- mx_medium$female$mx[,colnames(mx_medium$female$mx) %in% projection_years]
    mx_f <- as.data.frame(mx_f)
    mx_f$age_start <- as.numeric(row.names(mx_f))
    mx_f <- reshape(mx_f,
                    idvar = "age_start",
                    direction = "long",
                    varying = list(names(mx_f)[1:(ncol(mx_f)-1)]),
                    times = names(mx_f)[1:(ncol(mx_f)-1)],
                    timevar = "time_start",
                    v.names = "value")
    mx_f$age_span <- ifelse(mx_f$age_start < max(mx_f$age_start), 1, 1000)
    mx_f$time_span <- 1
    mx_f$sex <- "female"

    mx_m <- mx_medium$male$mx[,colnames(mx_medium$male$mx) %in% projection_years]
    mx_m <- as.data.frame(mx_m)
    mx_m$age_start <- as.numeric(row.names(mx_m))
    mx_m <- reshape(mx_m,
                    idvar = "age_start",
                    direction = "long",
                    varying = list(names(mx_m)[1:(ncol(mx_m)-1)]),
                    times = names(mx_m)[1:(ncol(mx_m)-1)],
                    timevar = "time_start",
                    v.names = "value")
    mx_m$age_span <- ifelse(mx_m$age_start < max(mx_m$age_start), 1, 1000)
    mx_m$time_span <- 1
    mx_m$sex <- "male"

    mx_all_long <- rbind(mx_f, mx_m)
    mx_all_long$time_start <- as.numeric(mx_all_long$time_start)
  } else {

    mx_all_long <- mx_median_all_locs[mx_median_all_locs$locid == locid, c("time_start", "time_span", "sex", "age_start", "age_span", "value")]

  }

  lts_all <- list()
  for (i in 1:(length(projection_years))) {

    ltf <- DemoTools::lt_single_mx(nMx = mx_all_long$value[mx_all_long$sex == "female" & mx_all_long$time_start == projection_years[i]],
                                   Age = mx_all_long$age_start[mx_all_long$sex == "female" & mx_all_long$time_start == projection_years[i]],
                                   Sex = "f", a0rule = a0rule)
    ltf$sex <- "female"
    ltf$time_start <- projection_years[i]
    ltm <- DemoTools::lt_single_mx(nMx = mx_all_long$value[mx_all_long$sex == "male" & mx_all_long$time_start == projection_years[i]],
                                   Age = mx_all_long$age_start[mx_all_long$sex == "male" & mx_all_long$time_start == projection_years[i]],
                                   Sex = "m" , a0rule = a0rule)
    ltm$sex <- "male"
    ltm$time_start <- projection_years[i]

    lts_all[[i]] <- rbind(ltf, ltm)

  }
  lts_all <- do.call(rbind, lts_all)

  lts_all_long <- reshape(lts_all,
                          idvar = c("time_start", "sex", "Age"),
                          drop = c("AgeInt"),
                          direction = "long",
                          varying = list(names(lts_all)[3:11]),
                          times = names(lts_all)[3:11],
                          timevar = "indicator",
                          v.names = "value")

  lts_all_long$age_start <- lts_all_long$Age
  lts_all_long$age_span <- 1
  lts_all_long$age_span[lts_all_long$age_start == 130] <- 1000
  lts_all_long$time_span <- 1
  lts_all_long$indicator <- paste0("lt_", lts_all_long$indicator)
  lts_all_long <- lts_all_long[,c("indicator", "time_start", "time_span", "sex", "age_start", "age_span", "value")]

  # net international migration inputs
  mig_net_count_age_sex <- list()
  for (i in 1:length(projection_years)) {
    mig_net_count_age_sex[[i]] <- data.frame(time_start = rep(projection_years[i], 262),
                                             age_start = rep(0:130,2),
                                             sex = c(rep("male", 131), rep("female", 131)))
  }
  mig_net_count_age_sex <- do.call(rbind, mig_net_count_age_sex)
  mig_net_count_age_sex <- merge(mig_net_count_age_sex,
                                 mig_net_count_proj_all_locs[mig_net_count_proj_all_locs$locid == locid &
                                                               mig_net_count_proj_all_locs$time_start %in% projection_years,
                                                             c("time_start", "sex", "age_start", "value")],
                                 by = c("time_start", "sex", "age_start"), all.x = TRUE, all.y= FALSE)
  mig_net_count_age_sex$time_span <- 1
  mig_net_count_age_sex$age_span <- 1
  mig_net_count_age_sex$age_span[mig_net_count_age_sex$age_start == 130] <- 1000
  mig_net_count_age_sex$value[is.na(mig_net_count_age_sex$value)] <- 0
  mig_net_count_age_sex <- mig_net_count_age_sex[mig_net_count_age_sex$time_start <= max(projection_years),
                                                 c("time_start", "time_span", "sex", "age_start", "age_span", "value")]

  mig_net_rate_age_sex <- mig_net_count_age_sex
  mig_net_rate_age_sex$value <- 0 # This is a placeholder -- not in use for WPP 2021
  mig_net_count_tot_b <- sum_last_column(mig_net_count_age_sex[,c("time_start", "time_span", "value")]) # This is a placeholder -- not in use for WPP 2022

  mig_parameter <- data.frame(indicator = c(rep("mig_type", length(projection_years)), rep("mig_assumption", length(projection_years))),
                              time_start = rep(projection_years,2),
                              time_span = rep(1, length(projection_years)*2),
                              value = c(rep("counts", length(projection_years)), rep("end", length(projection_years))))

  # assemble the ccmppWPP input file needed to carry out the deterministic projection for the medium variant
  inputs <- list(pop_count_age_sex_base = pop_count_age_sex_base,
                 life_table_age_sex     = lts_all_long[,c("indicator","time_start","time_span","sex","age_start","age_span","value")],
                 fert_rate_age_f        = asfr_medium[,c("time_start", "time_span", "age_start", "age_span", "value")],
                 srb                    = srb,
                 mig_net_count_age_sex  = mig_net_count_age_sex,
                 mig_net_rate_age_sex   = mig_net_rate_age_sex,
                 mig_net_count_tot_b    = mig_net_count_tot_b,
                 mig_parameter          = mig_parameter)

  # set attributes
  attributes <- attributes(ccmpp_output)
  attr(inputs, "locid") <- attributes$locid
  attr(inputs, "locname") <- attributes$locname
  attr(inputs, "variant") <- "medium"
  attr(inputs, "a0rule") <- attributes$a0rule

  return(inputs)

}

# THIS FUNCTION PREPARES THE INPUT FILES FOR EACH OF THE DETERMINISTIC PROJECTION VARIANT SCENARIOS

#' Derive inputs needed for deterministic projection variants
#'
#' @description Returns a list of age specific fertility rates and life table values needed as inputs to the
#' deterministic projection variants, including low, high and constant fertility, instant replacement fertility and
#' momentum, and constant mortality
#'
#' @author Sara Hertog
#'
#' @param medium_variant_outputs list of data frames. medium variant output from the ccmppWPP_workflow_one_country_variant function
#'
#' @details accesses the global model in the data frame "pasfr_global_model" needed to infer pasfr from tfr
#'
#' @return a list of data frames
#' @export
#'

ccmpp_input_file_proj_variants <- function(ccmppWPP_estimates,
                                           ccmppWPP_medium,
                                           PasfrGlobalNorm,
                                           PasfrNorm) {


  # create vector of projection time_starts
  projection_times <- unique(ccmppWPP_medium$fert_rate_age_f$time_start)
  projection_start_year <- projection_times[1]

  # extract asfr estimates
  asfr_df <- ccmppWPP_estimates$fert_rate_age_f[, c("time_start", "age_start", "value")]

  # extract tfr estimates
  tfr_est_df <- sum_last_column(asfr_df[,c("time_start", "value")])
  tfr_est <- tfr_est_df$value
  names(tfr_est) <- unique(asfr_df$time_start)

  # compute pasfr
  pasfr_est <- merge(asfr_df, tfr_est_df, by = "time_start")
  pasfr_est$value <- pasfr_est$value.x/pasfr_est$value.y
  pasfr_est <- pasfr_est[, c("time_start", "age_start", "value")]

  # reshape to matrix
  pasfr_estimates <- reshape(pasfr_est, idvar = "age_start", timevar = "time_start", direction = "wide")
  pasfr_estimates <- as.matrix(pasfr_estimates[,c(2:ncol(pasfr_estimates))])
  rownames(pasfr_estimates) <- unique(pasfr_est$age_start)
  colnames(pasfr_estimates) <- unique(pasfr_est$time_start)

  ############################
  ############################
  # compute asfr inputs for low/high fertility variants

  # extract medium variant tfr
  tfr_med <- sum_last_column(ccmppWPP_medium$fert_rate_age_f[, c("time_start", "value")])$value
  names(tfr_med) <- projection_times

  #    for tfr adjustment, subtract/add 0.25 children in first five years, 0.4 children in next five years, 0.5 children thereafter
  tfr_adj <- rep(0.5, length(projection_times))
  tfr_adj[which(projection_times >= projection_start_year & projection_times < projection_start_year+5)] <- 0.25
  tfr_adj[which(projection_times >= projection_start_year+5 & projection_times < projection_start_year+10)] <- 0.40

  #    compute asfr for low-fertility variant
  tfr_low <- tfr_med - tfr_adj

  pasfr_low <- pasfr_given_tfr(PasfrGlobalNorm = PasfrGlobalNorm,
                               PasfrNorm = PasfrNorm,
                               pasfr_observed = pasfr_estimates[11:55,],
                               tfr_observed_projected = c(tfr_est, tfr_low),
                               years_projection = projection_times,
                               num_points = 15)

  asfr_low <- t(tfr_low * t(pasfr_low/100))

  asfr_0_9 <- matrix(0, nrow = 10, ncol = ncol(asfr_low))
  asfr_55_130 <- matrix(0, nrow = 76, ncol = ncol(asfr_low))

  asfr_low <- rbind(asfr_0_9,  asfr_low, asfr_55_130)
  rownames(asfr_low) <- 0:130

  # transform to long data frame
  fert_rate_age_f_low <- as.data.frame(asfr_low)
  fert_rate_age_f_low$age_start <- as.numeric(row.names(asfr_low))
  fert_rate_age_f_low <- reshape(fert_rate_age_f_low,
                                 idvar = "age_start",
                                 direction = "long",
                                 varying = list(names(fert_rate_age_f_low)[1:(ncol(fert_rate_age_f_low)-1)]),
                                 times = names(fert_rate_age_f_low)[1:(ncol(fert_rate_age_f_low)-1)],
                                 timevar = "time_start",
                                 v.names = "value")
  fert_rate_age_f_low$age_span <- ifelse(fert_rate_age_f_low$age_start < 130, 1, 1000)
  fert_rate_age_f_low$time_span <- 1
  fert_rate_age_f_low <- fert_rate_age_f_low[, c("time_start", "time_span", "age_start", "age_span", "value")]

  #    compute asfr for high-fertility variant
  tfr_high <- tfr_med + tfr_adj

  pasfr_high <- pasfr_given_tfr(PasfrGlobalNorm = PasfrGlobalNorm,
                                PasfrNorm = PasfrNorm,
                                pasfr_observed = pasfr_estimates[11:55,],
                                tfr_observed_projected = c(tfr_est, tfr_high),
                                years_projection = projection_times,
                                num_points = 15)

  asfr_high <- t(tfr_high * t(pasfr_high/100))
  asfr_high <- rbind(asfr_0_9,  asfr_high, asfr_55_130)
  rownames(asfr_high) <- 0:130

  # transform to long data frame
  fert_rate_age_f_high <- as.data.frame(asfr_high)
  fert_rate_age_f_high$age_start <- as.numeric(row.names(asfr_high))
  fert_rate_age_f_high <- reshape(fert_rate_age_f_high,
                                  idvar = "age_start",
                                  direction = "long",
                                  varying = list(names(fert_rate_age_f_high)[1:(ncol(fert_rate_age_f_high)-1)]),
                                  times = names(fert_rate_age_f_high)[1:(ncol(fert_rate_age_f_high)-1)],
                                  timevar = "time_start",
                                  v.names = "value")
  fert_rate_age_f_high$age_span <- ifelse(fert_rate_age_f_high$age_start < 100, 1, 1000)
  fert_rate_age_f_high$time_span <- 1
  fert_rate_age_f_high <- fert_rate_age_f_high[, c("time_start", "time_span", "age_start", "age_span", "value")]


  ############################
  ############################
  # extract asfr inputs for constant-fertility variant (constant at first projected) (for 2022 revision to avoid a Covid year)

  asfr_first_projected <- ccmppWPP_medium$fert_rate_age_f[ccmppWPP_medium$fert_rate_age_f$time_start == projection_start_year,]
  fert_rate_age_f_constant <- NULL
  for (i in 1:length(projection_times)) {
    asfr_add <- asfr_first_projected
    asfr_add$time_start <- projection_times[i]
    fert_rate_age_f_constant <- rbind(fert_rate_age_f_constant, asfr_add)
  }

  ############################
  ############################
  #    compute asfr for instant replacement fertility (NRR = 1) (this code I adapted from Abacus)

  fert_rate_age_f_instant <- list()
  for (i in 1:length(projection_times)) {

    srb_time              <- ccmppWPP_medium$srb[ccmppWPP_medium$srb$time_start == projection_times[i],]
    fert_rate_age_f_time  <- ccmppWPP_medium$fert_rate_age_f[ccmppWPP_medium$fert_rate_age_f$time_start == projection_times[i],]
    lx_f_time             <- ccmppWPP_medium$lt_complete_age_sex[ccmppWPP_medium$lt_complete_age_sex$time_start == projection_times[i] &
                                                                   ccmppWPP_medium$lt_complete_age_sex$indicator == "lt_lx" &
                                                                   ccmppWPP_medium$lt_complete_age_sex$sex == "female",]
    # interpolate lx to middle of each age group
    lx_f_mid           <- (lx_f_time$value + c(lx_f_time$value[2:length(lx_f_time$value)],NA))/2
    # compute female births implied by lx_f_mid, period asfr and period srb
    births_f_age       <- (lx_f_mid * fert_rate_age_f_time$value) * (1/(1+srb_time$value))
    births_f_age       <- births_f_age[!is.na(births_f_age)]
    # compute scaling factor needed to achieve NRR=1
    scaling_factor    <- 100000/sum(births_f_age)
    # compute instant replacement asfr by multiplying period asfr by scaling factor
    fert_rate_age_f_time$value   <- fert_rate_age_f_time$value * scaling_factor
    fert_rate_age_f_instant[[i]] <- fert_rate_age_f_time

  }
  fert_rate_age_f_instant <- do.call(rbind, fert_rate_age_f_instant)

  ############################
  ############################
  # extract life table inputs for constant-mortality variant

  # take sex-specific life tables from last observed period
  lt_first_projected <- ccmppWPP_medium$lt_complete_age_sex[ccmppWPP_medium$lt_complete_age_sex$time_start == projection_start_year &
                                                              ccmppWPP_medium$lt_complete_age_sex$sex %in% c("male","female"),]
  # initialize constant-mortality output
  lt_constant <- list()
  # repeat starting period sex-specific life table for each period in projection horizon
  for (i in 1:length(projection_times)) {

    lt_time            <- lt_first_projected
    lt_time$time_start <- projection_times[i]
    lt_constant[[i]]   <- lt_time

  }
  # assemble life table outputs into one data frame
  life_table_age_sex_constant <- do.call(rbind, lt_constant)

  ############################
  ############################
  #    compute asfr for momentum (constant mortality and NRR = 1)

  fert_rate_age_f_momentum <- list()
  for (i in 1:length(projection_times)) {

    srb_time              <- ccmppWPP_medium$srb[ccmppWPP_medium$srb$time_start == projection_times[i],]
    fert_rate_age_f_time  <- ccmppWPP_medium$fert_rate_age_f[ccmppWPP_medium$fert_rate_age_f$time_start == projection_times[i],]
    lx_f_time             <- life_table_age_sex_constant[life_table_age_sex_constant$time_start == projection_times[i] &
                                                           life_table_age_sex_constant$indicator=="lt_lx" &
                                                           life_table_age_sex_constant$sex=="female",]
    # interpolate lx to middle of each age group
    lx_f_mid            <- (lx_f_time$value + c(lx_f_time$value[2:length(lx_f_time$value)],NA))/2
    # compute female births implied by lx_mid, period asfr and period srb
    births_f_age       <- (lx_f_mid * fert_rate_age_f_time$value) * (1/(1+srb_time$value))
    births_f_age       <- births_f_age[!is.na(births_f_age)]
    # compute scaling factor needed to achieve NRR=1
    scaling_factor    <- 100000/sum(births_f_age)
    # compute instant replacement asfr by multiplying period asfr by scaling factor
    fert_rate_age_f_time$value   <- fert_rate_age_f_time$value * scaling_factor
    fert_rate_age_f_momentum[[i]] <- fert_rate_age_f_time

  }
  fert_rate_age_f_momentum <- do.call(rbind, fert_rate_age_f_momentum)

  ### QUESTION: DO WE HOLD SRB CONSTANT AT 2020 LEVEL OVER INSTANT REPLACEMENT AND MOMENTUM SCENARIOS
  ### OR USE MEDIUM VARIANT SRB EVEN IF IT CHANGES OVER THE PROJECTION HORIZON?
  ### CURRENT IMPLEMENTATION IS USING THE MEDIUM VARIANT SRB

  # assemble the ccmppWPP input objects needed for deterministic variants

  pop_count_age_sex_base <- ccmppWPP_estimates$pop_count_age_sex[ccmppWPP_estimates$pop_count_age_sex$time_start == projection_start_year &
                                                                       ccmppWPP_estimates$pop_count_age_sex$sex %in% c("male", "female"),]

  # make a dummy filler for migration rates since these are not operationalized yet
  mig_net_rate_age_sex <- ccmppWPP_medium$mig_net_count_age_sex[ccmppWPP_medium$mig_net_count_age_sex$sex %in% c("male","female"),]
  mig_net_rate_age_sex$value <- 0
  mig_net_count_tot_b <- sum_last_column(ccmppWPP_medium$mig_net_count_age_sex[,c("time_start", "time_span", "value")])

  mig_parameter <- data.frame(indicator = c(rep("mig_type", length(projection_times)), rep("mig_assumption", length(projection_times))),
                              time_start = rep(projection_times,2),
                              time_span = rep(1, length(projection_times)*2),
                              value = c(rep("counts", length(projection_times)), rep("end", length(projection_times))))

  # first, compile the medium variant inputs based on intermediate ccmpp outputs
  inputs_medium <- list(pop_count_age_sex_base = pop_count_age_sex_base,
                     life_table_age_sex = ccmppWPP_medium$lt_complete_age_sex[ccmppWPP_medium$lt_complete_age_sex$sex %in% c("male","female"),],
                     fert_rate_age_f = ccmppWPP_medium$fert_rate_age_f,
                     srb = ccmppWPP_medium$srb,
                     mig_net_count_age_sex = ccmppWPP_medium$mig_net_count_age_sex[ccmppWPP_medium$mig_net_count_age_sex$sex %in% c("male","female"),],
                     mig_net_rate_age_sex = mig_net_rate_age_sex,
                     mig_net_count_tot_b = mig_net_count_tot_b,
                     mig_parameter = mig_parameter)

  # all inputs same as medium variant, except asfr, which are low
  inputs_low <- inputs_medium
  inputs_low$fert_rate_age_f <- fert_rate_age_f_low
  # assign attributes
  attr(inputs_low, "locid") <- attributes(ccmppWPP_estimates)$locid
  attr(inputs_low, "locname") <- attributes(ccmppWPP_estimates)$locname
  attr(inputs_low, "variant") <- "Low fertility"
  attr(inputs_low, "a0rule")  <- attributes(ccmppWPP_estimates)$a0rule

  # same as medium variant, but with high asfr
  inputs_high <- inputs_medium
  inputs_high$fert_rate_age_f <- fert_rate_age_f_high
  # assign attributes
  attr(inputs_high, "locid") <- attributes(ccmppWPP_estimates)$locid
  attr(inputs_high, "locname") <- attributes(ccmppWPP_estimates)$locname
  attr(inputs_high, "variant") <- "High fertility"
  attr(inputs_high, "a0rule")  <- attributes(ccmppWPP_estimates)$a0rule

  # same as medium variant, but with constant asfr
  inputs_constant_fert <- inputs_medium
  inputs_constant_fert$fert_rate_age_f <- fert_rate_age_f_constant
  # assign attributes
  attr(inputs_constant_fert, "locid") <- attributes(ccmppWPP_estimates)$locid
  attr(inputs_constant_fert, "locname") <- attributes(ccmppWPP_estimates)$locname
  attr(inputs_constant_fert, "variant") <- "Constant fertility"
  attr(inputs_constant_fert, "a0rule")  <- attributes(ccmppWPP_estimates)$a0rule

  # instant replacement same as medium variant, but with instant replacement asfr
  inputs_instant <- inputs_medium
  inputs_instant$fert_rate_age_f <- fert_rate_age_f_instant
  # assign attributes
  attr(inputs_instant, "locid") <- attributes(ccmppWPP_estimates)$locid
  attr(inputs_instant, "locname") <- attributes(ccmppWPP_estimates)$locname
  attr(inputs_instant, "variant") <- "Instant replacement fertility"
  attr(inputs_instant, "a0rule")  <- attributes(ccmppWPP_estimates)$a0rule

  # momentum is instant replacement asfr, constant mortality, zero migration
  inputs_momentum <- inputs_instant
  inputs_momentum$life_table_age_sex <- life_table_age_sex_constant
  inputs_momentum$mig_net_count_age_sex$value <- 0
  inputs_momentum$mig_net_count_tot_b$value <- 0
  # assign attributes
  attr(inputs_momentum, "locid") <- attributes(ccmppWPP_estimates)$locid
  attr(inputs_momentum, "locname") <- attributes(ccmppWPP_estimates)$locname
  attr(inputs_momentum, "variant") <- "Momentum"
  attr(inputs_momentum, "a0rule")  <- attributes(ccmppWPP_estimates)$a0rule

  # no change is medium inputs with constant fertility and constant mortality
  inputs_nochange <- inputs_constant_fert
  inputs_nochange$life_table_age_sex <- life_table_age_sex_constant
  # assign attributes
  attr(inputs_nochange, "locid") <- attributes(ccmppWPP_estimates)$locid
  attr(inputs_nochange, "locname") <- attributes(ccmppWPP_estimates)$locname
  attr(inputs_nochange, "variant") <- "No change"
  attr(inputs_nochange, "a0rule")  <- attributes(ccmppWPP_estimates)$a0rule

  # constant mortality is same as medium variant but with constant life tables
  inputs_constant_mort <- inputs_medium
  inputs_constant_mort$life_table_age_sex <- life_table_age_sex_constant
  # assign attributes
  attr(inputs_constant_mort, "locid") <- attributes(ccmppWPP_estimates)$locid
  attr(inputs_constant_mort, "locname") <- attributes(ccmppWPP_estimates)$locname
  attr(inputs_constant_mort, "variant") <- "Constant mortality"
  attr(inputs_constant_mort, "a0rule")  <- attributes(ccmppWPP_estimates)$a0rule

  # zero migration is medium inputs but zero migration
  inputs_zeromig <- inputs_medium
  inputs_zeromig$mig_net_count_age_sex$value <- 0
  inputs_zeromig$mig_net_count_tot_b$value <- 0
  # assign attributes
  attr(inputs_zeromig, "locid") <- attributes(ccmppWPP_estimates)$locid
  attr(inputs_zeromig, "locname") <- attributes(ccmppWPP_estimates)$locname
  attr(inputs_zeromig, "variant") <- "Zero migration"
  attr(inputs_zeromig, "a0rule")  <- attributes(ccmppWPP_estimates)$a0rule

  # Instant replacement and zero migration
  inputs_instantzeromig <- inputs_instant
  inputs_instantzeromig$mig_net_count_age_sex$value <- 0
  inputs_instantzeromig$mig_net_count_tot_b$value <- 0
  # assign attributes
  attr(inputs_instantzeromig, "locid") <- attributes(ccmppWPP_estimates)$locid
  attr(inputs_instantzeromig, "locname") <- attributes(ccmppWPP_estimates)$locname
  attr(inputs_instantzeromig, "variant") <- "Instant replacement zero migration"
  attr(inputs_instantzeromig, "a0rule")  <- attributes(ccmppWPP_estimates)$a0rule

  # compile deterministic variant inputs into a list and return
  variant_inputs <- list(medium = inputs_medium,
                         low_fert = inputs_low,
                         high_fert = inputs_high,
                         constant_fert = inputs_constant_fert,
                         instant_replacement_fert = inputs_instant,
                         instant_zero_mig = inputs_instantzeromig,
                         momentum = inputs_momentum,
                         no_change = inputs_nochange,
                         constant_mort = inputs_constant_mort,
                         zero_mig = inputs_zeromig)

  return(variant_inputs)
}



# THIS FUNCTION PERFORMS A BASEPOP ADJUSTMENT TO 1950 POPULATION SO THAT POP COUNTS ARE CONSISTENT WITH FERT AND MORT

#' Adjust base population children for consistency with fertility and mortality
#'
#' @description Invokes DemoTools::basepop_five to adjust counts of children under age 10 to be consistent with 1950
#' fertility and mortality
#'
#' @author Sara Hertog
#'
#' @param input_file_path character string pointing to excel input file
#'
#' @details uses life tables, asfr and srb from the excel input file
#'
#' @return data frame with adjusted 1950 base population
#' @export
#'
basepop_adjust_1950_population <- function(pop_count_age_sex_base,
                                           input_file_path) {

  # read metadata from parameters sheet of excel input file
  meta <-   readxl::read_xlsx(path = input_file_path,
                              sheet = "parameters",
                                              n_max = 1048576)

  meta <- meta %>%
    dplyr::select(parameter, value) %>%
    dplyr::filter(!is.na(parameter))

  meta.list <- list()
  for (i in 1:nrow(meta)) {
    meta.list[[i]] <- ifelse(!is.na(suppressWarnings(as.numeric(meta$value[i]))), as.numeric(meta$value[i]), meta$value[i])
    names(meta.list)[i] <- gsub(" ", "_", meta$parameter[i])
  }
  rm(meta)

  # import life tables from excel input file
  life_table_age_sex <-   readxl::read_xlsx(path = input_file_path, sheet = "life_table_age_sex",
                                              n_max = 1048576)
  # import age specific fertility rates from excel input file
  fert_rate_age_f <-   readxl::read_xlsx(path = input_file_path, sheet = "fert_rate_age_f",
                                              n_max = 1048576)
  # import sex ratios at birth from excel input file
  srb <-   readxl::read_xlsx(path = input_file_path, sheet = "srb",
                                              n_max = 1048576)

  # parse nLx for males and females transform into the matrices needed for basepop
  nLxDatesIn <- 1950.0 - c(0.5, 2.5, 7.5)

  nLxMatMale <- life_table_age_sex %>%
    dplyr::filter(indicator == "lt_nLx" & sex == "male" & time_start == 1950) %>%
    select(value) %>%
    apply(MARGIN = 2, FUN = function(S) {DemoTools::single2abridged(Age = S)}) %>%
    as.data.frame() %>%
    mutate(value2 = value,
           value3 = value) %>%
    as.matrix()
  colnames(nLxMatMale) <- nLxDatesIn

  nLxMatFemale <- life_table_age_sex %>%
    dplyr::filter(indicator == "lt_nLx" & sex == "female" & time_start == 1950) %>%
    select(value) %>%
    apply(MARGIN = 2, FUN = function(S) {DemoTools::single2abridged(Age = S)}) %>%
    as.data.frame() %>%
    mutate(value2 = value,
           value3 = value) %>%
    as.matrix()
  colnames(nLxMatFemale) <- nLxDatesIn

  radix <- life_table_age_sex$value[life_table_age_sex$indicator=="lt_lx" &
                                      life_table_age_sex$age_start == 0][1]

  # parse ASFR and transform into the matrix needed for basepop

  AsfrMat <- fert_rate_age_f[, c("time_start", "age_start", "value")]

  AsfrMat <- AsfrMat %>%
    dplyr::filter(time_start == 1950) %>%
    mutate(age5 = 5 * floor(age_start/5)) %>%
    group_by(age5) %>%
    summarise(asfr = mean(value)) %>%
    dplyr::filter(age5 >=15 & age5 <= 45) %>%
    mutate(asfr2 = asfr,
           asfr3 = asfr) %>%
    select(-age5) %>%
    as.matrix()

  colnames(AsfrMat) <- nLxDatesIn
  rownames(AsfrMat) <- seq(15,45,5)
  AsfrDatesIn <- nLxDatesIn

  # get SRB
  SRBDatesIn <- floor(1950.5 - c(0.5, 2.5, 7.5))

  parse_columns <- ifelse(SRBDatesIn < 1950, 1950, SRBDatesIn)

  SRB <- NULL
  for (k in 1:length(parse_columns)) {
    SRB <- c(SRB, srb$value[srb$time_start == parse_columns[k]])
  }
  SRBDatesIn <- SRBDatesIn + 0.5


  popin <- pop_count_age_sex_base %>%
    mutate(value = replace(value, is.na(value), 0)) %>%
    arrange(sex, age_start)
  Age1 <- popin$age_start[popin$sex == "male"]
  popM <- popin$value[popin$sex=="male"]
  popF <- popin$value[popin$sex=="female"]

  # group to abridged age groups
  popM_abr <- DemoTools::single2abridged(popM)
  popF_abr <- DemoTools::single2abridged(popF)
  Age_abr  <- as.numeric(row.names(popM_abr))

  # run basepop_five()
  BP1 <- DemoTools::basepop_five(location = meta.list$LocationID,
                                 refDate = 1950.0,
                                 Age = Age_abr,
                                 Females_five = popF_abr,
                                 Males_five = popM_abr,
                                 nLxFemale = nLxMatFemale,
                                 nLxMale   = nLxMatMale,
                                 nLxDatesIn = nLxDatesIn,
                                 AsfrMat = AsfrMat,
                                 AsfrDatesIn = AsfrDatesIn,
                                 SRB = SRB,
                                 SRBDatesIn = SRBDatesIn,
                                 radix = radix,
                                 verbose = FALSE)

  # graduate result to single year of age
  popM_BP1 <- DemoTools::graduate_mono(Value = BP1[[2]], Age = Age_abr, AgeInt = DemoTools::age2int(Age_abr), OAG = TRUE)
  popF_BP1 <- DemoTools::graduate_mono(Value = BP1[[1]], Age = Age_abr, AgeInt = DemoTools::age2int(Age_abr), OAG = TRUE)

  # define childhood ages to be adjusted with basepop
  adjust_basepop_1950_maxage <- as.numeric(meta.list$adjust_basepop_1950_maxage)
  adjust_basepop_1950_maxage <- ifelse(length(adjust_basepop_1950_maxage)==0, 9, adjust_basepop_1950_maxage)
  adjust_basepop_1950_maxage <- ifelse(is.na(adjust_basepop_1950_maxage), 9, adjust_basepop_1950_maxage)

  popM_out <- c(popM_BP1[1:(adjust_basepop_1950_maxage+1)],popM[(adjust_basepop_1950_maxage+2):length(popM)])
  popF_out <- c(popF_BP1[1:(adjust_basepop_1950_maxage+1)],popF[(adjust_basepop_1950_maxage+2):length(popF)])

  popout <- popin %>%
    mutate(value = replace(value, sex == "male", round(popM_out)),
           value = replace(value, sex == "female", round(popF_out)))

 return(popout)

}

# THIS FUNCTION BACK PROJECTS A CENSUS POPULATION TO JANUARY 1 1950

#' Adjust base population children for consistency with fertility and mortality
#'
#' @description Back projects from census one year at a time using input survival ratios, then interpolates to 1 Jan 1950
#'
#' @author Sara Hertog
#'
#' @param census_protocol_adjusted data frame with census population by single year of age and sex output from census adjustment protocol
#' @param census_reference_date numeric. census reference date as decimal
#' @param life_table_age_sex data frame. the life_table_age_sex table from input file
#'
#' @details extends both population and life tables to oag = 130+; NO INTERNATIONAL MIGRATION
#'
#' @return data frame with 1950 base population back projected from the earliest census
#' @export
#'

census_back_project_1950 <- function(census_protocol_adjusted, census_reference_date, life_table_age_sex) {

  lts <- life_table_age_sex[life_table_age_sex$time_start <= census_reference_date,]
  lts <- lts[order(lts$time_start, lts$sex, lts$indicator, lts$age_start),]
  age_max <- max(lts$age_start)
  if (age_max < 130) {
    lts$id <- paste(lts$time_start, lts$sex, sep = " - ")
    ids <- unique(lts$id)

    lts_new <- list()
    for (i in 1:length(ids)) {
      df <- lts[lts$id == ids[i] & lts$indicator == "lt_nMx",]
      df_ext <- DemoTools::lt_single_mx(nMx = df$value, Age = df$age_start, Sex = substr(df$sex[1],1,1), OAnew = 130,
                                        extrapFit = 90:age_max, extrapFrom = age_max, extrapLaw = "Kannisto")

      nMx_max1 <- ifelse(df_ext$nMx<1, df_ext$nMx, 1)
      df_ext1 <- DemoTools::lt_single_mx(nMx = nMx_max1, Age = df_ext$Age, Sex = substr(df$sex[1],1,1), OAnew = 130)

      df_ext1 <- reshape(df_ext1, idvar = c("Age", "AgeInt"),
                         direction = "long",
                         times = paste0("lt_",names(df_ext1)[3:11]), timevar = "indicator",
                         varying = list(names(df_ext1)[3:11]), v.names = "value")

      df_ext1$time_start <- df$time_start[1]
      df_ext1$time_span <- df$time_span[1]
      df_ext1$sex <- df$sex[1]
      df_ext1$age_start <- df_ext1$Age
      df_ext1$age_span <- df_ext1$AgeInt
      df_ext1 <- df_ext1[,names(df)[1:7]]

      lts_new[[i]] <- df_ext1
    }
    lts <- do.call(rbind,lts_new)
  }

  # get reference years for back projection
  back_dts <- census_reference_date-(1:(census_reference_date-1949))

  # extend census population to OAG 130+

  redist_from_age <- max(65, census_adj_all[[1]]$age_redist_start - (length(back_dts) + 10))

  census_extended_M <- DemoTools::OPAG(Pop = census_protocol_adjusted$DataValue[census_protocol_adjusted$SexID == 1],
                                       Age_Pop = census_protocol_adjusted$AgeStart[census_protocol_adjusted$SexID == 1],
                                       nLx = lts$value[lts$time_start == max(floor(census_reference_date),1950) &
                                                         lts$sex == "male" & lts$indicator == "lt_nLx"],
                                       Age_nLx = 0:130,
                                       Redistribute_from = redist_from_age,
                                       OAnew = 130)$Pop_out

  # smooth the join point of the extension a bit

  # if the value at the join point is lower than that of the previous age
  if (census_extended_M[redist_from_age+1] < census_extended_M[redist_from_age]) {
    census_extended_M[redist_from_age+1] <- (census_extended_M[redist_from_age] + census_extended_M[redist_from_age+2])/2
    if (census_extended_M[redist_from_age+2] < census_extended_M[redist_from_age+1]) {
      census_extended_M[redist_from_age+2] <- (census_extended_M[redist_from_age+1] + census_extended_M[redist_from_age+3])/2
      if (census_extended_M[redist_from_age+3] < census_extended_M[redist_from_age+2]) {
        census_extended_M[redist_from_age+3] <- (census_extended_M[redist_from_age+2] + census_extended_M[redist_from_age+4])/2
      }
    }
  } else {

  # identify the minimum age above the redist_from_age at which the difference between the value from extended and the
  # previous age group value from original census is negative (such that population shrinks with age)
  nages <- length(census_protocol_adjusted$DataValue[census_protocol_adjusted$SexID == 1])
  compare <- data.frame(age_ext = 1:(nages-1),
                        ext = census_extended_M[2:nages],
                        age_cen = 0:(nages-2),
                        cen = census_protocol_adjusted$DataValue[census_protocol_adjusted$SexID == 1][1:(nages-1)])
  compare$diff <- compare$ext - compare$cen
  compare$pct <- compare$diff/compare$cen *100
  age_for_blend <- min(compare$age_ext[compare$age_ext >= redist_from_age & compare$pct <= -9])
  rm(compare)

  census_extended_M <- c(census_protocol_adjusted$DataValue[census_protocol_adjusted$SexID == 1][1:age_for_blend],
                         census_extended_M[(age_for_blend+1):131])
  names(census_extended_M) <- 0:130

  }


  census_extended_F <- DemoTools::OPAG(Pop = census_protocol_adjusted$DataValue[census_protocol_adjusted$SexID == 2],
                                       Age_Pop = census_protocol_adjusted$AgeStart[census_protocol_adjusted$SexID == 2],
                                       nLx = lts$value[lts$time_start == max(floor(census_reference_date),1950) &
                                                         lts$sex == "female" & lts$indicator == "lt_nLx"],
                                       Age_nLx = 0:130,
                                       Redistribute_from = redist_from_age,
                                       OAnew = 130)$Pop_out

  # smooth the join point of the extension a bit

  # if the value at the join point is lower than that of the previous age
  if (census_extended_F[redist_from_age+1] < census_extended_F[redist_from_age]) {
    census_extended_F[redist_from_age+1] <- (census_extended_F[redist_from_age] + census_extended_F[redist_from_age+2])/2
    if (census_extended_F[redist_from_age+2] < census_extended_F[redist_from_age+1]) {
      census_extended_F[redist_from_age+2] <- (census_extended_F[redist_from_age+1] + census_extended_F[redist_from_age+3])/2
      if (census_extended_F[redist_from_age+3] < census_extended_F[redist_from_age+2]) {
        census_extended_F[redist_from_age+3] <- (census_extended_F[redist_from_age+2] + census_extended_F[redist_from_age+4])/2
      }
    }
  } else {

  # identify the minimum age above the redist_from_age at which the difference between the value from extended and the
  # previous age group value from original census is negative (such that population shrinks with age)
  nages <- length(census_protocol_adjusted$DataValue[census_protocol_adjusted$SexID == 2])
  compare <- data.frame(age_ext = 1:(nages-1),
                        ext = census_extended_F[2:nages],
                        age_cen = 0:(nages-2),
                        cen = census_protocol_adjusted$DataValue[census_protocol_adjusted$SexID == 2][1:(nages-1)])
  compare$diff <- compare$ext - compare$cen
  compare$pct <- compare$diff/compare$cen *100
  age_for_blend <- min(compare$age_ext[compare$age_ext >= redist_from_age & compare$pct <= -9])
  rm(compare)

  census_extended_F <- c(census_protocol_adjusted$DataValue[census_protocol_adjusted$SexID == 2][1:age_for_blend],
                         census_extended_F[(age_for_blend+1):131])
  names(census_extended_F) <- 0:130

  }


  # create a matrix to store population by age back projected
  popM_back <- as.matrix(census_extended_M)
  colnames(popM_back) <- census_reference_date

  popF_back <- as.matrix(census_extended_F)
  colnames(popF_back) <- census_reference_date

  # initialize starting population
  popM <- popM_back[,1]
  popF <- popF_back[,1]

  # loop thru back dates, one year back at a time
  for (i in 1:length(back_dts)) {

    # extract male Sx
    Sx_age_m <- lts$value[lts$sex == "male" & lts$time_start == max(1950,floor(back_dts[i])) &
                            lts$indicator == "lt_Sx"]

    # back project males one step
    popM <- project_backwards_no_mig(pop_count_age_start = popM, Sx_age = Sx_age_m)
    # add a 0 for the last age group (fine because there's no one this old in 1950)
    popM <- c(popM,0)
    names(popM) <- 0:130

    # extract female Sx
    Sx_age_f <- lts$value[lts$sex == "female" & lts$time_start == max(1950,floor(back_dts[i])) &
                            lts$indicator == "lt_Sx"]

    # back project females one step
    popF <- project_backwards_no_mig(pop_count_age_start = popF, Sx_age = Sx_age_f)
    # add a 0 for the last age group
    popF <- c(popF,0)
    names(popF) <- 0:130

    popM_back <- cbind(popM_back, popM)
    colnames(popM_back)[i+1] <- back_dts[i]

    popF_back <- cbind(popF_back, popF)
    colnames(popF_back)[i+1] <- back_dts[i]

  }

  # interpolate to 1 January 1950

  interpM <- DemoTools::interpolatePop(Pop1 = popM_back[,ncol(popM_back)],
                                       Pop2 = popM_back[,ncol(popM_back)-1],
                                       Date1 = as.numeric(colnames(popM_back)[ncol(popM_back)]),
                                       Date2 = as.numeric(colnames(popM_back)[ncol(popM_back)-1]),
                                       DesiredDate = 1950.0,
                                       method = "linear")

  interpF <- DemoTools::interpolatePop(Pop1 = popF_back[,ncol(popF_back)],
                                       Pop2 = popF_back[,ncol(popF_back)-1],
                                       Date1 = as.numeric(colnames(popF_back)[ncol(popF_back)]),
                                       Date2 = as.numeric(colnames(popF_back)[ncol(popF_back)-1]),
                                       DesiredDate = 1950.0,
                                       method = "linear")

  # assemble into a data frame

  pop_out <- data.frame(time_start = rep(1950, 262),
                        time_span = rep(0,262),
                        sex = c(rep("male", 131), rep("female", 131)),
                        age_start = rep(0:130,2),
                        age_span = rep(c(rep(1,130),1000),2),
                        value = c(interpM, interpF))

  return(pop_out)

}


project_backwards_no_mig <- function(pop_count_age_start,
                                     Sx_age) {

  # check that lengths of inputs agree
  check_length <- length(pop_count_age_start) == length(Sx_age)
  if (isFALSE(check_length)) { stop("Input columns are not all the same length")}

  # get input ages
  ages <- names(pop_count_age_start)
  nage <- length(ages) # number of age groups

  # get backwards population
  pop_count_age_end <- pop_count_age_start[2:nage]/ Sx_age[1:(nage-1)]
  names(pop_count_age_end) <- ages[1:nage-1]

  return(pop_count_age_end)

}

