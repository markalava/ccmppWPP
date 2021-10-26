
# read ccmppWPP inputs from country-specific excel input files

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

  # base year population by sex and single year of age from 0:100
  pop_count_age_sex_base <- readxl::read_xlsx(path = input_file_path,
                                              sheet = "pop_count_age_sex_base",
                                              range = "A1:F203")
  
  # replace any NA values with 0
  pop_count_age_sex_base$value[is.na(pop_count_age_sex_base$value)] <- 0
  
  # asfr by single year of mother's age and single year of time (jan 1 to dec 31)
  fert_rate_age_f <- readxl::read_xlsx(path = input_file_path,
                                       sheet = "fert_rate_age_f")  
  fert_rate_age_f <- fert_rate_age_f[fert_rate_age_f$time_start >= base_year & fert_rate_age_f$time_start < begin_proj_year,]
  # ensure sort by time_start and age_start
  fert_rate_age_f <- fert_rate_age_f[order(fert_rate_age_f$time_start, fert_rate_age_f$age_start),]
  
  # srb estimates by single year of time (jan 1 to dec 31)
  srb <- readxl::read_xlsx(path = input_file_path,
                           sheet = "srb")  
  srb <- srb[srb$time_start >= base_year & srb$time_start < begin_proj_year,]
  # ensure sort by time_start
  srb <- srb[order(srb$time_start),]
  
  # read the mortality estimation parameters
  MORT_PARAMS <- readxl::read_xlsx(path = input_file_path, sheet = "MORT_PARAMS")
  mp <- MORT_PARAMS[MORT_PARAMS$type == "Estimation", c(2,3)]
  
  # for classic model life table families, we use the Coale-Demeny a0 rule, otherwise we use Andreev-Kinkaid
  a0rule <- ifelse(mp$value[mp$parameter == "Age_Specific_Mortality_Type"] == "Model-based" &
                     !is.na(mp$value[mp$parameter == "Age_Specific_MLT_Region"]) & 
                     mp$value[mp$parameter == "Age_Specific_MLT_Region"] %in% 
                     c("CD_West","CD_East","CD_North","CD_South", "UN_Chilean","UN_Far_Eastern",
                       "UN_General","UN_Latin_American","UN_South_Asian"), "cd", "ak")
  
  # asfr by single year of mother's age and single year of time (jan 1 to dec 31)
  life_table_age_sex <- readxl::read_xlsx(path = input_file_path,
                                       sheet = "life_table_age_sex")
  life_table_age_sex <- life_table_age_sex[life_table_age_sex$time_start >= base_year & life_table_age_sex$time_start < begin_proj_year,]
  # ensure sort by time_start, sex and age_start
  life_table_age_sex <- life_table_age_sex[order(life_table_age_sex$time_start, life_table_age_sex$sex, life_table_age_sex$age_start),] 
  
  # net international migration
  # estimate of counts by sex and single year of age
  mig_net_count_age_sex <- readxl::read_xlsx(path = input_file_path,
                                             sheet = "mig_net_count_age_sex")
  mig_net_count_age_sex$value[is.na(mig_net_count_age_sex$value)] <- 0
  mig_net_count_age_sex <- mig_net_count_age_sex[mig_net_count_age_sex$time_start >= base_year & mig_net_count_age_sex$time_start < begin_proj_year,]
  # ensure sort by time_start, sex and age_start
  mig_net_count_age_sex <- mig_net_count_age_sex[order(mig_net_count_age_sex$time_start, mig_net_count_age_sex$sex, mig_net_count_age_sex$age_start),]
  
  # totals (in case need to apply age distribution)
  mig_net_count_tot_b <- readxl::read_xlsx(path = input_file_path,
                                           sheet = "mig_net_count_tot_b")
  mig_net_count_tot_b$value[is.na(mig_net_count_tot_b$value)] <- 0
  mig_net_count_tot_b <- mig_net_count_tot_b[mig_net_count_tot_b$time_start >= base_year & mig_net_count_tot_b$time_start < begin_proj_year,]
  # ensure sort by time_start
  mig_net_count_tot_b <- mig_net_count_tot_b[order(mig_net_count_tot_b$time_start),]
  
  # rates -- THIS IS NOT AN OPTION FOR THE 2022 REVISION SO WE CREATE A PLACE HOLDER FOR FUTURE REVISIONS
  mig_net_rate_age_sex <- mig_net_count_age_sex
  mig_net_rate_age_sex$value <- 0
  
  # parameters
  mig_parameter <- readxl::read_xlsx(path = input_file_path,
                                     sheet = "mig_parameter")
  # ensure sort by time_start
  mig_parameter <- mig_parameter[order(mig_parameter$time_start),]
  
  
  ccmppWPP_inputs_estimates <- list(pop_count_age_sex_base = pop_count_age_sex_base,
                                    life_table_age_sex     = life_table_age_sex, 
                                    fert_rate_age_f        = fert_rate_age_f, 
                                    srb                    = srb,
                                    mig_net_count_age_sex  = mig_net_count_age_sex, 
                                    mig_net_rate_age_sex   = mig_net_rate_age_sex,
                                    mig_net_count_tot_b    = mig_net_count_tot_b,
                                    mig_parameter          = mig_parameter)
  
  # set attributes
  attr(ccmppWPP_inputs_estimates, "revision") <- "WPP2021"
  attr(ccmppWPP_inputs_estimates, "locid")    <- meta.list$LocationID
  attr(ccmppWPP_inputs_estimates, "locname")    <- meta.list$Location
  attr(ccmppWPP_inputs_estimates, "variant")  <- "estimates"
  attr(ccmppWPP_inputs_estimates, "a0rule")  <- a0rule 
  
  return(ccmppWPP_inputs_estimates)
  
}

# this function takes 1x1 inputs to the ccmppWPP and extends them to a new open age group
# we use this to extend all inputs to OAG = 130+, which then ensures consistency in life tables across 3 sex categories (female, male, both)
ccmppWPP_input_file_extend <- function(ccmppWPP_inputs, OAnew = 130, a0rule = "ak") {
 
  ############
  # life_table
  ############
  lt_in <- ccmppWPP_inputs$life_table_age_sex
  lt_in <- lt_in[order(lt_in$time_start, lt_in$sex, lt_in$indicator, lt_in$age_start),]
  ages  <- unique(lt_in$age_start)
  maxage <- max(ages)
  
  # if the OA is less then OAnew, then extend the life tables to OAnew
  lts <- NULL
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
    
  } else {
    
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
  } else {
    pop_count_age_sex_base <- pop_in
  }
  rm(ages,maxage)
  
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

  } else {
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
    
  } else {
    mig_net_count_age_sex <- mig_in
    mig_net_rate_age_sex <- ccmppWPP_inputs$mig_net_count_rate_sex
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

ccmppWPP_input_file_medium <- function(tfr_median_all_locs, # medium tfr from bayesian model 
                                       srb_median_all_locs, # projected srb
                                       e0_median_all_locs, # medium e0 from bayesian model 
                                       mig_net_count_proj_all_locs, # projected net migration by age and sex
  
                                       PasfrGlobalNorm, # pasfr global norm from pasfr_global_model() function
  
                                       input_file_path, # file path name for Excel input file
                                       ccmpp_estimates_130_folder) { # file path to folder where ccmpp intermediate outputs for ages 0 to 130 are stored
  
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
  
  locid <- meta.list$LocationID
  projection_years <- meta.list$Projection_First_Year:meta.list$Projection_Last_Year
  
  # load the intermediate outputs file for estimates that contains population by single year of age from 0 to 130+
  load(paste0(ccmpp_estimates_130_folder,locid,"_ccmpp_output.RData"))
  
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
  # get projected srb from input file for now (later let's read this from somewhere else that analysts can't accidentally edit)
  
  srb <-   srb_median_all_locs[srb_median_all_locs$LocID == locid & srb_median_all_locs$time_start %in% projection_years,]

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
                                  Smooth_Latest_Age_Mortality_Pattern = eval(parse(text = MORT_PARAMS$value[MORT_PARAMS$parameter=="Smooth_Latest_Age_Mortality_Pattern"])))
  
  
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
                           Smooth_Latest_Age_Mortality_Pattern = Age_Mort_Proj_arguments$Smooth_Latest_Age_Mortality_Pattern)
  
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
  
  lts_all_long <- rbind(mx_f, mx_m)
  lts_all_long$indicator <- "lt_nMx"
  lts_all_long$time_start <- as.numeric(lts_all_long$time_start)
  
  
  # net international migration inputs
  
  mig_net_count_age_sex  = mig_net_count_proj_all_locs[mig_net_count_proj_all_locs$LocID == locid &
                                                         mig_net_count_proj_all_locs$time_start %in% projection_years,
                                                        c("time_start","time_span","sex","age_start","age_span","value")]
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
  attr(inputs, "revision") <- attributes$revision
  attr(inputs, "locid") <- attributes$locid
  attr(inputs, "locname") <- attributes$locname
  attr(inputs, "variant") <- "medium"
  attr(inputs, "a0rule") <- attributes$a0rule
  
  return(inputs)
  
}


