
# this set of functions produces the inputs and outputs for the various deterministic projection variants projection variants
# requires the output of ccmppWPP_workflow_one_country_variant for the medium variant projection


#' Compute global normative model of proportion age-specific fertility rates
#'
#' @description This function calls bayesPop functions to return the proportional age-specific fertility rates associated with a global norm
#' based on historical patterns of TFR and PASFR
#'
#' @author Sara Hertog
#'
#' @param tfr_all_locs character. file path to RData file with annual TFR; countries in rows, years in columns
#' @param pasfr_all_locs character. file path to RData file with 1x1 PASFR; countries and ages from 10 to 54 in rows, years in columns
#' @param pasfr_pattern character. file path to RData file with 1 record per LocID; country_code = LocID, PASFRNorm = "Global Norm", 
#' PasfrGlobalNorm is c(0,1)flag indicating whether the countrys data should be used in computing global norm.
#' @param present_year numeric. Last year of observed data?
#'
#' @details calls embedded functions from bayesPop
#'
#' @return a matrix of annual PASFR norms; age from 10 to 54 in row, year in columns
#' @export
#' 

pasfr_global_model <- function(tfr_all_locs = "data/tfr_estimates_projections_for_pasfr_global_norm.RData",
                               pasfr_all_locs = "data/pasfr_estimates_for_pasfr_global_norm.RData",
                               pasfr_pattern = "data/PASFRpattern.RData",
                               present_year = 2019) {
  

  load(tfr_all_locs)   # tfr estimates and projections
  load(pasfr_all_locs)  # 1x1 pasfr estimates
  load(pasfr_pattern)  # flags which countries are included for global norm computation
  
  # reshape TFR estimates and projections to long data frame with year, country_code and TFR value
  TFRpred <- reshape(tfr_estimates_projections, direction = "long", 
                     varying = list(names(tfr_estimates_projections)[3:153]),
                     times = as.factor(names(tfr_estimates_projections)[3:153]),
                     timevar = "year",
                     v.names = "value")
  TFRpred <- TFRpred[,c("year", "country_code", "value")]
  
  # compile inputs to bayesPop:::compute.pasfr.global.norms function (this is "kant" from Hana's code)
  inputs <- list(end.year = 2020,
                      observed = list(PASFR = pasfr_estimates[, names(pasfr_estimates) != "name"]),
                      PASFR = NULL,
                      PASFRnorms = NULL,
                      PASFRpattern = PASFRpattern,
                      present.year = 2019,
                      TFRpred = TFRpred)
  pasfr_global_norm <- bayesPop:::compute.pasfr.global.norms(inputs = inputs)$PasfrGlobalNorm
  
  rownames(pasfr_global_norm) <- 10:54
   
  return(pasfr_global_norm)
  
}

#' Infer proportion age-specific fertility rates from tfr using a global normative model
#'
#' @description This function returns the proportional age-specific fertility rates given a projected level of
#' total fertility by converging the initial pasfr to a global model 
#'
#' @author Sara Hertog
#'
#' @param PasfrGlobalNorm matrix. output from pasfr_global_model() function
#' @param pasfr_observed matrix. with age in single years from 10 to 54 in rows and year in columns. Value is country estimates of pasfr
#' @param tfr_observed_projected numeric vector. TFR values for the country. Names are years.
#' @param years_projection numeric vector.  Years for which pasfr is to be projected
#' @param num_points numeric scalar. The number of past point estimates to be used to model pasfr
#'
#' @details accesses the global model in the data frame "pasfr_global_model"
#'
#' @return a data frame with the inferred pasfr in the "value" field.
#' @export
#' 
#' 
pasfr_given_tfr <- function(PasfrGlobalNorm, pasfr_observed, tfr_observed_projected, years_projection = 2020:2100, num_points = 15) {
  
  pasfr_global_norm <- list(PasfrGlobalNorm = PasfrGlobalNorm)
  
  # assemble the country-specific inputs required for the bayesPop:::kantorova.pasfr function
  inputs <- list(PASFRpattern = data.frame(PasfrNorm = "Global Norm"),
                         # observed pasfr can be overwritten here if different from the initial pasfr file
                         observed = list(PASFR = as.matrix(pasfr_observed)))
  
  # computation
  #############
  pasfr <- bayesPop:::kantorova.pasfr(tfr = tfr_observed_projected, 
                                      inputs = inputs,
                                      norms = pasfr_global_norm, 
                                      proj.years = years_projection, 
                                      tfr.med = tfr_observed_projected[length(tfr_observed_projected)], 
                                      annual = TRUE, 
                                      nr.est.points = num_points)
  
  # as percentage
  pasfr <- round(pasfr*100, 2)
  
  # set time as column names and age as row names
  colnames(pasfr) <- years_projection
  rownames(pasfr) <- rownames(pasfr_observed)

  return(pasfr)
  
}

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
#' 
#' 



project_variants_inputs <- function(medium_variant_tfr,
                                    medium_variant_e0,
                                    medium_variant_mig,
                                    past_pasfr,
                                    last_mx,
                                    projection_start_year = 2020,
                                    projection_end_year = 2100) {
  
  

    # 
    # data("wpp_tfr_med")
    # data("wpp_pasfr_med")
    # medium_variant_tfr <- wpp_tfr_med[wpp_tfr_med$LocID == 4,]
    # load("C:/Users/SARAH/OneDrive - United Nations/PEPS/Engine/testData/WPP19 1x1 inputs/Estimates 1950-2020/4_1950_2020.RData")
    # 
    # past_pasfr <- inputs$fert_rate_age_f %>% 
    #   group_by(time_start) %>% 
    #   mutate(value = value/sum(value)) %>% # convert asfr to pasfr
    #   ungroup() %>% 
    #   pivot_wider(names_from = "time_start", values_from = "value") %>% 
    #   filter(age_start %in% 10:54) %>% 
    #   select(-time_span, -age_span, -age_start) 
    # past_pasfr <- as.matrix(past_pasfr)
    # rownames(past_pasfr) <- 10:54
    # 
    
    
  # create vector of projection time_starts
  projection_times <- projection_start_year:projection_end_year
  
  ############################
  ############################
  # compute asfr inputs for medium variant
  tfr_med <- medium_variant_tfr$value[medium_variant_tfr$time_start %in% projection_times]
  names(tfr_med) <- projection_times
  
  # assemble the inputs
  country.inputs <- list(PASFRpattern = data.frame(PasfrNorm = "Global Norm"),
                         # observed pasfr can be overwritten here if different from the initial pasfr file
                         observed = list(PASFR=past_pasfr))
  
  # computation
  #############
  pasfr <- bayesPop:::kantorova.pasfr(tfr = tfr_med, 
                                      country.inputs, 
                                      norms = PASFRnorms, 
                                      proj.years = projection_times, 
                                      tfr.med = tfr_med[length(tfr_med)], 
                                      annual = TRUE, 
                                      nr.est.points = 15)
  
  # as percentage
  pasfr <- round(pasfr*100, 2)
  
  PASFRpattern <- data.frame(PasfrNorm = "Global Norm")
  
  
  ############################
  ############################
  # compute asfr inputs for low/high fertility variants
  
  
  #    for tfr subtract/add 0.25 children in first five years, 0.4 children in next five years, 0.5 children thereafter
  tfr_adj <- rep(0.5, length(projection_times))
  tfr_adj[which(projection_times >= proj_start & projection_times < proj_start+5)] <- 0.25
  tfr_adj[which(projection_times >= proj_start+5 & projection_times < proj_start+10)] <- 0.40
  
  #    compute asfr for low-fertility variant
  tfr_low             <- df$fert_rate_tot_f
  tfr_low$value       <- df$fert_rate_tot_f$value - tfr_adj
  fert_rate_age_f_low <- list()
  for (i in 1:length(projection_times)) {

    fert_pct_age_f_time              <- pasfr_given_tfr(pasfr_initial = df$fert_pct_age_f_1x1[df$fert_pct_age_f_1x1$time_start == proj_start,], 
                                                        tfr_initial   = df$fert_rate_tot_f$value[df$fert_rate_tot_f$time_start == proj_start], 
                                                        tfr_given     = tfr_low$value[i])
    fert_pct_age_f_time$time_start   <- projection_times[i]
    fert_pct_age_f_time$time_span    <- diff(projection_times)[1]
    fert_pct_age_f_time$age_start    <- df$fert_pct_age_f_1x1$age_start[df$fert_pct_age_f_1x1$time_start == proj_start]
    fert_pct_age_f_time$age_span     <- df$fert_pct_age_f_1x1$age_span[df$fert_pct_age_f_1x1$time_start == proj_start]
    fert_rate_age_f_time             <- fert_pct_age_f_time[,c("time_start", "time_span", "age_start", "age_span")]
    fert_rate_age_f_time$value       <- fert_pct_age_f_time$value * tfr_low$value[i]
    fert_rate_age_f_low[[i]]         <- fert_rate_age_f_time

    
  }
  fert_rate_age_f_low <- do.call(rbind, fert_rate_age_f_low)

 
  #    compute asfr for high-fertility variant
  tfr_high             <- df$fert_rate_tot_f
  tfr_high$value       <-  df$fert_rate_tot_f$value + tfr_adj
  fert_rate_age_f_high <- list()
  for (i in 1:length(projection_times)) {
    
    fert_pct_age_f_time               <- pasfr_given_tfr(pasfr_initial = df$fert_pct_age_f_1x1[df$fert_pct_age_f_1x1$time_start == proj_start,], 
                                                         tfr_initial   = df$fert_rate_tot_f$value[df$fert_rate_tot_f$time_start == proj_start], 
                                                         tfr_given     = tfr_high$value[i])
    fert_pct_age_f_time$time_start   <- projection_times[i]
    fert_pct_age_f_time$time_span    <- diff(projection_times)[1]
    fert_pct_age_f_time$age_start    <- df$fert_pct_age_f_1x1$age_start[df$fert_pct_age_f_1x1$time_start == proj_start]
    fert_pct_age_f_time$age_span     <- df$fert_pct_age_f_1x1$age_span[df$fert_pct_age_f_1x1$time_start == proj_start]
    fert_rate_age_f_time             <- fert_pct_age_f_time[,c("time_start", "time_span", "age_start", "age_span")]
    fert_rate_age_f_time$value       <- fert_pct_age_f_time$value * tfr_high$value[i]
    fert_rate_age_f_high[[i]]        <- fert_rate_age_f_time
    
    
  }
  fert_rate_age_f_high <- do.call(rbind, fert_rate_age_f_high)


  ############################
  ############################
  # extract asfr inputs for constant-fertility variant
  
  fert_rate_age_f_proj_start <- df$fert_pct_age_f_1x1[which(df$fert_pct_age_f_1x1$time_start == proj_start),]
  fert_rate_age_f_constant <- list()
  for (i in 1:length(projection_times)) {
    
    fert_rate_age_f_time            <- fert_rate_age_f_proj_start
    fert_rate_age_f_time$time_start <- projection_times[i]
    fert_rate_age_f_constant[[i]]   <- fert_rate_age_f_time
    
  }
  fert_rate_age_f_constant <- do.call(rbind, fert_rate_age_f_constant)

  
  ############################
  ############################
  #    compute asfr for instant replacement fertility (NRR = 1)
  
  fert_rate_age_f_instant <- list()
  for (i in 1:length(projection_times)) {
    
    srb_time              <- df$srb[df$srb$time_start == projection_times[i],]
    fert_rate_age_f_time  <- df$fert_rate_age_f_1x1[df$fert_rate_age_f_1x1$time_start == projection_times[i],]
    lx_f_time             <- df$lt_complete_age_sex[df$lt_complete_age_sex$time_start == projection_times[i] & 
                                                      df$lt_complete_age_sex$indicator == "lt_lx" & 
                                                      df$lt_complete_age_sex$sex == "female",]
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
  
  # take sex-specific life tables from first period of medium-variant projection
  lt_proj_start <- df$lt_complete_age_sex[df$lt_complete_age_sex$time_start == proj_start & 
                                            df$lt_complete_age_sex$sex %in% c("male","female"),]
  # initialize constant-mortality output
  lt_constant <- list()
  # repeat starting period sex-specific life table for each period in projection horizon
  for (i in 1:length(projection_times)) {
    
    lt_time            <- lt_proj_start
    lt_time$time_start <- projection_times[i]
    lt_constant[[i]]   <- lt_time
    
  }
  # assemble life table outputs into one data frame
  lt_complete_age_sex_constant <- do.call(rbind, lt_constant)

  ############################
  ############################
  #    compute asfr for momentum (constant mortality and NRR = 1)
  
  fert_rate_age_f_momentum <- list()
  for (i in 1:length(projection_times)) {
    
    srb_time              <- df$srb[df$srb$time_start == projection_times[i],]
    fert_rate_age_f_time  <- df$fert_rate_age_f_1x1[df$fert_rate_age_f_1x1$time_start == projection_times[i],]
    lx_f_time             <- lt_complete_age_sex_constant[lt_complete_age_sex_constant$time_start == projection_times[i] & 
                                                            lt_complete_age_sex_constant$indicator=="lt_lx" & 
                                                            lt_complete_age_sex_constant$sex=="female",]
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
  
  variant_inputs <- list(fert_rate_age_f_low          = fert_rate_age_f_low,
                         fert_rate_age_f_high         = fert_rate_age_f_high,
                         fert_rate_age_f_constant     = fert_rate_age_f_constant,
                         fert_rate_age_f_instant      = fert_rate_age_f_instant,
                         fert_rate_age_f_momentum     = fert_rate_age_f_momentum,
                         lt_complete_age_sex_constant = lt_complete_age_sex_constant)
  
  return(variant_inputs)
}


project_variants_deterministic <- function(medium_variant_outputs, variant_inputs) {
  
  df <- medium_variant_outputs
  proj_start <- df$pop_count_tot_sex$time_start[1]
  
  # assemble a temporary input file corresponding to medium variant from the medium variant projection
  # outputs.  We do this from outputs rather than from the medium variant input files directly to be sure
  # we use the migration counts that may have been overridden to prevent negative population values
  inputs_med <- list()
  
  # inherit attributes (less names) from medium variant outputs
  atr <- attributes(medium_variant_outputs)
  atr <- atr[names(atr) %in% "names" == FALSE]  
  attributes(inputs_med) <- atr

  inputs_med$pop_count_age_sex_base <- df$pop_count_age_sex_1x1[df$pop_count_age_sex_1x1$time_start==2020 &
                                                                  df$pop_count_age_sex_1x1$sex %in% c("male", "female"),]
  inputs_med$life_table_age_sex     <- df$lt_complete_age_sex[df$lt_complete_age_sex$sex %in% c("male", "female"),]
  inputs_med$fert_rate_age_f        <- df$fert_rate_age_f_1x1
  inputs_med$srb                    <- df$srb
  inputs_med$mig_net_count_age_sex  <- df$mig_net_count_age_sex_1x1[df$mig_net_count_age_sex_1x1$sex %in% c("male", "female"),]
  inputs_med$mig_net_rate_age_sex   <- inputs_med$mig_net_count_age_sex
  inputs_med$mig_net_rate_age_sex$value <- 0 # only use counts for deterministic projection scenarios
  inputs_med$mig_net_count_tot_b    <- df$mig_net_count_tot_sex[df$mig_net_count_tot_sex$sex == "both",]
  inputs_med$mig_parameter          <- df$mig_parameter
  inputs_med$mig_parameter$value[which(inputs_med$mig_parameter$indicator=="mig_type")] <- "counts"

  variant_inputs <- variant_inputs 
  
  # initialize list of outputs from all projection variants
  proj_variants_outputs <- list()
  attr(proj_variants_outputs, "revision") <- atr[names(atr) == "revision"]  
  attr(proj_variants_outputs, "locid") <- atr[names(atr) == "locid"]  
  
  # low-fertility variant
  inputs_low_fert <- inputs_med
  inputs_low_fert$fert_rate_age_f <- variant_inputs$fert_rate_age_f_low # low variant asfrs
  attr(inputs_low_fert, "variant") <- "low fertility"
  proj_variants_outputs$outputs_low_fert <- ccmppWPP_workflow_one_country_variant(inputs_low_fert)
  
  # high-fertility variant
  inputs_high_fert <- inputs_med
  inputs_high_fert$fert_rate_age_f <- variant_inputs$fert_rate_age_f_high # high variant asfrs
  attr(inputs_high_fert, "variant") <- "high fertility"
  proj_variants_outputs$outputs_high_fert <- ccmppWPP_workflow_one_country_variant(inputs_high_fert)
  
  # constant-fertility variant
  inputs_constant_fert <- inputs_med
  inputs_constant_fert$fert_rate_age_f <- variant_inputs$fert_rate_age_f_constant # constant asfrs
  attr(inputs_constant_fert, "variant") <- "constant fertility"
  proj_variants_outputs$outputs_constant_fert <- ccmppWPP_workflow_one_country_variant(inputs_constant_fert)
  
  # instant replacement fertility variant
  inputs_instant_fert <- inputs_med
  inputs_instant_fert$fert_rate_age_f <- variant_inputs$fert_rate_age_f_instant # instant replacement asfrs
  attr(inputs_instant_fert, "variant") <- "instant replacement fertility"
  proj_variants_outputs$outputs_instant_fert <- ccmppWPP_workflow_one_country_variant(inputs_instant_fert)
  
  # constant mortality variant
  inputs_constant_mort <- inputs_med
  inputs_constant_mort$life_table_age_sex <- variant_inputs$lt_complete_age_sex_constant # constant life tables
  attr(inputs_constant_mort, "variant") <- "constant mortality"
  proj_variants_outputs$outputs_constant_mort <- ccmppWPP_workflow_one_country_variant(inputs_constant_mort)
  
  # constant fertility and constant mortality
  inputs_constant_fert_mort <- inputs_constant_fert # inputs with constant asfrs
  inputs_constant_fert_mort$life_table_age_sex <- variant_inputs$lt_complete_age_sex_constant # constant life tables
  attr(inputs_constant_fert_mort, "variant") <- "constant fertility and mortality"
  proj_variants_outputs$outputs_constant_fert_mort <- ccmppWPP_workflow_one_country_variant(inputs_constant_fert_mort)
  
  # zero-migration variant
  inputs_zero_mig <- inputs_med
  inputs_zero_mig$mig_net_count_age_sex$value <- 0 # zero net migration at all ages
  attr(inputs_zero_mig, "variant") <- "zero migration"
  proj_variants_outputs$outputs_zero_mig <- ccmppWPP_workflow_one_country_variant(inputs_zero_mig)
  
  # instant replacement and zero-migration variant
  inputs_instant_zero_mig <- inputs_instant_fert # instant replacement variant asfrs
  inputs_instant_zero_mig$mig_net_count_age_sex$value <- 0 # zero net migration at all ages
  attr(inputs_instant_zero_mig, "variant") <- "instant replacement and zero migration"
  proj_variants_outputs$outputs_instant_zero_mig <- ccmppWPP_workflow_one_country_variant(inputs_instant_zero_mig)
  
  # momentum
  inputs_momentum <- inputs_constant_mort # inputs with constant life tables
  inputs_momentum$fert_rate_age_f <- variant_inputs$fert_rate_age_f_momentum # momentum variant asfrs
  inputs_momentum$mig_net_count_age_sex$value <- 0 # zero net migration at all ages
  proj_variants_outputs$outputs_momentum <- ccmppWPP_workflow_one_country_variant(inputs_momentum)
  
return(proj_variants_outputs)
  
}




