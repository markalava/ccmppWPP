
# this set of functions produces the inputs and outputs for the various deterministic projection variants projection variants
# requires the output of ccmppWPP_workflow_one_country_variant for the medium variant projection


#' Infer proportion age-specific fertility rates from tfr using a global normative model
#'
#' @description This function returns the proportional age-specific fertility rates given a projected level of
#' total fertility by converging the initial pasfr to a global model 
#'
#' @author Sara Hertog
#'
#' @param pasfr_initial data frame. with age_start and age_span indicating age groups and initial pasfr in the value column
#' @param tfr_initial numeric. the initial level of total fertility
#' @param tfr_given numeric.  the projected level of total fertility for which we want to infer a pasfr
#'
#' @details accesses the global model in the data frame "pasfr_global_model"
#'
#' @return a data frame with the inferred pasfr in the "value" field.
#' @export
#' 
pasfr_given_tfr <- function(pasfr_initial, tfr_initial, tfr_given) {
  
  ####### we need to swap in Hana's implementation here

  # for now we do a proportional adjustment to pasfr based on difference between original and adjusted
  pasfr  <- pasfr_initial * (tfr_given/tfr_initial)
  
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
project_variants_inputs <- function(medium_variant_outputs) {
  
  df <- medium_variant_outputs
  
  # create vector of projection time_starts
  projection_times <- df$fert_rate_tot_f$time_start
  proj_start <- projection_times[1]

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




