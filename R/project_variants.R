
# this set of functions produces the inputs and outputs for the various deterministic projection variants projection variants
# requires the output of ccmppWPP_workflow_one_country_variant for the medium variant projection

# In what year has/will each location enter phase 3 of the fertility transition?
tfr_phase3_id <- function(tfr_all_locs, present_year = 2021) {
  
  years <- unique(tfr_all_locs$time_start)
  
  # reshape tfr data to a matrix with year in row and country in columns
  tfr_matrix <- reshape(tfr_all_locs[,c("LocID", "time_start", "value")], direction = "wide", idvar = "time_start", timevar = "LocID")
  tfr_matrix <- as.matrix(tfr_matrix[,2:ncol(tfr_matrix)])
  rownames(tfr_matrix) <- years
  colnames(tfr_matrix) <- unique(tfr_all_locs$LocID)
  
  # initialize output data frame
  phase3 <- data.frame(LocID = colnames(tfr_matrix),
                       year = rep(NA, ncol(tfr_matrix)))
  # for each country, determine whether the year entering phase III is less than or equal to present year
  for (i in 1:ncol(tfr_matrix)) {
    year_ind_phase3 <- bayesTFR:::find.lambda.for.one.country(tfr = tfr_matrix[,i], T_end = length(years), annual = TRUE) 
    phase3$year[i] <- years[year_ind_phase3]
  }
  phase3 <- phase3[order(phase3$LocID),]
  
  return(phase3)
  
}

# assemble the PASFRpattern data frame that bayesPop needs to compute global normative model of pasfr

pasfr_global_norm_include <- function(phase3,
                                      mac,
                                      present_year= 2021,
                                      mac_criterion = 30, 
                                      exclude_small = TRUE) {

  
  phase3_include_ids <- phase3$LocID[phase3$year <= present_year]
  mac_include_ids <- mac$LocID[mac$time_start == present_year & mac$value >= mac_criterion]

  # create the PASFR pattern dataset required as input to the bayesPop:::compute.pasfr.global.norms function
  PASFRpattern <- data.frame(country_code = phase3$LocID,
                             PasfrNorm = as.factor("Global Norm"))
  PASFRpattern$PasfrGlobalNorm <- ifelse(PASFRpattern$country_code %in% phase3_include_ids &
                                           PASFRpattern$country_code %in% mac_include_ids, 1, 0)
  
  if (exclude_small) {
    small_locs <- c(16,20,660,60,535,92,136,184,212,238,234,292,304,336,833,438,584,492,500,520,570,
                    580,585,652,654,659,663,666,674,534,772,796,798,876)
    PASFRpattern$PasfrGlobalNorm[as.numeric(PASFRpattern$country_code) %in% small_locs] <- 0
  }
  
  return(PASFRpattern)
  
}

#' Compute global normative model of proportion age-specific fertility rates
#'
#' @description This function calls bayesPop functions to return the proportional age-specific fertility rates associated with a global norm
#' based on historical patterns of TFR and PASFR
#'
#' @author Sara Hertog
#'
#' @param tfr_all_locs data.frame. file path to RData file with annual TFR; long format with LocID, time_start and value
#' @param pasfr_all_locs data.frame. file path to RData file with 1x1 PASFR; long format with LocID, time_start and value, age_start from 10 to 54
#' @param present_year numeric. Last year of observed data?
#' @param mac_criterion numeric. Lower bound threshold for mean age at childbearing to determine includision in computing global norm
#'
#' @details calls embedded functions from bayesPop
#'
#' @return a matrix of annual PASFR norms; age from 10 to 54 in row, year in columns
#' @export
#' 

pasfr_global_model <- function(tfr_all_locs, # tfr estimates and projections
                               pasfr_all_locs, # 1x1 pasfr estimates
                               PASFRpattern, # output of pasfr_global_norm_include()
                               present_year = 2021,
                               mac_criterion = 30) {

  # TFR estimates and projections as a long data frame with year, country_code and TFR value
  TFRpred <- as.data.frame(tfr_all_locs[,c("time_start", "LocID", "value")])
  names(TFRpred) <- c("year", "country_code", "value") # this is required by bayesPop
  
  # reshape pasfr estimates to the wide matrix form required by bayesPop
  pasfr_est <- pasfr_all_locs[pasfr_all_locs$age_start %in% 10:54, ] 
  pasfr_est <- as.data.frame(pasfr_est[order(pasfr_est$LocID, pasfr_est$time_start, pasfr_est$age_start),])

  pasfr_estimates <- reshape(pasfr_est[,c("LocID","time_start","age_start","value")], 
                             direction = "wide", idvar = c("LocID", "age_start"), timevar = "time_start" )
  names(pasfr_estimates) <- c("country_code","age",unique(pasfr_est$time_start))
  pasfr_estimates$country_code <- as.integer(pasfr_estimates$country_code)
  
  # compile inputs to bayesPop:::compute.pasfr.global.norms function (this is "kant" from Hana's code)
  inputs <- list(end.year = present_year + 1,
                      observed = list(PASFR = pasfr_estimates),
                      PASFR = NULL,
                      PASFRnorms = NULL,
                      PASFRpattern = PASFRpattern,
                      present.year = present_year,
                      TFRpred = TFRpred,
                      annual = TRUE)
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
#' @return a matrix of pasfr with age in rows and years in columns
#' @export
#' 
#' 
pasfr_given_tfr <- function(PasfrGlobalNorm, pasfr_observed, tfr_observed_projected, years_projection = 2022:2100, num_points = 15) {
  
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
  pasfr <- pasfr*100
  
  # set time as column names and age as row names
  colnames(pasfr) <- years_projection
  rownames(pasfr) <- rownames(pasfr_observed)

  return(pasfr)
  
}


mx_given_e0 <- function(mx_mat_m, # matrix of mx estimates (age in rows, years in columns)
                        mx_mat_f,
                        e0m, # vector of projected e0 (named with years)
                        e0f,
                        Age_Mort_Proj_Method1 = c("lc","pmd","mlt"),
                        Age_Mort_Proj_Method2 = "mlt",  # only used if first method is "pmd"
                        Age_Mort_Proj_Pattern = c("CD_West", "CD_North", "CD_South", "CD_East", 
                                        "UN_Far_Eastern", "UN_South_Asian", "UN_General"),
                        Age_Mort_Proj_Method_Weights = c(1,0.5),
                        Age_Mort_Proj_Adj_SR = TRUE,
                        Latest_Age_Mortality_Pattern = FALSE,
                        Smooth_Latest_Age_Mortality_Pattern = FALSE) {
  
  methods.allowed <- list(lc = "MortCast::mortcast", mlt = "MortCast::mltj", pmd = "MortCast::copmd", blend = "MortCast::mortcast.blend")
  
  method <- ifelse(Age_Mort_Proj_Method1 == "pmd" & (!is.na(Age_Mort_Proj_Method2) & Age_Mort_Proj_Method2 == "mlt"), "blend", Age_Mort_Proj_Method1)
  
  # Lee - Carter
  if (method == "lc") {
    
    # define whether using only latest mortality pattern and whether to smooth it
    if (Latest_Age_Mortality_Pattern == TRUE) {
      
      ax.index  <- ncol(mx_mat_f) # use only the last column in estimating lc parameters
      ax.smooth <- Smooth_Latest_Age_Mortality_Pattern # logical whether to smooth latest age pattern
      
    }  else {
      ax.index <- NULL
      ax.smooth <- FALSE
    }                        
    
    # estimate lee-carter parameters using the lileecarter.estimate() function from MortCast
    lee_carter_parameters <- MortCast:::lileecarter.estimate(mxM = mx_mat_m, 
                                                             mxF = mx_mat_f,
                                                             ax.index = ax.index, # which columns of mx matrices to use in estimation (default is all of them)
                                                             ax.smooth = ax.smooth) # smooth.spline(ax, df = ceiling(length(ax)/2))$y  ## NEED TO CONSIDER WHETHER THIS IS APPROPRIATE LEVEL OF SMOOTHING FOR BOTH n=1 AND n=5
    
    method.args <- list(e0m,
                        e0f,
                        lc.pars = lee_carter_parameters,
                        rotate = TRUE,
                        keep.lt = FALSE,
                        constrain.all.ages = FALSE)
  
    
  # Model life table   
  } else if (method == "mlt") {
    
    method.args <- list(e0m,
                        e0f,
                        type = Age_Mort_Proj_Pattern,
                        nx = 1)
    
  # Proportionate mortality decline
  } else if (method == "pmd") {
    
    method.args <- list(e0m,
                        e0f,
                        mxm0 = mx_mat_m[0:101,ncol(mx_mat_m)], # pmd only accepts age groups <= 100
                        mxf0 = mx_mat_f[0:101,ncol(mx_mat_f)],
                        nx = 1,
                        keep.lt = FALSE,
                        adjust.sr.if.needed = Age_Mort_Proj_Adj_SR) # This is #3 in bayesPop, performed dynamically using sex ratio in previous time point

  # Blend of PMD with MLT
  } else if (method == "blend") {
    
    method.args <- list(e0m,
                        e0f,
                        meth1 = "pmd",
                        meth2 = "mlt",
                        weights = Age_Mort_Proj_Method_Weights,
                        nx = 1,
                        apply.kannisto = TRUE,
                        min.age.groups = 131, # triggers kannisto extension if last age group is less than 100+
                        match.e0 = TRUE,
                        meth1.args = list(mxm0 = mx_mat_m[0:101,ncol(mx_mat_m)], mxf0 = mx_mat_f[0:101,ncol(mx_mat_f)], nx=1, keep.lt = FALSE, adjust.sr.if.needed = Age_Mort_Proj_Adj_SR),
                        meth2.args = list(type = Age_Mort_Proj_Pattern, nx = 1),
                        kannisto.args = list(est.ages = seq(80, 95, by=1), proj.ages = seq(100,130, by=1)))
  }
  
  # write a small function that allows us to call functions exported from packages
  getfun<-function(x) {
    if(length(grep("::", x))>0) {
      parts<-strsplit(x, "::")[[1]]
      getExportedValue(parts[1], parts[2])
    } else {
      x
    }
  }
  mx_proj <- do.call(getfun(methods.allowed[[method]]), method.args)
  
  if (method == "blend") {
    mx_proj <- mx_proj[1:2]  # retain only the mx matrices from the blended results
  }
  
  return(mx_proj) # list of two sex-specific mx matrices
  
}

# testlc1 <- mx_given_e0(mx_mat_m = mxM_mat, # matrix of mx estimates (age in rows, years in columns)
#                        mx_mat_f = mxF_mat,
#                        e0m = e0M, # vector of projected e0 (named with years)
#                        e0f = e0F,
#                        Age_Mort_Proj_Method1 = "lc",
#                        Age_Mort_Proj_Method2 = NA,  # only used if first method is "pmd"
#                        Age_Mort_Proj_Pattern = NULL,
#                        Age_Mort_Proj_Method_Weights = c(1,0.5),
#                        Age_Mort_Proj_Adj_SR = TRUE,
#                        Latest_Age_Mortality_Pattern = FALSE,
#                        Smooth_Latest_Age_Mortality_Pattern = FALSE)

# testlc2 <- mx_given_e0(mx_mat_m = mxM_mat, # matrix of mx estimates (age in rows, years in columns)
#                        mx_mat_f = mxF_mat,
#                        e0m = e0M, # vector of projected e0 (named with years)
#                        e0f = e0F,
#                        Age_Mort_Proj_Method1 = "lc",
#                        Age_Mort_Proj_Method2 = NA,  # only used if first method is "pmd"
#                        Age_Mort_Proj_Pattern = NULL,
#                        Age_Mort_Proj_Method_Weights = c(1,0.5),
#                        Age_Mort_Proj_Adj_SR = TRUE,
#                        Latest_Age_Mortality_Pattern = TRUE,
#                        Smooth_Latest_Age_Mortality_Pattern = FALSE)

# testlc3 <- mx_given_e0(mx_mat_m = mxM_mat, # matrix of mx estimates (age in rows, years in columns)
#                        mx_mat_f = mxF_mat,
#                        e0m = e0M, # vector of projected e0 (named with years)
#                        e0f = e0F,
#                        Age_Mort_Proj_Method1 = "lc",
#                        Age_Mort_Proj_Method2 = NA,  # only used if first method is "pmd"
#                        Age_Mort_Proj_Pattern = NULL,
#                        Age_Mort_Proj_Method_Weights = c(1,0.5),
#                        Age_Mort_Proj_Adj_SR = TRUE,
#                        Latest_Age_Mortality_Pattern = TRUE,
#                        Smooth_Latest_Age_Mortality_Pattern = TRUE)

# testmlt1 <- mx_given_e0(mx_mat_m = mxM_mat, # matrix of mx estimates (age in rows, years in columns)
#                         mx_mat_f = mxF_mat,
#                         e0m = e0M, # vector of projected e0 (named with years)
#                         e0f = e0F,
#                         Age_Mort_Proj_Method1 = "mlt",
#                         Age_Mort_Proj_Method2 = NA,  # only used if first method is "pmd"
#                         Age_Mort_Proj_Pattern = "CD_North",
#                         Age_Mort_Proj_Method_Weights = c(1,0.5),
#                         Age_Mort_Proj_Adj_SR = NA,
#                         Latest_Age_Mortality_Pattern = NA,
#                         Smooth_Latest_Age_Mortality_Pattern = NA)
# 
# testmlt2 <- mx_given_e0(mx_mat_m = mxM_mat, # matrix of mx estimates (age in rows, years in columns)
#                         mx_mat_f = mxF_mat,
#                         e0m = e0M, # vector of projected e0 (named with years)
#                         e0f = e0F,
#                         Age_Mort_Proj_Method1 = "mlt",
#                         Age_Mort_Proj_Method2 = NA,  # only used if first method is "pmd"
#                         Age_Mort_Proj_Pattern = "CD_West",
#                         Age_Mort_Proj_Method_Weights = c(1,0.5),
#                         Age_Mort_Proj_Adj_SR = NA,
#                         Latest_Age_Mortality_Pattern = NA,
#                         Smooth_Latest_Age_Mortality_Pattern = NA)
# 
# testpmd1 <- mx_given_e0(mx_mat_m = mxM_mat, # matrix of mx estimates (age in rows, years in columns)
#                         mx_mat_f = mxF_mat,
#                         e0m = e0M, # vector of projected e0 (named with years)
#                         e0f = e0F,
#                         Age_Mort_Proj_Method1 = "pmd",
#                         Age_Mort_Proj_Method2 = NA,  # only used if first method is "pmd"
#                         Age_Mort_Proj_Pattern = NA,
#                         Age_Mort_Proj_Method_Weights = NA,
#                         Age_Mort_Proj_Adj_SR = TRUE,
#                         Latest_Age_Mortality_Pattern = NA,
#                         Smooth_Latest_Age_Mortality_Pattern = NA)
# 
# testpmd2 <- mx_given_e0(mx_mat_m = mxM_mat, # matrix of mx estimates (age in rows, years in columns)
#                         mx_mat_f = mxF_mat,
#                         e0m = e0M, # vector of projected e0 (named with years)
#                         e0f = e0F,
#                         Age_Mort_Proj_Method1 = "pmd",
#                         Age_Mort_Proj_Method2 = NA,  # only used if first method is "pmd"
#                         Age_Mort_Proj_Pattern = NA,
#                         Age_Mort_Proj_Method_Weights = NA,
#                         Age_Mort_Proj_Adj_SR = FALSE,
#                         Latest_Age_Mortality_Pattern = NA,
#                         Smooth_Latest_Age_Mortality_Pattern = NA)

# testbld1 <- mx_given_e0(mx_mat_m = mxM_mat, # matrix of mx estimates (age in rows, years in columns)
#                         mx_mat_f = mxF_mat,
#                         e0m = e0M, # vector of projected e0 (named with years)
#                         e0f = e0F,
#                         Age_Mort_Proj_Method1 = "pmd",
#                         Age_Mort_Proj_Method2 = "mlt",  # only used if first method is "pmd"
#                         Age_Mort_Proj_Pattern = "CD_West",
#                         Age_Mort_Proj_Method_Weights = c(1,0.5),
#                         Age_Mort_Proj_Adj_SR = TRUE,
#                         Latest_Age_Mortality_Pattern = NA,
#                         Smooth_Latest_Age_Mortality_Pattern = NA)


mx_given_e0_new <- function(mx_mat_m, # matrix of mx estimates (age in rows, years in columns)
                        mx_mat_f,
                        e0m, # vector of projected e0 (named with years)
                        e0f,
                        Age_Mort_Proj_Method1 = c("lc","pmd","mlt"),
                        Age_Mort_Proj_Method2 = "mlt",  # only used if first method is "pmd"
                        Age_Mort_Proj_Pattern = c("CD_West", "CD_North", "CD_South", "CD_East", 
                                                  "UN_Far_Eastern", "UN_South_Asian", "UN_General"),
                        Age_Mort_Proj_Method_Weights = c(1,0.5),
                        Age_Mort_Proj_Adj_SR = TRUE,
                        Latest_Age_Mortality_Pattern = FALSE,
                        Latest_Age_Mortality_Pattern_Years = NULL,
                        Smooth_Latest_Age_Mortality_Pattern = FALSE,
                        Smooth_Latest_Age_Mortality_Pattern_Degree = NULL) {
  
  method <- ifelse(Age_Mort_Proj_Method1 == "pmd" & (!is.na(Age_Mort_Proj_Method2) & Age_Mort_Proj_Method2 == "mlt"), "blend", Age_Mort_Proj_Method1)
  
  # Lee - Carter
  if (method == "lc") {
    
    # define whether using only latest mortality pattern and whether to smooth it
    if (Latest_Age_Mortality_Pattern) {
      
      if (!is.null(Latest_Age_Mortality_Pattern_Years)) {
        ax.index <- 1:ncol(mx_mat_f)
        ax.index <- ax.index[colnames(mx_mat_f) %in% Latest_Age_Mortality_Pattern_Years]
        
      } else {
        ax.index <- ncol(mx_mat_f) # if no years specified, use the last one only
      }
      
      ax.smooth <- Smooth_Latest_Age_Mortality_Pattern # logical whether to smooth latest age pattern
      ax.smooth.df <- Smooth_Latest_Age_Mortality_Pattern_Degree
      
    }  else {
      ax.index <- NULL
      ax.smooth <- FALSE
      ax.smooth.df <- NULL
    }                        
    
    # estimate lee-carter parameters using the lileecarter.estimate() function from MortCast
    lc.pars <- MortCast:::lileecarter.estimate(mxM = mx_mat_m, 
                                               mxF = mx_mat_f,
                                               ax.index = ax.index, # which columns of mx matrices to use in estimation (default is all of them)
                                               ax.smooth = ax.smooth, # smooth.spline(ax, df = ceiling(length(ax)/2))  ## NEED TO CONSIDER WHETHER THIS IS APPROPRIATE LEVEL OF SMOOTHING FOR BOTH n=1 AND n=5
                                               ax.smooth.df = ax.smooth.df,
                                               nx = 1)
    
    mx_proj <- MortCast::mortcast(e0m, e0f, lc.pars, rotate = TRUE, keep.lt = FALSE, constrain.all.ages = FALSE)
    
    
    # Model life table   
  } else if (method == "mlt") {
    
    mx_proj <- MortCast::mltj(e0m, e0f, type = Age_Mort_Proj_Pattern, nx = 1)
    
    # Proportionate mortality decline
  } else if (method == "pmd") {
    
    mx_proj <- MortCast::copmd(e0m, e0f, 
                               mxm0 = mx_mat_m[0:101,ncol(mx_mat_m)], # pmd only accepts age groups <= 100
                               mxf0 = mx_mat_f[0:101,ncol(mx_mat_f)],
                               nx = 1,
                               keep.lt = FALSE,
                               adjust.sr.if.needed = Age_Mort_Proj_Adj_SR) # This is #3 in bayesPop, performed dynamically using sex ratio in previous time point
    
    # Blend of PMD with MLT
  } else if (method == "blend") {
    
    mx_proj <- MortCast::mortcast.blend(e0m,
                                        e0f,
                                        meth1 = "pmd",
                                        meth2 = "mlt",
                                        weights = Age_Mort_Proj_Method_Weights,
                                        nx = 1,
                                        apply.kannisto = TRUE,
                                        min.age.groups = 131, # triggers kannisto extension if last age group is less than 130+
                                        match.e0 = TRUE,
                                        meth1.args = list(mxm0 = mx_mat_m[0:101,ncol(mx_mat_m)], mxf0 = mx_mat_f[0:101,ncol(mx_mat_f)], nx=1, keep.lt = FALSE, adjust.sr.if.needed = Age_Mort_Proj_Adj_SR),
                                        meth2.args = list(type = Age_Mort_Proj_Pattern, nx = 1),
                                        kannisto.args = list(est.ages = seq(80, 95, by=1), proj.ages = seq(100,130, by=1)))
    
    mx_proj <- mx_proj[1:2]  # retain only the mx matrices from the blended results
    
  }
  
  return(mx_proj) # list of two sex-specific mx matrices
  
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


# # PasfrGlobalNorm <- pasfr_global_model()
# # 
# locid <- 840
# 
# data("tfr_estimates_projections_for_pasfr_global_norm")
# tfr_df <- tfr_estimates_projections[tfr_estimates_projections$country_code == locid,c(3:ncol(tfr_estimates_projections))]
# tfr <- as.vector(t(tfr_df))
# names(tfr) <- names(tfr_df)
# # 
# # 
# data("pasfr_estimates_for_pasfr_global_norm")
# pasfr_estimates_df <-  pasfr_estimates[pasfr_estimates$country_code == locid,]
# pasfr_estimates_mat <- as.matrix(pasfr_estimates_df[,c(4:ncol(pasfr_estimates_df))])
# rownames(pasfr_estimates_mat) <-pasfr_estimates_df$age
# colnames(pasfr_estimates_mat) <- names(pasfr_estimates_df)[4:ncol(pasfr_estimates_df)]

## TEST DATA FOR PROJECTING AGE PATTERNS OF MORTALITY

# # we need the mx from the estimates input file
# load("C:/Users/SARAH/OneDrive - United Nations/PEPS/Engine/testData/WPP19 1x1 inputs/Estimates 1950-2020/840_1950_2020.RData")
# inputs_estimates <- inputs
# 
# life_table_age_sex_mx <- inputs_estimates$life_table_age_sex[inputs_estimates$life_table_age_sex$indicator=="lt_nMx", ]

# # and we need the sex-specific e0 projections from the bayesian model (temporarily taken from medium variant estimate test files)
# load("C:/Users/SARAH/OneDrive - United Nations/PEPS/Engine/testData/WPP19 1x1 inputs/Medium variant 2020-2100/840_2020_2100.RData")
# inputs_medium <- inputs
# e0m <- inputs_medium$life_table_age_sex$value[inputs_medium$life_table_age_sex$indicator=="lt_ex" &
#                                                 inputs_medium$life_table_age_sex$age_start ==0 &
#                                                 inputs_medium$life_table_age_sex$sex == "male"]
# names(e0m) <- unique(inputs_medium$life_table_age_sex$time_start)
# 
# e0f <- inputs_medium$life_table_age_sex$value[inputs_medium$life_table_age_sex$indicator=="lt_ex" &
#                                                 inputs_medium$life_table_age_sex$age_start ==0 &
#                                                 inputs_medium$life_table_age_sex$sex == "female"]
# names(e0f) <- unique(inputs_medium$life_table_age_sex$time_start)
# 
# Age_Mort_Proj_Method1 <- "lc"
# Age_Mort_Proj_Method2 <- NA
# Age_Mort_Proj_Pattern <- NA
# Age_Mort_Proj_Method_Weights <- NA
# Age_Mort_Proj_Adj_SR <- NA
# Latest_Age_Mortality_Pattern <- FALSE
# Smooth_Latest_Age_Mortality_Pattern <- FALSE
# 
# Age_Mort_Proj_arguments <- list(Age_Mort_Proj_Method1 = Age_Mort_Proj_Method1,
#                                 Age_Mort_Proj_Method2 = Age_Mort_Proj_Method2,
#                                 Age_Mort_Proj_Pattern = Age_Mort_Proj_Pattern,
#                                 Age_Mort_Proj_Method_Weights = Age_Mort_Proj_Method_Weights,
#                                 Age_Mort_Proj_Adj_SR = Age_Mort_Proj_Adj_SR,
#                                 Latest_Age_Mortality_Pattern = Latest_Age_Mortality_Pattern,
#                                 Smooth_Latest_Age_Mortality_Pattern = Latest_Age_Mortality_Pattern)
# 
# ccmppWPP_estimates <- wpp_output_example

project_variants_inputs_deterministic <- function(ccmppWPP_estimates,
                                                  ccmppWPP_medium,
                                                  PasfrGlobalNorm) {
  
  
  # create vector of projection time_starts
  projection_times <- unique(ccmppWPP_medium$fert_rate_tot$time_start)
  projection_start_year <- projection_times[1]
  
  # extract pasfr estimates
  pasfr_df <- ccmppWPP_estimates$fert_pct_age_1x1[ccmppWPP_estimates$fert_pct_age_1x1$age_start %in% c(10:54),
                                                  c("time_start", "age_start", "value")]
  pasfr_estimates <- reshape(pasfr_df, idvar = "age_start", timevar = "time_start", direction = "wide")
  pasfr_estimates <- as.matrix(pasfr_estimates[,c(2:ncol(pasfr_estimates))])
  rownames(pasfr_estimates) <- unique(pasfr_df$age_start)
  colnames(pasfr_estimates) <- unique(pasfr_df$time_start)
  
  # extract tfr estimates
  tfr_est <- ccmppWPP_estimates$fert_rate_tot$value
  names(tfr_est) <- colnames(pasfr_estimates)
  
  # fill-in zeros for ages < 10 and > 54
  asfr_0_9 <- matrix(0.0, nrow=length(0:9), ncol = length(projection_times), dimnames = list(0:9, projection_times))
  asfr_55_100 <- matrix(0.0, nrow=length(55:100), length(projection_times), dimnames = list(55:100, projection_times))
  
  ############################
  ############################
  # compute asfr inputs for low/high fertility variants
  
  # extract medium variant tfr
  tfr_med <- ccmppWPP_medium$fert_rate_tot$value
  names(tfr_med) <- projection_times
  
  #    for tfr adjustment, subtract/add 0.25 children in first five years, 0.4 children in next five years, 0.5 children thereafter
  tfr_adj <- rep(0.5, length(projection_times))
  tfr_adj[which(projection_times >= projection_start_year & projection_times < projection_start_year+5)] <- 0.25
  tfr_adj[which(projection_times >= projection_start_year+5 & projection_times < projection_start_year+10)] <- 0.40
  
  #    compute asfr for low-fertility variant
  tfr_low <- tfr_med - tfr_adj
  
  pasfr_low <- pasfr_given_tfr(PasfrGlobalNorm = PasfrGlobalNorm,
                               pasfr_observed = pasfr_estimates,
                               tfr_observed_projected = c(tfr_est, tfr_low),
                               years_projection = projection_times,
                               num_points = 15)
  
  asfr_low <- t(tfr_low * t(pasfr_low/100))
  asfr_low <- rbind(asfr_0_9,  asfr_low, asfr_55_100)
  
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
  fert_rate_age_f_low$age_span <- ifelse(fert_rate_age_f_low$age_start < 100, 1, 1000)
  fert_rate_age_f_low$time_span <- 1
  

   #    compute asfr for high-fertility variant
  tfr_high <- tfr_med + tfr_adj
  
  pasfr_high <- pasfr_given_tfr(PasfrGlobalNorm = PasfrGlobalNorm,
                                pasfr_observed = pasfr_estimates,
                                tfr_observed_projected = c(tfr_est, tfr_high),
                                years_projection = projection_times,
                                num_points = 15)
  
  asfr_high <- t(tfr_high * t(pasfr_high/100))
  asfr_high <- rbind(asfr_0_9,  asfr_high, asfr_55_100)
  
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
  
  ############################
  ############################
  # extract asfr inputs for constant-fertility variant (constant at last estimated)
  
  asfr_last_observed <- ccmppWPP_estimates$fert_rate_age_1x1[ccmppWPP_estimates$fert_rate_age_1x1$time_start == projection_start_year-1,]
  fert_rate_age_f_constant <- NULL
  for (i in 1:length(projection_times)) {
    asfr_add <- asfr_last_observed
    asfr_add$time_start <- projection_times[i]
    fert_rate_age_f_constant <- rbind(fert_rate_age_f_constant, asfr_add)
  }
  
  ############################
  ############################
  #    compute asfr for instant replacement fertility (NRR = 1)
  
  fert_rate_age_f_instant <- list()
  for (i in 1:length(projection_times)) {
    
    srb_time              <- ccmppWPP_medium$srb[ccmppWPP_medium$srb$time_start == projection_times[i],]
    fert_rate_age_f_time  <- ccmppWPP_medium$fert_rate_age_1x1[ccmppWPP_medium$fert_rate_age_1x1$time_start == projection_times[i],]
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
  lt_last_observed <- ccmppWPP_estimates$lt_complete_age_sex[ccmppWPP_estimates$lt_complete_age_sex$time_start == projection_start_year-1 & 
                                                            ccmppWPP_estimates$lt_complete_age_sex$sex %in% c("male","female"),]
  # initialize constant-mortality output
  lt_constant <- list()
  # repeat starting period sex-specific life table for each period in projection horizon
  for (i in 1:length(projection_times)) {
    
    lt_time            <- lt_last_observed
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
    fert_rate_age_f_time  <- ccmppWPP_medium$fert_rate_age_1x1[ccmppWPP_medium$fert_rate_age_1x1$time_start == projection_times[i],]
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
  
  pop_count_age_sex_base <- ccmppWPP_estimates$pop_count_age_sex_1x1[ccmppWPP_estimates$pop_count_age_sex_1x1$time_start == projection_start_year &
                                                                       ccmppWPP_estimates$pop_count_age_sex_1x1$sex %in% c("male", "female"),]
  
  # make a dummy filler for migration rates since these are not operationalized yet
  mig_net_rate_age_sex = ccmppWPP_medium$mig_net_count_age_sex_1x1[ccmppWPP_medium$mig_net_count_age_sex_1x1$sex %in% c("male","female"),]
  mig_net_rate_age_sex$value <- 0
  mig_net_count_tot_b <- ccmppWPP_medium$mig_net_count_tot_sex[ccmppWPP_medium$mig_net_count_tot_sex$sex == "both", 
                                                               names(ccmppWPP_medium$mig_net_count_tot_sex) != "sex"]
  
  # all inputs same as medium variant, except asfr, which are low
  inputs_low <- list(pop_count_age_sex_base = pop_count_age_sex_base,
                     life_table_age_sex = ccmppWPP_medium$lt_complete_age_sex[ccmppWPP_medium$lt_complete_age_sex$sex %in% c("male","female"),],
                     fert_rate_age_f = fert_rate_age_f_low,
                     srb = ccmppWPP_medium$srb,
                     mig_net_count_age_sex = ccmppWPP_medium$mig_net_count_age_sex_1x1[ccmppWPP_medium$mig_net_count_age_sex_1x1$sex %in% c("male","female"),],
                     mig_net_rate_age_sex = mig_net_rate_age_sex,
                     mig_net_count_tot_b = mig_net_count_tot_b,
                     mig_parameter = ccmppWPP_medium$mig_parameter)
  
    # assign attributes
    attr(inputs_low, "revision") <- attributes(ccmppWPP_estimates)$revision
    attr(inputs_low, "locid") <- attributes(ccmppWPP_estimates)$locid
    attr(inputs_low, "variant") <- "low fertility"
  
  
  # same as medium variant, but with high asfr
  inputs_high <- inputs_low
  inputs_high$fert_rate_age_f <- fert_rate_age_f_high
    # assign attributes
    attr(inputs_high, "revision") <- attributes(ccmppWPP_estimates)$revision
    attr(inputs_high, "locid") <- attributes(ccmppWPP_estimates)$locid
    attr(inputs_high, "variant") <- "high fertility"
  
  # same as medium variant, but with constant asfr
  inputs_constant <- inputs_low
  inputs_constant$fert_rate_age_f <- fert_rate_age_f_constant
    # assign attributes
    attr(inputs_constant, "revision") <- attributes(ccmppWPP_estimates)$revision
    attr(inputs_constant, "locid") <- attributes(ccmppWPP_estimates)$locid
    attr(inputs_constant, "variant") <- "constant fertility"
  
  # instant replacement same as medium variant, but with instant replacement asfr
  inputs_instant <- inputs_low
  inputs_instant$fert_rate_age_f <- fert_rate_age_f_instant
    # assign attributes
    attr(inputs_instant, "revision") <- attributes(ccmppWPP_estimates)$revision
    attr(inputs_instant, "locid") <- attributes(ccmppWPP_estimates)$locid
    attr(inputs_instant, "variant") <- "instant replacement"
  
  # momentum is instant replacement asfr, constant mortality, zero migration
  inputs_momentum <- inputs_instant
  inputs_instant$life_table_age_sex <- life_table_age_sex_constant
  inputs_instant$mig_net_count_age_sex$value <- 0
  inputs_instant$mig_net_count_tot_b$value <- 0
    # assign attributes
    attr(inputs_momentum, "revision") <- attributes(ccmppWPP_estimates)$revision
    attr(inputs_momentum, "locid") <- attributes(ccmppWPP_estimates)$locid
    attr(inputs_momentum, "variant") <- "momentum"
  
  # no change is medium inputs with constant fertility and constant mortality
  inputs_nochange <- inputs_constant
  inputs_nochange$life_table_age_sex <- life_table_age_sex_constant
    # assign attributes
    attr(inputs_nochange, "revision") <- attributes(ccmppWPP_estimates)$revision
    attr(inputs_nochange, "locid") <- attributes(ccmppWPP_estimates)$locid
    attr(inputs_nochange, "variant") <- "no change"
  
  # constant mortality is same as medium variant but with constant life tables
  inputs_constant_mort <- inputs_nochange
  inputs_constant_mort$fert_rate_age_f <- ccmppWPP_medium$fert_rate_age_1x1
    # assign attributes
    attr(inputs_constant_mort, "revision") <- attributes(ccmppWPP_estimates)$revision
    attr(inputs_constant_mort, "locid") <- attributes(ccmppWPP_estimates)$locid
    attr(inputs_constant_mort, "variant") <- "constant mortality"
    
  # zero migration is medium inputs but zero migration
  inputs_nomig <- inputs_low
  inputs_nomig$fert_rate_age_f <- ccmppWPP_medium$fert_rate_age_1x1
  inputs_nomig$mig_net_count_age_sex$value <- 0
  inputs_nomig$mig_net_count_tot_b$value <- 0
    # assign attributes
    attr(inputs_nomig, "revision") <- attributes(ccmppWPP_estimates)$revision
    attr(inputs_nomig, "locid") <- attributes(ccmppWPP_estimates)$locid
    attr(inputs_nomig, "variant") <- "zero migration"
  
  
  
  variant_inputs <- list(low_fert = inputs_low,
                         high_fert = inputs_high,
                         constant_fert = inputs_constant,
                         instant_replace = inputs_instant,
                         momentum = inputs_momentum,
                         no_change = inputs_nochange,
                         constant_mort = inputs_constant,
                         zero_mig = inputs_nomig)
  
  return(variant_inputs)
}


project_variants_deterministic <- function(medium_variant_outputs) {
  
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




