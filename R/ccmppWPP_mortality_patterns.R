
## ccmppWPP_mortality_patterns

ccmppWPP_mortality_patterns_SVDcomp <- function(model_inputs, 
                                                adjust_oldage_lc = FALSE, # whether to use pre-hiv lee-carter extrapolations for older ages
                                                adjust_oldage_lc_fit_years = 1950:1989, # years to fit lee-carter extrapolations
                                                adjust_oldage_blend_ages = 65:80) {
  
  source(paste0(root_dir,"MLT/lt_model_hiv_SVDcomp.R"))
  
  lts_svd <- lt_model_hiv_SVDcomp(inputs = model_inputs, # a data frame with time_start, time_span, sex, q5, q1, q1545, hiv and art)
                                   adjust_oldage_lc = adjust_oldage_lc, # whether to use pre-hiv lee-carter extrapolations for older ages
                                   adjust_oldage_lc_fit_years = adjust_oldage_lc_fit_years, # years to fit lee-carter extrapolations
                                   adjust_oldage_blend_ages = adjust_oldage_blend_ages) # ages to blend lee-carter extrapolations with SVDcomp mortality pattern
    
  
   female <- lts_svd[lts_svd$sex == "female", c("Age","AgeInt","nMx","nAx","nqx","lx","ndx","nLx","Sx","Tx","ex","time_start")]
   male <- lts_svd[lts_svd$sex == "male", c("Age","AgeInt","nMx","nAx","nqx","lx","ndx","nLx","Sx","Tx","ex","time_start")]
   mlts <- list(female = female, male = male) 
   
   return(mlts)  
  
} 

    
## This function fits an abridged life table with logquad and then graduates it to single year of age
ccmppWPP_mortality_patterns_LogQuad <- function(inputs_type, model_inputs) {
      
  source(paste0(root_dir,"MLT/lt_model_lq_single.R"))
  
  yrs <- unique(model_inputs$time_start)
  
  input_values <- model_inputs
  
  male <- list()
  female <- list()
  for (i in 1:length(yrs)) {
    
    ltm <- lt_model_lq_single(Sex = "m",
                              q0_5 = ifelse(is.na(input_values$q5_value[input_values$time_start == yrs[i] &
                                                                          input_values$sex == "male"]), NULL,
                                                    input_values$q5_value[input_values$time_start == yrs[i] &
                                                                            input_values$sex == "male"]),
                              q15_45 = ifelse(is.na(input_values$q1545_value[input_values$time_start == yrs[i] &
                                                                          input_values$sex == "male"]), NULL,
                                            input_values$q1545_value[input_values$time_start == yrs[i] &
                                                                    input_values$sex == "male"]),
                              axmethod = "un",
                              a0rule = "ak",
                              IMR = input_values$q1_value[input_values$time_start == yrs[i] &
                                                                         input_values$sex == "male"], 
                              OAnew = 90) # we go to 90 because this is what is fit by logquad. Will extend after
    
    ltm$time_start <- yrs[i]
    male[[i]] <- ltm
    rm(ltm)
    
    ltf <- lt_model_lq_single(Sex = "f",
                              q0_5 = ifelse(is.na(input_values$q5_value[input_values$time_start == yrs[i] &
                                                                          input_values$sex == "female"]), NULL,
                                            input_values$q5_value[input_values$time_start == yrs[i] &
                                                                    input_values$sex == "female"]),
                              q15_45 = ifelse(is.na(input_values$q1545_value[input_values$time_start == yrs[i] &
                                                                               input_values$sex == "female"]), NULL,
                                              input_values$q1545_value[input_values$time_start == yrs[i] &
                                                                         input_values$sex == "female"]),
                              axmethod = "un",
                              a0rule = "ak",
                              IMR = input_values$q1_value[input_values$time_start == yrs[i] &
                                                            input_values$sex == "female"], 
                              OAnew = 90) # we go to 90 because this is what is fit by logquad. Will extend after
    
    ltf$time_start <- yrs[i]
    female[[i]] <- ltf
    rm(ltf)
  }
      
  female <- do.call(rbind, female)
  male <- do.call(rbind, male)
  mlts <- list(female = female, male = male) 
  
  return(mlts)  
      
}

    
## This function pulls model life tables from the classic CD and UN families
ccmppWPP_mortality_patterns_classicMLT <- function(model_region, inputs_type, model_inputs) {
  
  source(paste0(root_dir,"MLT/lt_model_cdun_match_single.R"))
  source(paste0(root_dir,"MLT/lt_model_cdun_combin_single.R"))
  source(paste0(root_dir,"MLT/lt_model_un_bestft.R"))
  
  yrs <- unique(model_inputs$time_start)
  
  # 1 parameter models use match
    if (inputs_type %in% c("5q0","e0")) {
      
      if (inputs_type == "5q0") {
        input_values <- model_inputs[,c("time_start","sex","q5_value")]
      } else if (inputs_type == "e0") {
        input_values <- model_inputs[,c("time_start","sex","e0_value")]
      }
      
      male <- list()
      female <- list()
      for (i in 1:length(yrs)) {
        ltm <- lt_model_cdun_match_single(type = model_region,
                                          Sex = "m",
                                          indicator = inputs_type,
                                          value = as.numeric(input_values[input_values$time_start == yrs[i] &
                                                                 input_values$sex == "male", 3]),
                                          a0rule = "cd",
                                          OAnew = 130)
        ltm$time_start <- yrs[i]
        male[[i]] <- ltm
        rm(ltm)
        
        ltf <- lt_model_cdun_match_single(type = model_region,
                                          Sex = "f",
                                          indicator = inputs_type,
                                          value = as.numeric(input_values[input_values$time_start == yrs[i] &
                                                                 input_values$sex == "female", 3]),
                                          a0rule = "cd",
                                          OAnew = 130)
        ltf$time_start <- yrs[i]
        female[[i]] <- ltf
        rm(ltf)
      }
      
    } 
  
  # multi-parameter models use combin
  if (inputs_type %in% c("5q0 and 45q15", "1q0, 5q0 and 45q15")) {
    
    # then we use combin
    input_values <- model_inputs[, c("time_start","sex","q1_value","q5_value","q1545_value")]
    if (inputs_type == "5q0 and 45q15") {
      input_values$q1_value <- NA # replace q1 values with NA if they are not to be used
    }
    
    male <- list()
    female <- list()
    for (i in 1:length(yrs)) {
      
      ltm <- lt_model_cdun_combin_single(type = model_region, 
                                         Sex = "m",
                                         q1 = input_values$q1_value[input_values$time_start == yrs[i] &
                                                                       input_values$sex == "male"], 
                                         q5 = input_values$q5_value[input_values$time_start == yrs[i] &
                                                                       input_values$sex == "male"], 
                                         indicator_adult = "45q15", 
                                         value_adult = input_values$q1545_value[input_values$time_start == yrs[i] &
                                                                                   input_values$sex == "male"], 
                                         a0rule = "cd",
                                         OAnew = 130)
      ltm$time_start <- yrs[i]
      male[[i]] <- ltm
      rm(ltm)
      
      ltf <- lt_model_cdun_combin_single(type = model_region, 
                                         Sex = "f",
                                         q1 = input_values$q1_value[input_values$time_start == yrs[i] &
                                                                       input_values$sex == "female"], 
                                         q5 = input_values$q5_value[input_values$time_start == yrs[i] &
                                                                       input_values$sex == "female"], 
                                         indicator_adult = "45q15", 
                                         value_adult = input_values$q1545_value[input_values$time_start == yrs[i] &
                                                                                   input_values$sex == "female"], 
                                         a0rule = "cd",
                                         OAnew = 130)
      ltf$time_start <- yrs[i]
      female[[i]] <- ltf
      rm(ltf)
      
    }
    
  }
 
 female <- do.call(rbind, female)
 male <- do.call(rbind, male)
 mlts <- list(female = female, male = male) 
 
 return(mlts)
    
}
