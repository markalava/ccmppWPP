## code to prepare `pasfr_global_model` dataset goes here
pasfr_global_model <- function() {
    
    # define the global fertility patterns associated with different models
    
    #WPP19 used the following models for five year age groups
    # MF - Model to use for the fertility pattern (PASFR), each corresponding to TFR=2.1
    #    1 - EARLY MODEL c(0.20,0.40,0.25,0.10,0.04,0.01,0.00)
    #    2 - INTERMEDIATE MODEL c(0.12,0.31,0.31,0.16,0.08,0.02,0.00)
    #    3 - LATE MODEL c(0.04,0.22,0.40,0.22,0.10,0.02,0.00)
    
    # for now I have just divided the 5-year intermediate pasfr by five to graduate to single year of age
    # ultimately these need to be replaced with a 1x model
    
    pasfr_global_model <- data.frame(age_start = seq(0,100,1),
                        age_span = rep(1,101),
                        value     = c(rep(0,15),
                                      rep(0.12/5,5),
                                      rep(0.31/5,5),
                                      rep(0.31/5,5),
                                      rep(0.16/5,5),
                                      rep(0.08/5,5),
                                      rep(0.02/5,5),
                                      rep(0.0/5,5),
                                      rep(0, 51)))

    return(pasfr_global_model)
    
}


## Overwrites function
pasfr_global_model <- pasfr_global_model()

## Creates the data .rda file
usethis::use_data(pasfr_global_model, overwrite = TRUE)
