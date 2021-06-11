## create a dataset that identifies locations to be included in computing pasfr global norm

# data(UNlocations, package = "wpp2019")
# 
# PASFRpattern <- UNlocations[as.numeric(UNlocations$country_code) < 900, c("country_code","name")]
# PASFRpattern$PasfrNorm <- as.factor("Global Norm")
# PASFRpattern$PasfrGlobalNorm <- 0
# PASFRpattern$PasfrGlobalNorm[PASFRpattern$country_code %in% c(40,56,830,344,446,158,203,208,233,246,250,276,380,392,442,470,528,578,702,705,724,752,756,826)] <- 1
# PASFRpattern <- PASFRpattern[order(PASFRpattern$country_code), c(1,3,4)]
# 
# save(PASFRpattern, file = "data/PASFRpattern.RData")

#############################
#############################
# create an RData file that has PASFR estimates for single year of age from 10 to 54 for all countries

# pasfr_estimates <- read.delim(file = "data/pasfr_estimates_for_pasfr_global_norm.txt", comment.char = "#", check.names = FALSE)
# save(pasfr_estimates, file = "data/pasfr_estimates_for_pasfr_global_norm.RData")

#############################
#############################
# create an RData file that has TFR estimates for single year for all countries

# tfr_estimates_projections <- read.delim(file = "data/tfr_estimates_projections_for_pasfr_global_norm.txt", comment.char = "#", check.names = FALSE)
# save(tfr_estimates_projections, file = "data/tfr_estimates_projections_for_pasfr_global_norm.RData")


