
example(source)
sourceDir("../R")

canada <- get(load("../data/canada_wpp_1950_2020_ccmpp_inputs_1x1.rda"))

head(canada$fert_rate_age_f)
head(canada$mig_net_count_age_sex)

fm <- matrix(data = canada$fert_rate_age_f$value,
             nrow = length(unique(canada$fert_rate_age_f$age)),
       dimnames = list(ages = unique(canada$fert_rate_age_f$age),
                       periods = unique(canada$fert_rate_age_f$year)))

################################################################################

(load("ccmpp_input_example.rda"))

ccmpp_input_example$srb
