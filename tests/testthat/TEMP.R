
test <- expand.grid(age_start = 0:12, time_start = 1950:1961, sex = c("male", "female"),
                    age_span = 1, time_span = 1, stringsAsFactors = FALSE)

test1 <- expand.grid(age_start = 0:2, time_start = 1950:1961, sex = c("male", "female")
                     , stringsAsFactors = FALSE)
test1 <- data.frame(test1, age_span = c(1, 2, 1, rep(rep(1, 3), 3)), time_span = 1, value = 999
                    , stringsAsFactors = FALSE)

test2 <- transform(test1, age_span = 5, time_span = 5)




undebug(tabulate_lexis_squares)

tabulate_lexis_squares(test2)

mig_net_count_age_sex(test1)






debug(verify_complete_time_age_sex_sequence)

verify_complete_time_age_sex_sequence(test)
verify_complete_time_age_sex_sequence(test1)
verify_complete_time_age_sex_sequence(test2)









data(canada_wpp_1950_2020_ccmpp_inputs_1x1)

canada_wpp_1950_2020_ccmpp_inputs_1x1$pop_count_age_sex_base$time_span <- 0
save(canada_wpp_1950_2020_ccmpp_inputs_1x1,
     file = here::here("data/canada_wpp_1950_2020_ccmpp_inputs_1x1.rda"))


france_wpp_1950_2020_population_data$ccmppWPP_inputs$pop_count_age_sex_base$time_span <- 0
france_wpp_1950_2020_population_data$pop_count_age_sex_reference$time_span <- 0
save(france_wpp_1950_2020_population_data,
     file = here::here("data/france_wpp_1950_2020_population_data.rda"))


data(kuwait_wpp_1950_2020_ccmpp_inputs_1x1)

kuwait_wpp_1950_2020_ccmpp_inputs_1x1$pop_count_age_sex_base$time_span <- 0
save(kuwait_wpp_1950_2020_ccmpp_inputs_1x1,
     file = here::here("data/kuwait_wpp_1950_2020_ccmpp_inputs_1x1.rda"))


data(mexico_wpp_1950_2020_ccmpp_inputs_1x1)

mexico_wpp_1950_2020_ccmpp_inputs_1x1$pop_count_age_sex_base$time_span <- 0
save(mexico_wpp_1950_2020_ccmpp_inputs_1x1,
     file = here::here("data/mexico_wpp_1950_2020_ccmpp_inputs_1x1.rda"))


data(sweden_1993_Preston_5x5)

sweden_1993_Preston_5x5



data(wpp_input_example)
wpp_input_example
