
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
