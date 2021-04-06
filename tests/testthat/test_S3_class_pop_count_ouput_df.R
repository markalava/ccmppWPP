

test_that("valid member created from a data frame", {
    x <- data.frame(
        expand.grid(time_start = 1950:1954, age_start = 0:4, sex = c("male", "female"),
                     time_span = 1, age_span = 1),
                     value = 1:50)
    expect_s3_class(pop_count_age_sex(x),
                    "pop_count_age_sex")
})

test_that("valid member created from a ccmpp input list", {
    x <- ccmpp_input_list(pop_count_age_sex_base = wpp_input_example$pop_count_age_sex_base,
                     life_table_age_sex = wpp_input_example$life_table_age_sex,
                     fert_rate_age_f = wpp_input_example$fert_rate_age_f,
                     srb = wpp_input_example$srb,
                     mig_net_count_age_sex = wpp_input_example$mig_net_count_age_sex,
                     mig_net_rate_age_sex = wpp_input_example$mig_net_rate_age_sex,
                     mig_net_count_tot_b = wpp_input_example$mig_net_count_tot_b,
                     mig_parameter = wpp_input_example$mig_parameter)
    expect_s3_class(pop_count_age_sex(x),
                    "pop_count_age_sex")
})


### MAKE OBJECT AVAILABLE TO REMAINDER OF TESTS

pop_count_age_sex_df <-
    pop_count_age_sex(data.frame(
        expand.grid(time_start = 1950:1954, age_start = 0:4, sex = c("male", "female"),
                     time_span = 1, age_span = 1),
                     value = 1:50))


test_that("Invalid 'value's are caught", {
    x <- pop_count_age_sex_df
    x[1, "value"]  <- -1
    expect_error(pop_count_age_sex(x),
                 "'value' column has negative elements")
})
