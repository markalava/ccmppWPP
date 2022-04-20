
### OBJECTS NEEDED (already tested)

ccmpp_input_list_example <-
    ccmpp_input_list(pop_count_age_sex_base = wpp_input_example$pop_count_age_sex_base,
                     life_table_age_sex = wpp_input_example$life_table_age_sex,
                     fert_rate_age_f = wpp_input_example$fert_rate_age_f,
                     srb = wpp_input_example$srb,
                     mig_net_count_age_sex = wpp_input_example$mig_net_count_age_sex,
                     mig_net_rate_age_sex = wpp_input_example$mig_net_rate_age_sex,
                     mig_net_count_tot_b = wpp_input_example$mig_net_count_tot_b,
                     mig_parameter = wpp_input_example$mig_parameter)


test_that("ccmpp loop over time works as expected", {
    x <- expect_error(project_ccmpp_loop_over_time(ccmpp_input_list_example),
                      NA)
    expect_is(x, "list")
    y <- expect_error(data_reshape_ccmpp_output(x), NA)
    expect_is(y, "list")

    ## One-step only
    input <- subset_time(ccmpp_input_list_example, times = 1950)
    x1 <- expect_error(project_ccmpp_loop_over_time(input),
                      NA)
    expect_is(x1, "list")
    y1 <- expect_error(data_reshape_ccmpp_output(x1), NA)
    expect_is(y1, "list")

    expect_identical(unique(y1$pop_count_age_sex$time_start), 1951)
    expect_equal(subset(y1$pop_count_age_sex, time_start == 1951)$value,
                 subset(y$pop_count_age_sex, time_start == 1951)$value)

    expect_identical(unique(y1$death_count_cohort$time_start), 1950)
    expect_equal(subset(y1$death_count_cohort, time_start == 1950)$value,
                 subset(y$death_count_cohort, time_start == 1950)$value)
})
