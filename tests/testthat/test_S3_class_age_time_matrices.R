context("Conversion to age time matrices")


test_that("'age_time_matrix's are created properly", {
    expect_error(as_age_time_matrix(dcc_df_time_age_sex),
                 "'x' has dimension \"sex\"; select a single sex")
    expect_is(as_age_time_matrix(subset_sex(dcc_df_time_age_sex, sexes = "female", drop = TRUE)),
              "matrix")
    })


test_that("age * time matrices are produced", {
    expect_is(as_age_time_matrix(mig_net_count_tot_input_df_time),
                 "matrix")
    expect_is(as_age_time_matrix(fert_rate_input_df_time_age),
                 "matrix")
    expect_is(as_age_time_matrix(fert_rate_input_df_time_age,
                                 drop_zero_fert_ages = TRUE),
              "matrix")

    expect_error(as_age_time_matrix(ccmpp_input_df_time_age_sex),
                 "select a single sex")
    expect_is(as_age_time_matrix(subset_sex(ccmpp_input_df_time_age_sex,
                                            "female", drop = TRUE)),
              "matrix")

    expect_error(as_age_time_matrix(mig_parameter_input_df_indicator_time),
                 "select a single indicator")
})


test_that("age * time matrix lists are produced", {
    expect_is(as_age_time_matrix_list(ccmpp_input_list_example),
              "list")
})


test_that("zero fertility rate ages are dropped", {
    fert_rate_df <-
        expand.grid(age_start = seq(from = 0, to = 10, by = 1),
                    time_start = c(1990, 1991),
                    time_span = 1, age_span = 1)
    fert_rate_df$value <- rep(0.1, nrow(fert_rate_df))
    fert_rate_df <-
        fert_rate_age_f(fert_rate_df,
                        non_zero_fert_ages = 4:7)

    expect_identical(dim(as_age_time_matrix(fert_rate_df)),
                     c(11L, 2L))

    expect_identical(dim(as_age_time_matrix(fert_rate_df,
                                            drop_zero_fert_ages = TRUE)),
                     c(4L, 2L))


    ccmpp_list <- as_age_time_matrix_list(ccmpp_input_list_example)
    expect_identical(dim(ccmpp_list$fert_rate_age_f),
                     c(101L, 70L))

    ccmpp_list <-
        as_age_time_matrix_list(ccmpp_input_list_example,
                                drop_zero_fert_ages = TRUE)
    expect_identical(dim(ccmpp_list$fert_rate_age_f),
                     c(42L, 70L))
})


test_that("age * time matrices can be coerced to demog_change_component_df", {
    x <- subset_sex(dcc_df_time_age_sex,
                    sexes = "female", drop = TRUE)
    y <- as_age_time_matrix(x)
    z <- as_demog_change_component_df(y)
    expect_is(y, "matrix")
    expect_s3_class(z, "demog_change_component_df")
    expect_identical(x$value, z$value)
    })


test_that("age * time matrices can be coerced to ccmpp_input_df", {
    x <- subset_sex(ccmpp_input_df_time_age_sex,
                    sexes = "female", drop = TRUE)
    y <- as_age_time_matrix(x)
    z <- as_ccmpp_input_df(y)
    expect_is(y, "matrix")
    expect_s3_class(z, "ccmpp_input_df")
    expect_identical(x$value, z$value)
})


test_that("age * time matrix lists are produced", {
    x <- ccmpp_input_df_time_age_sex
    y <- as_age_time_matrix_list(x)
    expect_is(y, "list")
    expect_is(y[[1]], "matrix")
    expect_is(y[[2]], "matrix")
})
