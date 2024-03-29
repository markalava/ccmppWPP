

test_that("valid member created", {
    expect_s3_class(ccmpp_input_list(
        pop_count_age_sex_base = wpp_input_example$pop_count_age_sex_base,
                     life_table_age_sex = wpp_input_example$life_table_age_sex,
                     fert_rate_age_f = wpp_input_example$fert_rate_age_f,
                     srb = wpp_input_example$srb,
                     mig_net_count_age_sex = wpp_input_example$mig_net_count_age_sex,
                     mig_net_rate_age_sex = wpp_input_example$mig_net_rate_age_sex,
                     mig_net_count_tot_b = wpp_input_example$mig_net_count_tot_b,
                     mig_parameter = wpp_input_example$mig_parameter),
                    "ccmpp_input_list")
})


################################################################################

### Make objects available to subsequent tests

ccmpp_input_list_example <-
    ccmpp_input_list(pop_count_age_sex_base = wpp_input_example$pop_count_age_sex_base,
                     life_table_age_sex = wpp_input_example$life_table_age_sex,
                     fert_rate_age_f = wpp_input_example$fert_rate_age_f,
                     srb = wpp_input_example$srb,
                     mig_net_count_age_sex = wpp_input_example$mig_net_count_age_sex,
                     mig_net_rate_age_sex = wpp_input_example$mig_net_rate_age_sex,
                     mig_net_count_tot_b = wpp_input_example$mig_net_count_tot_b,
                     mig_parameter = wpp_input_example$mig_parameter)

survival_ratio_input_df_time_age_sex <-
    survival_ratio_age_sex(subset(wpp_input_example$life_table_age_sex,
           indicator == "lt_Sx", select = -indicator))

################################################################################


test_that("all elements required", {
    x <- ccmpp_input_list_example
    y <- x[1:3]
    y <- new_ccmpp_input_list(y, age_span = age_span(x),
                              time_span = time_span(x))
    expect_error(validate_ccmppWPP_object(y),
                 "must have these elements")
})


test_that("subset replacements issue warnings and basic lists,", {
    x <- ccmpp_input_list_example
    expect_not_s3_class(expect_warning({ x[1] <- 1 },
                                   "will not preserve the class or attributes"),
                    "ccmpp_input_list")

    x <- ccmpp_input_list_example
    expect_not_s3_class(expect_warning({ x[[1]] <- 1 },
                   "will not preserve the class or attributes"),
                    "ccmpp_input_list")

    x <- ccmpp_input_list_example
    expect_not_s3_class(expect_warning({ x$pop_count_age_sex_base <- 1 },
                   "will not preserve the class or attributes"),
                   "ccmpp_input_list")
    })


test_that("subsetting returns valid objects", {
    expect_s3_class(subset_time(ccmpp_input_list_example, times = 1950),
                    "ccmpp_input_list")

    expect_s3_class(subset_age(ccmpp_input_list_example, ages = 0:60),
                    "ccmpp_input_list")

    expect_error(subset_age(ccmpp_input_list_example, ages = 10:60),
                 "'0' must be in 'ages'")
})


test_that("common set of times is enforced", {
    x <- ccmpp_input_list_example
    y <- x
    ty <- times(y[["fert_rate_age_f"]])
    y[["fert_rate_age_f"]] <-
        subset_time(y[["fert_rate_age_f"]],
                    times = ty[seq_len(length(ty) / 2)]
                    )
    y <- new_ccmpp_input_list(y, age_span = age_span(x),
                              time_span = time_span(x))
        expect_error(capture.output(validate_ccmppWPP_object(y),
                                    file = OS_null_file_string),
                 "must have the same number of unique times")

    y <- x
    ty <- y[["fert_rate_age_f"]]$time_start
    y[["fert_rate_age_f"]]$time_start <- ty + 1
    y[["fert_rate_age_f"]] <-
        new_fert_rate_age_f(y[["fert_rate_age_f"]],
                            age_span = age_span(x),
                            time_span = time_span(x),
                            value_scale = 1,
                            non_zero_fert_ages = non_zero_fert_ages(x[["fert_rate_age_f"]])
                            )
    y <- new_ccmpp_input_list(y, age_span = age_span(x),
                              time_span = time_span(x))
        expect_error(capture.output(validate_ccmppWPP_object(y),
                                   file = OS_null_file_string),
                     "must have the same unique times")
})


test_that("non-zero fert ages can be changed", {
    x <- ccmpp_input_list_example
    nzfa <- non_zero_fert_ages(x)
    non_zero_fert_ages(x) <- 20:30
    expect_false(identical(as.double(nzfa),
                           as.double(non_zero_fert_ages(x))))
    expect_identical(as.double(20:30),
                     as.double(non_zero_fert_ages(x)))
})


test_that("'fert_rate_age_f' component can be changed", {
    z <- ccmpp_input_list_example
    x <- fert_rate_age_f(z)
    expect_s3_class(x, "fert_rate_age_f")

    suppressWarnings({x$value <- 0.999})

    expect_error(fert_rate_age_f(z) <- x, NA)
    expect_equal(values(fert_rate_age_f(z)), rep(0.999, nrow(x)))
})


test_that("'life_table_age_sex' component can be changed", {
    z <- ccmpp_input_list_example
    x <- life_table_age_sex(z)
    expect_s3_class(x, "life_table_age_sex")
    expect_error(life_table_age_sex(z) <- x, NA)
})


test_that("'death_count_age_sex' component can be changed", {
    z <- ccmpp_input_list_example
    x <- death_count_age_sex(z)
    expect_s3_class(x, "death_count_age_sex")
    expect_error(death_count_age_sex(z) <- x, NA)
})


test_that("'death_probability_age_sex' component can be changed", {
    z <- ccmpp_input_list_example
    x <- death_probability_age_sex(z)
    expect_s3_class(x, "death_probability_age_sex")
    expect_error(death_probability_age_sex(z) <- x, NA)
})


test_that("'mig_net_count_age_sex' component can be changed", {
    z <- ccmpp_input_list_example
    x <- mig_net_count_age_sex(z)
    expect_s3_class(x, "mig_net_count_age_sex")
    expect_error(mig_net_count_age_sex(z) <- x, NA)
})


test_that("'mig_net_count_tot_b' component can be changed", {
    z <- ccmpp_input_list_example
    x <- mig_net_count_tot_b(z)
    expect_s3_class(x, "mig_net_count_tot_b")
    expect_error(mig_net_count_tot_b(z) <- x, NA)
})


test_that("'mig_parameter' component can be changed", {
    z <- ccmpp_input_list_example
    x <- mig_parameter(z)
    expect_s3_class(x, "mig_parameter")
    expect_error(mig_parameter(z) <- x, NA)
})


test_that("'mort_rate_age_sex' component can be changed", {
    z <- ccmpp_input_list_example
    x <- mort_rate_age_sex(z)
    expect_s3_class(x, "mort_rate_age_sex")
    expect_error(mort_rate_age_sex(z) <- x, NA)
})


test_that("'pop_count_age_sex_base' component can be changed", {
    x <- pop_count_age_sex_base(ccmpp_input_list_example)
    expect_s3_class(x, "pop_count_age_sex_base")

    suppressWarnings({x$value <- 1})

    expect_equal(x$value, rep(1, nrow(x)))
})


test_that("'srb' component can be changed", {
    x <- srb(ccmpp_input_list_example)
    expect_s3_class(x, "srb")

    suppressWarnings({x$value <- 1})

    expect_equal(x$value, rep(1, nrow(x)))
})


test_that("survival ratio component can be changed", {
    suppressWarnings({
    x <- survival_ratio_input_df_time_age_sex
    x$value <- 1

    y <- life_table_age_sex(ccmpp_input_list_example)
    expect_false(isTRUE(
        all.equal(values(survival_ratio_age_sex(y)),
                  rep(1, nrow(survival_ratio_age_sex(y))))))

    survival_ratio_age_sex(y) <- x
    expect_equal(values(survival_ratio_age_sex(y)),
                 rep(1, nrow(survival_ratio_age_sex(y))))

    z <- ccmpp_input_list_example
    expect_false(isTRUE(
        all.equal(values(survival_ratio_age_sex(z)),
                  rep(1, nrow(survival_ratio_age_sex(z))))))
    survival_ratio_age_sex(z) <- x
    expect_equal(values(survival_ratio_age_sex(z)),
                 rep(1, nrow(survival_ratio_age_sex(z))))
    })
})


### Migration details

test_that("mig assumption can be changed", {
    suppressWarnings({
    expect_error(mig_assumption(ccmpp_input_list_example) <- "test",
                 "is not TRUE")

    y <- mig_parameter(ccmpp_input_list_example)
    z <- mig_assumption(y)
    mig_assumption(y) <-
        switch(z, end = "even", even = "end")
    expect_false(identical(z, mig_assumption(y)))

    x <- ccmpp_input_list_example
    mig_a_x <- mig_assumption(x)
    mig_assumption(x) <-
        switch(mig_assumption(x), end = "even", even = "end")
    expect_false(identical(mig_a_x, mig_assumption(x)))
    })
})


test_that("mig type can be changed", {
    suppressWarnings({
    expect_error(mig_type(ccmpp_input_list_example) <- "test",
                 "is not TRUE")

    y <- mig_parameter(ccmpp_input_list_example)
    z <- mig_type(y)
    mig_type(y) <-
        switch(z, counts = "rates", rates = "counts")
    expect_false(identical(z, mig_type(y)))

    x <- ccmpp_input_list_example
    mig_a_x <- mig_type(x)
    mig_type(x) <-
        switch(mig_type(x), counts = "rates", rates = "counts")
    expect_false(identical(mig_a_x, mig_type(x)))
    })
})




test_that("'mig_net_rate_age_sex' component can be changed", {
    z <- ccmpp_input_list_example
    expect_false(all(values(mig_net_rate_age_sex(z)) == 0))
    x <- mig_net_rate_age_sex(z)
    values(x) <- 0
    expect_s3_class(x, "mig_net_rate_age_sex")
    expect_error(mig_net_rate_age_sex(z) <- x, NA)
    expect_true(all(values(mig_net_rate_age_sex(z)) == 0))

    ## Reset counts
    z <- ccmpp_input_list_example
    expect_false(all(values(mig_net_count_age_sex(z)) == 0))
    expect_false(all(values(mig_net_rate_age_sex(z)) == 0))

    x <- mig_net_rate_age_sex(z)
    values(x) <- 0
    expect_error(mig_net_rate_age_sex(z, reset_mig_counts = TRUE) <- x, NA)
    expect_true(all(values(mig_net_rate_age_sex(z)) == 0))
    expect_true(all(values(mig_net_count_age_sex(z)) == 0))

    ## Manually reset
    z <- ccmpp_input_list_example
    expect_false(all(values(mig_net_rate_age_sex(z)) == 0))
    x <- mig_net_rate_age_sex(z)
    values(x) <- 0
    expect_s3_class(x, "mig_net_rate_age_sex")
    expect_error(mig_net_rate_age_sex(z) <- x, NA)
    z <- make_mig_counts_match_rates(z)
    expect_true(all(values(mig_net_rate_age_sex(z)) == 0))
    expect_true(all(values(mig_net_count_age_sex(z)) == 0))
})
