context("Test S3 class 'ccmpp_input_list'")

test_that("valid member created", {
    expect_s3_class(ccmpp_input_list_example,
                    "ccmpp_input_list")
})


test_that("all elements required", {
    x <- ccmpp_input_list_example
    y <- x[1:3]
    y <- new_ccmpp_input_list(y, age_span = age_span(x),
                              time_span = time_span(x))
    expect_error(validate_ccmpp_object(y),
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
        expect_error(capture.output(validate_ccmpp_object(y),
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
        expect_error(capture.output(validate_ccmpp_object(y),
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


test_that("'pop_count_age_sex_base' component can be changed", {
    x <- pop_count_base_component(ccmpp_input_list_example)
    expect_s3_class(x, "pop_count_age_sex_base")

    values(x) <- 1
    expect_s3_class(x, "pop_count_age_sex_base")

    expect_equal(values(x), rep(1, nrow(x)))
    })


