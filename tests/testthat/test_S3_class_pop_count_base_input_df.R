context("Test methods for S3 class 'pop_count_age_sex_base'")

test_that("valid member created", {
    expect_s3_class(pop_count_base_input_df_time_age_sex,
                    "pop_count_age_sex_base")
    })


test_that("Non-zero age detected", {
    y <- pop_count_base_input_df_time_age_sex
    z <- subset(y, age_start > 0)
    z <- ccmppWPP:::new_pop_count_age_sex_base(z,
                                               value_scale = 1,
                                     age_span = age_span(y),
                                     time_span = time_span(y))
    expect_error(validate_ccmpp_object(z),
                 "'age_start' does not start at '0'")
})


test_that("More than one time period detected", {
    y <- pop_count_base_input_df_time_age_sex
    w <- transform(y, time_start = times(y) + 1)
    z <- ccmppWPP:::new_pop_count_age_sex_base(sort_demog_change_component_df(rbind(y, w)),
                                               value_scale = 1,
                                     time_span = time_span(y),
                                     age_span = age_span(y))
    expect_error(validate_ccmpp_object(z),
                 "has more than one unique value")
    })


test_that("Invalid 'value's are caught", {
    x <- pop_count_base_input_df_time_age_sex
    x[1, "value"]  <- -1
    expect_error(pop_count_age_sex_base(x),
                 "'value' column has negative elements")
})

test_that("Equal sex values are detected", {
    x <- pop_count_base_input_df_time_age_sex

    y <- x
    y[y$sex == "male", "value"] <- y[y$sex == "female", "value"]
    expect_warning(pop_count_age_sex_base(y),
                   "Female and male baseline population counts are ")

    y <- x
    tol <- 0.5 / 100 * mean(y[y$sex == "female", "value"])
    y[y$sex == "male", "value"] <-
        y[y$sex == "female", "value"] + runif(n = nrow(y[y$sex == "female",]),
                                              min = 1, max = 0.9 * tol)
    expect_warning(pop_count_age_sex_base(y),
                   "Female and male baseline population counts are ")

    })
