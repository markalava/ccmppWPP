

### OBJECTS NEEDED (tested already)

dcc_df_time_age_sex <-
    demog_change_component_df(S3_demog_change_component_time_age_sex_test_df)


test_that("subsetting works properly", {

    z <- subset_time(dcc_df_time_age_sex, times = 1950, drop = TRUE)
    expect_false(is_by_time(z))
    expect_true(is_by_age(z))
    expect_true(is_by_sex(z))

    z <- subset_age(dcc_df_time_age_sex, ages = 10, drop = TRUE)
    expect_false(is_by_age(z))
    expect_true(is_by_time(z))
    expect_true(is_by_sex(z))

    z <- subset_sex(dcc_df_time_age_sex, sexes = "female", drop = TRUE)
    expect_false(is_by_sex(z))
    expect_true(is_by_time(z))
    expect_true(is_by_age(z))

    z <- subset_time(dcc_df_time_age_sex, times = 1950, drop = FALSE)
    expect_true(is_by_time(z))
    expect_true(is_by_age(z))
    expect_true(is_by_sex(z))

    z <- subset_age(dcc_df_time_age_sex, ages = 0, drop = FALSE)
    expect_true(is_by_age(z))
    expect_true(is_by_time(z))
    expect_true(is_by_sex(z))

    z <- subset_sex(dcc_df_time_age_sex, sexes = "female", drop = FALSE)
    expect_true(is_by_sex(z))
    expect_true(is_by_time(z))
    expect_true(is_by_age(z))

    z <- subset_time(dcc_df_time_age_sex, times = 1950:1960)
    expect_true(is_by_time(z))
    expect_true(is_by_age(z))
    expect_true(is_by_sex(z))

    z <- subset_age(dcc_df_time_age_sex, ages = 0:10)
    expect_true(is_by_time(z))
    expect_true(is_by_age(z))
    expect_true(is_by_sex(z))

    z <- subset_sex(dcc_df_time_age_sex, sexes = c("female", "male"))
    expect_true(is_by_time(z))
    expect_true(is_by_age(z))
    expect_true(is_by_sex(z))
})


test_that("subsetting errors are caught", {

    expect_warning(expect_error(subset_time(dcc_df_time_age_sex, times = "a"),
                 "is.finite"), "NAs introduced")
    expect_warning(expect_error(subset_age(dcc_df_time_age_sex, ages = "a"),
                 "is.finite"), "NAs introduced")
    expect_error(subset_sex(dcc_df_time_age_sex, sexes = 1),
                 "character")

    expect_error(subset_time(subset_time(dcc_df_time_age_sex, times = 1950:1960), 1970),
                 "does not have any entries with")
    expect_error(subset_age(subset_age(dcc_df_time_age_sex, ages = 0:10), 11),
                 "does not have any entries with")

    expect_error(subset_sex(subset_sex(dcc_df_time_age_sex, sexes = "male", drop = TRUE),
                            sex = "female"),
                 "is_by_sex")
    expect_error(subset_time(subset_time(dcc_df_time_age_sex, times = 1950, drop = TRUE),
                             time = 1960),
                 "is_by_time")
    expect_error(subset_age(subset_age(dcc_df_time_age_sex, ages = 10, drop = TRUE),
                            age = 20),
                 "is_by_age")
})


test_that("'value_type' is checked properly", {

    x <- S3_demog_change_component_time_age_sex_test_df

    expect_error(demog_change_component_df(x,
                                           value_type = "census"),
                 "'value_type' must be one of")

        expect_error(demog_change_component_df(x,
                                           value_type = "proportion"),
                     "values less than 0 or greater than 1 are present")
})


test_that("'value_type' is set properly", {
    x <- dcc_df_time_age_sex
    value_type(x) <- "real"
    expect_identical(value_type(x), "real")
    value_type(x) <- "ratio"
    expect_identical(value_type(x), "ratio")
    expect_true(is.na(value_scale(x)))
})


test_that("'value_scale' is set properly", {
    z <- dcc_df_time_age_sex
    expect_warning(value_scale(z) <- 1,
                   ("Changing the 'value_scale' attribute does not automatically re-scale the 'value' column in the data"))
    expect_identical(value_scale_numeric(z), 1)
    expect_warning(value_scale(z) <- 10,
                   ("Changing the 'value_scale' attribute does not automatically re-scale the 'value' column in the data"))
    expect_identical(value_scale_numeric(z), 10)
    })


test_that("dimensions are correctly detected", {
    y <- demog_change_component_df(S3_demog_change_component_time_age_sex_test_df)
    expect_true(is_by_time(y))
    expect_true(is_by_age(y))
    expect_true(is_by_sex(y))
})

test_that("collapse_demog_dimension  works", {

    ## SPECIFY 'by_dimensions'

    x <- collapse_demog_dimension(dcc_df_time_age_sex, by_dimensions = "time")
    expect_s3_class(x, "demog_change_component_df")
    expect_identical(sort(colnames(x)), sort(c("time_start", "time_span", "value")))

    x <- collapse_demog_dimension(dcc_df_time_age_sex, by_dimensions = "age")
    expect_s3_class(x, "demog_change_component_df")
    expect_identical(sort(colnames(x)), sort(c("age_start", "age_span", "value")))

    x <- collapse_demog_dimension(as_ccmpp_input_df(dcc_df_time_age_sex), by_dimensions = "time")
    expect_s3_class(x, "ccmpp_input_df")
    x <- collapse_demog_dimension(as_ccmpp_input_df(dcc_df_time_age_sex), by_dimensions = "age")
    expect_s3_class(x, "ccmpp_input_df")

    x <- subset_age(dcc_df_time_age_sex, ages = 2:10)
    class(x) <- c("ccmpp_input_df", class(x))
    expect_error(collapse_demog_dimension(x, by_dimensions = "age"),
                 "The result of collapsing 'x' cannot be coerced to the class in argument 'out_class'")
    expect_s3_class(collapse_demog_dimension(as_demog_change_component_df(x),
                                             by_dimensions = "time"),
                    "demog_change_component_df")
    expect_s3_class(collapse_demog_dimension(x, by_dimensions = "time",
                                             out_class = "demog_change_component_df"),
                    "demog_change_component_df")
    expect_s3_class(collapse_demog_dimension(x, by_dimensions = "time",
                                             out_class = "data.frame"),
                    "data.frame")

    x <- dcc_df_time_age_sex
    value_type(x) <- "ratio"
    expect_error(collapse_demog_dimension(x, by_dimensions = "time"),
                 "but the only aggregatable 'value_type's are")

    expect_error(collapse_demog_dimension(dcc_df_time_age_sex,
                                          by_dimensions = "age", out_class = "matrix"),
                 "'out_class' must only use classes in this list")


    ## SPECIFY 'collapse_dimensions'

    x <- collapse_demog_dimension(dcc_df_time_age_sex, collapse_dimensions = "time")
    expect_s3_class(x, "demog_change_component_df")
    expect_identical(sort(colnames(x)), sort(c("sex", "age_start", "age_span", "value")))

    x <- collapse_demog_dimension(dcc_df_time_age_sex, collapse_dimensions = "age")
    expect_s3_class(x, "demog_change_component_df")
    expect_identical(sort(colnames(x)), sort(c("sex", "time_start", "time_span", "value")))

    x <- collapse_demog_dimension(dcc_df_time_age_sex, collapse_dimensions = "sex")
    expect_s3_class(x, "demog_change_component_df")
    expect_identical(sort(colnames(x)), sort(c("age_start", "age_span",
                                               "time_start", "time_span", "value")))

})


## ### MAKE OBJECT AVAILABLE TO REMAINDER OF TESTS

## ccmpp_input_df_test <- ccmpp_input_df(S3_demog_change_component_time_age_sex_test_df)

## test_that("Abridging works for 'ccmpp_input_df's", {
##     ## Just by age
##     expect_error(abridge(ccmpp_input_df_test,
##                          age_start_abridged = seq(from = 0, to = 100, by = 5)),
##                  "The result of abridging 'x' cannot be coerced to the class in argument 'out_class'")
##    expect_error(abridge(ccmpp_input_df_test, age_span_abridged = 5),
##                  "The result of abridging 'x' cannot be coerced to the class in argument 'out_class'")
##     expect_s3_class(abridge(ccmpp_input_df_test,
##                             age_start_abridged = seq(from = 0, to = 100, by = 5),
##                             out_class = "demog_change_component_df"),
##                     "demog_change_component_df")
##     expect_s3_class(abridge(ccmpp_input_df_test,
##                             age_span_abridged = 5,
##                             out_class = "demog_change_component_df"),
##                     "demog_change_component_df")

##     ## Just by time
##     expect_error(abridge(ccmpp_input_df_test,
##                          time_start_abridged = seq(from = 1950, to = 2019, by = 5)),
##                  "The result of abridging 'x' cannot be coerced to the class in argument 'out_class'")
##     expect_error(abridge(ccmpp_input_df_test, time_span_abridged = 5),
##                  "The result of abridging 'x' cannot be coerced to the class in argument 'out_class'")
##     expect_s3_class(abridge(ccmpp_input_df_test,
##                             time_start_abridged = seq(from = 1950, to = 2019, by = 5),
##                             out_class = "demog_change_component_df"),
##                     "demog_change_component_df")
##     expect_s3_class(abridge(ccmpp_input_df_test,
##                             time_span_abridged = 5,
##                             out_class = "demog_change_component_df"),
##                     "demog_change_component_df")

##     ## By age and time
##     expect_s3_class(abridge(ccmpp_input_df_test,
##                             age_start_abridged = seq(from = 0, to = 100, by = 5),
##                             time_start_abridged = seq(from = 1950, to = 2019, by = 5)),
##                     "ccmpp_input_df")
##     expect_s3_class(abridge(ccmpp_input_df_test,
##                             age_span_abridged = 5,
##                             time_span_abridged = 5),
##                     "ccmpp_input_df")

##     ## Check some actual sums
##     x <- ccmpp_input_df(expand.grid(age_start = 0:5, time_start = 1950:1954, value = 1))
##     x <- abridge(x, age_span_abridged = 5, time_span_abridged = 5)
##     expect_identical(x$value, c(25, 5))
##     expect_identical(x$age_span, c(5, 5))
##     expect_identical(x$time_span, c(5, 5))

## })


## ## test_that("Abridging works for 'death_count_age_sex_df's", {
## ##     test_df <- death_count_age_sex(rbind(expand.grid(age_start = 0:5,
## ##                                                     time_start = 1950:1954,
## ##                                                     sex = c("male"),
## ##                                                     value = 1),
## ##                                          expand.grid(age_start = 0:5,
## ##                                                     time_start = 1950:1954,
## ##                                                     sex = c("female"),
## ##                                                     value = 2)),
## ##                                                     value_scale = 1)
## ##     expect_error(abridge(test_df, span_abridged = 5),
## ##                  "The aggregate of 'x' cannot be coerced to the class in argument 'out_class'")
## ##     expect_s3_class(abridge(test_df, span_abridged = 5,
## ##                             out_class = "demog_change_component_df"),
## ##                     "demog_change_component_df")


## ##     x <- abridge(test_df, span_abridged = 5, time_span = 5)
## ##     expect_s3_class(x, "death_count_age_sex")
## ##     expect_identical(x$value, c(25, 5, 50, 10))
## ##     })


## ## test_that("Abridging works for 'pop_count_age_sex_base_df's", {
## ##     test_df <- pop_count_age_sex_base(rbind(expand.grid(age_start = 0:5,
## ##                                                     time_start = 1950,
## ##                                                     sex = c("male"),
## ##                                                     value = 1),
## ##                                          expand.grid(age_start = 0:5,
## ##                                                     time_start = 1950,
## ##                                                     sex = c("female"),
## ##                                                     value = 2)),
## ##                                                     value_scale = 1)
## ##     x <- abridge(test_df, span_abridged = 5)
## ##     expect_s3_class(x, "pop_count_age_sex_base")
## ##     expect_identical(x$value, c(5, 1, 10, 2))
## ##     })


## ## test_that("Abridging works for 'mig_net_count_age_sex_df's", {
## ##     test_df <- mig_net_count_age_sex(rbind(expand.grid(age_start = 0:5,
## ##                                                     time_start = 1950:1954,
## ##                                                     sex = c("male"),
## ##                                                     value = 1),
## ##                                          expand.grid(age_start = 0:5,
## ##                                                     time_start = 1950:1954,
## ##                                                     sex = c("female"),
## ##                                                     value = 2)),
## ##                                                     value_scale = 1)
## ##     expect_error(abridge(test_df, span_abridged = 5),
## ##                  "The aggregate of 'x' cannot be coerced to the class in argument 'out_class'")
## ##     expect_s3_class(abridge(test_df, span_abridged = 5,
## ##                             out_class = "demog_change_component_df"),
## ##                     "demog_change_component_df")


## ##     x <- abridge(test_df, span_abridged = 5, time_span = 5)
## ##     expect_s3_class(x, "mig_net_count_age_sex")
## ##     expect_identical(x$value, c(25, 5, 50, 10))
## ##     })


## ## test_that("Abridging works for 'life_table_df's", {
## ##     test_df <- life_table_age_sex(wpp_input_example$life_table_age_sex)

## ##     x <- test_df

## ##     abr_1 <- subset_indicator(as_demog_change_component_df(x),
## ##                               c("lt_ndx" ,"lt_lx", "lt_nLx"))
## ##     abr_1 <- abridge(abr_1, span_abridged_abridged = 5, time_span_abridged = 5)

## ##     nax <- aggregate(as.data.frame(abr_1),
## ##                      by = abr_1[, c("age_start", "time_start", "span_abridged", "time_span")],
## ##                      FUN = function(z) {
## ##         z$lt_nLx -




## ##     bob <- subset_indicator(as_ccmpp_input_df(test_df), "lt_ndx")
## ##     bob5 <- abridge(bob, span_abridged_abridged = 5,
## ##                     time_span_abridged = 5)







## ##     lx <- subset(test_df, indicator == "lt_lx")
## ##     nLx <- subset(test_df, indicator == "lt_nLx")
## ##     ex <- subset(test_df, indicator == "lt_ex")

## ##     x_ab <- lt_single2abridged_loop_over_time(lx_single = lx,
## ##                                               nLx_single = nLx,
## ##                                               ex_single = ex,
## ##                                               sex = "female")



## ##     })
