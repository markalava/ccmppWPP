

test_that("valid member created", {
    expect_s3_class(pop_count_age_sex_base(wpp_input_example$pop_count_age_sex_base),
                    "pop_count_age_sex_base")
})


### MAKE OBJECT AVAILABLE TO REMAINDER OF TESTS

pop_count_base_input_df_time_age_sex <-
    pop_count_age_sex_base(wpp_input_example$pop_count_age_sex_base)


## CHECKS for 'ccmpp_input_df's: ---->

test_that("time_span not zero detected", {
    x <- pop_count_base_input_df_time_age_sex
    x$time_span[1] <- 1
    expect_error(pop_count_age_sex_base(x),
                 "Objects of this class must have all 'time_span' values = 0")
})


test_that("Inconsistency between spans and starts are detected", {
    x <- pop_count_base_input_df_time_age_sex
    x[1, "age_span"] <- 2
    expect_error(pop_count_age_sex_base(x),
                 "Spacings between each 'x\\$age_start' do not equal the corresponding values of 'x\\$age_span")

    x <- pop_count_base_input_df_time_age_sex
    x <- x[!x$age_start == 1, ]
    expect_error(pop_count_age_sex_base(x),
                 "Spacings between each 'x\\$age_start' do not equal the corresponding values of 'x\\$age_span")
})


test_that("Spans not all equal", {
    x <- pop_count_base_input_df_time_age_sex

    ## OK to have age_span 2
    y <- x[x$age_start %in% seq(from = min(x$age_start),
                                to = max(x$age_start),
                                by = 2),]
    y$age_span <- 2
    expect_error(pop_count_age_sex_base(y), NA)

    ## Not OK to have multiple age_spans
    z <- rbind(y, x[x$age_start == 1,])
    z[z$age_start == 0, "age_span"] <- 1
    expect_error(pop_count_age_sex_base(z), "All spans must be equal to a \\*single\\* common value")
})


test_that("Non-zero age detected", {
    y <- pop_count_base_input_df_time_age_sex
    z <- subset(y, age_start > 0)
    z <- ccmppWPP:::new_pop_count_age_sex_base(z,
                                               value_scale = 1,
                                     age_span = age_span(y))
    expect_error(validate_ccmppWPP_object(z),
                 "'age_start' does not start at '0'")
})


## CHECKS for 'pop_count_age_sex_base_df's --->
## (Note that the check that time_span == 0 is done by the 'ccmpp_input_df' method).

test_that("More than one time period detected", {
    y <- pop_count_base_input_df_time_age_sex
    w <- transform(y, time_start = times(y) + 1)
    z <- ccmppWPP:::new_pop_count_age_sex_base(sort_demog_change_component_df(rbind(y, w)),
                                               value_scale = 1,
                                     age_span = age_span(y))
    expect_error(validate_ccmppWPP_object(z),
                 "has more than one unique value")
    })


## OTHER checks --->

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
