

test_that("valid member created", {
    expect_s3_class(pop_count_age_sex_reference(S3_pop_count_age_sex_reference),
                    "pop_count_age_sex_reference")
})


### MAKE OBJECT AVAILABLE TO REMAINDER OF TESTS

pop_count_reference_input_df_time_age_sex <-
    pop_count_age_sex_reference(S3_pop_count_age_sex_reference)


test_that("time_span not zero detected", {
    x <- pop_count_reference_input_df_time_age_sex
    x$time_span[1] <- 1
    expect_error(pop_count_age_sex_reference(x),
                 "Objects of this class must have all 'time_span' values = 0")
})


test_that("time_span attribute set", {
    x <- pop_count_reference_input_df_time_age_sex
    expect_identical(as.numeric(time_span(x)),
                     as.numeric(0))
})


test_that("Spans not all equal", {
    x <- pop_count_reference_input_df_time_age_sex

    ## OK to have age_span 2
    y <- x[x$age_start %in% seq(from = min(x$age_start),
                                to = max(x$age_start),
                                by = 2),]
    y$age_span <- 2
    expect_error(pop_count_age_sex_reference(y), NA)
})


test_that("Invalid 'value's are caught", {
    x <- pop_count_reference_input_df_time_age_sex
    x[1, "value"]  <- -1
    expect_error(pop_count_age_sex_reference(x),
                 "'value' column has negative elements")
})

test_that("Equal sex values are detected", {
    x <- pop_count_reference_input_df_time_age_sex

    y <- x
    y[y$sex == "male", "value"] <- y[y$sex == "female", "value"]
    expect_warning(pop_count_age_sex_reference(y),
                   "Female and male reference population counts are ")

    y <- x
    tol <- 0.5 / 100 * mean(y[y$sex == "female", "value"])
    y[y$sex == "male", "value"] <-
        y[y$sex == "female", "value"] + runif(n = nrow(y[y$sex == "female",]),
                                              min = 1, max = 0.9 * tol)
    expect_warning(pop_count_age_sex_reference(y),
                   "Female and male reference population counts are ")

    })
