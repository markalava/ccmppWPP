context("Test methods for S3 class 'ccmpp_input_df'")

### OBJECTS NEEDED (already tested)

ccmpp_input_df_time_age_sex <-
    ccmpp_input_df(S3_demog_change_component_time_age_sex_test_df,
                   dimensions = c("time", "age", "sex"))



test_that("subsetting drops classes", {

    y <- ccmpp_input_df_time_age_sex

    ## NB Warning only issued if run from top level
    expect_not_s3_class(y[, "age_start"], "ccmpp_input_df")
    expect_not_s3_class(y$age_start, "ccmpp_input_df")
    expect_not_s3_class(y[["age_start"]], "ccmpp_input_df")

    expect_not_s3_class(y[1:nrow(y), colnames(y)], "ccmpp_input_df")
})


test_that("replacement with valid columns drops classes", {

    z <- ccmpp_input_df_time_age_sex
    z[, "age_start"] <- z$age_start
    expect_not_s3_class(z, "ccmpp_input_df")
    z <- ccmpp_input_df_time_age_sex
    z$age_start <- z$age_start
    expect_not_s3_class(z, "ccmpp_input_df")
    z <- ccmpp_input_df_time_age_sex
    z[["age_start"]] <- z$age_start
    expect_not_s3_class(z, "ccmpp_input_df")

    z <- ccmpp_input_df_time_age_sex
    z[, "time_start"] <- z$time_start
    expect_not_s3_class(z, "ccmpp_input_df")
    z <- ccmpp_input_df_time_age_sex
    z$time_start <- z$time_start
    expect_not_s3_class(z, "ccmpp_input_df")
    z <- ccmpp_input_df_time_age_sex
    z[["time_start"]] <- z$time_start
    expect_not_s3_class(z, "ccmpp_input_df")

    z <- ccmpp_input_df_time_age_sex
    z[, "sex"] <- z$sex
    expect_not_s3_class(z, "ccmpp_input_df")
    z <- ccmpp_input_df_time_age_sex
    z$sex <- z$sex
    expect_not_s3_class(z, "ccmpp_input_df")
    z <- ccmpp_input_df_time_age_sex
    z[["sex"]] <- z$sex
    expect_not_s3_class(z, "ccmpp_input_df")
 })


test_that("'rbind()' drops class", {
    z <- rbind(ccmpp_input_df_time_age_sex, ccmpp_input_df_time_age_sex)
    expect_not_s3_class(z, "ccmpp_input_df")
    })
