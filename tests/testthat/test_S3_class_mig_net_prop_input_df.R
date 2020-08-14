context("Test methods for S3 class 'mig_net_prop_age_sex'")

test_that("valid member created", {
    expect_s3_class(mig_net_prop_input_df_time_age_sex,
                    "mig_net_prop_age_sex")

    x <- ccmpp_input_list_example

        pop_count_age_sex <-
            rbind(x$pop_count_age_sex_base,
                  data_reshape_ccmpp_output(
                      project_ccmpp_loop_over_time(indata = x))$pop_count_age_sex)
    pop_count_age_sex <-
        pop_count_age_sex[pop_count_age_sex$sex %in% c("male", "female"),]

    x <- mig_net_count_input_df_time_age_sex
    stopifnot(identical(nrow(x), nrow(pop_count_age_sex)))
        stopifnot(identical(sort(as.numeric(x$age_start)),
                            sort(as.numeric(pop_count_age_sex$age_start))))
        stopifnot(identical(sort(as.character(x$sex)),
                            sort(as.character(pop_count_age_sex$sex))))
        stopifnot(identical(sort(as.numeric(x$time_start)),
                            sort(as.numeric(pop_count_age_sex$time_start))))
        x <- merge(x = x, y = pop_count_age_sex,
                         by = c("age_start", "sex", "time_start"))





})

