context("Test methods for S3 class 'mig_net_count_age_sex'")

### OBJECTS NEEDED (already tested)

mig_net_count_input_df_time_age_sex <-
    mig_net_count_age_sex(wpp_input_example$mig_net_count_age_sex)

ccmpp_input_list_example <-
    ccmpp_input_list(pop_count_age_sex_base = wpp_input_example$pop_count_age_sex_base,
                     life_table_age_sex = wpp_input_example$life_table_age_sex,
                     fert_rate_age_f = wpp_input_example$fert_rate_age_f,
                     srb = wpp_input_example$srb,
                     mig_net_count_age_sex = wpp_input_example$mig_net_count_age_sex,
                     mig_net_rate_age_sex = wpp_input_example$mig_net_rate_age_sex,
                     mig_net_count_tot_b = wpp_input_example$mig_net_count_tot_b,
                     mig_parameter = wpp_input_example$mig_parameter)



test_that("valid member created", {
    expect_s3_class(mig_net_count_input_df_time_age_sex,
                    "mig_net_count_age_sex")
    })


test_that("Non-zero age detected", {
    x <- mig_net_count_input_df_time_age_sex
    y <- subset(x, age_start > 0)
    z <- ccmppWPP:::new_mig_net_count_age_sex(y, value_scale = 1, age_span = 1, time_span = 1)
    expect_error(validate_ccmppWPP_object(z),
                 "'age_start' does not start at '0'")
    expect_error(mig_net_count_age_sex(y),
                 "'age_start' does not start at '0'")
})


test_that("indicator dimension detected", {
    x <- mig_net_count_input_df_time_age_sex
    y <- cbind(x, indicator = "ltX")
    z <- ccmppWPP:::new_mig_net_count_age_sex(y, value_scale = 1, age_span = 1, time_span = 1)
    attr(z, "dimensions") <- c(attr(z, "dimensions"), "indicator")
    expect_error(validate_ccmppWPP_object(z),
                 "must have dimension")

    expect_false("indicator" %in% colnames(mig_net_count_age_sex(y)))
})


test_that("indicator column removed", {
    y <- mig_net_count_input_df_time_age_sex
    z <- cbind(y, indicator = "ltX")
    z <- mig_net_count_age_sex(z)
    expect_false("indicator" %in% colnames(z))
})


test_that("'mig_net_count' can be created from 'mig_net_prop'", {

    x <- ccmpp_input_list_example
    expect_s3_class(mig_net_prop_age_sex(x),
                    "mig_net_prop_age_sex")

    pop_count_age_sex <-
        rbind(x$pop_count_age_sex_base,
              data_reshape_ccmpp_output(
                  project_ccmpp_loop_over_time(indata = x))$pop_count_age_sex)
    pop_count_age_sex <-
        pop_count_age_sex[pop_count_age_sex$sex %in% c("male", "female"),]

    expect_s3_class(y <- mig_net_prop_age_sex(mig_net_count_input_df_time_age_sex,
                                         pop_count_age_sex),
                    "mig_net_prop_age_sex")

    expect_s3_class(z <- mig_net_count_age_sex(y, pop_count_age_sex),
                    "mig_net_count_age_sex")

    expect_equal(mig_net_count_input_df_time_age_sex,
                 z)
    })
