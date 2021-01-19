context("Test methods for S3 class 'ccmpp_input_list'")

### OBJECTS NEEDED (already tested)

ccmpp_input_list_example <-
    ccmpp_input_list(pop_count_age_sex_base = wpp_input_example$pop_count_age_sex_base,
                     life_table_age_sex = wpp_input_example$life_table_age_sex,
                     fert_rate_age_f = wpp_input_example$fert_rate_age_f,
                     srb = wpp_input_example$srb,
                     mig_net_count_age_sex = wpp_input_example$mig_net_count_age_sex,
                     mig_net_rate_age_sex = wpp_input_example$mig_net_rate_age_sex,
                     mig_net_count_tot_b = wpp_input_example$mig_net_count_tot_b,
                     mig_parameter = wpp_input_example$mig_parameter)


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

