load(system.file("testdata", "S3_demog_change_component_time_age_sex_test_df.rda", package = "ccmppWPP"))
load(system.file("testdata", "S3_demog_change_component_time_age_test_df.rda", package = "ccmppWPP"))
load(system.file("testdata", "S3_demog_change_component_time_sex_test_df.rda", package = "ccmppWPP"))
load(system.file("testdata", "S3_demog_change_component_time_test_df.rda", package = "ccmppWPP"))
load(system.file("testdata", "S3_fert_rate_time_age_df.rda", package = "ccmppWPP"))

expect_not_s3_class <- function(object, class, exact = FALSE) {
        expect_error(expect_s3_class(object = object, class = class,
                                     exact = exact))
    }


dcc_df_time_age_sex <-
    demog_change_component_df(S3_demog_change_component_time_age_sex_test_df,
                              dimensions = c("time", "age", "sex"))

ccmpp_input_df_time_age_sex <-
    ccmpp_input_df(S3_demog_change_component_time_age_sex_test_df,
                   dimensions = c("time", "age", "sex"))

fert_rate_input_df_time_age <-
    fert_rate_input_df(S3_fert_rate_time_age_df)

survival_ratio_input_df_time_age_sex <- ccmpp_input_df_time_age_sex
survival_ratio_input_df_time_age_sex$value <-
    abs(survival_ratio_input_df_time_age_sex$value /
        max(survival_ratio_input_df_time_age_sex$value))
survival_ratio_input_df_time_age_sex <-
    survival_ratio_input_df(survival_ratio_input_df_time_age_sex)

pop_count_base_input_df_time_age_sex <-
    pop_count_base_input_df(wpp_input_example$pop_count_age_sex_base)

srb_input_df_time <- srb_input_df(wpp_input_example$srb)

mig_net_rate_input_df_time_age_sex <-
    mig_net_rate_input_df(wpp_input_example$mig_net_rate_age_sex)

mig_net_count_input_df_time_age_sex <-
    mig_net_count_input_df(wpp_input_example$mig_net_count_age_sex)

mig_net_count_tot_input_df_time <-
    mig_net_count_tot_input_df(wpp_input_example$mig_net_count_tot_b)
