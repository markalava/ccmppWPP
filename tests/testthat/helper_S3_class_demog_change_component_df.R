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
    fert_rate_age_f(S3_fert_rate_time_age_df)

life_table_input_df_indicator_time_age_sex <-
    life_table_age_sex(wpp_input_example$life_table_age_sex)

mig_net_count_input_df_time_age_sex <-
    mig_net_count_age_sex(wpp_input_example$mig_net_count_age_sex)

mig_net_count_tot_input_df_time <-
    mig_net_count_tot_b(wpp_input_example$mig_net_count_tot_b)

mig_net_rate_input_df_time_age_sex <-
    mig_net_rate_age_sex(wpp_input_example$mig_net_rate_age_sex)

mig_parameter_input_df_indicator_time <-
    mig_parameter(wpp_input_example$mig_parameter)

pop_count_base_input_df_time_age_sex <-
    pop_count_age_sex_base(wpp_input_example$pop_count_age_sex_base)

srb_time <- srb(wpp_input_example$srb)

survival_ratio_input_df_time_age_sex <-
    subset(wpp_input_example$life_table_age_sex,
           indicator == "lt_Sx", select = -indicator)
survival_ratio_input_df_time_age_sex <-
    survival_ratio_age_sex(survival_ratio_input_df_time_age_sex)

ccmpp_input_list_example <-
    ccmpp_input_list(pop_count_age_sex_base = wpp_input_example$pop_count_age_sex_base,
                     life_table_age_sex = wpp_input_example$life_table_age_sex,
                     fert_rate_age_f = wpp_input_example$fert_rate_age_f,
                     srb = wpp_input_example$srb,
                     mig_net_count_age_sex = wpp_input_example$mig_net_count_age_sex,
                     mig_net_rate_age_sex = wpp_input_example$mig_net_rate_age_sex,
                     mig_net_count_tot_b = wpp_input_example$mig_net_count_tot_b,
                     mig_parameter = wpp_input_example$mig_parameter)
