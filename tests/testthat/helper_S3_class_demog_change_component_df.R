op <- options()
options(ccmppWPP.suppress_S3_class_messages = TRUE)

load(system.file("testdata", "S3_demog_change_component_time_age_sex_test_df.rda", package = "ccmppWPP"))
load(system.file("testdata", "S3_demog_change_component_time_age_test_df.rda", package = "ccmppWPP"))
load(system.file("testdata", "S3_demog_change_component_time_sex_test_df.rda", package = "ccmppWPP"))
load(system.file("testdata", "S3_demog_change_component_time_test_df.rda", package = "ccmppWPP"))
load(system.file("testdata", "S3_fert_rate_time_age_df.rda", package = "ccmppWPP"))

if(.Platform$OS.type == "unix") {
    OS_null_file_string <- "/dev/null"
} else {
    OS_null_file_string <- "NUL"
}

expect_not_s3_class <- function(object, class, exact = FALSE) {
        expect_error(expect_s3_class(object = object, class = class,
                                     exact = exact))
    }

######### !!!!! MOVE THE REST OF THESE TO THE APPROPRIATE 'test_' files.

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




options(op)
