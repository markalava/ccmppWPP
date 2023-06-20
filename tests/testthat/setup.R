
op <- options(ccmppWPP.suppress_S3_class_messages = TRUE, timeout = 10000)

withr::defer(options(op), teardown_env())



load(system.file("testdata", "S3_demog_change_component_time_age_sex_test_df.rda", package = "ccmppWPP"))
load(system.file("testdata", "S3_demog_change_component_time_age_test_df.rda", package = "ccmppWPP"))
load(system.file("testdata", "S3_demog_change_component_time_sex_test_df.rda", package = "ccmppWPP"))
load(system.file("testdata", "S3_demog_change_component_time_test_df.rda", package = "ccmppWPP"))
load(system.file("testdata", "S3_fert_rate_time_age_df.rda", package = "ccmppWPP"))
load(system.file("testdata", "S3_pop_count_age_sex_reference.rda", package = "ccmppWPP"))

if(.Platform$OS.type == "unix") {
    OS_null_file_string <- "/dev/null"
} else {
    OS_null_file_string <- "NUL"
}
