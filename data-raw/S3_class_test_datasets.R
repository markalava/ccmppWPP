## Create data sets for testing S3 classes

## data("wpp_input_example")

## S3_demog_change_component_time_age_sex_test_df <- wpp_input_example$mig_net_count_age_sex

## S3_demog_change_component_time_age_test_df <-
##     subset(S3_demog_change_component_time_age_sex_test_df,
##            sex == "female", select = -sex)

## S3_demog_change_component_time_sex_test_df <-
##     subset(S3_demog_change_component_time_age_sex_test_df,
##            age_start == 0, select = -c(age_start, age_span))
## save(S3_demog_change_component_time_sex_test_df,
##      file = "inst/testdata/S3_demog_change_component_time_sex_test_df.rda")

## S3_demog_change_component_time_test_df <-
##     subset(S3_demog_change_component_time_age_sex_test_df,
##            age_start == 0 & sex == "female",
##            select = -c(age_start, age_span, sex))
## save(S3_demog_change_component_time_test_df,
##      file = "inst/testdata/S3_demog_change_component_time_test_df.rda")
