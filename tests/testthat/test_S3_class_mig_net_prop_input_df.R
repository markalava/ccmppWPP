context("Test methods for S3 class 'mig_net_prop_age_sex'")

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


## test_that("valid member created", {
##     skip("This is actually done in 'test_S3_class_mig_net_count_input.R'")

## })

test_that("subsetting works", {
    x <- ccmpp_input_list_example
    x <- mig_net_prop_age_sex(x)

    y <- subset_sex(x, "female")
    expect_s3_class(y, "mig_net_prop_age_sex")
    expect_identical(demog_change_component_dims(y),
                     c("time", "sex", "age"))
    })
