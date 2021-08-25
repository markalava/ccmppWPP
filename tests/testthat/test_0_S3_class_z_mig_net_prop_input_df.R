

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


test_that("Can create 'mig_net_prop' from 'mig_net_count'", {
    x <- mig_net_count_age_sex(ccmpp_input_list_example)
    y <- pop_count_age_sex(ccmpp_input_list_example)
    expect_s3_class(mig_net_prop_age_sex(x, y), "mig_net_prop_age_sex")
})


test_that("Can create 'mig_net_prop' from 'ccmpp_input_list'", {
    expect_s3_class(mig_net_prop_age_sex(ccmpp_input_list_example), "mig_net_prop_age_sex")
    })


test_that("subsetting works", {
    x <- ccmpp_input_list_example
    x <- mig_net_prop_age_sex(x)

    y <- subset_age(x, 0:10)
    expect_s3_class(y, "mig_net_prop_age_sex")
    expect_identical(demog_change_component_dims(y),
                     c("time", "sex", "age"))
    expect_identical(as.numeric(unique(y$age_start)), as.numeric(0:10))
})


test_that("unusual values trigger warning", {
    x <- data.frame(expand.grid(age_start = 0:4, time_start = 1950:1954,
                                sex = c("male", "female")),
                    value = 0.01)
    x[c(2,5), "value"] <- ccmppWPP:::get_mig_net_prop_value_warning_threshold() * 2
    expect_warning(mig_net_prop_age_sex(x),
                   "2, 5")

    ## pop_count_age_sex <-
    ##         rbind(ccmpp_input_list_example$pop_count_age_sex_base,
    ##               data_reshape_ccmpp_output(
    ##                   project_ccmpp_loop_over_time(
    ##                       indata = ccmpp_input_list_example))$pop_count_age_sex)

    ## mig_net_count_component(ccmpp_input_list_example)
})


test_that("'mig_net_count' can be created from 'mig_net_prop'", {
    prop <- mig_net_prop_age_sex(ccmpp_input_list_example)
    pop_count <- pop_count_age_sex(ccmpp_input_list_example)
    expect_s3_class(mig_net_count_age_sex(prop, pop_count),
                    "mig_net_count_age_sex")
    })


## test_that("'mig_net_prop' can be replaced in a 'ccmpp_input_list'", {
##     prop_new <- mig_net_prop_age_sex(ccmpp_input_list_example)
##     values(prop_new) <- values(prop_new) * 0.8

##     x <- ccmpp_input_list_example




##     debug(data_reshape_ccmpp_output)
##     debug(data_gather_ccmpp_output)

##     mig_net_prop_age_sex(x) <- prop_new

