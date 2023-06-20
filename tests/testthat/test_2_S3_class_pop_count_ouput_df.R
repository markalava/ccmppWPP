

test_that("valid member created from a data frame", {
    x <- data.frame(
        expand.grid(time_start = 1950:1954, age_start = 0:4, sex = c("male", "female"),
                     time_span = 0, age_span = 1),
        value = 1:50)
    x[x$age_start == 4, "age_span"] <- 1000
    expect_s3_class(pop_count_age_sex(x), "pop_count_age_sex")
})

test_that("valid member created from a ccmpp input list", {
    x <- ccmpp_input_list(pop_count_age_sex_base = wpp_input_example$pop_count_age_sex_base,
                     life_table_age_sex = wpp_input_example$life_table_age_sex,
                     fert_rate_age_f = wpp_input_example$fert_rate_age_f,
                     srb = wpp_input_example$srb,
                     mig_net_count_age_sex = wpp_input_example$mig_net_count_age_sex,
                     mig_net_rate_age_sex = wpp_input_example$mig_net_rate_age_sex,
                     mig_net_count_tot_b = wpp_input_example$mig_net_count_tot_b,
                     mig_parameter = wpp_input_example$mig_parameter)
    expect_s3_class(pop_count_age_sex(x),
                    "pop_count_age_sex")
})


### MAKE OBJECT AVAILABLE TO REMAINDER OF TESTS

pop_count_age_sex_df <-
    data.frame(expand.grid(time_start = 1950:1954, age_start = 0:4, sex = c("male", "female"),
                           time_span = 0, age_span = 1), value = 1:50)
pop_count_age_sex_df[pop_count_age_sex_df$age_start == 4, "age_span"] <- 1000

pop_count_age_sex_df <- pop_count_age_sex(pop_count_age_sex_df)


## CHECKS for 'ccmpp_output_df's: ---->

test_that("time_span not zero detected", {
    x <- pop_count_age_sex_df
    x$time_span[1] <- 1
    expect_error(pop_count_age_sex(x),
                 "Objects of this class must have all 'time_span' values = 0")
})


test_that("Inconsistency between spans and starts are detected", {
    x <- pop_count_age_sex_df
    x[1, "age_span"] <- 2
    expect_error(pop_count_age_sex(x),
                 "Spacings between each 'x\\$age_start' do not equal the corresponding values of 'x\\$age_span")

    x <- pop_count_age_sex_df
    x <- x[!x$age_start == 1, ]
    expect_error(pop_count_age_sex(x),
                 "Spacings between each 'x\\$age_start' do not equal the corresponding values of 'x\\$age_span")
})


test_that("Spans not all equal", {
    x <- pop_count_age_sex_df

    ## OK to have age_span 2
    y <- x[x$age_start %in% seq(from = min(x$age_start),
                                to = max(x$age_start),
                                by = 2),]
    y$age_span <- 2
    y[y$age_start == 4, "age_span"] <- 1000
    expect_error(pop_count_age_sex(y), NA)

    ## Not OK to have multiple age_spans
    z <- rbind(y, x[x$age_start == 1,])
    z[z$age_start == 0, "age_span"] <- 1
    expect_error(pop_count_age_sex(z), "All spans must be equal to a \\*single\\*")
})


test_that("Non-zero age detected", {
    y <- pop_count_age_sex_df
    z <- subset(y, age_start > 0)
    z <- ccmppWPP:::new_pop_count_age_sex(z,
                                          value_scale = 1,
                                          age_span = age_span(y))
    expect_error(validate_ccmppWPP_object(z),
                 "'age_start' does not start at '0'")
})


## OTHER checks --->

test_that("Invalid 'value's are caught", {
    x <- pop_count_age_sex_df
    x[1, "value"]  <- -1
    expect_error(pop_count_age_sex(x),
                 "'value' column has negative elements")
})
