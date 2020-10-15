context("Test methods for S3 class 'ccmpp_input_list'")

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

