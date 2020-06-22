################################################################################
###
### S3 Classes for CCMPP Objects --- TESTS
###
################################################################################

###-----------------------------------------------------------------------------
### * Set Up

load("example_datasets/wpp_input_example.rda")
source("S3_classes.R")
source("S3_methods.R")

x <- wpp_input_example$mig_net_count_age_sex


## Should not fail:
y <- vital_rate_df_time_age_sex(x)

## Should not fail:
x <- x[, c("time_start", "sex", "age_start", "value")]
y <- vital_rate_df_time_age_sex(x, age_span = 1, time_span = 1)

###-----------------------------------------------------------------------------
### * Subsetting

## Should return 'TRUE' and a warning:
!inherits(y[, "age_start"], "vital_rate_df_time_age_sex")
!inherits(y$age_start, "vital_rate_df_time_age_sex")
!inherits(y[["age_start"]], "vital_rate_df_time_age_sex")

###-----------------------------------------------------------------------------
### * Replacing elements

## Should not fail:
y[, "age_start"] <- y$age_start
y$age_start <- y$age_start
y[["age_start"]] <- y$age_start
y[, "time_start"] <- y$time_start
y$time_start <- y$time_start
y[["time_start"]] <- y$time_start
y[, "sex"] <- y$sex
y$sex <- y$sex
y[["sex"]] <- y$sex

## Fail: Not sorted properly
y[, "age_start"] <- rev(y$age_start)
y$age_start <- rev(y$age_start)
y[["age_start"]] <- rev(y$age_start)
y[, "time_start"] <- rev(y$time_start)
y$time_start <- rev(y$time_start)
y[["time_start"]] <- rev(y$time_start)
y[, "sex"] <- rep(c("male", "female"), nrow(y) / 2)
y$sex <- rep(c("male", "female"), nrow(y) / 2)
y[["sex"]] <- rep(c("male", "female"), nrow(y) / 2)

## Fail: Superfluous columns
y$source <- "census"
y[, "source"] <- "census"
y[["source"]] <- "census"
