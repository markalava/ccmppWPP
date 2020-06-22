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
### * Creation Object

## Fail: not a data frame
vital_rate_df_time_age_sex(as.list(x),
                      age_span = attr(y, "age_span"),
                      time_span = attr(y, "time_span"))

## Fail: not a data frame
vital_rate_df_time_age_sex(as.matrix(x),
                      age_span = attr(y, "age_span"),
                      time_span = attr(y, "time_span"))

## Fail: not a data frame
vital_rate_df_time_age_sex(data.matrix(x),
                      age_span = attr(y, "age_span"),
                      time_span = attr(y, "time_span"))

###-----------------------------------------------------------------------------
### * Columns

## Fail: Missing columns
vital_rate_df_time_age_sex(x[, c("sex",
                                 "age_start", "value")],
                           age_span = 1, time_span = 1)
vital_rate_df_time_age_sex(x[, c("time_start",
                                 "age_start", "value")],
                           age_span = 1, time_span = 1)
vital_rate_df_time_age_sex(x[, c("time_start", "sex",
                                 "value")],
                           age_span = 1, time_span = 1)
vital_rate_df_time_age_sex(x[, c("time_start", "sex",
                                 "age_start")],
                      age_span = 1, time_span = 1)

## Superfluous columns
z <- data.frame(x, source = "census")

## No fail: Automatically removes column
isTRUE(!("source" %in%
         colnames(vital_rate_df_time_age_sex(z, age_span = 1, time_span = 1))))

## Fail: Catches the extra column
z <- new_vital_rate_df_time_age_sex(z, age_span = 1, time_span = 1)
validate_vital_rate_df_time_age_sex(z)

## Should not Fail: Should re-sort correctly
z <- x
z[, "age_start"] <- rev(z$age_start)
z <- vital_rate_df_time_age_sex(z, age_span = 1, time_span = 1)
identical(z$age_start, x$age_start) #'TRUE'

z <- x
z[, "time_start"] <- rev(z$time_start)
z <- vital_rate_df_time_age_sex(z, age_span = 1, time_span = 1)
identical(z$time_start, x$time_start) #'TRUE'

z <- x[order(x$time_start, x$age_start),] #sex = male,female,male,female,...
z <- vital_rate_df_time_age_sex(z, age_span = 1, time_span = 1)
identical(z$sex, x$sex) #'TRUE'
