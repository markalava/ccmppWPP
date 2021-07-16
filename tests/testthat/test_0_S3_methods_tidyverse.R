

### OBJECTS NEEDED (tested already)

dcc_df_time_age_sex <-
    demog_change_component_df(S3_demog_change_component_time_age_sex_test_df,
                              dimensions = c("time", "age", "sex"))


test_that("Classes and attributes are dropped by dplyr functions", {
    ## Only a few .. too many to test all!
    if (!requireNamespace("dplyr", quietly = TRUE)) skip("dplyr not installed.")

    x <- dplyr::arrange(dcc_df_time_age_sex, time_start)
    expect_not_s3_class(x, "demog_change_component_df")

    x <- dplyr::filter(dcc_df_time_age_sex, time_start == 1950)
    expect_not_s3_class(x, "demog_change_component_df")

    x <- dplyr::mutate(dcc_df_time_age_sex, time_start = time_start)
    expect_not_s3_class(x, "demog_change_component_df")
    })

test_that("Classes and attributes are dropped by tidyr functions", {
    ## Only a few .. too many to test all!
    if (!requireNamespace("tidyr", quietly = TRUE)) skip("tidyr not installed.")

    x <- tidyr::separate(dcc_df_time_age_sex,
                         sex, sep = "a", into = c("a", "b"))
    expect_not_s3_class(x, "demog_change_component_df")

    x <- tidyr::unite(dcc_df_time_age_sex,
                      age_start, sex, sep = "_")
    expect_not_s3_class(x, "demog_change_component_df")
    })

test_that("tibbles are handled", {
    if (!requireNamespace("tibble", quietly = TRUE))
        skip("'tibble' not installed.")
    expect_s3_class(demog_change_component_df(
        tibble::as_tibble(S3_demog_change_component_time_age_sex_test_df)),
        "demog_change_component_df")
})
