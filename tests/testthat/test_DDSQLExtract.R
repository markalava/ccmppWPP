data("france_wpp_1950_2020_population_data", package = "ccmppWPP")
data("census_years", package = "ccmppWPP")

ref_pop_years_all <-
   c(1950, 1951, 1952, 1953, 1954, 1955, 1956, 1957, 1958, 1959, 1960,
1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, 1971,
1972, 1973, 1974, 1975, 1976, 1977, 1978, 1979, 1980, 1981, 1982,
1983, 1984, 1985, 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993,
1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004,
2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015,
2016, 2017, 2018, 2019)

ref_pop_years_census <-
    c("1954", "1962", "1968", "1975", "1982", "1990", "1999", "2004-2008",
"2009-2013", "2011-2015")


test_that("Census years can be found", {
    expect_equal(get_census_years(250),
                 c("1954", "1962", "1968", "1975", "1982", "1990", "1999", "2004-2008",
                   "2009-2013", "2011-2015"))
})


test_that("Reference population years can be parsed", {
    x <- get_census_years(250)
    expect_equal(x, ref_pop_years_census)

    expect_error(parse_census_years_ranges(x), NA)
})


test_that("Reference population years can be determined from DDSQL extract", {
    x <- ccmppWPP:::get_pop_count_reference_times(france_wpp_1950_2020_population_data)
    expect_equal(x, ref_pop_years_all)

    x <- ccmppWPP:::exclude_baseline_pop_count_times(france_wpp_1950_2020_population_data,
                                          times = c(1950, 1951))
    expect_equal(x, 1951)
})


test_that("Census years can be determined from DDSQL extract", {
    x <- get_census_years(france_wpp_1950_2020_population_data)
    expect_equal(x, ref_pop_years_census)
    })


test_that("Reference population data can be created", {
    x <- DDextract_get_pop_count_age_sex_reference(x = france_wpp_1950_2020_population_data)
    expect_s3_class(x, "demog_change_component_df")
    expect_equal(times(x), ref_pop_years_all)

    x <- DDextract_get_pop_count_age_sex_reference(
        france_wpp_1950_2020_population_data, "census_excl_baseline")
    expect_s3_class(x, "demog_change_component_df")
    expect_equal(times(x), ccmppWPP:::exclude_baseline_pop_count_times(
                                          france_wpp_1950_2020_population_data,
                               parse_census_years_ranges(ref_pop_years_census)))
})


test_that("DDextract can be coerced to ccmpp_input_df objects", {
    x <- lapply(france_wpp_1950_2020_population_data$ccmppWPP_inputs,
                function(z) is_ccmpp_input_df(as_ccmpp_input_df(z)))
    expect_true(all(unlist(x)))
    })



test_that("Raw data can be coerced to ccmpp_input_list objects", {
    x <- DDextract_get_ccmpp_input_list(france_wpp_1950_2020_population_data)
    expect_s3_class(as_ccmpp_input_list(x), "ccmpp_input_list")

    tx <- unique(unlist(lapply(x, "times")))
    expect_equal(tx, c(1950, 1951, 1952, 1953, 1954, 1955, 1956, 1957, 1958, 1959,
                       1960, 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970,
                       1971, 1972, 1973, 1974, 1975, 1976, 1977, 1978, 1979, 1980, 1981,
                       1982, 1983, 1984, 1985, 1986, 1987, 1988, 1989, 1990, 1991, 1992,
                       1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003,
                       2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014,
                       2015, 2016, 2017, 2018))
})
