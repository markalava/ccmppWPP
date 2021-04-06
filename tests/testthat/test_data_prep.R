


## Load required data
data("france_wpp_1950_2020_population_data")


test_that("Raw data can be coerced to ccmpp_input_df objects", {
    x <- lapply(france_wpp_1950_2020_population_data$ccmppWPP_inputs,
                function(z) is_ccmpp_input_df(as_ccmpp_input_df(z)))
    expect_true(all(unlist(x)))
    })


test_that("Raw data can be coerced to ccmpp_input_list objects", {
    x <- DDextract_to_ccmpp_input_list(france_wpp_1950_2020_population_data)
    expect_s3_class(as_ccmpp_input_list(x), "ccmpp_input_list")

    tx <- unique(unlist(lapply(x, "times")))
    expect_equal(tx,
                 c(1950, 1951, 1952, 1953, 1954, 1955, 1956, 1957, 1958, 1959,
                   1960, 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970,
                   1971, 1972, 1973, 1974, 1975, 1976, 1977, 1978, 1979, 1980, 1981,
                   1982, 1983, 1984, 1985, 1986, 1987, 1988, 1989, 1990, 1991, 1992,
                   1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003,
                   2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014,
                   2015, 2016))
    })

