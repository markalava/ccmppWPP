################################################################################
###
### Make example data set for France
###
################################################################################

op <- getOption("ccmppWPP.suppress_S3_class_messages")
options(ccmppWPP.suppress_S3_class_messages = TRUE)
on.exit(options(ccmppWPP.suppress_S3_class_messages = op))

library(magrittr)

data("france_wpp_1950_2020_population_data", package = "ccmppWPP")
data("census_years", package = "ccmppWPP")

### Make all have the same years

france_times <-
    lapply(france_wpp_1950_2020_population_data$ccmppWPP_inputs[
                                       c("life_table_age_sex", "fert_rate_age_f",
                                         "srb", "mig_net_count_age_sex",
                                         "mig_net_rate_age_sex", "mig_net_count_tot_b",
                                         "mig_parameter")],
           function(z) ccmppWPP::times(ccmppWPP::ccmpp_input_df(z)))
france_common_times <- Reduce("intersect", france_times)

france_data_ccmpp_input_list <-
    lapply(france_wpp_1950_2020_population_data$ccmppWPP_inputs,
           function(z, common_times) {
        z <- ccmppWPP::ccmpp_input_df(z)
        t_z <- ccmppWPP::times(z)
        common_times <- t_z[t_z %in% common_times]
        ccmppWPP::subset_time(z, common_times)
    }, common_times = france_common_times) %>%
    ccmppWPP::as_ccmpp_input_list()

france_data_pop_count_age_sex_reference <-
    ccmppWPP::subset_time(
        ccmppWPP::as_demog_change_component_df(france_wpp_1950_2020_population_data$pop_count_age_sex_reference),
        ccmppWPP::times(ccmppWPP::pop_count_base_component(france_data_ccmpp_input_list)),
        include = FALSE)

### France has had a rolling census since 2004. Keep only the
### mid-point year in the window.

france_census_years <-
    subset(census_years, LocName == "France")$ReferencePeriod[-1] %>%
                                # first element is the baseline year
    sapply(function(z) {
        spl <- as.numeric(unlist(strsplit(z, "-")))
        if (identical(length(spl), 1L)) return(spl)
        else if (identical(length(spl), 2L))
            ## return(seq(from = spl[1], to = spl[2], by = 1))
            return(floor(mean(spl)))
        else stop("length not 1 or 2.")
    }) %>%
    unlist() %>% unique()

france_data_pop_count_age_sex_census_years <-
    france_wpp_1950_2020_population_data$pop_count_age_sex_reference %>%
    ccmppWPP::as_demog_change_component_df() %>%
    ccmppWPP::subset_time(france_census_years)

### Use it

usethis::use_data(france_data_ccmpp_input_list, france_data_pop_count_age_sex_census_years,
                  overwrite = TRUE)
