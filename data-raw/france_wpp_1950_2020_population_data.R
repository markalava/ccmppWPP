
## op <- options(timeout = 10000)
## library(ccmppWPP)

## ds_all <- DDSQLtools::get_datasources(shortNames = c("EuroStat", "HFC-STAT", "HFD", "HMD"))
## france_wpp_1950_2020_population_data <-
##     DDextract_ccmppWPPinputs_tier1(LocID = "France",
##                                        times = 1950:2020,
##                                        times_censored_common = FALSE,
##                                        data_source_pop = c("HMD", "EuroStat"),
##                                        data_source_mort = c("HMD", "EuroStat"),
##                                        data_source_fert = c("HFD", "EuroStat", "HFC-STAT"),
##                                        data_source_year_hmd = max(ds_all[ds_all$ShortName == "HMD",]$Year),
##                                        data_source_year_hfd = max(ds_all[ds_all$ShortName == "HFD",]$Year),
##                                        data_source_year_eurostat =
##                                            max(ds_all[ds_all$ShortName == "EuroStat",]$Year),
##                                        revision = "test",
##                                        variant = "estimates")

## save(france_wpp_1950_2020_population_data,
##      file = "../data/france_wpp_1950_2020_population_data.rda")

## options(op)
