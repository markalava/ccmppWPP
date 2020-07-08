setwd(here::here())

devtools::document()
devtools::load_all()

source("tests/testthat/helper_S3_class_demog_change_component_df.R")

for(x in dir(path = "tests/testthat", pattern = "^test_S3_.*\\.R$")) {
    source(file.path("tests/testthat", x))
}



## cat(paste0("source(", dir(path = "tests/testthat", pattern = "^test_S3_.*\\.R$"), ")\n", collapse = ""))

source("tests/testthat/test_S3_class_ccmpp_input_df.R")
source("tests/testthat/test_S3_class_demog_change_component_df.R")
source("tests/testthat/test_S3_class_fert_rate_input_df.R")
source("tests/testthat/test_S3_class_mig_net_count_input_df.R")
source("tests/testthat/test_S3_class_mig_net_count_tot_input_df.R")
source("tests/testthat/test_S3_class_mig_net_rate_input_df.R")
source("tests/testthat/test_S3_class_pop_count_base_input_df.R")
source("tests/testthat/test_S3_class_srb_input_df.R")
source("tests/testthat/test_S3_class_surv_ratio_input_df.R")
source("tests/testthat/test_S3_classes_demog_change_component_df_utilities.R")
source("tests/testthat/test_S3_internals.R")
source("tests/testthat/test_S3_methods_ccmpp_input_df.R")
source("tests/testthat/test_S3_methods_demog_change_component_df.R")
source("tests/testthat/test_S3_methods_fert_rate_input_df.R")
source("tests/testthat/test_S3_methods_pop_count_base_input_df.R")
source("tests/testthat/test_S3_methods_surv_ratio_input_df.R")
