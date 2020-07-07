setwd(here::here())

devtools::document()
devtools::load_all()

source("tests/testthat/helper_S3_class_demog_change_component_df.R")

for(x in dir(path = "tests/testthat", pattern = "^test_S3_.*\\.R$")) {
    source(file.path("tests/testthat", x))
}
