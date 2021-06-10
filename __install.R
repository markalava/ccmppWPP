################################################################################
###
###  DATE CREATED: 2020-03-06
###
###  AUTHOR: Mark Wheldon
###
###  PROJECT: Population Projection
###
###  DESCRIPTION:
###
###     Build and install package --- mainly for interactive testing
###     and development.
###
################################################################################

###-----------------------------------------------------------------------------
### * Setup

###-----------------------------------------------------------------------------
### * Functions

divider <- function(x) {
    cat("\n\n\n********************************************************************************\n** ", x,
        "\n********************************************************************************\n\n")
}

## From 'example("source", package = "base")
sourceDir <- function(path, trace = TRUE, ...) {
    for (nm in list.files(path, pattern = "[.][RrSsQq]$")) {
        if(trace) cat(nm,":")
        source(file.path(path, nm), ...)
        if(trace) cat("\n")
    }
}

###-----------------------------------------------------------------------------
### * Install and Test

### Make all data
divider("MAKING DATA")
sourceDir("data-raw")

### Document
divider("MAKING DOCUMENTATION")
devtools::document()

### Build
## divider("BUILDING")
## devtools::build(vignettes = TRUE, manual = TRUE)
## devtools::build(binary = TRUE, vignettes = TRUE, manual = TRUE)

### Install
divider("INSTALLING")
## write_sha1_DESC(pkg_dir = ".", git_dir = "..")
devtools::install(build_vignettes = FALSE, upgrade = "never")
## clean_sha1_DESC(pkg_dir = ".")

### Do all tests
divider("RUNNING TESTS")
testthat::test_package(package = "ccmppWPP", reporter = c("summary", "fail"))

## ### Benchmark
## divider("BENCHMARKING")

## rvers <- function() {
##     project_z_by_z(z=sweden_1993$n, P0M=sweden_1993$P0M, P0F=sweden_1993$P0F,
##                        SxM=sweden_1993$SxM, SxF=sweden_1993$SxF, asfr=sweden_1993$asfr,
##                        NMxM=sweden_1993$NMxM, NMxF=sweden_1993$NMxF, srb=sweden_1993$srb,
##                    mig_assumption = "even")$PzF}
## cppvers <- function() {
##     proj_pop_cpp(step_size = sweden_1993$n,
##                       pop_count_age_m_t0 = sweden_1993$P0M,
##                       pop_count_age_f_t0 = sweden_1993$P0F,
##                       surv_prop_age_m = sweden_1993$SxM,
##                       surv_prop_age_f = sweden_1993$SxF,
##                       fert_rate_age = sweden_1993$asfr,
##                       net_mig_count_age_m = sweden_1993$NMxM,
##                       net_mig_count_age_f = sweden_1993$NMxF,
##                      srb_tot = sweden_1993$srb,
##                      mig_assumption = "even")$pop_count_age_f_t1
##     }

## bench::mark(rvers(), cppvers(), min_iterations = 1000)
