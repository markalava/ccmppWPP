
## Form taken from 'R Packages' by Hadley Wickham, 'https://r-pkgs.org/r.html'
.onLoad <- function(libname, pkgname) {

    ## -------* OPTIONS

  op <- options()
  op.ccmppWPP <- list(
    ccmppWPP.suppress_S3_class_messages = FALSE,
    ccmppWPP.suppress_S3_class_warnings = FALSE
  )
  toset <- !(names(op.ccmppWPP) %in% names(op))
    if(any(toset)) options(op.ccmppWPP[toset])

    ## -------* METHODS FOR FUNCTIONS IN OTHER PACKAGES

    vctrs__s3_register("dplyr::across", "demog_change_component_df")
    vctrs__s3_register("dplyr::arrange", "demog_change_component_df")
    vctrs__s3_register("dplyr::add_column", "demog_change_component_df")
    vctrs__s3_register("dplyr::add_row", "demog_change_component_df")
    vctrs__s3_register("dplyr::column_to_rownames", "demog_change_component_df")
    vctrs__s3_register("dplyr::filter", "demog_change_component_df")
    vctrs__s3_register("dplyr::mutate", "demog_change_component_df")
    vctrs__s3_register("dplyr::mutate_all", "demog_change_component_df")
    vctrs__s3_register("dplyr::mutate_if", "demog_change_component_df")
    vctrs__s3_register("dplyr::mutate_at", "demog_change_component_df")
    vctrs__s3_register("dplyr::remove_rownames", "demog_change_component_df")
    vctrs__s3_register("dplyr::rename", "demog_change_component_df")
    vctrs__s3_register("dplyr::rownames_to_column", "demog_change_component_df")
    vctrs__s3_register("dplyr::pull", "demog_change_component_df")
    vctrs__s3_register("dplyr::select", "demog_change_component_df")
    vctrs__s3_register("dplyr::slice", "demog_change_component_df")
    vctrs__s3_register("dplyr::summarise", "demog_change_component_df")
    vctrs__s3_register("dplyr::summarize", "demog_change_component_df")
    vctrs__s3_register("dplyr::transmute", "demog_change_component_df")
    vctrs__s3_register("dplyr::transmute_all", "demog_change_component_df")
    vctrs__s3_register("dplyr::transmute_if", "demog_change_component_df")
    vctrs__s3_register("dplyr::transmute_at", "demog_change_component_df")

    vctrs__s3_register("dplyr::anti_join", "demog_change_component_df")
    vctrs__s3_register("dplyr::bind_rows", "demog_change_component_df")
    vctrs__s3_register("dplyr::full_join", "demog_change_component_df")
    vctrs__s3_register("dplyr::inner_join", "demog_change_component_df")
    vctrs__s3_register("dplyr::intersect", "demog_change_component_df")
    vctrs__s3_register("dplyr::left_join", "demog_change_component_df")
    vctrs__s3_register("dplyr::right_join", "demog_change_component_df")
    vctrs__s3_register("dplyr::semi_join", "demog_change_component_df")
    vctrs__s3_register("dplyr::setdiff", "demog_change_component_df")
    vctrs__s3_register("dplyr::union", "demog_change_component_df")

    vctrs__s3_register("tidyr::complete", "demog_change_component_df")
    vctrs__s3_register("tidyr::drop_na", "demog_change_component_df")
    vctrs__s3_register("tidyr::expand", "demog_change_component_df")
    vctrs__s3_register("tidyr::fill", "demog_change_component_df")
    vctrs__s3_register("tidyr::gather", "demog_change_component_df")
    vctrs__s3_register("tidyr::replace_na", "demog_change_component_df")
    vctrs__s3_register("tidyr::separate", "demog_change_component_df")
    vctrs__s3_register("tidyr::separate_rows", "demog_change_component_df")
    vctrs__s3_register("tidyr::spread", "demog_change_component_df")
    vctrs__s3_register("tidyr::unite", "demog_change_component_df")

  invisible()
}


vctrs__s3_register <- function (generic, class, method = NULL)
    ## Taken from 'vctrs' source code as suggested in
    ## ?vctrs::s3_register. Thanks!
{
    stopifnot(is.character(generic), length(generic) == 1)
    stopifnot(is.character(class), length(class) == 1)
    pieces <- strsplit(generic, "::")[[1]]
    stopifnot(length(pieces) == 2)
    package <- pieces[[1]]
    generic <- pieces[[2]]
    caller <- parent.frame()
    get_method_env <- function() {
        top <- topenv(caller)
        if (isNamespace(top)) {
            asNamespace(environmentName(top))
        }
        else {
            caller
        }
    }
    get_method <- function(method, env) {
        if (is.null(method)) {
            get(paste0(generic, ".", class), envir = get_method_env())
        }
        else {
            method
        }
    }
    method_fn <- get_method(method)
    stopifnot(is.function(method_fn))
    setHook(packageEvent(package, "onLoad"), function(...) {
        ns <- asNamespace(package)
        method_fn <- get_method(method)
        registerS3method(generic, class, method_fn, envir = ns)
    })
    if (!isNamespaceLoaded(package)) {
        return(invisible())
    }
    envir <- asNamespace(package)
    if (exists(generic, envir)) {
        registerS3method(generic, class, method_fn, envir = envir)
    }
    invisible()
}
