S3_class_message <- function(...) {
    if (!getOption("ccmppWPP.suppress_S3_class_messages", default = FALSE))
        message(...)
    invisible()
    }

S3_class_warning <- function(...) {
    if (!getOption("ccmppWPP.suppress_S3_class_warnings", default = FALSE))
        warning(..., call. = FALSE)
    invisible()
    }
