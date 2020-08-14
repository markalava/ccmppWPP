S3_class_message <- function(...) {
    if (!getOption("ccmppWPP.suppress_S3_class_messages", default = FALSE))
        message(...)
    invisible()
    }
