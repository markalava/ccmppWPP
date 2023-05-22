
op <- options(ccmppWPP.suppress_S3_class_messages = TRUE, timeout = 10000)

withr::defer(options(op), teardown_env())
