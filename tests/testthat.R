library(testthat)
library(ccmppWPP)

op <- options()
options(ccmppWPP.suppress_S3_class_messages = TRUE)

test_check("ccmppWPP")

options(op)
