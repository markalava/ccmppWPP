
## Form taken from 'R Packages' by Hadley Wickham, 'https://r-pkgs.org/r.html'
.onLoad <- function(libname, pkgname) {
  op <- options()
  op.ccmppWPP <- list(
    ccmppWPP.suppress_S3_class_messages = FALSE,
    ccmppWPP.suppress_S3_class_warnings = FALSE
  )
  toset <- !(names(op.ccmppWPP) %in% names(op))
  if(any(toset)) options(op.ccmppWPP[toset])

  invisible()
}
