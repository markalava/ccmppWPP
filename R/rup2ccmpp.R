# 
# 
# 
# 
# 
# interp_coh <- function (c1, c2, date1, date2, age1 = 1:length(c1) - 1, age2 = 1:length(c2) - 
#             1, dates_out = NULL, lxMat = NULL, age_lx = NULL, dates_lx = NULL, 
#           births = NULL, years_births = NULL, location = NULL, sex = "both", 
#           midyear = FALSE, verbose = TRUE, ...) 
# {
#   date1 <- dec.date(date1)
#   date2 <- dec.date(date2)
#   res_list <- rup(c1 = c1, c2 = c2, date1 = date1, date2 = date2, 
#                   age1 = age1, age2 = age2, dates_out = dates_out, lxMat = lxMat, 
#                   age_lx = age_lx, dates_lx = dates_lx, births = births, 
#                   years_births = years_births, location = location, sex = sex, 
#                   midyear = midyear, verbose = verbose, ... = ...)
#   pop_jan1 <- res_list$pop_jan1
#   dates_out <- res_list$dates_out
#   . <- NULL
#   age <- NULL
#   discount <- NULL
#   pop_jan1_pre <- NULL
#   resid <- NULL
#   year <- NULL
#   pop_jan1[, `:=`(pop_jan1 = pop_jan1 + resid * discount)]
#   pop_jan1 <- pop_jan1[!is.na(cohort)]
#   PopAP <- pop_jan1 %>% .[, list(age, year, pop_jan1)] %>% 
#     data.table::dcast(age ~ year, value.var = "pop_jan1") %>% 
#     .[order(age)]
#   matinterp <- PopAP[age <= max(age1), -1] %>% as.matrix()
#   rownames(matinterp) <- age1
#   ind <- is.na(matinterp)
#   if (any(ind) & verbose) {
#     cat("\n", sum(ind), "NA detected in output.\nThese have been imputed with 0s.\nThis could happen in the highest ages,\nand you may consider extending the open ages of the census inputs?\n")
#     matinterp[ind] <- 0
#   }
#   ind <- matinterp < 0
#   if (any(ind) & verbose) {
#     cat("\n", sum(ind), "negatives detected in output.\nThese have been imputed with 0s.\n")
#     matinterp[ind] <- 0
#   }
#   yrsIn <- as.numeric(colnames(matinterp))
#   if (all(yrsIn > date1)) {
#     matinterp <- cbind(c1, matinterp)
#     yrsIn <- c(date1, yrsIn)
#   }
#   if (all(yrsIn < date2)) {
#     matinterp <- cbind(matinterp, c2[1:length(c2)])
#     yrsIn <- c(yrsIn, date2)
#   }
#   colnames(matinterp) <- yrsIn
#   out <- interp(matinterp, datesIn = yrsIn, datesOut = as.numeric(dates_out), 
#                 rule = 1)
#   if (any(out < 0)) {
#     if (verbose) {
#       cat("\nSome of the interpolated values resulted to be negative, replacing with zeroes\n")
#     }
#     out[out < 0] <- 0
#   }
#   out
# }
# 
# 
# rup <- function (c1, c2, date1, date2, age1, age2, dates_out, lxMat, 
#           age_lx, dates_lx, births, years_births, location, sex, midyear, 
#           verbose, ...) 
# {
#   check_args(lxMat = lxMat, births = births, location = location, 
#              age1 = age1, age2 = age2, c1 = c1, c2 = c2, verbose = verbose)
#   if (is.na(date1) | is.na(date2)) {
#     stop("\nCensus dates didn't parse\n")
#   }
#   if (!is.null(dates_out)) {
#     dates_out <- sapply(dates_out, dec.date)
#     if (any(is.na(dates_out))) {
#       cat("\nSome dates_out didn't parse, FYI, you should have a look\n")
#       dates_out <- dates_out[!is.na(dates_out)]
#     }
#     if (length(dates_out) == 0) {
#       stop("\nno valid dates to interpolate to\n")
#     }
#     dates_out_keep <- data.table::between(dates_out, date1, 
#                                           date2, incbounds = FALSE)
#     dates_out_for_real <- dates_out[dates_out_keep]
#     if (length(dates_out_for_real) != length(dates_out) & 
#         verbose) {
#       cat("\nFollowing dates requested, but not returned\nbecause they'd require extrapolation:\n", 
#           paste(dates_out[!dates_out_keep], collapse = ", "), 
#           "\n")
#     }
#     if (length(dates_out) == 0) {
#       stop("\nuh oh! This method is strictly for cohort component interpolation\nYour requested dates_out didn't have anything between date1 and date2\n")
#     }
#   }
#   dates_out <- transform_datesout(dates_out, date1, date2, 
#                                   midyear)
#   DD <- date2 - date1
#   if (DD >= 15 & verbose) {
#     cat("\nFYI, there are", DD, "years between c1 and c2\nBe wary.\n")
#   }
#   f1 <- date1 %>% magrittr::subtract(date1 %>% floor)
#   f2 <- date2 %>% magrittr::subtract(date2 %>% floor)
#   pxt <- transform_pxt(lxMat = lxMat, location = location, 
#                        sex = sex, date1 = date1, date2 = date2, dates_lx = dates_lx, 
#                        verbose = verbose, age_lx = age_lx, age1 = age1, ... = ...)
#   yrs_births <- seq(floor(date1), floor(date2), 1)
#   if (f2 == 0) {
#     pxt <- pxt[, -ncol(pxt), drop = FALSE]
#     yrs_births <- yrs_births[-length(yrs_births)]
#     f2 <- 1
#   }
#   births <- fetch_wpp_births(births = births, yrs_births = yrs_births, 
#                              location = location, sex = sex, verbose = verbose)
#   if (!is.null(years_births)) {
#     stopifnot(length(births) == length(years_births))
#     years_births <- floor(years_births)
#     yrs_keep <- data.table::between(years_births, min(yrs_births), 
#                                     max(yrs_births), incbounds = TRUE)
#     births <- births[yrs_keep]
#   }
#   stopifnot(length(births) == length(yrs_births))
#   pop_jan1 <- reshape_pxt(pxt = pxt, births = births, c1 = c1, 
#                           c2 = c2, age1 = age1, age2 = age2, date1 = date1, date2 = date2, 
#                           f1 = f1, f2 = f2, yrs_births = yrs_births)
#   list(pop_jan1 = pop_jan1, dates_out = dates_out)
# }
# 
# 
# transform_pxt <- function (lxMat, location, sex, date1, date2, dates_lx, verbose, 
#           age_lx, age1, ...) 
# {
#   if (is.null(lxMat)) {
#     pxt <- suppressMessages(interp_coh_download_mortality(location = location, 
#                                                           sex = sex, date1 = date1, date2 = date2, OAnew = max(age1), 
#                                                           verbose = verbose))
#   }
#   else {
#     if (is.null(dates_lx)) {
#       dates_lx <- seq(date1, date2, length.out = ncol(lxMat))
#       if (verbose) {
#         cat("lxMat specified, but not dates_lx\nAssuming:", 
#             paste(dates_lx, collapse = ", "), "\n")
#       }
#     }
#     available_dates <- data.table::between(dates_lx, date1, 
#                                            date2)
#     if (!all(available_dates)) 
#       stop("All `dates_lx` must be within the range of `date1` and `date2`")
#     dates_df <- expand.grid(dates_lx = dates_lx, dates = c(date1, 
#                                                            date2))
#     dates_df$diff <- with(dates_df, abs(dates_lx - dates))
#     if (min(dates_df$diff) > 7 && verbose) {
#       d_lx <- dates_df$dates_lx[which.min(dates_df$dif)]
#       date_compare <- dates_df$dates[which.min(dates_df$dif)]
#       cat("The shortest distance from `dates_lx` (", 
#           d_lx, ") to `date1/date2`(", date_compare, 
#           ") is greater than 7 years. Be wary.")
#     }
#     ic_period <- date2 - date1
#     lx_mm <- range(dates_lx)
#     overlap <- min(c(lx_mm[2], date2)) - c(max(lx_mm[1], 
#                                                date1))
#     extrap_low <- lx_mm[1] - min(lx_mm[1], date1)
#     extrap_high <- max(lx_mm[2], date2) - lx_mm[2]
#     t1 <- overlap/ic_period < 0.25
#     t2 <- extrap_low > 6
#     t3 <- extrap_high > 6
#     if (any(c(t1, t2, t3))) 
#       cat("\nRange between `date1` and `date2` must overlap with `lx_dates` for at least 25% of the range or 6 years.\n")
#     if (is.null(age_lx)) {
#       if (nrow(lxMat) < 26) {
#         N <- nrow(lxMat)
#         age_lx <- c(0, 1, seq(5, 5 * (N - 2), by = 5))
#       }
#       else {
#         age_lx <- 1:nrow(lxMat) - 1
#       }
#       if (verbose) {
#         cat("lxMat specified, but Age_lx missing\nAssuming:", 
#             paste(age_lx, collapse = ", "), "\n")
#       }
#     }
#     pxt <- interp_coh_lxMat_pxt(lxMat = lxMat, dates_lx = dates_lx, 
#                                 age_lx = age_lx, date1 = date1, date2 = date2, OAnew = max(age1), 
#                                 control = list(deg = 3, lambda = 100), ...)
#   }
#   pxt
# }
# 
# interp_coh_lxMat_pxt <- function (lxMat, dates_lx, age_lx, date1, date2, OAnew, ...) 
# {
#   date1 <- dec.date(date1)
#   date2 <- dec.date(date2)
#   year1 <- floor(date1) + 1
#   year2 <- floor(date2)
#   year_seq <- year1:year2
#   dates_out <- c(dec.date(date1), year_seq)
#   a1 <- 0:OAnew
#   qx1 <- matrix(ncol = ncol(lxMat), nrow = length(a1), dimnames = list(a1, 
#                                                                        dates_lx))
#   for (i in 1:ncol(lxMat)) {
#     if (is_abridged(age_lx)) {
#       LT1 <- lt_abridged2single(lx = lxMat[, i], Age = age_lx, 
#                                 OAnew = OAnew, ...)
#       qx1[, i] <- LT1$nqx
#     }
#     else {
#       qx <- lt_id_l_q(lxMat[, i])
#       LT1 <- lt_single_qx(nqx = qx, Age = 1:length(qx) - 
#                             1, OAnew = OAnew, ...)
#       qx1[, i] <- LT1$nqx
#     }
#   }
#   logit_qx <- log(qx1/(1 - qx1))
#   logit_qx_interp <- interp(popmat = logit_qx, datesIn = dates_lx, 
#                             datesOut = dates_out, rule = 2, negatives = TRUE)
#   QX <- exp(logit_qx_interp)/(1 + exp(logit_qx_interp))
#   QX[nrow(QX), ] <- 1
#   f1 <- diff(dates_out)[1]
#   f2 <- date2 - floor(date2)
#   PX <- apply(QX, 2, function(q) {
#     lt_single_qx(nqx = q, Age = a1, OAnew = OAnew, ...)$Sx
#   })
#   rownames(PX) <- rownames(QX)
#   PX[, 1] <- PX[, 1]^f1
#   PX[, ncol(PX)] <- PX[, ncol(PX)]^f2
#   PX
# }
