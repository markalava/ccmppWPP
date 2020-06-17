

# reorganize the output from project_ccmpp_loop_over_time by indicator for easier parsing later

data_reshape_ccmpp_output <- function(ccmpp_output) {

  times <- sapply(ccmpp_output, "[[", "time_start")
  time_start             <- min(times)
  time_span              <- diff(times)[1]
  time_end               <- max(times) + time_span
  age_start              <- ccmpp_output[[1]]$age_start
  oag                    <- max(age_start)

  # population by age and sex, including aggregation to both sexes combined
    pop_f   <- data_gather_ccmpp_output(ccmpp_output=ccmpp_output, object ="pop_count_age_f_end",
                                        time_start = time_start+time_span, time_end = time_end, time_span = time_span,
                                        ids    = age_start)
    pop_f$sex <- "female"

    pop_m   <- data_gather_ccmpp_output(ccmpp_output=ccmpp_output, object ="pop_count_age_m_end",
                                        time_start = time_start+time_span, time_end = time_end, time_span = time_span,
                                        ids    = age_start)
    pop_m$sex <- "male"
    
    pop <- rbind(pop_f, pop_m)
    
    pop_b <- sum_last_column(pop[, c("time_start", "age_start", "value")])
    pop_b$sex <- "both"
    
    pop <- rbind(pop, pop_b)
    pop$age_span <- pop$time_span <- time_span
    pop$age_span[which(pop$age_start == oag)] <- 1000
    pop_count_age_sex <- pop[order(pop$time_start, pop$sex, pop$age_start), 
                             c("time_start", "time_span", "sex", "age_start", "age_span", "value")]
    
   
  # cohort deaths by age and sex, including aggregation to both sexes combined
    dth_f <- data_gather_ccmpp_output(ccmpp_output=ccmpp_output, object ="death_count_cohort_f",
                                      time_start = time_start, time_end = time_end-time_span, time_span = time_span,
                                      ids    = age_start)
    dth_f$sex <- "female"
    
    dth_m <- data_gather_ccmpp_output(ccmpp_output=ccmpp_output, object ="death_count_cohort_m",
                                      time_start = time_start, time_end = time_end-time_span, time_span = time_span,
                                      ids    = age_start)
    dth_m$sex <- "male"
    
    dth <- rbind(dth_f, dth_m)
    dth_b <- sum_last_column(dth[, c("time_start", "age_start", "value")])
    dth_b$sex <- "both"
    
    dth <- rbind(dth, dth_b)
    
    dth$age_span <- dth$time_span <- time_span
    dth$age_span[which(dth$age_start == oag)] <- 1000
    death_count_cohort_sex <- dth[order(dth$time_start, dth$sex, dth$age_start),
                                c("time_start", "time_span", "sex", "age_start", "age_span", "value")]

    
  # births by age of mother
    bth           <- data_gather_ccmpp_output(ccmpp_output=ccmpp_output, object ="birth_count_age_b",
                                                  time_start = time_start, time_end = time_end-time_span, time_span = time_span,
                                                  ids    = age_start)
    bth$age_span <- bth$time_span <- time_span
    bth$age_span[which(bth$age_start == oag)] <- 1000
    birth_count_age_b <- bth[order(bth$time_start, bth$age_start),
                                c("time_start", "time_span", "age_start", "age_span", "value")]


  # total births by sex
    bth_f <- data.frame(time_start = seq(time_start, time_end-time_span, time_span),
                        sex = "female", 
                        value = sapply(ccmpp_output, "[[", "birth_count_tot_f"))
    bth_m <- data.frame(time_start = seq(time_start, time_end-time_span, time_span),
                        sex = "male", 
                        value = sapply(ccmpp_output, "[[", "birth_count_tot_m"))
    bth_b <- data.frame(time_start = seq(time_start, time_end-time_span, time_span),
                        sex = "both", 
                        value = sapply(ccmpp_output, "[[", "birth_count_tot_b"))

    bth     <- rbind(bth_f, bth_m, bth_b)
    bth$time_span <- time_span
    birth_count_tot_sex <- bth[order(bth$time_start, bth$sex),
                               c("time_start", "time_span", "sex", "value")]

  # net migrants by age and sex, including aggregation to both sexes combined
    mig_f <- data_gather_ccmpp_output(ccmpp_output=ccmpp_output, object ="mig_net_count_age_f",
                                      time_start = time_start, time_end = time_end-time_span, time_span = time_span,
                                      ids    = age_start)
    mig_f$sex <- "female"
    
    mig_m <- data_gather_ccmpp_output(ccmpp_output=ccmpp_output, object ="mig_net_count_age_m",
                                      time_start = time_start, time_end = time_end-time_span, time_span = time_span,
                                      ids    = age_start)
    mig_m$sex <- "male"
    
    mig <- rbind(mig_f, mig_m)
    mig_b <- sum_last_column(mig[, c("time_start", "age_start", "value")])
    mig_b$sex <- "both"
    
    mig <- rbind(mig, mig_b)
    
    mig$age_span <- mig$time_span <- time_span
    mig$age_span[which(mig$age_start == oag)] <- 1000
    mig_net_count_age_sex <- mig[order(mig$time_start, mig$sex, mig$age_start),
                                  c("time_start", "time_span", "sex", "age_start", "age_span", "value")]
    
    
ccmpp_reshaped <- list(pop_count_age_sex = pop_count_age_sex,
                     death_count_cohort_sex = death_count_cohort_sex,
                     birth_count_age_b = birth_count_age_b,
                     birth_count_tot_sex = birth_count_tot_sex,
                     mig_net_count_age_sex = mig_net_count_age_sex)
return(ccmpp_reshaped)
}


