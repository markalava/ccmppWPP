# population helper functions

#' Compute median age of the population from 1x1 population counts
#'
#' @description This function computes the median age of the population from 1x1 population counts
#'
#' @author Sara Hertog
#'
#' @param pop_count_age data frame. columns "time_start, "age_start" and "value" where value is population counts
#'
#' @details single year of age required
#'
#' @return a data frame with time_start and median age of the population (years) in the "value" field 
#' @export

median_age <- function(pop_count_age) {
  
  median_age <- pop_count_age %>% 
    arrange(time_start, time_span, age_start) %>% 
    group_by(time_start, time_span) %>% 
    mutate(percentage = value/sum(value) * 100,
           cumdist = cumsum(percentage)) %>% 
    summarise(value = stats::splinefun(cumdist, 0:100, method = "monoH.FC")(50))
  
  median_age <- as.data.frame(median_age)
  return(median_age)
  
}


#' Compute mid-period population of the population from population counts
#'
#' @description This function computes mid period population by sex and age from population counts by sex and age
#'
#' @author Sara Hertog
#'
#' @param pop_count_age_sex data frame. columns "time_start, "sex", "age_start" and "value" where value is population counts
#'
#' @details simple average of population by age and sex at two successive points in time
#'
#' @return a data frame with time_start, sex, age_start and mid-period population counts in the "value" field 
#' @export

mid_period_pop <- function(pop_count_age_sex, period_length = 1) {
  
  ntimes <- length(unique(pop_count_age_sex$time_start))
  mid_period_pop <- pop_count_age_sex %>% 
                      arrange(sex, age_start, time_start) %>% 
                      group_by(sex, age_start) %>% 
                      mutate(nextyrpop  = c(value[2:ntimes], NA),
                             value = (value + nextyrpop)/2,
                             time_start = time_start + (period_length/2)) %>% 
                      dplyr::select(-nextyrpop) %>% 
                      dplyr::filter(!is.na(value)) %>% 
                      ungroup() %>% 
                      arrange(time_start, sex, age_start)
  
  mid_period_pop <- as.data.frame(mid_period_pop)
  
  return(mid_period_pop)
  
}