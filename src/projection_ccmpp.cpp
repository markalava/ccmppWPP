#include <Rcpp.h>
using namespace Rcpp;

//' Cohort component population projection (C++ implementation)
//'
//' The function projects the baseline population forward in time (one
//' step of length \code{step_size} years) by adding births and net
//' migrants and subtracting deaths. This implementation is written in
//' C++ (using the \pkg{Rcpp}
//' package). 
//'
//' The cohort component method of population projection (CCMPP) is a
//' deterministic method for projecting age-stratified population
//' counts forward in time, accounting for births, deaths, and net
//' migration. It is described in \cite{Preston, et al., (2001)},
//' Chapter 6, and many other places.
//'
//' @section Migration Assumption:
//' The CCMPP is a discrete time approximation of a continuous time
//' process in which certain assumptions are made. Two such
//' assumptions about net migration are implemented:
//' \describe{
//'   \item{\dQuote{even}}{Net migrants are distributed across the projection interval by adding half of the net migration to the baseline population at the beginning of the projection interval therby exposing them to fertility and mortality. The remaining half is added at the end and are not exposed to mortality and fertility.}
//'   \item{\dQuote{end}}{All net migration is added at the end of the projection step, therefore none are exposed to mortality and fertility.}}
//' The default is \dQuote{even}; \dQuote{end} is included for compatibilty with some earlier implementations of the CCMPP.
//'
//' @param step_size The size of the project step and age intervals. Usually this is 1 or 5.
//' @param pop_count_age_m_t0 Vector of population counts by age, of males in the baseline year.
//' @param pop_count_age_f_t0 Vector of population counts by age, of females in the baseline year.
//' @param surv_prop_age_m Vector of life table survival proportions by age, of males over the projection step.
//' @param surv_prop_age_f  Vector of life table survival proportions by age, of females over the projection step.
//' @param fert_rate_age Vector of fertility rates by age of mother during the projection step.
//' @param net_mig_count_age_m Vector of net number of male migrants arriving, by age, during the projection step.
//' @param net_mig_count_age_f Vector of net number of female migrants arriving, by age, during the projection step.
//' @param srb_tot Average sex ratio at birth across all births during the projection step.
//' @param mig_assumption Treatment of net migration. See \dQuote{Migration Assumption}.
//'
//' @author
//' Mark Wheldon, based on \R code by Sara Hertog
//'
//' @seealso \code{\link{project_z_by_z}}.
//' 
//' @references
//' Preston, S. H., Heuveline, P., and Guillot, M. (2001), \emph{Demography: Measuring and Modeling Population Processes}, Malden, Massachusetts: Blackwell.
//' @export
// [[Rcpp::export]]
DataFrame proj_pop_cpp(int step_size,
		  NumericVector pop_count_age_m_t0,
		  NumericVector pop_count_age_f_t0,
		  NumericVector surv_prop_age_m,
		  NumericVector surv_prop_age_f,
		  NumericVector fert_rate_age,
		  NumericVector net_mig_count_age_m,
		  NumericVector net_mig_count_age_f,
		  double srb_tot = 1.05,
		  String mig_assumption = "even") {

  int nage = pop_count_age_f_t0.size();
  int nage_minus_1 = nage - 1;
  int nage_minus_2 = nage - 2;
  
  NumericVector pop_count_age_m_w_half_mig(nage); 
  NumericVector pop_count_age_f_w_half_mig(nage);
  NumericVector mig_count_age_m_end_period(nage);
  NumericVector mig_count_age_f_end_period(nage);
  NumericVector lag_pop_count_age_m_w_half_mig(nage);
  NumericVector lag_pop_count_age_f_w_half_mig(nage);
  NumericVector death_count_age_m(nage);
  NumericVector death_count_age_f(nage);
  NumericVector pop_count_age_m_t1(nage);
  NumericVector pop_count_age_f_t1(nage);
  NumericVector birth_count_age_b(nage);
  double birth_count_tot_b = 0, birth_count_tot_f = 0, birth_count_tot_m = 0;
  NumericVector age(nage);
  
  /*
   * Migration assumption
   */
  
  if(mig_assumption == "even") {
    // uses Rcpp/sugar 
    pop_count_age_f_w_half_mig = pop_count_age_m_t0 + net_mig_count_age_m / 2;
      pop_count_age_m_w_half_mig = pop_count_age_f_t0 + net_mig_count_age_f / 2;
      mig_count_age_m_end_period = net_mig_count_age_m / 2;
      mig_count_age_f_end_period = net_mig_count_age_f / 2;
  }
  else if(mig_assumption == "end") {
    // uses Rcpp/sugar 
      pop_count_age_m_w_half_mig = pop_count_age_m_t0;
      pop_count_age_f_w_half_mig = pop_count_age_f_t0;
      mig_count_age_m_end_period = 0.0;
      mig_count_age_f_end_period = 0.0;
  }

  /*
   * Lagged variables
   */

  lag_pop_count_age_f_w_half_mig[0] = 0.0;
  lag_pop_count_age_m_w_half_mig[0] = 0.0;
  
  for(int i = 1; i < nage; ++i) {
    // first age group done already so start at i = 1
    lag_pop_count_age_f_w_half_mig[i] = pop_count_age_f_w_half_mig[i-1];
    lag_pop_count_age_m_w_half_mig[i] = pop_count_age_m_w_half_mig[i-1];
  }

  /*
   * compute deaths from year 0 population and survival ratios
   */
    // uses Rcpp/sugar 
    death_count_age_m = (1 - surv_prop_age_m) * lag_pop_count_age_m_w_half_mig;  
    death_count_age_f = (1 - surv_prop_age_f) * lag_pop_count_age_f_w_half_mig;
  
  death_count_age_m[nage_minus_1] = (1 - surv_prop_age_m[nage_minus_1]) *
    (pop_count_age_m_w_half_mig[nage_minus_2] + pop_count_age_m_w_half_mig[nage_minus_1]);
  death_count_age_f[nage_minus_1] = (1 - surv_prop_age_f[nage_minus_1]) *
    (pop_count_age_f_w_half_mig[nage_minus_2] + pop_count_age_f_w_half_mig[nage_minus_1]);

  /*
   * project population by age at year +1 from year 0 population and deaths
   */
    // uses Rcpp/sugar 
    pop_count_age_m_t1 = lag_pop_count_age_m_w_half_mig -
      death_count_age_m + mig_count_age_m_end_period;
    pop_count_age_f_t1 = lag_pop_count_age_f_w_half_mig -
      death_count_age_f + mig_count_age_f_end_period;
    
  pop_count_age_m_t1[nage_minus_1] = pop_count_age_m_w_half_mig[nage_minus_2] +
    pop_count_age_m_w_half_mig[nage_minus_1] -
    death_count_age_m[nage_minus_1] +
    mig_count_age_m_end_period[nage_minus_1];
  pop_count_age_f_t1[nage_minus_1] = pop_count_age_f_w_half_mig[nage_minus_2] +
    pop_count_age_f_w_half_mig[nage_minus_1] -
    death_count_age_f[nage_minus_1] +
    mig_count_age_f_end_period[nage_minus_1];

  /*
   * compute births from year 0 female population, fert_rate_age and srb_tot
   */
  
    // uses Rcpp/sugar 
  birth_count_age_b = step_size * fert_rate_age *
      (pop_count_age_f_w_half_mig + pop_count_age_f_t1) / 2;
  birth_count_age_b[0] = 0;
  birth_count_age_b[nage_minus_1] = 0;
  
  for(int i = 1; i < nage_minus_1; ++i) {
    birth_count_tot_b += birth_count_age_b[i]; // initialized as zero
  }
  birth_count_tot_f = birth_count_tot_b * (1 / (1 + srb_tot));
  birth_count_tot_m = birth_count_tot_b - birth_count_tot_f;
  
  /*
   * compute infant deaths from total births by sex and survival ratio
   */
  
  death_count_age_m[0] = (1 - surv_prop_age_m[0]) * birth_count_tot_m;
  death_count_age_f[0] = (1 - surv_prop_age_f[0]) * birth_count_tot_f;

  /*
   * project infant population at year +1 from births and infant deaths
   */
  
  pop_count_age_m_t1[0] = birth_count_tot_m -
    death_count_age_m[0] + mig_count_age_m_end_period[0];
  pop_count_age_f_t1[0] = birth_count_tot_f -
    death_count_age_f[0] + mig_count_age_f_end_period[0];

  /*
   * if end-of-period assumption then add migrants
   */
  
  if (mig_assumption == "end") {
      pop_count_age_m_t1  += net_mig_count_age_m;
      pop_count_age_f_t1 += net_mig_count_age_f;
  }

  /*
   * ensure no negative population by age and sex (0.0005 is same as Abacus) Do we need these to be nonzero?
   */

  for(int i = 0; i < nage; ++i) {
    if(pop_count_age_m_t1[i] < 0.0005) pop_count_age_m_t1[i] = 0;
    if(pop_count_age_f_t1[i] < 0.0005) pop_count_age_f_t1[i] = 0;
  }

  /*
   * construct age column
   */

  age[0] = 0;
  int a = 0;
  for(int i = 1; i < nage; ++i) {
    a += step_size;
    age[i] = a;
  }

  /*
   * RETURN
   */
  
  return DataFrame::create(_["age"] = age,
			   _["pop_count_age_m_t0"] = pop_count_age_m_t0,
			   _["pop_count_age_f_t0"] = pop_count_age_f_t0,
			   _["death_count_age_m"] = death_count_age_m,
			   _["death_count_age_f"] = death_count_age_f,
			   _["birth_count_age_b"] = birth_count_age_b,
			   _["pop_count_age_m_t1"] = pop_count_age_m_t1,
			   _["pop_count_age_f_t1"] = pop_count_age_f_t1);			  
}
