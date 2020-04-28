#include <Rcpp.h>
using namespace Rcpp;

//' Cohort component population projection (C++ implementation)
//'
//' The cohort component method of population projection (CCMPP) is a
//' deterministic method for projecting age-stratified population
//' counts forward in time, accounting for births, deaths, and net
//' migration. It is described in \cite{Preston, et al., (2001)},
//' Chapter 6, and many other places. 
//'
//' This implementation is written in C++ and projects the baseline
//' population forward once \code{step_size} years.
//'
//' @author
//' Mark Wheldon, Sara Hertog
//' 
//' @references
//' Preston, S. H., Heuveline, P., and Guillot, M. (2001), \emph{Demography: Measuring and Modeling Population Processes}, Malden, Massachusetts: Blackwell.
//' @export
// [[Rcpp::export]]
DataFrame project_ccmpp(int step_size,
		  const NumericVector& P0M,
		  const NumericVector& P0F,
		  const NumericVector& SxM,
		  const NumericVector& SxF,
		  const NumericVector& asfr,
		  const NumericVector& NMxM,
		  const NumericVector& NMxF,
		  double srb = 1.05,
		  int migration_assumption = 1) {

  int i, a, nage = P0F.size();  
  NumericVector pxm(nage); 
  NumericVector pxf(nage);
  NumericVector migm_end(nage);
  NumericVector migf_end(nage);
  NumericVector lag_pxm(nage);
  NumericVector lag_pxf(nage);
  NumericVector DxM(nage);
  NumericVector DxF(nage);
  NumericVector PzM(nage);
  NumericVector PzF(nage);
  NumericVector Bx(nage);
  double Btot = 0, BtotF, BtotM;
  NumericVector Age(nage);
  
  /*
   * Migration assumption
   */
  
  // '1' is "evenly over period", default
  // '2' is "end of period"
  
  if(migration_assumption == 1) {
    // half of increments between age x and x+1 are added at end of period and do not affect births or deaths in period
    // half of increments between age x-1 and x are added at beginning of period and survived to age x to x+1
    pxm = P0M + NMxM/2;
    pxf = P0F + NMxF/2;
    migm_end = NMxM/2;
    migf_end = NMxF/2;    
  }
  else if(migration_assumption == 2) {
    pxm = P0M;
    pxf = P0F;
    migm_end = rep(0,nage);
    migf_end = rep(0,nage);
  }

  /*
   * Lagged variables
   */

  lag_pxm[0] = NA_REAL;
  lag_pxf[0] = NA_REAL;
  
  for(i = 1; i < nage; i++) {
    lag_pxm[i] = pxm[i-1];
    lag_pxf[i] = pxf[i-1];
  }

  /*
   * compute deaths from year 0 population and survival ratios
   */

  for(i = 0; i < nage; i++) {  
    DxM[i] = (1 - SxM[i]) * lag_pxm[i];  
    DxF[i] = (1 - SxF[i]) * lag_pxf[i];
  }
  DxM[nage] = (1-SxM[nage]) * (pxm[nage - 1] + pxm[nage]);
  DxF[nage] = (1-SxF[nage]) * (pxf[nage - 1] + pxf[nage]);

  /*
   * project population by age at year +1 from year 0 population and deaths
   */

  for(i = 0; i < nage; i++) {
    PzM[i] = round(lag_pxm[i] - DxM[i] + migm_end[i]);
    PzF[i] = round(lag_pxf[i] - DxF[i] + migf_end[i]);
  }
  PzM[nage] = round(pxm[nage-1] + pxm[nage] - DxM[nage] + migm_end[nage]);
  PzF[nage] = round(pxf[nage-1] + pxf[nage] - DxF[nage] + migf_end[nage]);

  /*
   * compute births from year 0 female population, asfr and srb
   */

  Bx[0] = 0;  Bx[nage - 1] = 0;

  for(i = 0; i < nage; i++) {
    Bx[i] = step_size * asfr[i] * (pxf[i] + PzF[i])/2;
    Btot += Bx[i]; // initialized as zero
  }
  BtotF = Btot * (1 / (1 + srb));
  BtotM = Btot - BtotF;
  
  /*
   * compute infant deaths from total births by sex and survival ratio
   */
  
  DxM[0] = (1 - SxM[0]) * BtotM;
  DxF[0] = (1 - SxF[0]) * BtotF;

  /*
   * project infant population at year +1 from births and infant deaths
   */
  
  PzM[0] = round(BtotM - DxM[0] + migm_end[0]);
  PzF[0] = round(BtotF - DxF[0] + migf_end[0]);

  /*
   * if end-of-period assumption then add migrants
   */
  
  if (migration_assumption == 2) {
    for(i = 0; i < nage; i++) {
      PzM[i] = PzM[i] + NMxM[i];
      PzF[i] = PzF[i] + NMxF[i];
    }
  }

  /*
   * ensure no negative population by age and sex (0.0005 is same as Abacus) Do we need these to be nonzero?
   */

  for(i = 0; i < nage; i++) {
    if(PzM[i] < 0.0005) PzM[i] = 0;
    if(PzF[i] < 0.0005) PzF[i] = 0;
  }

  /*
   * RETURN
   */

  Age[0] = 0;
  a = 0;
  for(i = 1; i < nage; i++) {
    a += step_size;
    Age[i] = a;
  }
  
  return DataFrame::create(_["Age"] = Age,
			   _["P0M"] = P0M,
			   _["P0F"] = P0F,
			   _["DxM"] = DxM,
			   _["DxF"] = DxF,
			   _["Bx"] = Bx,
			   _["PzM"] = PzM,
			   _["PzF"] = PzF);			  
}
