// ==================================================================================== //
// Crab Catch-Survey-Analysis (CSA) based on Zheng et al. (2007)
// Version 1
// 
// Author: Tyler Jackson
//
// Last Update: 6/8/2023
//
// Indexs: 
// i: rows
// j: columns
// iyear: years
// jstage: stages
// xtage: columns of transition matrix
// ==================================================================================== //


#include <TMB.hpp>
template<class Type>
  Type objective_function<Type>::operator() ()
{
	// options

	DATA_INTEGER(recruit_likelihood);
	DATA_INTEGER(recruit_stage);
	DATA_INTEGER(preM_tau);


	// data
	DATA_VECTOR(yrs);				  // years with survey data
	DATA_VECTOR(catch_num); 	// retained catch in number 		  
	DATA_MATRIX(obs_index);		// survey abundance index by stage 
	DATA_VECTOR(tau_cs);			// fraction of year between mid-fishery and mid-surey
	DATA_VECTOR(tau_s);				// fraction of year between survey(i) and (i-1)
	DATA_SCALAR(M);						// natural mortality on post-recruit stages
	DATA_MATRIX(lambdas);		  // stage data weighting 

	DATA_MATRIX(avg_wt);			// average survey weight (lb) for pre-recruits and recruits nd year;



	// parameters
	PARAMETER_VECTOR(ln_index_init);		  // initial abundance index

	PARAMETER_VECTOR(trans_probs);				// stage transition probs diag

	PARAMETER(ln_Rbar);										// mean recruitment
	PARAMETER_VECTOR(Eps_R);				  		// annual recruitment
	PARAMETER(ln_sigmaR);									// recruitment random effect se

	PARAMETER(preM);						        	// offset to natural mortality of pre-recruit stages
	PARAMETER(ln_q);						  				// catchability
	PARAMETER(ln_mu);											// relative catchability of precruits
	
	// derived duantities
	int nyr = yrs.size();			  		// number of years to estimate
	int nstage = obs_index.row(0).size();		// number of stages
	matrix<Type> index(nyr,nstage);	  			// matrix of predicted index
	matrix<Type> abundance(nyr,nstage);			// matrix of predicted abundance
	vector<Type> PMB(nyr);									// prerecruit male biomass
	vector<Type> MMB(nyr);									// mature male biomass
	vector<Type> LMB(nyr);									// legal male biomass


	// transform pars
	vector<Type> index_init = exp(ln_index_init);
	Type Rbar = exp(ln_Rbar);
	Type sigmaR = exp(ln_sigmaR);
	Type q = exp(ln_q);
	Type mu = exp(ln_mu);


	// transition matrix
	matrix<Type> X(nstage,nstage);
  for (int i=0; i<nstage; i++) {
  	for (int j=0; j<nstage; j++){

  		if(j>i) {X(i,j) = 0;}
  		if((i-j)>=2) {X(i,j) = 0;}
  		if(i==(nstage-1) & j==(nstage-1)) {X(i,j) = 1;}
  		if(i==j) {X(i,j) = trans_probs(i);}
  		if((i-j)==1) {X(i,j) = trans_probs(j+nstage-1);} 

  	}
  }

  // catch matrix
  matrix<Type> catch_mat(nyr-1,nstage);
  for (int iyear=0; iyear<(nyr-1); iyear++) {
  	for (int jstage=0; jstage<nstage; jstage++){
  		if(jstage<(recruit_stage)) {catch_mat(iyear,jstage) = 0;}
  		if(jstage==(recruit_stage)) {catch_mat(iyear,jstage) = q*catch_num(iyear)*exp(-M*tau_cs(iyear));}
  	}
  }

	// procedure

	// fill predicted index matrix
	for (int iyear = 0; iyear < nyr; iyear++) {
		index(iyear, 0) = Rbar * exp(Eps_R(iyear));
		if(iyear==0) {
			for (int jstage = 1; jstage < nstage; jstage++) {
				index(iyear,jstage) = index_init(jstage-1);
				}
			}			

		if(iyear>0) {
			for (int jstage = 1; jstage < nstage; jstage++) {index(iyear,jstage) = 0;} 
			for (int jstage = 0; jstage < nstage; jstage++) {
				for (int xstage = 0; xstage < nstage; xstage++) {
					if(jstage<(recruit_stage)) {
						if(preM_tau == 0){index(iyear,jstage) += X(jstage,xstage) * index(iyear-1,xstage) * mu * exp(preM);}
						if(preM_tau == 1){index(iyear,jstage) += X(jstage,xstage) * index(iyear-1,xstage) * mu * exp(-(M+preM)*tau_s(iyear-1));}
					}
					if(jstage>=(recruit_stage)) {
					 	index(iyear,jstage) += X(jstage,xstage) * index(iyear-1,xstage) * exp(-M*tau_s(iyear-1));
					}
				}
			index(iyear, jstage) = std::max(Type(0.001), index(iyear, jstage) - catch_mat(iyear-1,jstage));
			}
		}	
	}



	// output vectors
	for (int iyear = 0; iyear < nyr; iyear++) {
		PMB(iyear) = 0; LMB(iyear) = 0; MMB(iyear) = 0; 
		for (int jstage = 0; jstage < nstage; jstage++) {
			abundance(iyear, jstage) = index(iyear, jstage) / q;
			if(jstage < (recruit_stage-1)) {PMB(iyear) += abundance(iyear, jstage) * avg_wt(iyear, 0);}
			if(jstage >= (recruit_stage-1)) {LMB(iyear) += abundance(iyear, jstage) * avg_wt(iyear, 1);}
			MMB(iyear) = PMB(iyear) + LMB(iyear);
		}
	}
	

	// objective function

	Type obj_fun = 0;
	Type index_lik = 0;
	Type R_lik = 0;

  // index likelihood
	for (int iyear = 0; iyear < nyr; iyear++) {
		for (int jstage = 0; jstage < nstage; jstage++) {
				index_lik += pow(log(obs_index(iyear,jstage)) - log(index(iyear,jstage)), Type(2)) * lambdas(iyear,jstage);
			}
		}
	// recruitment likelihood
	if(recruit_likelihood == 1) {
		for (int iyear = 0; iyear < nyr; iyear++) {
			R_lik += pow(Eps_R(iyear), Type(2)) / pow(sigmaR, Type(2)) + log(pow(sigmaR, Type(2)));
			}
		R_lik = R_lik * (0.5);
		}
		
	obj_fun = index_lik + R_lik;

 	// report
   
  REPORT(yrs); 
  REPORT(X);
 	REPORT(index);
 	REPORT(abundance);
 	ADREPORT(PMB);
 	ADREPORT(LMB);
 	ADREPORT(MMB);
 	REPORT(Eps_R);
 	REPORT(index_lik);
 	REPORT(R_lik);
 	REPORT(obj_fun);
 	
   
	return(obj_fun);
}
