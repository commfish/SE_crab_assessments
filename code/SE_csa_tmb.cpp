// SE King and Tanner crab CSA


#include <TMB.hpp>
template<class Type>
  Type objective_function<Type>::operator() ()
{
	// Input Data
	
	DATA_VECTOR(survey_yrs);								// years with survey data (nyr)
	DATA_INTEGER(nstage);									// number of stages
	DATA_VECTOR(ret_cat_lbs);							    // retained catch in lbs (nyr)
	DATA_MATRIX(index);										// survey abundance index by stage (nyr, nstage)
	DATA_VECTOR(tau_cs);									// fraction of year between mid-fishery and mid-surey
	DATA_VECTOR(tau_s);										// fraction of year between survey(i) and (i-1)
	
	DATA_SCALAR(M);											// natural mortality on post-recruit stages
	DATA_VECTOR(wt_survey);									// survey data weighting (nyr)
	
	
	
	// Parameters
	
	PARAMETER_VECTOR(ln_index_init);						// initial abundance index (nstage)
	PARAMETER_VECTOR(ln_rec);								// annual recruitment (nyr - 1)
	PARAMETER(ln_preM);										// natural mortality of pre-recruits
	PARAMETER(ln_q);										// catchability
	
	
	
	// Derived Quantities
	
	int nyr = survey_yrs.size();							// number of years to estimate
	
	matrix<Type> pred_index(nyr,nstage);					// vector of predicted catch
	
	vector<Type> survey_ssq(nyr);							// survey SSQ
	Type obj_fun = 0;										// objective function
	
	
	
	// Procedure
	
	// fill in predicted index
	for (int j = 0; j < nstage; j++) {
		pred_index(0,j) = exp(ln_index_init(j));
	}
	for (int i = 1; i < nyr; i++) {
		pred_index(i,0) = exp(ln_rec(i-1));
	}
	for (int i = 1; i < nyr; i++) {
		for (int j = 1; j < nstage; j++) {
			// recruits
			if(j < nstage-1) {
			pred_index(i,j) = pred_index(i-1,j-1)*exp(ln_preM);
			}
			// plus group
			if(j == nstage-1) {
			pred_index(i,j) = std::max(Type(0.001), ((pred_index(i-1,j-1) + pred_index(i-1,j)) * exp(-M*tau_s(i-1))) - (exp(ln_q) * ret_cat_lbs(i-1) * exp(-M*tau_cs(i))));
			}	
		}
	}
	
	// compute annual survey ssq
	for (int i = 0; i < nyr; i++) {
		for (int j = 0; j < nstage; j++) {
			survey_ssq(i) += pow(log(index(i,j)) - log(pred_index(i,j)), Type(2)) * wt_survey(i);
		}
	}
	
	// compute objective function
	for (int i = 0; i < nyr; i++) {
		obj_fun += survey_ssq(i);
	}



   // Report
   
   REPORT(pred_index);
   
   return(obj_fun);
}


	
	
	