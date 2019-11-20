// Compute the negative clog-likelihood of the CONDITIONAL likelihood function for SPAS
// The conditioning is on n(total number of animals seen) and uses the poisson likelihood of Plante et al

// Refer to http://kaskr.github.io/adcomp/matrix_arrays_8cpp-example.html for help in coding the log-likelihood function

// Refer to https://github.com/kaskr/adcomp/wiki/Development
// on instructions for including TMB code in an R package

#define TMB_LIB_INIT R_init_SPAS // stops R warning about symbolw
#include <TMB.hpp>                                // Links in the TMB libraries
template<class Type>
Type objective_function<Type>::operator() () 
{
  DATA_VECTOR(n_10);  // tags applied but not recovered (length s)
  DATA_VECTOR(u);     // unmarked recovered in each recovery stratum (length t)
  DATA_MATRIX(m);     // matrix of releases and recoveries (s x t)
  
  DATA_MATRIX(rowDM);  // row design matrix (for capture probabilities)
  
  // The order of these parameter statements determines the order of the estimates in the vector of parameters
  // If you change this, you need to also make changes in SPAS.fit.model() where we expand the beta estimates by the design
  // matrices. ***** CAREFUL HERE *****
  PARAMETER_VECTOR(beta_cap);    // probability of capture corresponding to the design matrix on logit scale
  PARAMETER_MATRIX(beta_theta) ; // expected number of recoveries in the movement matrix on the log() scale
  PARAMETER_VECTOR(beta_psi);    // number or tags applied and not recovered on log scale
  
  Type f;                                         // Declare the "objective function" (neg. log. likelihood)

     // Type tiny = Type(.0001);        // used to avoid log(0)
     // convert the beta values (from the design matrix) to actual values of parameters as needed
     // CaptureProbabilities
     vector<Type> logit_cap = (rowDM * beta_cap).col(0);  // convert from beta to logit values
     vector<Type> cap = invlogit(logit_cap);     // convert to a vector
 
     // Psi terms
     vector<Type> psi = exp(beta_psi); // convert from log() to regular scale
 
     // Theta terms
     matrix<Type> theta = exp(beta_theta.array());  // convert from log() to regular scale

     //matrix<Type> part2aa(theta.rows(), theta.cols());   // log-likelihood due to movement
     //for(int i=0;i<theta.rows();i++)
     //   for(int j=0;j<theta.cols();j++)
     //   part2aa(i,j)=(theta(i,j)+tiny);  // add a small constant to avoid taking log(0)
  
     matrix<Type>   part2c = m.array()*beta_theta.array();  // log-likelihood due to movement
     Type part2 =   part2c.sum()- theta.sum();

     vector<Type>   part3c = n_10 * beta_psi;  // log-likelihood due to animals tagged and not recaptured
     Type part3   = part3c.sum() - psi.sum() ;

     vector<Type>   part4aa = (Type(1.0)-cap);  // log-likelihood for fish recovered but not tagged
     vector<Type>   part4a = part4aa/ cap;
     matrix<Type>   part4b = part4a.transpose();
     matrix<Type>   part4ca = part4b * theta;
     vector<Type>   part4d  = part4ca.row(0);  // convert to a vector
     vector<Type>   part4e = log(part4d.array());
     vector<Type>   part4f = u * part4e;
     Type part4 =   part4f.sum() - part4d.sum();

     f = -(part2+ part3 + part4) ;
    
    // compute estimate of number of fish never seen
     Type neverseen = (part4a * psi).sum();
     REPORT(cap);
     REPORT(psi);
     REPORT(theta);
     //REPORT(part2);
     //REPORT(part3);
     //REPORT(part4);
     REPORT(neverseen);
     REPORT(rowDM);
     return f;
};

     
