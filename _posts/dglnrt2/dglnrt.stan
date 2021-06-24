data{
  int <lower=1> I;              // number of examinees          
  int <lower=1> J;              // number of items
  real RT[I,J];                 // matrix  the log of responses
}

parameters {
  real mu_beta;                 // mean for time intensity parameters
  real<lower=0> sigma_beta;     // sd for time intensity parameters
  
  real mu_alpha;                // mean for log of time discrimination parameters
  real<lower=0> sigma_alpha;    // sd for time discrimination parameters
  
  real<lower=0> sigma_taut;     // sd for tau_t
  real<lower=0> sigma_tauc;     // sd for tau_c
  
  corr_matrix[2] omega_P;       // 2 x 2 correlation matrix for person parameters
  corr_matrix[2] omega_I;       // 2 x 2 correlation matrix for item parameters
  
  vector<lower=0,upper=1>[J] pC; // vector of length J for the probability of item compromise status
  
  vector<lower=0,upper=1>[I] pH; // vector of length I for the probability of examinee item peknowledge 
  
  ordered[2] person[I];           // an array with length I for person specific latent parameters
  // Each array has two elements
  // first element is tau_t
  // second element is tau_c
  // ordered vector makes sure that tau_c > tau_t for every person
  // to make sure chains are exploring the same mode and 
  // do not go east and west leading multi-modal posteriors
  
  
  vector[2] item[J];           // an array with length J for item specific parameters
  // each array has two elements
  // first element is alpha
  // second element is beta
}

transformed parameters{
  
  vector[2] mu_P;                        // vector for mean vector of person parameters 
  vector[2] mu_I;                        // vector for mean vector of item parameters
  
  vector[2] scale_P;                     // vector of standard deviations for person parameters
  vector[2] scale_I;                     // vector of standard deviations for item parameters
  
  cov_matrix[2] Sigma_P;                 // covariance matrix for person parameters
  cov_matrix[2] Sigma_I;                 // covariance matrix for person parameters
  
  mu_P[1] = 0;
  mu_P[2] = 0;
  
  scale_P[1] = sigma_taut;               
  scale_P[2] = sigma_tauc;
  
  Sigma_P = quad_form_diag(omega_P, scale_P); 
  
  mu_I[1] = mu_alpha;
  mu_I[2] = mu_beta;
  
  scale_I[1] = sigma_alpha;               
  scale_I[2] = sigma_beta;
  
  Sigma_I = quad_form_diag(omega_I, scale_I); 
  
}

model{
  
  sigma_taut  ~ exponential(1);
  sigma_tauc  ~ exponential(1);
  sigma_beta  ~ exponential(1);
  sigma_alpha ~ exponential(1);
  
  mu_beta      ~ normal(4,1);
  mu_alpha     ~ lognormal(0,0.5);
  
  pC ~ beta(1,1);
  pH ~ beta(1,1);
  
  omega_P   ~ lkj_corr(1);
  omega_I   ~ lkj_corr(1);
  
  person  ~ multi_normal(mu_P,Sigma_P);
  
  item    ~ multi_normal(mu_I,Sigma_I);
  
  
  for (i in 1:I) {
    for(j in 1:J) {
      
      // item[j,1] represents log of parameter alpha of the jth item
          // that's why we use exp(item[j,1]) below 
      // item[j,2] represents parameter beta of the jth item
      
      //person[i,1] represents parameter tau_t of the ith person
      //person[i,2] represents parameter tau_c of the ith person
      
      
      real p_t = item[j,2]-person[i,1];   //expected response time for non-cheating response
      real p_c = item[j,2]-person[i,2];  //expected response time for cheating response
      
      // log of probability densities for each combination of two discrete parameters
      // (C,T) = {(0,0),(0,1),(1,0),(1,1)}
      
      real lprt1 = log1m(pC[j]) + log1m(pH[i]) + normal_lpdf(RT[i,j] | p_t, 1/exp(item[j,1]));  // T = 0, C=0
      real lprt2 = log1m(pC[j]) + log(pH[i])   + normal_lpdf(RT[i,j] | p_t, 1/exp(item[j,1]));  // T = 1, C=0
      real lprt3 = log(pC[j])   + log1m(pH[i]) + normal_lpdf(RT[i,j] | p_t, 1/exp(item[j,1]));  // T = 0, C=1
      real lprt4 = log(pC[j])   + log(pH[i])   + normal_lpdf(RT[i,j] | p_c, 1/exp(item[j,1]));  // T = 1, C=1 
      
      target += log_sum_exp([lprt1, lprt2, lprt3, lprt4]);
      
    }
  }
  
}