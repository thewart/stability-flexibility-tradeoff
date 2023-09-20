//
// This Stan program defines a simple model, with a
// vector of values 'y' modeled as normally distributed
// with mean 'mu' and standard deviation 'sigma'.
//
// Learn more about model development with Stan at:
//
//    http://mc-stan.org/users/interfaces/rstan.html
//    https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started
//

// The input data is a vector 'y' of length 'N'.
data {
  int<lower=0> N;
  int<lower=0> N_Subj; //numberof subjects
  int<lower=0> N_Trial; //trial-level variables
  int<lower=0> N_Block; //block-level variables
  int<lower=1,upper=N_Subj> Subj[N]; //subject num
  int<lower=1,upper=N_Trial> Trial[N];
  int<lower=1,upper=N_Block> Block[N];
  vector[N] Y; //reaction time
}

// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
  real mu_0; //global mean
  real<lower=0> sigma_0; //sdev across conditions
  matrix[N_Trial,N_Block] z_cond; //zscore for conditions
  
  vector<lower=0>[N_Trial] sigma_trial; //trial-level variances
  vector<lower=0>[N_Block] sigma_block; //block-level variances
  cholesky_factor_corr[N_Trial] L; //trial-level correlations
  cholesky_factor_corr[N_Block] R; //block-level correlations
  matrix[N_Trial,N_Block] z_subj[N_Subj]; //zscore for subjects
  
  real<lower=0> sigma_y; //residual
}

transformed parameters {
  matrix[N_Trial,N_Block] mu_cond = mu_0 + sigma_0 * z_cond;
  matrix[N_Trial,N_Block] mu_subj[N_Subj];
  
  for (m in 1:N_Subj) mu_subj[m] = mu_cond + diag_pre_multiply(sigma_trial,L) * z_subj[m] * diag_pre_multiply(sigma_block,R);
}

// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {
  vector[N] eta;
  for (i in 1:N) eta[i] = mu_subj[Subj[i]][Trial[i],Block[i]];
  Y ~ normal(eta, sigma_y);
  
  mu_0 ~ normal(0,1);
  sigma_0 ~ normal(0,2.5);
  to_vector(z_cond) ~ normal(0,1);
  
  sigma_trial ~ normal(0,2.5);
  sigma_block ~ normal(0,2.5);
  for (m in 1:N_Subj) to_vector(z_subj[m]) ~ normal(0,1);
  
  sigma_y ~ normal(0,2.5);
}

generated quantities {
  corr_matrix[N_Trial] rho_Trial = tcrossprod(L);
  corr_matrix[N_Block] rho_Block = tcrossprod(R);
}
