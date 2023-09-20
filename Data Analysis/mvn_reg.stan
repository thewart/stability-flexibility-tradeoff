data {
  int<lower=0> N;
  int<lower=0> P;
  int<lower=0> N_Subj; //numberof subjects
  int<lower=0> N_Block; //block-level variables
  int<lower=1,upper=N_Block> Block[N];
  int<lower=1,upper=N_Subj> Subj[N];
  vector[N] Y; //reaction time
  matrix[N,P] X; //design matrix
}

// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
  matrix[P,N_Block] mu_beta; //mean effect across conditions
  
  vector<lower=0>[P] sigma_effect; //trial-level variances
  vector<lower=0>[N_Block] sigma_block; //block-level variances
  cholesky_factor_corr[P] L; //trial-level correlations
  cholesky_factor_corr[N_Block] R; //block-level correlations
  matrix[P,N_Block] z_subj[N_Subj]; //zscore for subjects
  
  real<lower=0> sigma_y; //residual
}

transformed parameters {
  matrix[P,N_Block] beta_subj[N_Subj];
  
  for (m in 1:N_Subj) beta_subj[m] = mu_beta + diag_pre_multiply(sigma_effect,L) * z_subj[m] * diag_pre_multiply(sigma_block,R);
}

// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {
  vector[N] eta;
  for (i in 1:N) eta[i] = X[i] * beta_subj[Subj[i]][,Block[i]];
  Y ~ normal(eta, sigma_y);
  
  to_vector(mu_beta) ~ normal(0,2.5);
  
  sigma_effect ~ normal(0,2.5);
  sigma_block ~ normal(0,2.5);
  for (m in 1:N_Subj) to_vector(z_subj[m]) ~ normal(0,1);
  
  sigma_y ~ normal(0,2.5);
}

generated quantities {
  corr_matrix[P] rho_Trial = tcrossprod(L);
  corr_matrix[N_Block] rho_Block = tcrossprod(R);
}
