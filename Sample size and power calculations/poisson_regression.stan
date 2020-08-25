data {
  int<lower = 0> N;
  vector[N] pre_meat_servings;
  vector[N] treatment;
  int<lower=0> post_meat_servings[N];
}

parameters {
  real beta_treatment;
  real beta_pre_meat;
  real alpha;
}

model {
  beta_treatment ~ normal(0, 1);
  beta_pre_meat ~ normal(0,1);
  alpha ~ normal(0,1);
  post_meat_servings ~ poisson_log(alpha + beta_pre_meat * log(pre_meat_servings + 0.01) + beta_treatment * treatment);
}
