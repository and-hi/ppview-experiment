data {
  int<lower = 0> N;
  vector[N] pre_meat_servings;
  vector[N] treatment;
}
// generated quantities {
//   real beta_treatment = normal_rng(-3, 5);
//   //real<lower=0> sigma = lognormal_rng(0, 3.5);
//   real<lower=0> sigma = inv_gamma_rng(5, 12); //mean and variance = 3
//   real post_meat_servings_sim[N] = normal_rng(pre_meat_servings + beta_treatment * treatment, sigma);
// }
generated quantities {
  real beta_treatment = inv_gamma_rng(5.2, 3.36); //mean 0.8, variance 0.2
  //real<lower=0> sigma = lognormal_rng(0, 3.5);
  //real<lower=0> sigma = inv_gamma_rng(5, 12); //mean and variance = 3
  int<lower = 0> post_meat_servings_sim[N] = poisson_rng(pre_meat_servings + 1 + beta_treatment * treatment);
}
