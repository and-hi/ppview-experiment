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
// generated quantities {
//   real beta_treatment = normal_rng(-0.2, 0.1);
//   int<lower = 0> post_meat_servings_sim[N] = poisson_rng(pre_meat_servings .* exp(beta_treatment * treatment) + 0.01);
// }
// generated quantities {
//   real beta_treatment = normal_rng(-0.2, 0.1);
//   int<lower = 0> post_meat_servings_sim[N] = poisson_log_rng(log(pre_meat_servings + 0.01) + beta_treatment * treatment);
// }
generated quantities {
  real beta_treatment = lognormal_rng(-0.2, 0.1);
  int<lower = 0> post_meat_servings_sim[N] = poisson_log_rng(log(pre_meat_servings .* (beta_treatment * treatment) +0.01));
}
