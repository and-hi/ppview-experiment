library(ggplot2)
library(dplyr)

library("rstan")
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

###
# Prior predictive check with fixed pre_meat_servings
###

N = 44
pre_meat_servings = rep(0:(N/2-1),2)
treatment = c(rep(0,N/2), rep(1,N/2))
prior_predictive_data = list(N = N, pre_meat_servings = pre_meat_servings, treatment=treatment)

prior_predictive_DSO = stan_model(file = 'Sample size and power calculations/prior_predictive.stan')
prior_predictive_stanFit = sampling(object=prior_predictive_DSO , data=prior_predictive_data ,
                           chains=4 , iter=1000 , warmup=0 , thin=1,
                           algorithm="Fixed_param")
plot(prior_predictive_stanFit, pars = c("beta_treatment", "post_meat_servings_sim[1]", "post_meat_servings_sim[23]", "post_meat_servings_sim[2]", "post_meat_servings_sim[24]"), show_density=TRUE)
plot(prior_predictive_stanFit, pars = c("beta_treatment", "post_meat_servings_sim[21]", "post_meat_servings_sim[43]", "post_meat_servings_sim[22]", "post_meat_servings_sim[44]"), show_density=TRUE)
print(prior_predictive_stanFit)

df <- as.data.frame(prior_predictive_stanFit)
summary(prior_predictive_stanFit@sim['samples'][[1]][[1]]$'post_meat_servings_sim[43]'/prior_predictive_stanFit@sim['samples'][[1]][[1]]$'post_meat_servings_sim[21]')
summary(prior_predictive_stanFit@sim['samples'][[1]][[1]]$'post_meat_servings_sim[2]'/prior_predictive_stanFit@sim['samples'][[1]][[1]]$'post_meat_servings_sim[24]')

str(prior_predictive_stanFit@sim['samples'][[1]][[1]])

###
# Prior predictive check with random pre_meat_servings
###

n = 100
N = 2
pre_meat_servings_random = rpois(N, 10)
treatment = c(rep(0,N/2), rep(1,N/2))
prior_predictive_data_random = list(N = N, pre_meat_servings = pre_meat_servings_random, treatment=treatment)
prior_predictive_DSO = stan_model(file = 'Sample size and power calculations/prior_predictive.stan')
prior_predictive_stanFit_random = sampling(object=prior_predictive_DSO, data=prior_predictive_data_random ,
                                    chains=1 , iter=n , warmup=0 , thin=1,
                                    algorithm="Fixed_param")

print(prior_predictive_stanFit_random)
plot(prior_predictive_stanFit_random)

data_ppc_random <- as.data.frame(prior_predictive_stanFit_random)

data_ppc_random %>%
  select(`post_meat_servings_sim[1]`, `post_meat_servings_sim[2]`) %>%
  stack() %>%
  mutate(treatment = ifelse(ind=="post_meat_servings_sim[1]",0,1)) %>%
  rename(post_meat_servings = values) %>%
  select(-ind) -> data_simu





####
invgamma = stan(file = 'Sample size and power calculations/inv_gamma.stan', algorithm="Fixed_param")
plot(invgamma)
exp(20.7944)
