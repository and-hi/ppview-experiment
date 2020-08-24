library(ggplot2)
library(dplyr)

library("rstan")
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
###
# Simulate independent variables
###

n <- 100

min_age <- 18
max_age <- 29
age <- runif(n, min_age, max_age)

gender <-c(rep.int(0, n/2),rep.int(1, n/2))

treatment <- c(rep.int(c(rep.int(0, n/4), rep.int(1, n/4)),2))

min_meat <- 0
max_meat <- 21
pre_meat_servings <- runif(n, min_meat, max_meat)

###
# Simulate model parameters
###

beta_gender <- rep.int(rnorm(1, mean = 0, sd =1), 100)
beta_treatment <- rep.int(rnorm(1, mean = -5, sd =5), 100)
beta_treatment_gender <- rep.int(rnorm(1, mean = 0, sd =1), 100)


###
# Simulate dependent variables
###

post_meat_servings <- pre_meat_servings + beta_gender*gender + beta_treatment*treatment + beta_treatment_gender*treatment*gender
# missing noise

###
# Fit model
###

ols <- glm( post_meat_servings ~ pre_meat_servings + treatment*gender)

summary(ols)

###
# Visualizations
###
data_simu = tibble(id = seq(1:100), post_meat_servings, pre_meat_servings, gender, treatment = as.factor(treatment), age, beta_gender, beta_treatment, beta_treatment_gender)
qplot(x = treatment, y = post_meat_servings)

ggplot(data = data_simu, aes(x = treatment, y = post_meat_servings, group= treatment)) + geom_boxplot()



meat_long = tibble(id = rep(seq(1:100),2), time = c(rep("pre", 100), rep("post", 100)), meat_servings = c(pre_meat_servings, post_meat_servings))
ggplot(data = meat_long, aes(x = time, y = meat_servings, group = id)) + geom_line()

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
pre_meat_servings_random = rep(10,2)  #rpois(N, 10)
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
