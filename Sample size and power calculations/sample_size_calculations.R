library(ggplot2)
library(dplyr)
library(tidyr)

simulate_data <- function(n, treatment_effect){
  # Simulate independent variables
  min_meat <- 0
  max_meat <- 21
  pre_meat_servings <- round(runif(n, min_meat, max_meat), digits = 0)
  treatment <- c(rep(0, n/2), rep(1, n/2))
  
  # Simulate dependent variables
  post_meat_servings = rpois(n, pre_meat_servings * exp(treatment_effect * treatment) + 0.01)
  
  simulated_data = tibble(post_meat_servings, pre_meat_servings, treatment)
  return(simulated_data)
}

sim_data_and_fit_model <- function(sample_size, number_simulations) {
  treatment_effects = rnorm(number_simulations, -0.2, 0.1)
  data = vector("list", number_simulations)
  glm_fits = vector("list", number_simulations)
  for (i in seq_len(number_simulations)) {
    data[[i]] <- simulate_data(sample_size, treatment_effects[i])
    glm_fits[[i]] <- glm( post_meat_servings ~ pre_meat_servings + treatment,
                data = data[[i]],
                family = "poisson"
                )
  }
  return(list(data = data, glm_fits = glm_fits))
}

sim_data = simulate_data(1000, -0.2)

sample_size = 160
number_simulations = 1000
list_of_sims = sim_data_and_fit_model(sample_size, number_simulations)

coefficients(summary(list_of_sims[["glm_fits"]][[1]]))#[3,4]

###
# Descriptive Stats
###

significants = rep(0, number_simulations)
for(i in seq_len(number_simulations)){
  significants[[i]] = coefficients(summary(list_of_sims[["glm_fits"]][[i]]))[3,4]
}

summary(significants)

mean(significants < 0.05)

###
# Visualizations
###

ggplot(data = sim_data, aes(x = treatment, y = post_meat_servings, group= treatment)) +
  geom_violin(draw_quantiles = c(0.05,0.25,0.5,0.75,0.95))

sim_data <- sim_data %>%
  mutate(diff_meat_servings=post_meat_servings-pre_meat_servings)
ggplot(data = sim_data, aes(x = treatment, y = diff_meat_servings, group= treatment)) +
  geom_violin(draw_quantiles = c(0.05,0.25,0.5,0.75,0.95))

meat_long = sim_data %>%
  mutate(id = 1:nrow(sim_data)) %>%
  pivot_longer(c(post_meat_servings, pre_meat_servings),names_to = "time", values_to = "meat_servings") %>%
  mutate(time=factor(time, levels = c("pre_meat_servings", "post_meat_servings")))
ggplot(data = meat_long, aes(x = time, y = meat_servings, group = id, color = factor(treatment))) + geom_line()


###
# Fitting Poisson Regression wit stan
###
library("rstan")
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)


sim_data <- sim_data %>%
  mutate(log_pre_meat_servings = log(pre_meat_servings+0.001))

stan_data = list(N=nrow(sim_data),
                 treatment = sim_data$treatment,
                 pre_meat_servings = sim_data$log_pre_meat_servings,
                 post_meat_servings = sim_data$post_meat_servings)
pois_reg_DSO = stan_model(file = 'Sample size and power calculations/poisson_regression.stan')
pois_reg_stanFit_log = sampling(object=pois_reg_DSO , data=stan_data ,
                                    chains=4 , iter=1000 , warmup=200 , thin=1)
plot(pois_reg_stanFit_log)
print(pois_reg_stanFit_log)

traceplot((pois_reg_stanFit_log))

###
# Extract relevant info
###

posterior <- as.data.frame(pois_reg_stanFit)
posterior_log <- as.data.frame(pois_reg_stanFit_log)


summary(posterior_log)
post_interval_80 = quantile(posterior_log$beta_treatment, c(0.1,0.9))
widt_post_interval_80 = diff(post_interval_80)

prob_below_zero = sum(posterior_log$beta_treatment<0.0)/length(posterior_log$beta_treatment)

posterior %>%
  mutate(treated_12 = exp(alpha+beta_pre_meat*log(12)+beta_treatment),
         untreated_12 = exp(alpha+beta_pre_meat*log(12)),
         diff_12 = treated -untreated) ->posterior

###
# Visualization of 2 specifications of log(lambda)
###





ggplot(data = posterior, aes(diff)) + geom_density()

sim_data <- sim_data %>%
  mutate(prediction = exp(median(posterior$alpha)+
                                median(posterior$beta_pre_meat)*pre_meat_servings+
                                median(posterior$beta_treatment)*treatment),
         prediction_log = exp(median(posterior_log$alpha)+
                                median(posterior_log$beta_pre_meat)*log_pre_meat_servings+
                                median(posterior_log$beta_treatment)*treatment))

  
ggplot(sim_data, aes(x=post_meat_servings,y=prediction_log)) + geom_point()

ggplot(sim_data) +
  geom_point(aes(pre_meat_servings,post_meat_servings,color = factor(treatment)), alpha = 0.2) +
  geom_line(aes(x=pre_meat_servings,y=prediction, group=treatment, color=factor(treatment)), size=1.2)+
  geom_line(aes(x=pre_meat_servings,y=prediction_log, group=treatment, color=factor(treatment)), linetype= 10, size=1.2)






