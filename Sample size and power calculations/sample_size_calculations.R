library(ggplot2)
library(dplyr)

###
# Simulate independent variables
###

n <- 100

treatment <- c(rep.int(c(rep.int(0, n/4), rep.int(1, n/4)),2))

min_meat <- 0
max_meat <- 21
pre_meat_servings <- runif(n, min_meat, max_meat)

###
# Simulate model parameters
###


beta_treatment <- rep.int(rnorm(1, mean = -5, sd =5), 100)


###
# Simulate dependent variables
###

post_meat_servings <- pre_meat_servings + beta_treatment*treatment
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


####

simulate_data <- function(n, treatment_effect){
  min_meat <- 0
  max_meat <- 21
  pre_meat_servings <- round(runif(n, min_meat, max_meat), digits = 0)
  treatment <- c(rep(0, n/2), rep(1, n/2))
  post_meat_servings = rpois(n, pre_meat_servings * exp(treatment_effect * treatment) + 0.01)
  simulated_data = tibble(post_meat_servings, pre_meat_servings, treatment)
  return(simulated_data)
}

sim_data_and_fit_model <- function(sample_size, number_simulations) {
  treatment_effects = rnorm(number_simulations, -0.2, 0.1)
  data = vector("list", number_simulations)
  glm_fit = vector("list", number_simulations)
  for (i in seq_len(number_simulations)) {
    data[[i]] <- simulate_data(sample_size, treatment_effects[i])
    glm_fit[[i]] <- glm( post_meat_servings ~ pre_meat_servings + treatment,
                data = data[[i]],
                family = "poisson"
                )
  }
  return(data, glm_fit)
  
}

sim_data = simulate_data(10, -0.2)

list_of_sims = sim_data_and_fit_model(10, 5)
sample_size =10
number_simulations = 5
i =1
