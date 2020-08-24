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
