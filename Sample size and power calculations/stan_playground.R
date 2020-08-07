library("rstan")
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

#####
schools_dat <- list(J = 8, 
                    y = c(28,  8, -3,  7, -1,  1, 18, 12),
                    sigma = c(15, 10, 16, 11,  9, 11, 10, 18))

fit <- stan(file = 'Sample size and power calculations/schools.stan', data = schools_dat)

print(fit)
plot(fit)
pairs(fit, pars = c("mu", "tau", "lp__"))

la <- extract(fit, permuted = TRUE) # return a list of arrays 
mu <- la$mu 

### return an array of three dimensions: iterations, chains, parameters 
a <- extract(fit, permuted = FALSE) 

### use S3 functions on stanfit objects
a2 <- as.array(fit)
m <- as.matrix(fit)
d <- as.data.frame(fit)


####

schoolsDSO = stan_model(file = 'Sample size and power calculations/schools.stan')

J = 100

simu_data = list("J" = J)

schools_samples = sampling( object=schoolsDSO , data= schools_dat,
                    chains=3 , iter=10000 , warmup=200 , thin=1 )


####

N = 10
y = rep.int(3, 10)

normal_simu_data = list( y = y , N = N )

normalDSO = stan_model(file = 'Sample size and power calculations/normal.stan')

normal_stanfit = sampling(object=normalDSO, data=normal_simu_data, chains=4, iter=1000, warmup=0, thin=1)

print(normal_stanfit)
plot(normal_stanfit)


####

N = 50
z = 10
y = c(rep(1,z),rep(0,N-z))
coin_simu_data = list( y = y , N = N )

coinDSO = stan_model(file = 'Sample size and power calculations/coin_toss.stan')

coin_stanFit = sampling( object=coinDSO , data=coin_simu_data ,
                    chains=3 , iter=1000 , warmup=200 , thin=1 )

plot(coin_stanFit)
print(coin_stanFit)


####

coinDSO = stan_model(file = 'Sample size and power calculations/coin_toss_prior_sampling.stan')

coin_stanFit = sampling( object=coinDSO , data=coin_simu_data ,
                         chains=3 , iter=1000 , warmup=200 , thin=1 )

plot(coin_stanFit)
print(coin_stanFit)


######
# Prior Predictive Check
######

N = 100
x = rnorm(N, mean = 0, sd = 1)
poisson_data = list(N = N, x = x)

poissonDSO = stan_model(file = 'Sample size and power calculations/prior_predictive_poisson.stan')
  
poisson_stanFit = sampling(object=poissonDSO , data=poisson_data ,
                         chains=4 , iter=1000 , warmup=200 , thin=1,
                         algorithm="Fixed_param")
summary(poisson_stanFit)
plot(poisson_stanFit)
