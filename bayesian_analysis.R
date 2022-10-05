# Bayesian models (no RE)

# Megan Verma, 10/5/2022

setwd("/Users/meganverma/Desktop/git/MeganVermaMsc")

# Load all packages required for analysis
source("mscprojectscript_01_load_packages.R")

# Load RESPICAR data 
source("mscprojectscript_02_load_data.R")

# make carriage variable
respicar_socio     <- respicar_socio %>% 
    mutate(carriage = Positive/Total)

library(rethinking)
# Univariate models:------

# urban percent----

# standardize variables
respicar_socio$urban_percent_scaled <- scale(respicar_socio$urban_percent)
respicar_socio$carriage_scaled <- scale(respicar_socio$carriage)

# compute approximate quadratic posterior dist
m1 <- quap(alist(
    carriage_scaled ~ dnorm(mu , sigma) ,
    mu <- a + bU * urban_percent_scaled ,
    a ~ dnorm(0 , 0.2) , #restrict intercept to 0.2 SDs
    bU ~ dnorm(0 , 0.5) , #restrict to not extreme relationships
    sigma ~ dexp(1) #restrict to positive SDs, avg displacement is 1
) ,
data = respicar_socio)

# simulate from priors 
# plot lines over the range of 2 standard deviations for both outcome & predictor
set.seed(10)
prior <- extract.prior(m1)
mu <- link(m1 , post = prior , 
           data = list(urban_percent_scaled = c(-2, 2)))
plot(NULL , xlim = c(-2, 2) , ylim = c(-2, 2))
for (i in 1:50)
    lines(c(-2, 2) , mu[i, ] , col = col.alpha("black", 0.4))

# posterior predictions 
# compute percentile interval of mean
U_seq <- seq(from           = -3 ,
             to             = 3.2 ,
             length.out     = 30)
mu <- link(m1 , data      = list(urban_percent_scaled = U_seq))
mu.mean <- apply(mu , 2, mean)
mu.PI <- apply(mu , 2 , PI)

# plot it all
plot(carriage_scaled ~ urban_percent_scaled ,
     data  = respicar_socio ,
     col   = rangi2)
lines(U_seq ,
      mu.mean ,
      lwd  = 2)
shade(mu.PI ,
      U_seq)

precis(m1) # slope of b for urban percent: -0.17 (slight neg relationship)

