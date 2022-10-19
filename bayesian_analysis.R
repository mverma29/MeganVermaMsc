# Bayesian LINEAR models (no RE)

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
conflict_prefer("stan", "rethinking")

# Univariate models:------

# urban percent----

# standardize variables (tenth of urban percent)
respicar_socio$urban_percent_scaled <- scale(respicar_socio$urban_percent_tenth)

respicar_socio$carriage_scaled <- scale(respicar_socio$carriage)

summary(respicar_socio$urban_percent_scaled) # -2.4 to 1.6

# compute approximate quadratic posterior dist
m1 <- quap(alist(
    carriage_scaled ~ dnorm(mu , sigma) ,
    mu <- a + bU * urban_percent_scaled ,
    a ~ dnorm(0 , 0.2) , #restrict intercept to 0.2 SDs
    bU ~ dnorm(0 , 0.5) , #restrict to not extreme relationships
    sigma ~ dexp(1) #restrict to positive SDs, avg displacement is 1
) ,
data = respicar_socio)

# prior here is 50% prior prob that the effect size is between -0.33 and +0.33
# prior is a N(0, 0.5) then the inner 50% is the 25%ile to the 75%ile
qnorm(p= c(0.25, 0.75), sd=0.5)

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
U_seq <- seq(from           = -2.4 ,
             to             = 1.6 ,
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

precis(m1) # slope of b for urban percent: -0.17 (neg relationship)

# gdp??? ----

# use log10 GDP (scaled variable) against standardized carriage
summary(respicar_socio$log_gdp) # 2.2 to 4.9

# compute approximate quadratic posterior dist
m2 <- quap(alist(
    carriage_scaled ~ dnorm(mu , sigma) ,
    mu <- a + bG * log_gdp ,
    a ~ dnorm(0 , 0.2) , #restrict intercept to 0.2 SDs
    bG ~ dnorm(0 , 0.5) , #restrict to not extreme relationships
    sigma ~ dexp(1) #restrict to positive SDs, avg displacement is 1
) ,
data = respicar_socio)

# simulate from priors 
# plot lines over the range of 2 standard deviations for both outcome & predictor
set.seed(10)
prior <- extract.prior(m2)
mu <- link(m2 , post = prior , 
           data = list(log_gdp = c(-2, 2)))
plot(NULL , xlim = c(-2, 2) , ylim = c(-2, 2))
for (i in 1:50)
    lines(c(-2, 2) , mu[i, ] , col = col.alpha("black", 0.4))

# posterior predictions 
# compute percentile interval of mean
G_seq <- seq(from           = 2.2 ,
             to             =  5,
             length.out     = 30)
mu <- link(m2 , data      = list(log_gdp = G_seq))
mu.mean <- apply(mu , 2, mean)
mu.PI <- apply(mu , 2 , PI)

# plot it all
plot(carriage_scaled ~ log_gdp ,
     data  = respicar_socio ,
     col   = rangi2)
lines(G_seq ,
      mu.mean ,
      lwd  = 2)
shade(mu.PI ,
      G_seq)

precis(m2) # slope of b for log gdp: -0.09 (very slight neg relationship)

# gini ---- 

# standardize variables (gini percent, tenth)
respicar_socio$gini_scaled <- scale(respicar_socio$gini_tenth)

summary(respicar_socio$gini_scaled) # -2 to 3.02

# compute approximate quadratic posterior dist
m3 <- quap(alist(
    carriage_scaled ~ dnorm(mu , sigma) ,
    mu <- a + bG * gini_scaled ,
    a ~ dnorm(0 , 0.2) , #restrict intercept to 0.2 SDs
    bG ~ dnorm(0 , 0.5) , #restrict to not extreme relationships
    sigma ~ dexp(1) #restrict to positive SDs, avg displacement is 1
) ,
data = respicar_socio)

# simulate from priors 
# plot lines over the range of 2 standard deviations for both outcome & predictor
set.seed(10)
prior <- extract.prior(m3)
mu <- link(m3 , post = prior , 
           data = list(gini_scaled = c(-2, 2)))
plot(NULL , xlim = c(-2, 2) , ylim = c(-2, 2))
for (i in 1:50)
    lines(c(-2, 2) , mu[i, ] , col = col.alpha("black", 0.4))

# posterior predictions 
# compute percentile interval of mean
G_seq <- seq(from           = -2 ,
             to             = 3.1 ,
             length.out     = 30)
mu <- link(m3 , data      = list(gini_scaled = G_seq))
mu.mean <- apply(mu , 2, mean)
mu.PI <- apply(mu , 2 , PI)

# plot it all
plot(carriage_scaled ~ gini_scaled ,
     data  = respicar_socio ,
     col   = rangi2)
lines(G_seq ,
      mu.mean ,
      lwd  = 2)
shade(mu.PI ,
      G_seq)

precis(m3) # slope of b for gini: 0.11 (slight pos relationship)

# mean_hh ---- 

# standardize variables
respicar_socio$mean_hh_scaled <- scale(respicar_socio$mean_hh)
summary(respicar_socio$mean_hh_scaled) # -1.3 to 3.5

# compute approximate quadratic posterior dist
m4 <- quap(alist(
    carriage_scaled ~ dnorm(mu , sigma) ,
    mu <- a + bH * mean_hh_scaled ,
    a ~ dnorm(0 , 0.2) , #restrict intercept to 0.2 SDs
    bH ~ dnorm(0 , 0.5) , #restrict to not extreme relationships
    sigma ~ dexp(1) #restrict to positive SDs, avg displacement is 1
) ,
data = respicar_socio)

# simulate from priors 
# plot lines over the range of 2 standard deviations for both outcome & predictor
set.seed(10)
prior <- extract.prior(m4)
mu <- link(m4 , post = prior , 
           data = list(mean_hh_scaled = c(-2, 2)))
plot(NULL , xlim = c(-2, 2) , ylim = c(-2, 2))
for (i in 1:50)
    lines(c(-2, 2) , mu[i, ] , col = col.alpha("black", 0.4))

# posterior predictions 
# compute percentile interval of mean
H_seq <- seq(from           = -1.3 ,
             to             = 3.5 ,
             length.out     = 30)
mu <- link(m4 , data      = list(mean_hh_scaled = H_seq))
mu.mean <- apply(mu , 2, mean)
mu.PI <- apply(mu , 2 , PI)

# plot it all
plot(carriage_scaled ~ mean_hh_scaled ,
     data  = respicar_socio ,
     col   = rangi2)
lines(H_seq ,
      mu.mean ,
      lwd  = 2)
shade(mu.PI ,
      H_seq)

precis(m4) # slope of b for mean_hh: 0.17 (pos relationship)

# female_ed ---- 

# standardize variables (female ed tenth)
respicar_socio$female_ed_scaled <- scale(respicar_socio$female_ed_tenth)
summary(respicar_socio$female_ed_scaled) # -2.4 to 2.7

# compute approximate quadratic posterior dist
m5 <- quap(alist(
    carriage_scaled ~ dnorm(mu , sigma) ,
    mu <- a + bF * female_ed_scaled ,
    a ~ dnorm(0 , 0.2) , #restrict intercept to 0.2 SDs
    bF ~ dnorm(0 , 0.5) , #restrict to not extreme relationships
    sigma ~ dexp(1) #restrict to positive SDs, avg displacement is 1
) ,
data = respicar_socio)

# simulate from priors 
# plot lines over the range of 2 standard deviations for both outcome & predictor
set.seed(10)
prior <- extract.prior(m5)
mu <- link(m5 , post = prior , 
           data = list(female_ed_scaled = c(-2, 2)))
plot(NULL , xlim = c(-2, 2) , ylim = c(-2, 2))
for (i in 1:50)
    lines(c(-2, 2) , mu[i, ] , col = col.alpha("black", 0.4))

# posterior predictions 
# compute percentile interval of mean
F_seq <- seq(from           = -2.4 ,
             to             = 2.7 ,
             length.out     = 30)
mu <- link(m5 , data      = list(female_ed_scaled = F_seq))
mu.mean <- apply(mu , 2, mean)
mu.PI <- apply(mu , 2 , PI)

# plot it all
plot(carriage_scaled ~ female_ed_scaled ,
     data  = respicar_socio ,
     col   = rangi2)
lines(F_seq ,
      mu.mean ,
      lwd  = 2)
shade(mu.PI ,
      F_seq)

precis(m5) # slope of b for female ed: -0.11 (slight neg relationship)

# summary----
plot(precis(m1))
plot(precis(m2))
plot(precis(m3))
plot(precis(m4))
plot(precis(m5))



# multivariate models:------
m_full <- quap(alist(
    carriage_scaled ~ dnorm(mu , sigma) ,
    mu <- a + bU*urban_percent_scaled + bGDP*log_gdp + bG*gini_scaled + 
        bH*mean_hh_scaled + bF*female_ed_scaled,
    a ~ dnorm(0 , 0.2) , #restrict intercept to 0.2 SDs
    bU ~ dnorm(0 , 0.5) , #restrict to not extreme relationships
    bGDP ~ dnorm(0 , 0.5) , 
    bG ~ dnorm(0 , 0.5) ,
    bH ~ dnorm(0 , 0.5) ,
    bF ~ dnorm(0 , 0.5) ,
    sigma ~ dexp(1) #restrict to positive SDs, avg displacement is 1
) ,
data = respicar_socio)

precis(m_full)
# urban population has negative association with carriage
# GDP / Gini have barely any association 
# mean hh / female ed has positive association with carriage 



# Bayesian LOGISTIC models 

# univariate: 
# urban percent----

# standardize variables (tenth of urban percent)
respicar_socio$urban_percent_scaled <- scale(respicar_socio$urban_percent_tenth)

summary(respicar_socio$urban_percent_scaled) # -2.4 to 1.6


# make data list for model
dat_list <- list(
    Positive = respicar_socio$Positive,
    Total = respicar_socio$Total,
    urban_percent = respicar_socio$urban_percent_scaled)

# model
mUP <- ulam(alist(Positive ~ dbinom(Total, p),
                  logit(p) <- a + b*urban_percent,
                  a ~ dnorm(0 , 1.5),
                  b ~ dnorm(0, 0.5)),
            data = dat_list ,
            chains = 4,
            cores = 4,
            log_lik = TRUE
)

precis(mUP , depth = 2) # on the logistic scale 

# logit scale
post <- extract.samples(mUP)

# outcome scale (relative, ORs)
outcome_slope <- inv_logit(post$b)
dens(outcome_slope , adj = 0.1) 

precis(outcome_slope) # OR of 0.43 for b on the logistic scale (much more drastic)

# posterior predictions
p_post <- link(mUP , data = dat_list)
p_mu <- apply(p_post , 2 , mean)
p_ci <- apply(p_post , 2 , PI)
