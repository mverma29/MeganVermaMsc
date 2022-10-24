# brms LOGISTIC models-----

# Bayesian LOGISTIC models in brms 

# Megan Verma, 10/5/2022

library(tidyverse)
library(brms)

# brms info-------

# brm(model_description, 
#     data = my_data, 
#     family = the_family,
#     prior = the_prior)

# where: 
# model_description is the description of the regression model including any random effects similar to the notation used in the glm() and glmer functions
# my_data is the data frame containing the data
# family is the sampling family (normal, binomial, Poisson, etc)
# prior is the specification of the prior on the regression terms and the error standard deviation
# The output of the brm() function is an object of class brmsfit that contains the posterior samples and other information about the model.

# univariate models: ----
# urban percent-----
d <-
  as.data.frame(
    cbind(
      Positive      = respicar_socio$Positive,
      Total         = respicar_socio$Total,
      urban_percent =  respicar_socio$urban_percent_tenth
    )
  )

brms_mUP <-
  brm(
    data   = d,
    family = binomial,
    Positive | trials(Total) ~ 1 + urban_percent,
    prior  = c(prior(normal(0, 1000), class = Intercept),
              prior(normal(0, 1000), class = b)),
    iter   = 2500,
    warmup = 500,
    cores  = 4,
    chains = 4,
    seed   = 10
  ) 

plot(brms_mUP)

summary(brms_mUP)

fixef(brms_mUP)[2] %>%
  exp() %>%
  round(digits = 2) # OR=0.88-- matches! 

post <- as_draws(brms_mUP)

# gdp-----
d <-
    as.data.frame(
        cbind(
            Positive      = respicar_socio$Positive,
            Total         = respicar_socio$Total,
            log_gdp =  respicar_socio$log_gdp
        )
    )

brms_mGDP <-
    brm(
        data   = d,
        family = binomial,
        Positive | trials(Total) ~ 1 + log_gdp,
        prior  = c(prior(normal(0, 1000), class = Intercept),
                   prior(normal(0, 1000), class = b)),
        iter   = 2500,
        warmup = 500,
        cores  = 4,
        chains = 4,
        seed   = 10
    ) 

# plot(brms_mGDP)

summary(brms_mGDP)

fixef(brms_mGDP)[2] %>%
    exp() %>%
    round(digits = 2) # OR=0.58-- matches! 

# gini---- 

d <-
    as.data.frame(
        cbind(
            Positive      = respicar_socio$Positive,
            Total         = respicar_socio$Total,
            gini =  respicar_socio$gini_tenth
        )
    )

brms_mGINI <-
    brm(
        data   = d,
        family = binomial,
        Positive | trials(Total) ~ 1 + gini,
        prior  = c(prior(normal(0, 1000), class = Intercept),
                   prior(normal(0, 1000), class = b)),
        iter   = 2500,
        warmup = 500,
        cores  = 4,
        chains = 4,
        seed   = 10
    ) 

# plot(brms_mGINI)

summary(brms_mGINI)

fixef(brms_mGINI)[2] %>%
    exp() %>%
    round(digits = 2) 

# hh-----

d <-
    as.data.frame(
        cbind(
            Positive      = respicar_socio$Positive,
            Total         = respicar_socio$Total,
            hh =  respicar_socio$mean_hh
        )
    )

brms_mHH <-
    brm(
        data   = d,
        family = binomial,
        Positive | trials(Total) ~ 1 + hh,
        prior  = c(prior(normal(0, 1000), class = Intercept),
                   prior(normal(0, 1000), class = b)),
        iter   = 2500,
        warmup = 500,
        cores  = 4,
        chains = 4,
        seed   = 10
    ) 

# plot(brms_mHH)

summary(brms_mHH)

fixef(brms_mHH)[2] %>%
    exp() %>%
    round(digits = 2)

# female ed------

d <-
    as.data.frame(
        cbind(
            Positive      = respicar_socio$Positive,
            Total         = respicar_socio$Total,
            ed =  respicar_socio$female_ed_tenth
        )
    )

brms_mED <-
    brm(
        data   = d,
        family = binomial,
        Positive | trials(Total) ~ 1 + ed,
        prior  = c(prior(normal(0, 1000), class = Intercept),
                   prior(normal(0, 1000), class = b)),
        sample_prior = TRUE,
        iter   = 2500,
        warmup = 500,
        cores  = 4,
        chains = 4,
        seed   = 10
    ) 

# plot(brms_mED)

summary(brms_mED)

fixef(brms_mED)[2] %>%
    exp() %>%
    round(digits = 2)

# multivariate model---- 

d <-
    as.data.frame(
        cbind(
            Positive      = respicar_socio$Positive,
            Total         = respicar_socio$Total,
            urban_percent = respicar_socio$urban_percent_tenth, 
            log_gdp       = respicar_socio$log_gdp, 
            gini          = respicar_socio$gini_tenth, 
            hh            =  respicar_socio$mean_hh, 
            ed            = respicar_socio$female_ed_tenth
        )
    )

brms_mFULL <-
    brm(
        data   = d,
        family = binomial,
        Positive | trials(Total) ~ 1 + urban_percent + log_gdp + gini + hh + ed,
        prior  = c(prior(normal(0, 1000), class = Intercept),
                   prior(normal(0, 1000), class = b) 
                   ),
        iter   = 2500,
        warmup = 500,
        cores  = 4,
        chains = 4,
        seed   = 10
    ) 

# plot(brms_mFULL)

summary(brms_mFULL)

fixef(brms_mFULL)[2:6] %>%
    exp() %>%
    round(digits = 2) # matches frequentist unadjusted multivariate 
