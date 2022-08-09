
# basic model fitting (univariate)-----

# Fit a logistic GLM of the total carriage as a function of each covariate. 
# As each study has a different total individuals serotyped, we'll need to ensure that we use the 
# total serotyped as though it was a number of "trials", $n$, and the number of cases as a number 
# of "successes", $y$, in our GLM. This is done by specifying the left hand side of the regression 
# formula as `cbind(y, n-y)` with appropriate variable names in place of `n` and `y`.

# add RE for subregions in

# intercept-only model, to assess clustering in the data
intercept_glm <- glmer(
  data = respicar_socio %>% 
    mutate(p = Positive/Total),
  formula = p ~ 1 + (1|subregion),
  family  = "binomial", 
  weights = Total)

print(intercept_glm, corr = FALSE)


performance::icc(intercept_glm) # 8.3% of the variation in carriage 
# can be accounted for by clustering of the data by subregion 

# urban_percent 
urban_percent_glm <- glmer(
  data = respicar_socio %>% 
    mutate(p = Positive/Total),
  formula = p ~ urban_percent + (1|subregion),
  family  = "binomial", 
  weights = Total)

print(urban_percent_glm, corr = FALSE)

jtools:: summ(urban_percent_glm, exp=TRUE, confint=TRUE)

# tidy(urban_percent_glm, 
#      conf.int = T, 
#      exponentiate = TRUE)


# GDP
gdp_glm <- glmer(
  data = respicar_socio %>% 
    mutate(p = Positive/Total),
  formula = p ~ gdp_usd + (1|subregion),
  family  = "binomial", 
  weights = Total)

print(gdp_glm, corr = FALSE)

summ(gdp_glm, exp=TRUE)



# Gini
gini_glm <- glm(data    = respicar_socio %>% mutate(p = Positive/Total),
                formula = p ~ gini,
                family  = "binomial", weights = Total)

tidy(gini_glm, conf.int = T, exponentiate = TRUE)


# HH size
hh_glm <- glm(data    = respicar_socio %>% mutate(p = Positive/Total),
              formula = p ~ mean_hh,
              family  = "binomial", weights = Total)

tidy(hh_glm, conf.int = T, exponentiate = TRUE) # an actual association!


# female ed 
female_ed_glm <- glm(data    = respicar_socio %>% mutate(p = Positive/Total),
                     formula = p ~ female_ed,
                     family  = "binomial", weights = Total)

tidy(female_ed_glm, conf.int = T, exponentiate = TRUE)

