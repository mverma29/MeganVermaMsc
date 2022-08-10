# basic model fitting (univariate)-----

# Fit a logistic GLM of the total carriage as a function of each covariate. 
# As each study has a different total individuals serotyped, we'll need to ensure that we use the 
# total serotyped as though it was a number of "trials", $n$, and the number of cases as a number 
# of "successes", $y$, in our GLM. This is done by specifying the left hand side of the regression 
# formula as `cbind(y, n-y)` with appropriate variable names in place of `n` and `y`.

# add RE for subregions in using glmer function (lme4 package)

# intercept-only model, to assess clustering in the data
intercept_glm <- glmer(
    data    = respicar_socio,
    formula = carriage ~ 1 + (1 | subregion),
    family  = "binomial",
    weights = Total
)

print(intercept_glm, corr = FALSE)

performance::icc(intercept_glm) # 8.3% of the variation in carriage
# can be accounted for by clustering of the data by subregion 

# urban_percent
urban_percent_glm <- glmer(
    data    = respicar_socio,
    formula = carriage ~ urban_percent + (1 | subregion),
    family  = "binomial",
    weights = Total
)


print(urban_percent_glm, corr = FALSE)

summ(
    urban_percent_glm,
    exp     = TRUE,
    confint = TRUE,
    pvals   = TRUE,
    digits  = 3
) #OR of 0.62

# GDP
gdp_glm <- glmer(
  data              = respicar_socio, 
  formula           = carriage ~ log_gdp + (1|subregion),
  family            = "binomial", 
  weights           = Total)

print(gdp_glm, corr = FALSE)

summ(
    gdp_glm,
    exp             = TRUE,
    confint         = TRUE,
    pvals           = TRUE,
    digits          = 3
) #OR of 0.77


# Gini
gini_glm <- glmer(
    data             = respicar_socio,
    formula          = carriage ~ gini + (1|subregion),
    family           = "binomial", 
    weights          = Total)

print(gini_glm, corr = FALSE)

summ(
    gini_glm,
    exp              = TRUE,
    confint          = TRUE,
    pvals            = TRUE,
    digits           = 3
) #OR of 0.325


# HH size
hh_glm <- glmer(
    data           = respicar_socio,
    formula        = carriage ~ mean_hh + (1|subregion),
    family         = "binomial", 
    weights        = Total)

print(hh_glm, corr = FALSE)

summ(
    hh_glm,
    exp            = TRUE,
    confint        = TRUE,
    pvals          = TRUE,
    digits         = 3
) #OR of 1.36


# female ed 
female_ed_glm <- glmer(
    data           = respicar_socio,
    formula        = carriage ~ female_ed + (1|subregion),
    family         = "binomial", 
    weights        = Total)

print(female_ed_glm, corr = FALSE)

summ(
    female_ed_glm,
    exp = TRUE,
    confint = TRUE,
    pvals = TRUE,
    digits = 3
) #OR of 0.44

