# full model (without interaction), with RE for subregion correlation----

full_glm <- glmer(
    data    = respicar_socio,
    formula = carriage ~ urban_percent + log_gdp + gini + mean_hh + female_ed + (1|subregion),
    family  = "binomial",
    weights = Total
)

summ(
    full_glm,
    exp     = TRUE,
    confint = TRUE,
    pvals   = TRUE,
    digits  = 3
) 

# full model (with interaction), with RE for subregion correlation----

# check formatting for interaction term in lme4- cause of error
full_glm_interaction <- glmer(
    data = respicar_socio,
    formula = carriage ~ urban_percent + log_gdp * gini + mean_hh + female_ed + (1|subregion),
    family  = "binomial",
    weights = Total
)

summ(
    full_glm_interaction,
    exp     = TRUE,
    confint = TRUE,
    pvals   = TRUE,
    digits  = 3
) 
