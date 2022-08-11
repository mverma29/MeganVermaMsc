# full model (without interaction), WITHOUT RE for subregion correlation----

full_glm_no_re <- glm(
    data    = respicar_socio,
    formula = carriage ~ urban_percent + log_gdp + gini + mean_hh + female_ed,
    family  = "binomial",
    weights = Total
)

summ(
    full_glm_no_re,
    exp     = TRUE,
    confint = TRUE,
    pvals   = TRUE,
    digits  = 3
) 
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

full_glm_interaction <- glmer(
    data    = respicar_socio,
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


# AIC comparison/model-building with stepcAIC function 



chosen_model <- stepcAIC (
    object       = full_glm_interaction,
    direction    = "both",
    data         = respicar_socio,
    returnResult = TRUE,
    trace        = TRUE,
    digits       = 4
)
