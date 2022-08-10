# full model (without interaction), with RE for subregion correlation----

full_glm <- glmer(
<<<<<<< HEAD
    data    = respicar_socio,
=======
    data = respicar_socio,
>>>>>>> master
    formula = carriage ~ urban_percent + log_gdp + gini + mean_hh + female_ed + (1|subregion),
    family  = "binomial",
    weights = Total
)

summ(
    full_glm,
<<<<<<< HEAD
    exp     = TRUE,
    confint = TRUE,
    pvals   = TRUE,
    digits  = 3
=======
    exp = TRUE,
    confint = TRUE,
    pvals = TRUE,
    digits = 3
>>>>>>> master
) 

# full model (with interaction), with RE for subregion correlation----

full_glm_interaction <- glmer(
    data    = respicar_socio,
    formula = carriage ~ urban_percent + log_gdp*gini + mean_hh + female_ed + (1|subregion),
    family  = "binomial", 
    weights = Total)

summ(
    full_glm_interaction,
<<<<<<< HEAD
    exp     = TRUE,
    confint = TRUE,
    pvals   = TRUE,
    digits  = 3
=======
    exp = TRUE,
    confint = TRUE,
    pvals = TRUE,
    digits = 3
>>>>>>> master
) 
