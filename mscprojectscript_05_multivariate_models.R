# full model (without interaction), with RE for subregion correlation----

full_glm <- glmer(
<<<<<<< HEAD
    data = respicar_socio %>% 
        mutate(p = Positive/Total),
    formula = p ~ urban_percent + log_gdp + gini + mean_hh + female_ed + (1|subregion),
    family  = "binomial", 
    weights = Total)

summ(full_glm, exp=TRUE, confint=TRUE)  #only urban_percent (barely) and mean_hh have ORs other than 1
=======
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
>>>>>>> master

# full model (with interaction), with RE for subregion correlation----

# check formatting for interaction term in lme4- cause of error
full_glm_interaction <- glmer(
    data = respicar_socio %>% 
        mutate(p = Positive/Total),
    formula = p ~ urban_percent + log_gdp*gini + mean_hh + female_ed + (1|subregion),
    family  = "binomial", 
    weights = Total)

<<<<<<< HEAD
summ(full_glm_interaction, exp=TRUE, confint=TRUE)  #only urban_percent (barely) and mean_hh have ORs other than 1
=======
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
>>>>>>> master
