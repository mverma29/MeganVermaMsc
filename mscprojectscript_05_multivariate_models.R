# full model (without interaction), with RE for subregion correlation----

full_glm <- glmer(
    data = respicar_socio %>% 
        mutate(p = Positive/Total),
    formula = p ~ urban_percent + log_gdp + gini + mean_hh + female_ed + (1|subregion),
    family  = "binomial", 
    weights = Total)

summ(full_glm, exp=TRUE, confint=TRUE)  #only urban_percent (barely) and mean_hh have ORs other than 1

# full model (with interaction), with RE for subregion correlation----

# check formatting for interaction term in lme4- cause of error
full_glm_interaction <- glmer(
    data = respicar_socio %>% 
        mutate(p = Positive/Total),
    formula = p ~ urban_percent + log_gdp*gini + mean_hh + female_ed + (1|subregion),
    family  = "binomial", 
    weights = Total)

summ(full_glm_interaction, exp=TRUE, confint=TRUE)  #only urban_percent (barely) and mean_hh have ORs other than 1
