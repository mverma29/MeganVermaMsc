# full model (without interaction), with RE for subregion correlation----

# add in RE 
full_glm <- glm(data    = respicar_socio %>% mutate(p = Positive/Total),
                formula = p ~ urban_percent + gdp + gini + mean_hh + female_ed,
                family  = "binomial", weights = Total) 

# full model (with interaction), with RE for subregion correlation----

# add in RE 
# check formatting for interaction term (*??)
full_glm <- glm(data    = respicar_socio %>% mutate(p = Positive/Total),
                formula = p ~ urban_percent + gdp*gini + mean_hh + female_ed,
                family  = "binomial", weights = Total) 
