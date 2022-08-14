# full model (without interaction, without RE for subregion correlation)----

full_glm_no_re <- glm(
    data    = respicar_socio,
    formula = cbind(Positive, Total - Positive) ~
        urban_percent + log_gdp + gini + mean_hh + female_ed,
    family  = "binomial")

tidy(
    x        = full_glm_no_re,
    exp      = TRUE,
    conf.int = TRUE,
    pvals    = TRUE,
    digits   = 3
) 

# full model (without interaction, WITH RE for subregion correlation)----

full_glm_re <- gamm4(
    data    = respicar_socio,
    formula = cbind(Positive, Total - Positive) ~
        urban_percent + log_gdp + gini + mean_hh + female_ed,
    random  = ~(1|subregion),
    family  = "binomial")

tidy(
    x        = full_glm_re$mer,
    exp      = TRUE,
    conf.int = TRUE,
    pvals    = TRUE,
    digits   = 3
) 

# lrtest of null hypothesis of no correlation by subregion
lmtest::lrtest(full_glm_no_re, full_glm_re$mer) # <2e-16 (reject null hyp of no clustering)


# buildgamm4 version (w/ stepwise regression)

# full model (with interaction), without RE for subregion correlation----

full_glm_no_re_interaction <- glm(
    data    = respicar_socio,
    formula = cbind(Positive, Total - Positive) ~
        urban_percent + log_gdp*gini + mean_hh + female_ed,
    family  = "binomial")

tidy(
    full_glm_no_re_interaction,
    exp     = TRUE,
    conf.int = TRUE,
    pvals   = TRUE,
    digits  = 3
) 

# full model (with interaction), WITH RE for subregion correlation----

full_glm_re_interaction <- gamm4(
    data    = respicar_socio,
    formula = cbind(Positive, Total - Positive) ~
        urban_percent + log_gdp*gini + mean_hh + female_ed,
    random  = ~(1|subregion),
    family  = "binomial")

tidy(
    full_glm_re_interaction$mer,
    exp     = TRUE,
    conf.int = TRUE,
    pvals   = TRUE,
    digits  = 3
) 

# lrtest of null hypothesis of no correlation by subregion
lmtest::lrtest(full_glm_no_re_interaction, full_glm_re_interaction$mer) # <2e-16 (reject null hyp of no clustering)

# lrtest of interaction of gini and log gdp
lmtest::lrtest(full_glm_re$mer, full_glm_re_interaction$mer) # <2e-16 (reject null hyp of no interaction)


# AIC comparison/model-building with buildgamm4 function-------

chosen_model_re <- buildgamm4 (
    data    = respicar_socio,
    formula = cbind(Positive, Total - Positive) ~
        urban_percent + log_gdp * gini + mean_hh + female_ed + (1 | subregion),
    family  = "binomial"
)

summary(chosen_model_re@model) 

# AIC: 35408 


chosen_model_no_re<- buildgamm4 (
    data    = respicar_socio,
    formula = cbind(Positive, Total - Positive) ~
        urban_percent + log_gdp * gini + mean_hh + female_ed,
    family  = "binomial"
)


summary(chosen_model_no_re@model) 

# AIC: 37873

# full model with RE has the smallest AIC, & therefore fits the best 