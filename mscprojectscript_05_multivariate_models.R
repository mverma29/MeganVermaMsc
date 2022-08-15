# full model (without interaction, without RE for subregion correlation)----

full_glm_no_re <- glm(
    data    = respicar_socio,
    formula = cbind(Positive, Total - Positive) ~
        urban_percent_tenth + log_gdp + gini_tenth + mean_hh + female_ed_tenth,
    family  = "binomial")

tidy(
    x        = full_glm_no_re,
    exp      = TRUE,
    conf.int = TRUE,
    pvals    = TRUE,
    digits   = 3
) 

# full p-val of model? 
summary(full_glm_no_re)$coefficients[1,4] #<0.001

# full model (without interaction, WITH RE for subregion correlation)----

full_glm_re <- gamm4(
    data    = respicar_socio,
    formula = cbind(Positive, Total - Positive) ~
        urban_percent_tenth + log_gdp + gini_tenth + mean_hh + female_ed_tenth,
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



# full model (with interaction), without RE for subregion correlation ----

full_glm_no_re_interaction <- glm(
    data    = respicar_socio,
    formula = cbind(Positive, Total - Positive) ~
        urban_percent_tenth + log_gdp*gini_tenth + mean_hh + female_ed_tenth,
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
        urban_percent_tenth + log_gdp*gini_tenth + mean_hh + female_ed_tenth,
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
        urban_percent_tenth + log_gdp * gini_tenth + mean_hh + female_ed_tenth + (1 | subregion),
    family  = "binomial"
)
    
summary(chosen_model_re@model) 

# make output table 
chosen_mod_re <- tbl_regression(chosen_model_re@model, 
                                exponentiate = TRUE, 
                                tidy_fun = broom.mixed::tidy) %>% 
    add_glance_source_note(include = c("logLik"))

chosen_mod_re %>%
    as_flex_table() %>%
    save_as_docx(path = "outputs/chosen_mod_re.docx")


# AIC: 35678


chosen_model_no_re<- buildgamm4 (
    data    = respicar_socio,
    formula = cbind(Positive, Total - Positive) ~
        urban_percent_tenth + log_gdp * gini_tenth + mean_hh + female_ed_tenth,
    family  = "binomial"
)


summary(chosen_model_no_re@model) 
broom::glance(chosen_model_no_re@model) 

# make output table 

chosen_mod_no_re <- tbl_regression(chosen_model_no_re@model, 
                                exponentiate = TRUE, 
                                tidy_fun = broom.mixed::tidy) %>% 
    add_glance_source_note(include = c("logLik"))


as_flex_table(chosen_mod_no_re) %>%
    flextable::save_as_docx(path = "outputs/chosen_mod_no_re.docx")


# AIC: 38220

# full model with RE has the smallest AIC, & therefore fits the best 