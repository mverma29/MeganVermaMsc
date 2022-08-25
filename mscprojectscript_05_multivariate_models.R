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

full_glm_no_re_tbl <- tbl_regression(full_glm_no_re, 
                                exponentiate = TRUE, 
                                tidy_fun = broom.mixed::tidy) 

full_glm_no_re_tbl %>%
    as_flex_table() %>%
    save_as_docx(path = "outputs/full_glm_no_re.docx")


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
    digits   = 3) %>%
    select(-effect, -group) %>%
    mutate(term = sub(pattern = "^X", replacement = "", x = term))

full_glm_re_tbl <- tbl_regression(full_glm_re$mer, 
                                     exponentiate = TRUE, 
                                     tidy_fun = broom.mixed::tidy) 

full_glm_re_tbl %>%
    as_flex_table() %>%
    save_as_docx(path = "outputs/full_glm_re.docx")


# lrtest of null hypothesis of no correlation by subregion
lmtest::lrtest(full_glm_no_re, full_glm_re$mer) # <2e-16 (reject null hyp of no clustering)

# calculate MSE 
summ_full_glm_re <- summary(full_glm_re$mer)
mean(summ_full_glm_re$residuals^2)

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

full_glm_no_re_interaction_tbl <- tbl_regression(full_glm_no_re_interaction, 
                                     exponentiate = TRUE, 
                                     tidy_fun = broom.mixed::tidy) 

full_glm_no_re_interaction_tbl %>%
    as_flex_table() %>%
    save_as_docx(path = "outputs/full_glm_no_re_interaction.docx")


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

full_glm_re_interaction_tbl <- tbl_regression(full_glm_re_interaction$mer, 
                                                 exponentiate = TRUE, 
                                                 tidy_fun = broom.mixed::tidy) 

full_glm_re_interaction_tbl %>%
    as_flex_table() %>%
    save_as_docx(path = "outputs/full_glm_re_interaction.docx")

# lrtest of null hypothesis of no correlation by subregion
lmtest::lrtest(full_glm_no_re_interaction, full_glm_re_interaction$mer) # <2e-16 (reject null hyp of no clustering)

# lrtest of interaction of gini and log gdp
lmtest::lrtest(full_glm_re$mer, full_glm_re_interaction$mer) # <2e-16 (reject null hyp of no interaction)

# calculate MSE 
summ_full_glm_re_interaction <- summary(full_glm_re_interaction$mer)
mean(summ_full_glm_re_interaction$residuals^2)

# stepwise regression with buildgamm4 function-------

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


# calculate MSE 
summ_chosen_mod_re <- summary(chosen_model_re@model)
mean(summ_chosen_mod_re$residuals^2)


# no RE 
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

tidy(
    chosen_model_no_re@model,
    exp     = TRUE,
    conf.int = TRUE,
    pvals   = TRUE,
    digits  = 3
) 

# AIC: 38220

# lrtest of null hypothesis of no correlation by subregion
lmtest::lrtest(chosen_model_no_re@model, chosen_model_re@model) # <2e-16 (reject null hyp of no clustering)

chosen_model_re_no_interaction <- buildgamm4 (
    data    = respicar_socio,
    formula = cbind(Positive, Total - Positive) ~
        urban_percent_tenth + log_gdp + gini_tenth + mean_hh + female_ed_tenth + (1 | subregion),
    family  = "binomial"
)

summary(chosen_model_re_no_interaction@model) 

# lrtest of null hypothesis of no interaction
lmtest::lrtest(chosen_model_re_no_interaction@model, chosen_model_re@model) # <2e-16 (reject null hyp of no clustering)


# full model with RE has the smallest AIC, & therefore fits the best 

# should give you the OR for gini_tenth when GDP per capita is 1000
exp(coef(full_glm_re_interaction$gam)['gini_tenth'] + 3 * coef(full_glm_re_interaction$gam)['log_gdp:gini_tenth'])

# should give you the OR for gini_tenth when GDP per capita is 10000
exp(coef(full_glm_re_interaction$gam)['gini_tenth'] + 4 * coef(full_glm_re_interaction$gam)['log_gdp:gini_tenth'])


# try to interpret interaction models: R equivalent of lincom 
library(foreign)
library(multcomp)

names(coef(full_glm_re_interaction$gam))

summary(glht(full_glm_re_interaction$gam, 
             linfct = c("gini_tenth + log_gdp:gini_tenth = 0"))) # 0.35313

summary(glht(full_glm_re_interaction$gam, 
             linfct = c("log_gdp + log_gdp:gini_tenth = 0"))) #0.23225

# should give you the OR for gini_tenth when GDP per capita is 1000 for stepwise 
exp(coef(chosen_model_re)['gini_tenth'] + 3 * coef(chosen_model_re)['log_gdp:gini_tenth'])
# not working, do manually
exp(0.50737 + 3*(-0.15645)) # OR is 1.04 (still)

# should give you the OR for gini_tenth when GDP per capita is 10000 for stepwise 
exp(coef(full_glm_re_interaction$gam)['gini_tenth'] + 4 * coef(full_glm_re_interaction$gam)['log_gdp:gini_tenth'])
exp(0.50737 + 4*(-0.15645)) # OR is 0.89 (still)


# add in column in dataset for gini:GDP interaction--------

respicar_socio$interaction <-  respicar_socio$gini_tenth * respicar_socio$log_gdp

#repeat stepwise to ensure interaction wont be dropped 

chosen_model_re_new <- buildgamm4 (
    data    = respicar_socio,
    formula = cbind(Positive, Total - Positive) ~
        urban_percent_tenth + log_gdp + gini_tenth + mean_hh + female_ed_tenth + interaction + (1 | subregion),
    family  = "binomial"
)

summary(chosen_model_re_new@model) 


# make output table 
chosen_mod_re_new_tbl <- tbl_regression(chosen_model_re_new@model, 
                                exponentiate = TRUE, 
                                tidy_fun = broom.mixed::tidy) 

chosen_mod_re_new_tbl %>%
    as_flex_table() %>%
    save_as_docx(path = "outputs/chosen_mod_re_new.docx")

# same as stepwise, confirmed that interaction is not dropped



# MSE experiment-----

# "current model (full, no interaction)" has MSE of 72.58

# drop urban percent
no_u_p <- gamm4(
    data    = respicar_socio,
    formula = cbind(Positive, Total - Positive) ~
         log_gdp + gini_tenth + mean_hh + female_ed_tenth,
    random  = ~(1|subregion),
    family  = "binomial")

summ_no_u_p <- summary(no_u_p$mer)
mean(summ_no_u_p$residuals^2)

# deltaMSE
72.58445-72.58458 # -0.00013


# drop GDP 
no_gdp <- gamm4(
    data    = respicar_socio,
    formula = cbind(Positive, Total - Positive) ~
        urban_percent_tenth + gini_tenth + mean_hh + female_ed_tenth,
    random  = ~(1|subregion),
    family  = "binomial")

summ_no_gdp <- summary(no_gdp$mer)
mean(summ_no_gdp$residuals^2)

# deltaMSE
72.6-72.58458 # 0.01542


# drop Gini 
no_gini <- gamm4(
    data    = respicar_socio,
    formula = cbind(Positive, Total - Positive) ~
        urban_percent_tenth + log_gdp + mean_hh + female_ed_tenth,
    random  = ~(1|subregion),
    family  = "binomial")

summ_no_gini <- summary(no_gini$mer)
mean(summ_no_gini$residuals^2)

# deltaMSE
72.66586-72.58458 # 0.08128

# drop meanhh 
no_hh <- gamm4(
    data    = respicar_socio,
    formula = cbind(Positive, Total - Positive) ~
        urban_percent_tenth + log_gdp + gini_tenth + female_ed_tenth,
    random  = ~(1|subregion),
    family  = "binomial")

summ_no_hh <- summary(no_hh$mer)
mean(summ_no_hh$residuals^2)

# deltaMSE
74.50835-72.58458 # 1.92377

# drop female ed 
no_ed <- gamm4(
    data    = respicar_socio,
    formula = cbind(Positive, Total - Positive) ~
        urban_percent_tenth + log_gdp + gini_tenth + mean_hh,
    random  = ~(1|subregion),
    family  = "binomial")

summ_no_ed <- summary(no_ed$mer)
mean(summ_no_ed$residuals^2)

# deltaMSE
72.85638-72.58458 # 0.2718

