# Sensitivity analyses 


# only unfilled covariate values:------
# full model (with interaction), without RE for subregion correlation ----

full_glm_no_re_interaction_unfilled <- glm(
  data    = respicar_unfilled,
  formula = cbind(Positive, Total - Positive) ~
    urban_percent_tenth + log_gdp*gini_tenth + mean_hh + female_ed_tenth,
  family  = "binomial")

tidy(
  full_glm_no_re_interaction_unfilled,
  exp     = TRUE,
  conf.int = TRUE,
  pvals   = TRUE,
  digits  = 3
) 

full_mod_no_re_unfilled <- tbl_regression(full_glm_no_re_interaction_unfilled, 
                                   exponentiate = TRUE, 
                                   tidy_fun = broom.mixed::tidy)
as_flex_table(full_mod_no_re_unfilled) %>%
  flextable::save_as_docx(path = "outputs/full_mod_no_re_unfilled.docx")

# full model (with interaction), WITH RE for subregion correlation----

full_glm_re_interaction_unfilled <- gamm4(
  data    = respicar_unfilled,
  formula = cbind(Positive, Total - Positive) ~
    urban_percent_tenth + log_gdp*gini_tenth + mean_hh + female_ed_tenth,
  random  = ~(1|subregion),
  family  = "binomial")

tidy(
  full_glm_re_interaction_unfilled$mer,
  exp     = TRUE,
  conf.int = TRUE,
  pvals   = TRUE,
  digits  = 3
) 

full_mod_re_unfilled <- tbl_regression(full_glm_re_interaction_unfilled@model, 
                                          exponentiate = TRUE, 
                                          tidy_fun = broom.mixed::tidy)
as_flex_table(full_mod_re_unfilled) %>%
  flextable::save_as_docx(path = "outputs/full_mod_re_unfilled.docx")


# lrtest of null hypothesis of no correlation by subregion
lmtest::lrtest(full_glm_no_re_interaction_unfilled, full_glm_re_interaction_unfilled$mer) # <2e-16 (reject null hyp of no clustering)

# lrtest of interaction of gini and log gdp
lmtest::lrtest(full_glm_re$mer, full_glm_re_interaction$mer) # <2e-16 (reject null hyp of no interaction)


# AIC comparison/model-building with buildgamm4 function-------

chosen_model_re <- buildgamm4 (
  data    = respicar_unfilled,
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
  data    = respicar_unfilled,
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

# gives the OR for gini_tenth when GDP per capita is 1000
exp(coef(full_glm_re_interaction$gam)['gini_tenth'] + 3*coef(full_glm_re_interaction$gam)['log_gdp:gini_tenth'])
