# Sensitivity analyses 

# only unfilled covariate values: THIS IS NOT PLAUSIBLE-----
# full model (without interaction, WITH RE for subregion correlation)----

full_glm_re_unfilled <- gamm4(
  data    = respicar_unfilled,
  formula = cbind(Positive, Total - Positive) ~
    urban_percent_tenth + log_gdp + gini_tenth + mean_hh + female_ed_tenth,
  random  = ~(1|subregion),
  family  = "binomial")

tidy(
  x        = full_glm_re_unfilled$mer,
  exp      = TRUE,
  conf.int = TRUE,
  pvals    = TRUE,
  digits   = 3) %>%
  select(-effect, -group) %>%
  mutate(term = sub(pattern = "^X", replacement = "", x = term))

# full glm, with RE, without gini or interaction-----
full_glm_re_unfilled_nogini <- gamm4(
  data    = respicar_unfilled,
  formula = cbind(Positive, Total - Positive) ~
    urban_percent_tenth + log_gdp + mean_hh + female_ed_tenth,
  random  = ~(1|subregion),
  family  = "binomial")

tidy(
  x        = full_glm_re_unfilled_nogini$mer,
  exp      = TRUE,
  conf.int = TRUE,
  pvals    = TRUE,
  digits   = 3) %>%
  select(-effect, -group) %>%
  mutate(term = sub(pattern = "^X", replacement = "", x = term))

-
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


# models for each age group------
# under 5s: ---- 

respicar_under_5 <- filter(respicar_socio, `Age`=="<5y") #344 obs

# full model, with RE, without interaction 
full_glm_under_5 <- gamm4(
    data    = respicar_under_5,
    formula = cbind(Positive, Total - Positive) ~
        urban_percent_tenth + log_gdp + gini_tenth + mean_hh + female_ed_tenth,
    random  = ~(1|subregion),
    family  = "binomial")

tidy(
    x        = full_glm_under_5$mer,
    exp      = TRUE,
    conf.int = TRUE,
    pvals    = TRUE,
    digits   = 3) %>%
    select(-effect, -group) %>%
    mutate(term = sub(pattern = "^X", replacement = "", x = term))

full_glm_under_5_tbl <- tbl_regression(full_glm_under_5$mer, 
                                              exponentiate = TRUE, 
                                              tidy_fun = broom.mixed::tidy())
full_glm_under_5_tbl %>%
    as_flex_table() %>%
    save_as_docx(path = "outputs/full_glm_under_5.docx")

# full model, with RE, with interaction 

full_glm_under_5_interaction <- gamm4(
    data    = respicar_under_5,
    formula = cbind(Positive, Total - Positive) ~
        urban_percent_tenth + log_gdp*gini_tenth + mean_hh + female_ed_tenth,
    random  = ~(1|subregion),
    family  = "binomial")

tidy(
    x        = full_glm_under_5_interaction$mer,
    exp      = TRUE,
    conf.int = TRUE,
    pvals    = TRUE,
    digits   = 3) %>%
    select(-effect, -group) %>%
    mutate(term = sub(pattern = "^X", replacement = "", x = term))

full_glm_under_5_interaction_tbl <- tbl_regression(full_glm_under_5_interaction$mer, 
                                       exponentiate = TRUE, 
                                       tidy_fun = broom.mixed::tidy)
full_glm_under_5_interaction_tbl %>%
    as_flex_table() %>%
    save_as_docx(path = "outputs/full_glm_under_5_interaction.docx")

# lrtest of interaction of gini and log gdp
lmtest::lrtest(full_glm_under_5$mer, full_glm_under_5_interaction$mer) # <2e-16 (reject null hyp of no interaction)

# stepwise model, with RE & interaction 

stepwise_under_5 <- buildgamm4 (
    data    = respicar_under_5,
    formula = cbind(Positive, Total - Positive) ~
        urban_percent_tenth + log_gdp * gini_tenth + mean_hh + female_ed_tenth + (1 | subregion),
    family  = "binomial"
)

summary(stepwise_under_5@model) 

# make output table 
stepwise_under_5_tbl <- tbl_regression(stepwise_under_5@model, 
                                exponentiate = TRUE, 
                                tidy_fun = broom.mixed::tidy)

stepwise_under_5_tbl %>%
    as_flex_table() %>%
    save_as_docx(path = "outputs/stepwise_under_5.docx")


# OR for gini_tenth when GDP per capita is 1000 for stepwise 
exp(1.242115 + 3*(-0.330413)) # OR is 1.285151

# OR for gini_tenth when GDP per capita is 10000 for stepwise 
exp(1.242115 + 4*(-0.330413)) # OR is 0.9235438

# 5-17 years: ------

respicar_5_17 <- filter(respicar_socio, `Age`=="5-17y") #48 obs

# full model, with RE, without interaction 
full_glm_5_17 <- gamm4(
    data    = respicar_5_17,
    formula = cbind(Positive, Total - Positive) ~
        urban_percent_tenth + log_gdp + gini_tenth + mean_hh + female_ed_tenth,
    random  = ~(1|subregion),
    family  = "binomial")

tidy(
    x        = full_glm_5_17$mer,
    exp      = TRUE,
    conf.int = TRUE,
    pvals    = TRUE,
    digits   = 3) %>%
    select(-effect, -group) %>%
    mutate(term = sub(pattern = "^X", replacement = "", x = term))

full_glm_5_17_tbl <- tbl_regression(full_glm_5_17$mer, 
                                       exponentiate = TRUE, 
                                       tidy_fun = broom.mixed::tidy)
full_glm_5_17_tbl %>%
    as_flex_table() %>%
    save_as_docx(path = "outputs/full_glm_5_17.docx")


# full model, with RE, with interaction 

full_glm_interaction_5_17 <- gamm4(
    data    = respicar_5_17,
    formula = cbind(Positive, Total - Positive) ~
        urban_percent_tenth + log_gdp*gini_tenth + mean_hh + female_ed_tenth,
    random  = ~(1|subregion),
    family  = "binomial")

tidy(
    full_glm_interaction_5_17$mer,
    exp     = TRUE,
    conf.int = TRUE,
    pvals   = TRUE,
    digits  = 3) %>%
    select(-effect, -group) %>%
    mutate(term = sub(pattern = "^X", replacement = "", x = term))

full_glm_5_17_interaction_tbl <- tbl_regression(full_glm_interaction_5_17$mer, 
                                    exponentiate = TRUE, 
                                    tidy_fun = broom.mixed::tidy)
full_glm_5_17_interaction_tbl %>%
    as_flex_table() %>%
    save_as_docx(path = "outputs/full_glm_5_17_interaction.docx")

# lrtest of interaction of gini and log gdp
lmtest::lrtest(full_glm_5_17$mer, full_glm_interaction_5_17$mer) # <2e-16 (reject null hyp of no interaction)

# stepwise model, with RE & interaction 

stepwise_5_17 <- buildgamm4 (
    data    = respicar_5_17,
    formula = cbind(Positive, Total - Positive) ~
        urban_percent_tenth + log_gdp * gini_tenth + mean_hh + female_ed_tenth + (1 | subregion),
    family  = "binomial"
)

summary(stepwise_5_17@model) 

# make output table 
stepwise_5_17_tbl <- tbl_regression(stepwise_5_17@model, 
                                   exponentiate = TRUE, 
                                   tidy_fun = broom.mixed::tidy)

stepwise_5_17_tbl %>%
    as_flex_table() %>%
    save_as_docx(path = "outputs/stepwise_5_17.docx")

# OR for gini_tenth when GDP per capita is 1000 for stepwise 
exp(5.45160 + 3*(-1.11313)) # OR is 8.26649

# OR for gini_tenth when GDP per capita is 10000 for stepwise 
exp(5.45160 + 4*(-1.11313)) # OR is 2.715782

# 18+ years---------

respicar_18 <- filter(respicar_socio, `Age`=="18+y") #46 obs

# full model, with RE, without interaction 
full_glm_18 <- gamm4(
    data    = respicar_18,
    formula = cbind(Positive, Total - Positive) ~
        urban_percent_tenth + log_gdp + gini_tenth + mean_hh + female_ed_tenth,
    random  = ~(1|subregion),
    family  = "binomial")

tidy(
    x        = full_glm_18$mer,
    exp      = TRUE,
    conf.int = TRUE,
    pvals    = TRUE,
    digits   = 3) %>%
    select(-effect, -group) %>%
    mutate(term = sub(pattern = "^X", replacement = "", x = term))

full_glm_18_tbl <- tbl_regression(full_glm_18$mer, 
                                                exponentiate = TRUE, 
                                                tidy_fun = broom.mixed::tidy)
full_glm_18_tbl %>%
    as_flex_table() %>%
    save_as_docx(path = "outputs/full_glm_18.docx")

# full model, with RE, with interaction 

full_glm_interaction_18 <- gamm4(
    data    = respicar_18,
    formula = cbind(Positive, Total - Positive) ~
        urban_percent_tenth + log_gdp*gini_tenth + mean_hh + female_ed_tenth,
    random  = ~(1|subregion),
    family  = "binomial")

tidy(
    full_glm_interaction_18$mer,
    exp     = TRUE,
    conf.int = TRUE,
    pvals   = TRUE,
    digits  = 3
) 

full_glm_interaction_18_tbl <- tbl_regression(full_glm_interaction_18$mer, 
                                  exponentiate = TRUE, 
                                  tidy_fun = broom.mixed::tidy)
full_glm_interaction_18_tbl %>%
    as_flex_table() %>%
    save_as_docx(path = "outputs/full_glm_interaction_18.docx")

# lrtest of interaction of gini and log gdp
lmtest::lrtest(full_glm_18$mer, full_glm_interaction_18$mer) # 3.399e-12 (reject null hyp of no interaction)


# stepwise model, with RE & interaction 

stepwise_18 <- buildgamm4 (
    data    = respicar_18,
    formula = cbind(Positive, Total - Positive) ~
        urban_percent_tenth + log_gdp * gini_tenth + mean_hh + female_ed_tenth + (1 | subregion),
    family  = "binomial"
)

summary(stepwise_18@model) 

# make output table 
stepwise_18_tbl <- tbl_regression(stepwise_18@model, 
                                    exponentiate = TRUE, 
                                    tidy_fun = broom.mixed::tidy)

stepwise_18_tbl %>%
    as_flex_table() %>%
    save_as_docx(path = "outputs/stepwise_18.docx")

# OR for gini_tenth when GDP per capita is 1000 for stepwise 
exp(1.87534 + 3*(-0.57044)) # OR is 1.178238

# OR for gini_tenth when GDP per capita is 10000 for stepwise 
exp(1.87534 + 4*(-0.57044)) # OR is 0.6660304



# excluding ethnic minorities--------- 

respicar_no_minority <- filter(respicar_socio, `Ethnic Minority`=="No") #376 obs

# full model, with RE, without interaction 
full_glm_no_minority <- gamm4(
    data    = respicar_no_minority,
    formula = cbind(Positive, Total - Positive) ~
        urban_percent_tenth + log_gdp + gini_tenth + mean_hh + female_ed_tenth,
    random  = ~(1|subregion),
    family  = "binomial")

tidy(
    x        = full_glm_no_minority$mer,
    exp      = TRUE,
    conf.int = TRUE,
    pvals    = TRUE,
    digits   = 3) %>%
    select(-effect, -group) %>%
    mutate(term = sub(pattern = "^X", replacement = "", x = term))

# full_glm_no_minority_tbl <- tbl_regression(full_glm_no_minority$mer, 
#                                   exponentiate = TRUE, 
#                                   tidy_fun = broom.mixed::tidy)
# full_glm_no_minority_tbl %>%
#     as_flex_table() %>%
#     save_as_docx(path = "outputs/full_glm_no_minority.docx")

# full model, with RE, with interaction 

full_glm_interaction_no_minority <- gamm4(
    data    = respicar_no_minority,
    formula = cbind(Positive, Total - Positive) ~
        urban_percent_tenth + log_gdp*gini_tenth + mean_hh + female_ed_tenth,
    random  = ~(1|subregion),
    family  = "binomial")

tidy(
    full_glm_interaction_no_minority$mer,
    exp     = TRUE,
    conf.int = TRUE,
    pvals   = TRUE,
    digits  = 3
) 

full_glm_interaction_no_minority_tbl <- tbl_regression(full_glm_interaction_no_minority$mer, 
                                              exponentiate = TRUE, 
                                              tidy_fun = broom.mixed::tidy)
full_glm_interaction_no_minority_tbl %>%
    as_flex_table() %>%
    save_as_docx(path = "outputs/full_glm_interaction_no_minority.docx")

# lrtest of interaction of gini and log gdp
lmtest::lrtest(full_glm_no_minority$mer, full_glm_interaction_no_minority$mer) # <2 e-16(reject null hyp of no interaction)


# stepwise model, with RE & interaction 

stepwise_no_minority <- buildgamm4 (
    data    = respicar_no_minority,
    formula = cbind(Positive, Total - Positive) ~
        urban_percent_tenth + log_gdp * gini_tenth + mean_hh + female_ed_tenth + (1 | subregion),
    family  = "binomial"
)

summary(stepwise_no_minority@model) #still drops out urban percent

# make output table 
stepwise_no_minority_tbl <- tbl_regression(stepwise_no_minority@model, 
                                  exponentiate = TRUE, 
                                  tidy_fun = broom.mixed::tidy)

stepwise_no_minority_tbl %>%
    as_flex_table() %>%
    save_as_docx(path = "outputs/stepwise_no_minority.docx")

# OR for gini_tenth when GDP per capita is 1000 for stepwise 
exp(0.489454 + 3*(-0.154219)) # OR is 1.027159

# OR for gini_tenth when GDP per capita is 10000 for stepwise 
exp(0.489454 + 4*(-0.154219)) # OR is 0.8803621


# only cross-sectional data --------- 

respicar_x_sectional <- filter(respicar_socio, `Design`=="Cross-sectional") #312 obs

# full model, with RE, without interaction 
full_glm_x_sectional <- gamm4(
    data    = respicar_x_sectional,
    formula = cbind(Positive, Total - Positive) ~
        urban_percent_tenth + log_gdp + gini_tenth + mean_hh + female_ed_tenth,
    random  = ~(1|subregion),
    family  = "binomial")

tidy(
    x        = full_glm_x_sectional$mer,
    exp      = TRUE,
    conf.int = TRUE,
    pvals    = TRUE,
    digits   = 3) %>%
    select(-effect, -group) %>%
    mutate(term = sub(pattern = "^X", replacement = "", x = term))

# full glm w/ interaction, RE

full_glm_interaction_x_sectional <- gamm4(
    data    = respicar_x_sectional,
    formula = cbind(Positive, Total - Positive) ~
        urban_percent_tenth + log_gdp*gini_tenth + mean_hh + female_ed_tenth,
    random  = ~(1|subregion),
    family  = "binomial")

tidy(
    full_glm_interaction_x_sectional$mer,
    exp     = TRUE,
    conf.int = TRUE,
    pvals   = TRUE,
    digits  = 3
) 

full_glm_interaction_x_sectional_tbl <- tbl_regression(full_glm_interaction_x_sectional$mer, 
                                                       exponentiate = TRUE, 
                                                       tidy_fun = broom.mixed::tidy)
full_glm_interaction_x_sectional_tbl %>%
    as_flex_table() %>%
    save_as_docx(path = "outputs/full_glm_interaction_x_sectional.docx")

# lrtest of interaction of gini and log gdp
lmtest::lrtest(full_glm_x_sectional$mer, full_glm_interaction_x_sectional$mer) #no evidence for an interaction

# stepwise model, with RE & without interaction 

stepwise_x_sectional <- buildgamm4 (
    data    = respicar_x_sectional,
    formula = cbind(Positive, Total - Positive) ~
        urban_percent_tenth + log_gdp + gini_tenth + mean_hh + female_ed_tenth + (1 | subregion),
    family  = "binomial"
)

summary(stepwise_x_sectional@model) # drops out urban percent & gini

# make output table 
stepwise_x_sectional_tbl <- tbl_regression(stepwise_x_sectional@model, 
                                           exponentiate = TRUE, 
                                           tidy_fun = broom.mixed::tidy)

stepwise_x_sectional_tbl %>%
    as_flex_table() %>%
    save_as_docx(path = "outputs/stepwise_x_sectional.docx")

