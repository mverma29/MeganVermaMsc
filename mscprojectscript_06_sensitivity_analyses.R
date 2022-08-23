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
                                              tidy_fun = broom.mixed::tidy)
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


