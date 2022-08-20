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
) ``