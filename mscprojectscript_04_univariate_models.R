
# basic model fitting (univariate): 

# Fit a logistic GLM of the total carriage as a function of each covariate. 
# As each study has a different total individuals serotyped, we'll need to ensure that we use the 
# total serotyped as though it was a number of "trials", $n$, and the number of cases as a number 
# of "successes", $y$, in our GLM. This is done by specifying the left hand side of the regression 
# formula as `cbind(y, n-y)` with appropriate variable names in place of `n` and `y`.

# use glm for simple model 
# gamm4 allows for RE to be added to model 
# conduct an LRT to test null hypothesis of no within subregion clustering 

# urban percent ----------
# urban_percent model, no RE
urban_percent_glm <- glm(
    data    = respicar_socio,
    formula = cbind(Positive, Total - Positive) ~ urban_percent, 
    family  = "binomial")

tidy(urban_percent_glm, exp=TRUE, conf.int=TRUE) # OR: 0.987
summary(urban_percent_glm) # p-val (Wald approx): <2e-16

urban_percent_glm_re <- gamm4(
  data    = respicar_socio,
  formula = cbind(Positive, Total - Positive) ~ urban_percent, 
  random  = ~(1|subregion),
  family  = "binomial")

tidy(urban_percent_glm_re$mer, exp=TRUE, conf.int=TRUE) # OR: 0.995
summary(urban_percent_glm_re$mer) # p-val (Wald approx): <2e-16
    

lmtest::lrtest(urban_percent_glm, urban_percent_glm_re$mer) # <2e-16 (reject null hyp of no clustering)

# make table of model outputs
urban_percent_re <- tbl_regression(urban_percent_glm_re$mer, 
                                exponentiate = TRUE, 
                                tidy_fun = broom.mixed::tidy)

urban_percent_re %>%
    as_flex_table() %>%
    flextable::save_as_docx(path = "/Users/meganverma/Desktop/git/MeganVermaMsc/outputs/urban_percent_re.docx")


# GDP -----------

# gdp model, no RE
gdp_glm <- glm(
    data    = respicar_socio,
    formula = cbind(Positive, Total - Positive) ~ log_gdp, 
    family  = "binomial")

tidy(gdp_glm, exp=TRUE, conf.int=TRUE) # OR: 0.578
summary(urban_percent_glm) # p-val (Wald approx): <2e-16

# gdp model w/ RE
gdp_glm_re <- gamm4(
    data    = respicar_socio,
    formula = cbind(Positive, Total - Positive) ~ log_gdp, 
    random  = ~(1|subregion),
    family  = "binomial")

tidy(gdp_glm_re$mer, exp=TRUE, conf.int=TRUE) # OR:0.558

lmtest::lrtest(gdp_glm, gdp_glm_re$mer) # <2e-16 (reject null hyp of no clustering)

# make table of model outputs
gdp_re_model <- tbl_regression(gdp_glm_re$mer, 
                                   exponentiate = TRUE, 
                                   tidy_fun = broom.mixed::tidy)

gdp_re_model %>%
    as_flex_table() %>%
    flextable::save_as_docx(path = "/Users/meganverma/Desktop/git/MeganVermaMsc/outputs/gdp_re_model.docx")


# Gini-------

# gini model, no RE
gini_glm <- glm(
    data    = respicar_socio,
    formula = cbind(Positive, Total - Positive) ~ gini, 
    family  = "binomial")

tidy(gini_glm, exp=TRUE, conf.int=TRUE) # OR: 1.02
summary(gini_glm) # p-val (Wald approx): <2e-16

# gini model w/ RE
gini_glm_re <- gamm4(
    data    = respicar_socio,
    formula = cbind(Positive, Total - Positive) ~ gini, 
    random  = ~(1|subregion),
    family  = "binomial")

tidy(gini_glm_re$mer, exp=TRUE, conf.int=TRUE) # OR: 0.989
lmtest::lrtest(gini_glm, gini_glm_re$mer) # <2e-16 (reject null hyp of no clustering)

# make table of model outputs
gini_re_model <- tbl_regression(gini_glm_re$mer, 
                               exponentiate = TRUE, 
                               tidy_fun = broom.mixed::tidy)

gini_re_model %>%
    as_flex_table() %>%
    flextable::save_as_docx(path = "/Users/meganverma/Desktop/git/MeganVermaMsc/outputs/gini_re_model.docx")


# HH size------

# HH model, no RE
hh_glm <- glm(
    data    = respicar_socio,
    formula = cbind(Positive, Total - Positive) ~ mean_hh, 
    family  = "binomial")

tidy(hh_glm, exp=TRUE, conf.int=TRUE) # OR: 1.32
summary(hh_glm) # p-val (Wald approx): <2e-16

# HH model w/ RE
hh_glm_re <- gamm4(
    data    = respicar_socio,
    formula = cbind(Positive, Total - Positive) ~ mean_hh, 
    random  = ~(1|subregion),
    family  = "binomial")

tidy(hh_glm_re$mer, exp=TRUE, conf.int=TRUE) #OR: 1.36
lmtest::lrtest(hh_glm, hh_glm_re$mer) # <2e-16 (reject null hyp of no clustering)


# make table of model outputs
hh_re_model <- tbl_regression(hh_glm_re$mer, 
                               exponentiate = TRUE, 
                               tidy_fun = broom.mixed::tidy)

hh_re_model %>%
    as_flex_table() %>%
    flextable::save_as_docx(path = "/Users/meganverma/Desktop/git/MeganVermaMsc/outputs/hh_re_model.docx")


# female ed------

female_ed_glm <- glm(
    data    = respicar_socio,
    formula = cbind(Positive, Total - Positive) ~ female_ed, 
    family  = "binomial")

tidy(female_ed_glm, exp=TRUE, conf.int=TRUE) # OR: 0.991
summary(female_ed_glm) # p-val (Wald approx): <2e-16

# female ed model w/ RE
female_ed_glm_re <- gamm4(
    data    = respicar_socio,
    formula = cbind(Positive, Total - Positive) ~ female_ed, 
    random  = ~(1|subregion),
    family  = "binomial")

tidy(female_ed_glm_re$mer, exp=TRUE, conf.int=TRUE) # OR: 0.992
lmtest::lrtest(female_ed_glm, female_ed_glm_re$mer) # <2e-16 (reject null hyp of no clustering)


# make table of model outputs
ed_re_model <- tbl_regression(female_ed_glm_re$mer, 
                               exponentiate = TRUE, 
                               tidy_fun = broom.mixed::tidy)

ed_re_model %>%
    as_flex_table() %>%
    flextable::save_as_docx(path = "/Users/meganverma/Desktop/git/MeganVermaMsc/outputs/ed_re_model.docx")

