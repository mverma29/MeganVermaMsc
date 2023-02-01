# For Europnuemo -- restrict final results to ethnic majority & under 5's

# investigate stats ----
respicar_eth_maj_5 <- filter(respicar_socio, `Ethnic Minority`=="No" 
                             & `Age`=="<5y") #305 obs

unique(respicar_eth_maj_5$Country) # now 73 countries

# examine country level distribution of total isolates
respicar_eth_maj_5_country <- respicar_eth_maj_5 %>% 
    mutate(prop_perc = Total/sum(Total)*100) %>% 
    group_by(Country) %>% 
    summarise(prop_perc_country = sum(prop_perc))
# 5 countries w/ greatest proportion of samples: 
# Gambia: 5.38
# Greece: 4.95
# Taiwan: 4.43
# Israel 4.30
# Kenya 4.24


# examine country level distribution of total isolates
respicar_eth_maj_5_subregion <- respicar_eth_maj_5 %>% 
    mutate(prop_perc = Total/sum(Total)*100) %>% 
    group_by(subregion) %>% 
    summarise(prop_perc_subregion = sum(prop_perc))
# 5 subregion w/ greatest proportion of samples:
# Sub-Saharan Africa 17.22
# Latin America and the Caribbean 12.60
# Northern Europe 11.83
# Southern Europe 10.74
# Southern Asia 8.78

# total prevalence 
respicar_eth_maj_5_carriage <- respicar_eth_maj_5 %>% 
    mutate(carriage = Positive/Total) %>% 
    dplyr::group_by(`ISO 3166-1`) %>%
    dplyr::summarise(carriage_country =
                         weighted.mean(x     = carriage,
                                       w     = Total,
                                       na.rm = T),
                     n                = n())

# range: 85.3% (The Gambia) to 3.13% (Saudi Arabia)

# univariate models------ 

# urban percent ----------
# urban_percent model, no RE
urban_percent_glm <- glm(
    data    = respicar_eth_maj_5,
    formula = cbind(Positive, Total - Positive) ~ urban_percent_tenth, 
    family  = "binomial")

tidy(urban_percent_glm, exp=TRUE, conf.int=TRUE) # OR: 0.866
summary(urban_percent_glm) # p-val (Wald approx): <2e-16

# urban_percent model, RE
urban_percent_glm_re <- gamm4(
    data    = respicar_eth_maj_5,
    formula = cbind(Positive, Total - Positive) ~ urban_percent_tenth, 
    random  = ~(1|subregion),
    family  = "binomial")

tidy(urban_percent_glm_re$mer, exp=TRUE, conf.int=TRUE) # OR: 1.02
summary(urban_percent_glm_re$mer) # p-val (Wald approx): 0.00321


lmtest::lrtest(urban_percent_glm, urban_percent_glm_re$mer) # <2e-16 (reject null hyp of no clustering)

# make table of model outputs
urban_percent_re <- tbl_regression(urban_percent_glm_re$mer, 
                                   exponentiate = TRUE, 
                                   intercept= FALSE,
                                   tidy_fun = broom.mixed::tidy) %>% 
    add_glance_source_note(include = c("logLik"))

urban_percent_re %>%
    as_flex_table() %>%
    flextable::save_as_docx(path = "outputs/urban_percent_re.docx")


# GDP -----------

# gdp model, no RE
gdp_glm <- glm(
    data    = respicar_eth_maj_5,
    formula = cbind(Positive, Total - Positive) ~ log_gdp, 
    family  = "binomial")

tidy(gdp_glm, exp=TRUE, conf.int=TRUE) # OR: 0.540
summary(urban_percent_glm) # p-val (Wald approx): <2e-16

# gdp model w/ RE
gdp_glm_re <- gamm4(
    data    = respicar_eth_maj_5,
    formula = cbind(Positive, Total - Positive) ~ log_gdp, 
    random  = ~(1|subregion),
    family  = "binomial")

tidy(gdp_glm_re$mer, exp=TRUE, conf.int=TRUE) # OR: 0.705

lmtest::lrtest(gdp_glm, gdp_glm_re$mer) # <2e-16 (reject null hyp of no clustering)

# make table of model outputs
gdp_re_model <- tbl_regression(gdp_glm_re$mer, 
                               exponentiate = TRUE, 
                               intercept= FALSE, 
                               tidy_fun = broom.mixed::tidy) %>% 
    add_glance_source_note(include = c("logLik"))

gdp_re_model %>%
    as_flex_table() %>%
    flextable::save_as_docx(path = "outputs/gdp_re_model.docx")


# Gini-------

# gini model, no RE
gini_glm <- glm(
    data    = respicar_eth_maj_5,
    formula = cbind(Positive, Total - Positive) ~ gini_tenth, 
    family  = "binomial")

tidy(gini_glm, exp=TRUE, conf.int=TRUE) # OR: 1.27
summary(gini_glm) # p-val (Wald approx): <2e-16

# gini model w/ RE
gini_glm_re <- gamm4(
    data    = respicar_eth_maj_5,
    formula = cbind(Positive, Total - Positive) ~ gini_tenth, 
    random  = ~(1|subregion),
    family  = "binomial")

tidy(gini_glm_re$mer, exp=TRUE, conf.int=TRUE) # OR: 1.04
lmtest::lrtest(gini_glm, gini_glm_re$mer) # <2e-16 (reject null hyp of no clustering)

# make table of model outputs
gini_re_model <- tbl_regression(gini_glm_re$mer, 
                                exponentiate = TRUE, 
                                intercept= FALSE, 
                                tidy_fun = broom.mixed::tidy) %>% 
    add_glance_source_note(include = c("logLik"))

gini_re_model %>%
    as_flex_table() %>%
    flextable::save_as_docx(path = "outputs/gini_re_model.docx")


# HH size------

# HH model, no RE
hh_glm <- glm(
    data    = respicar_eth_maj_5,
    formula = cbind(Positive, Total - Positive) ~ mean_hh, 
    family  = "binomial")

tidy(hh_glm, exp=TRUE, conf.int=TRUE) # OR: 1.35
summary(hh_glm) # p-val (Wald approx): <2e-16

# HH model w/ RE
hh_glm_re <- gamm4(
    data    = respicar_eth_maj_5,
    formula = cbind(Positive, Total - Positive) ~ mean_hh, 
    random  = ~(1|subregion),
    family  = "binomial")

tidy(hh_glm_re$mer, exp=TRUE, conf.int=TRUE) #OR: 1.17
lmtest::lrtest(hh_glm, hh_glm_re$mer) # <2e-16 (reject null hyp of no clustering)


# make table of model outputs
hh_re_model <- tbl_regression(hh_glm_re$mer, 
                              exponentiate = TRUE, 
                              intercept= FALSE, 
                              tidy_fun = broom.mixed::tidy) %>% 
    add_glance_source_note(include = c("logLik"))


hh_re_model %>%
    as_flex_table() %>%
    flextable::save_as_docx(path = "outputs/hh_re_model.docx")


# female ed------

female_ed_glm <- glm(
    data    = respicar_eth_maj_5,
    formula = cbind(Positive, Total - Positive) ~ female_ed_tenth, 
    family  = "binomial")

tidy(female_ed_glm, exp=TRUE, conf.int=TRUE) # OR: 0.913
summary(female_ed_glm) # p-val (Wald approx): <2e-16

# female ed model w/ RE
female_ed_glm_re <- gamm4(
    data    = respicar_eth_maj_5,
    formula = cbind(Positive, Total - Positive) ~ female_ed_tenth, 
    random  = ~(1|subregion),
    family  = "binomial")

tidy(female_ed_glm_re$mer, exp=TRUE, conf.int=TRUE) # OR: 0.988
lmtest::lrtest(female_ed_glm, female_ed_glm_re$mer) # <2e-16 (reject null hyp of no clustering)


# make table of model outputs
ed_re_model <- tbl_regression(female_ed_glm_re$mer, 
                              exponentiate = TRUE,
                              intercept= FALSE, 
                              tidy_fun = broom.mixed::tidy) %>% 
    add_glance_source_note(include = c("logLik"))

ed_re_model %>%
    as_flex_table() %>%
    flextable::save_as_docx(path = "outputs/ed_re_model.docx")


# multivariate models---- 

# test for RE (no interaction)---- 

# full model, without RE, without interaction 
full_glm_sensitivity_no_re <- glm(
    data    = respicar_eth_maj_5,
    formula = cbind(Positive, Total - Positive) ~
        urban_percent_tenth + log_gdp + gini_tenth + mean_hh + female_ed_tenth,
    family  = "binomial")


# full model, with RE, without interaction 
full_glm_sensitivity_re <- gamm4(
    data    = respicar_eth_maj_5,
    formula = cbind(Positive, Total - Positive) ~
        urban_percent_tenth + log_gdp + gini_tenth + mean_hh + female_ed_tenth,
    random  = ~(1|subregion),
    family  = "binomial")

tidy(
    x        = full_glm_sensitivity_re$mer,
    exp      = TRUE,
    conf.int = TRUE,
    pvals    = TRUE,
    digits   = 3) %>%
    select(-effect, -group) %>%
    mutate(term = sub(pattern = "^X", replacement = "", x = term))

# lrt of RE 
lmtest::lrtest(full_glm_sensitivity_re$mer, full_glm_sensitivity_no_re) # <2.2e-16-- evidence for RE 

# test for interaction (specifying RE)---- 
# full glm w/ interaction, RE

full_glm_interaction_sensitivity_re <- gamm4(
    data    = respicar_eth_maj_5,
    formula = cbind(Positive, Total - Positive) ~
        urban_percent_tenth + log_gdp*gini_tenth + mean_hh + female_ed_tenth,
    random  = ~(1|subregion),
    family  = "binomial")

tidy(
    full_glm_interaction_sensitivity_re$mer,
    exp     = TRUE,
    conf.int = TRUE,
    pvals   = TRUE,
    digits  = 3
) 

# lrtest of interaction of gini and log gdp
# compare this model with RE and interaction to one w/ only interaction 
lmtest::lrtest(full_glm_sensitivity_re$mer, full_glm_interaction_sensitivity_re$mer) # <2.2e-16-- evidence for interaction 


full_glm_interaction_sensitivity_tbl <- tbl_regression(full_glm_interaction_sensitivity_re$mer, 
                                                       exponentiate = TRUE, 
                                                       tidy_fun = broom.mixed::tidy)
full_glm_interaction_sensitivity_tbl %>%
    as_flex_table() %>%
    save_as_docx(path = "outputs/full_glm_interaction_sensitivity_tbl.docx")


# stepwise model, with RE (& W/O interaction)----

stepwise_sensitivity_re <- buildgamm4 (
    data    = respicar_eth_maj_5,
    formula = cbind(Positive, Total - Positive) ~
        urban_percent_tenth + log_gdp + gini_tenth + mean_hh + female_ed_tenth + (1 | subregion),
    family  = "binomial"
)

summary(stepwise_sensitivity_re@model) # drops out gini

# make output table 
stepwise_sensitivity_re_tbl <- tbl_regression(stepwise_sensitivity_re@model, 
                                                          exponentiate = TRUE, 
                                                          tidy_fun = broom.mixed::tidy)

stepwise_sensitivity_re_tbl %>%
    as_flex_table() %>%
    save_as_docx(path = "outputs/stepwise_sensitivity_re_tbl.docx")



# stepwise model, with RE & with interaction---- 

stepwise_sensitivity_re_interaction <- buildgamm4 (
    data    = respicar_eth_maj_5,
    formula = cbind(Positive, Total - Positive) ~
        urban_percent_tenth + log_gdp*gini_tenth + mean_hh + female_ed_tenth + (1 | subregion),
    family  = "binomial"
)

summary(stepwise_sensitivity_re_interaction@model) # all are retained

# make output table 
stepwise_sensitivity_re_interaction_tbl <- tbl_regression(stepwise_sensitivity_re_interaction@model, 
                                           exponentiate = TRUE, 
                                           tidy_fun = broom.mixed::tidy)

stepwise_sensitivity_re_interaction_tbl %>%
    as_flex_table() %>%
    save_as_docx(path = "outputs/stepwise_sensitivity_re_interaction.docx")

# effect of interaction??? 
# should give you the OR for gini_tenth when GDP per capita is 1000 for stepwise 
exp(coef(stepwise_sensitivity_re_interaction$gam)['gini_tenth'] + 3 * coef(stepwise_sensitivity_re_interaction$gam)['log_gdp:gini_tenth'])
exp(-0.828 + 3*(0.820)) # OR is 5.114093

# should give you the OR for gini_tenth when GDP per capita is 10000 for stepwise 
exp(-0.828 + 4*(0.820)) # OR is 11.61155

# same for GDP per cap of $1
exp(-0.828 + .1*(0.820)) # OR is 5.114093

