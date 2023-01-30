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

# models---- 

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

