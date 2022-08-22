# Megan Verma 
# 8/16/2022 

# summaries/tidying------
# view(respicar) 
summary(respicar_socio) 

summary(respicar_socio$female_ed) # max is >1 

excess_ed <- respicar_socio %>% 
  filter(`female_ed` > 100) # 155 datasets with ed > 100% 

t1              <- table(respicar_socio$Country)
study_countries <- as.data.frame(t1)
# 76 countries, most from US then portugal/israel/brazil (44 & 22 each)
names(respicar)

# covariate staleness-----

staleness_socio <- get_staleness_socio(respicar_socio)
# shouldn't there be 438*5=2190 values here?
# the `distinct` 

summary(staleness_socio) # max: -23 (23 years in the future)

stale_studies <- staleness_socio %>% 
    filter(staleness != 0) %>% 
               distinct(id) #196 studies with at least
# one stale covariate 

# look at the total datasets with vars of 5+ years of staleness, for each var: 

staleness_urban <- covariate_staleness_socio("urban_percent")
staleness_gdp <- covariate_staleness_socio("gdp_usd")
staleness_gini <- covariate_staleness_socio("gini")
staleness_hh <- covariate_staleness_socio("mean_hh")
staleness_female_ed <- covariate_staleness_socio("female_ed")



# how many are not exactly matched for each covariate?----
respicar_unfilled_urban <- respicar_unfilled %>% 
    filter(is.na(urban_percent)) # 7 obs

respicar_unfilled_gdp <- respicar_unfilled %>% 
    filter(is.na(gdp_usd)) # 2 obs

respicar_unfilled_gini <- respicar_unfilled %>% 
    filter(is.na(gini)) # 201 obs

respicar_unfilled_hh <- respicar_unfilled %>% 
    filter(is.na(mean_hh)) # 340 obs

respicar_unfilled_ed <- respicar_unfilled %>% 
    filter(is.na(female_ed)) # 121 obs

# exploratory analysis covariates: scatter plots -------





# carriage and urban percent
p1 <- ggplot(respicar_socio, aes(y = carriage, x = urban_percent)) +
  geom_point(size          = 2) +
  ylab("Carriage") +
  xlab("Urban Population (Percent)") +
  #scale_x_log10() +
  #scale_y_log10() + 
  # there may be some relationship--less carriage in higher urbanized populations??
  geom_smooth(method       = 'lm') + 
    theme_bw()

ggsave("outputs/scatter_urban_percent_carriage.png")


# carriage and GDP
p2 <- ggplot(respicar_socio, aes(y = carriage, x = log_gdp)) +
  geom_point(size = 2) +
  ylab("Carriage") +
  xlab("Log GDP (USD) per capita") +
  # scale_x_log10() +
  # scale_y_log10() + # carriage decreases as gdp inc
  geom_smooth(method = 'lm') + 
    theme_bw()

ggsave("outputs/scatter_gdp_carriage.png")

# carriage and Gini
p3 <- ggplot(respicar_socio, aes(y = carriage, x = gini)) +
  geom_point(size = 2) +
  ylab("Carriage") +
  xlab("Gini Coefficient of Inequality (Percent)") +
  # scale_x_log10() +
  #scale_y_log10() + # when gini is higher (more unequal), carriage is higher
  geom_smooth(method = 'lm') + 
    theme_bw()

ggsave("outputs/scatter_gini_carriage.png")


# carriage and HH size 
p4 <- ggplot(respicar_socio, aes(y = carriage,x = mean_hh)) + 
  geom_point(size=2) + 
  ylab("Carriage") + 
  xlab("Average Household Size") +
  #scale_x_log10() +
  #scale_y_log10() + # when average hh size increases, carriage increases
  geom_smooth(method = 'lm') + 
    theme_bw()

ggsave("outputs/scatter_hh_carriage.png")


# carriage and female education
p5 <- ggplot(respicar_socio, aes(y = carriage,x = female_ed)) + 
  geom_point(size=2) + 
  ylab("Carriage") + 
  xlab("Female Secondary Education Rate (Percent)") +
  #scale_x_log10() + 
  #scale_y_log10() + # when female ed increases, carriage decreases
  geom_smooth(method = 'lm') + 
    theme_bw()

ggsave("outputs/scatter_female_ed_carriage.png")

scatter <- p1 + p2 + p3 + p4 + p5 + plot_annotation(
    title =
        "Covariates and Carriage",
    tag_levels = 'A',
    theme = theme(plot.tag = element_text(size = 4), 
                  plot.title = element_text(size= 18, hjust = 0.5)))

ggsave(
    filename = "outputs/scatterplot.png",
    plot     = scatter,
    device   = png,
    width    = 12,
    height   = 7,
    units    = 'in',
    res      = 600
)


# pairs plots-----

# w/ unfilled covariate respicar data
pairs_plot_unfilled <- ggpairs(
    respicar_unfilled,
    columns = c(
        "urban_percent",
        "log_gdp",
        "gini",
        "mean_hh",
        "female_ed",
        "carriage"
    ),
    columnLabels = c(
        "Urban Population",
        "Log10 GDP per capita",
        "Gini Coefficient",
        "Average Household Size",
        "Female Secondary Education Enrollment",
        "Carriage"
    ),
    title = "Sociodemographic Covariates and Carriage", 
    labeller = label_wrap_gen(10)) + 
    theme(strip.placement = "outside", text = element_text(size = 10)) +
    theme_bw() +
    theme(strip.background = element_rect(fill="lightblue", size=1, color="darkblue")) +
    theme(plot.title = element_text(hjust = 0.5))
    

ggsave(
    filename = "outputs/pairsplotunfilled.png",
    plot     = pairs_plot_unfilled,
    device   = png,
    width    = 7,
    height   = 7,
    units    = 'in',
    res      = 600
)

# w/ filled covariate respicar data
pairs_plot_filled <- ggpairs(
    respicar_socio,
    columns = c(
        "urban_percent",
        "log_gdp",
        "gini",
        "mean_hh",
        "female_ed",
        "carriage"
    ),
    columnLabels = c(
        "Urban Population",
        "Log10 GDP per capita",
        "Gini Coefficient",
        "Average Household Size",
        "Female Secondary Education Enrollment",
        "Carriage"
    ),
    title = "Sociodemographic Covariates and Carriage", 
    labeller = label_wrap_gen(10)) + 
    theme(strip.placement = "outside", text = element_text(size = 10)) +
    theme_bw() +
    theme(strip.background = element_rect(fill="lightblue", size=1, color="darkblue")) +
    theme(plot.title = element_text(hjust = 0.5))


ggsave(
    filename = "outputs/pairsplot.png",
    plot     = pairs_plot_filled,
    device   = png,
    width    = 7,
    height   = 7,
    units    = 'in',
    res      = 600
)

# correlation matrix-------

unfilled_covariates <- respicar_unfilled %>% select(c(
    "urban_percent",
    "log_gdp",
    "gini",
    "mean_hh",
    "female_ed",
    "carriage")) 

corr_unfilled <- cor(unfilled_covariates, use="pairwise.complete.obs") %>% 
    as.data.frame()
write_csv(corr_unfilled,
          file = "outputs/corr_unfilled.csv")

correlation_matrix_unfilled <- rcorr(as.matrix(unfilled_covariates))
correlation_matrix_unfilled
                                     

filled_covariates <- respicar_socio %>% select(c(
    "urban_percent",
    "log_gdp",
    "gini",
    "mean_hh",
    "female_ed",
    "carriage")) 

corr_filled <- cor(filled_covariates, use="complete.obs") %>% 
    as.data.frame()
write_csv(corr_filled,
          file = "outputs/corr_filled.csv")


correlation_matrix_filled <- rcorr(as.matrix(filled_covariates))
correlation_matrix_filled

# descriptive stats by covariate for table 1/2-----
summary(respicar_socio)
sum(respicar_socio$Total)

age_under_5 <- filter(respicar_socio, `Age`=="<5y")
sum(age_under_5$Total)

age_5_17 <- filter(respicar_socio, `Age`=="5-17y")
sum(age_5_17$Total)

age_18_plus <- filter(respicar_socio, `Age`=="18+y")
sum(age_18_plus$Total)


ethnic_minority <- filter(respicar_socio, `Ethnic Minority`=="Yes")
# 62 studies in ethnic minority groups 
sum(ethnic_minority$Total)

ethnic_majority <- filter(respicar_socio, `Ethnic Minority`=="No")
sum(ethnic_majority$Total)

cross_sectional <- filter(respicar_socio, `Design`=="Cross-sectional")
sum(cross_sectional$Total)

cohort <- filter(respicar_socio, `Design`=="Longitudinal")
sum(cohort$Total)

unknown_design <- filter(respicar_socio, `Design`=="Unknown design")
sum(unknown_design$Total)

group_sampling <- filter(respicar_socio, `Sampling strategy`=="Group")
sum(group_sampling$Total)

outpatient_sampling <- filter(respicar_socio, `Sampling strategy`=="Outpatients")
sum(outpatient_sampling$Total)

random_sampling <- filter(respicar_socio, `Sampling strategy`=="Random/community")
sum(random_sampling$Total)

trial_sampling <- filter(respicar_socio, `Sampling strategy`=="Trial arm")
sum(trial_sampling$Total)

other_sampling <- filter(respicar_socio, `Sampling strategy`=="Other")
sum(other_sampling$Total)



# 376 entries (studies) if we take out ethnic minority
# 438 datasets
# Cross-sectional:312
# Longitudinal   :125
# Unknown design :  1

us_studies <- filter(respicar_socio, `Country`=="United States")
sum(us_studies$Total)

brazil_studies <- filter(respicar_socio, `Country`=="Brazil")
sum(brazil_studies$Total)

israel_studies <- filter(respicar_socio, `Country`=="Israel")
sum(israel_studies$Total)

portugal_studies <- filter(respicar_socio, `Country`=="Portugal")
sum(portugal_studies$Total)

france_studies <- filter(respicar_socio, `Country`=="France")
sum(france_studies$Total)

norway_studies <- filter(respicar_socio, `Country`=="Norway")
sum(norway_studies$Total)

other_studies <- respicar_socio %>% 
  filter(!`Country` %in% c("United States","Brazil", "Israel", 
                           "Portugal","France","Norway"))

sum(other_studies$Total)

# check what country has the most individuals sampled
country_stats <- respicar_socio %>% 
    group_by(`Country`) %>% 
    summarize(.groups="keep", sum(Total))


