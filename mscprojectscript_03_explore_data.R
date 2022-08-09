# Megan Verma 
# 7/25/2022 

# summaries/tidying------
# view(respicar) 
summary(respicar_socio) 

t1  <- table(respicar_socio$Country)
study_countries <- as.data.frame(t1)
# 76 countries, most from US then portugal/israel/brazil (44 & 22 each)
names(respicar)

# change certain character variables to factor
respicar_socio$Country             <- as.factor(respicar_socio$Country)
respicar_socio$`ISO 3166-1`        <- as.factor(respicar_socio$`ISO 3166-1`)
respicar_socio$Continent           <- as.factor(respicar_socio$Continent)
respicar_socio$Age                 <- as.factor(respicar_socio$Age)
respicar_socio$`Ethnic Minority`   <- as.factor(respicar_socio$`Ethnic Minority`)
respicar_socio$Design              <- as.factor(respicar_socio$Design)
respicar_socio$`Sampling strategy` <- as.factor(respicar_socio$`Sampling strategy`)

summary(respicar_socio)
ethnic_minority <- filter(respicar_socio, `Ethnic Minority`=="Yes")
# 62 studies in ethnic minority groups 

# 377 entries (studies) if we take out ethnic minority
# 439 datasets
# Cross-sectional:313
# Longitudinal   :125
# Unknown design :  1

respicar_israel    <- filter(respicar_socio,`Country` == "Israel")
# 22 studies in Israel, 1993-2009
respicar_palestine <- filter(respicar_socio,`Country` == "Palestinian Territories")
# 5 studies in Palestine, 2009 only

# basic map-----
# make carriage variable
respicar_socio <- respicar_socio %>% 
  mutate(carriage = Positive/Total)

# make weighted mean of carriage by country

respicar_carriage <- respicar_socio %>% 
  dplyr::group_by(`ISO 3166-1`) %>% 
  dplyr::summarise(carriage_country = weighted.mean
                   (x = carriage, w = Total, na.rm = T), n= n())

# strange structure (not condensing into groups correctly but shouldn't matter)

# because assumed fixed effect, we take a weighted average of the carriage 
# by the population in the study (total individuals)

# can't do the same for covariates-- they change over time 

# plot carriage by country 
world <- ne_countries(scale = "small", returnclass = "sf")
world <- world %>% filter(sovereignt!="Antarctica")

#ggplot(data = world) + geom_sf() + theme_void()

world_with_carriage <- merge (x=respicar_carriage, y=world, 
                              by.x = "ISO 3166-1", 
                              by.y = "iso_n3", 
                              all.y=TRUE)

# the respicar_carriage dataset gets merged in but since it's not condensed, makes a mess

country_map <- ggplot(data = world_with_carriage) +  
  geom_sf(aes(geometry= geometry, #world map geometry (polygons)
              fill=carriage_country)) + #color map w/ cont. values of total cases
  scale_fill_gradient(low="yellow", high="red", na.value="azure2", 
                      name = 'Weighted carriage rate', 
                      limits = c(0,1)) + #set color fill 
  theme_bw() + #theme of dark text on light background 
  ggtitle("Worldwide Streptococcus pneumoniae Carriage")

summary(world_with_carriage$carriage)

# re-map into UN subregions carriage-----

# make weighted mean of carriage by UN subregion 


respicar_subregion <- respicar_socio %>% 
  dplyr::group_by(`subregion`) %>% 
  dplyr::summarise(carriage_subregion = weighted.mean(x = carriage, 
                                                      w = Total, na.rm = T),
                   n= n())

# aggregate world geometry into subregions
subworld <- world %>% 
  group_by(subregion) %>%
  # Mock the data field
  summarise(data=n())

# merge map data with subregion 
world_with_subregion_carriage <- merge (x=respicar_subregion, y=subworld, 
                                        by= "subregion", # these don't match perfectly
                                        all.y=TRUE)

# re-map onto subregions 
subregion_map <- ggplot(data = world_with_subregion_carriage) +  
  geom_sf(aes(geometry= geometry, #world map geometry (polygons)
              fill=carriage_subregion)) + #color map w/ cont. values of total cases
  scale_fill_gradient(low="yellow", high="red", na.value="azure2", 
                      name = 'Carriage rate (weighted average)', 
                      limits = c(0,1)) + #set color fill 
  theme_bw() + #theme of dark text on light background 
  theme(legend.position = 'bottom') +
  ggtitle("Worldwide Streptococcus pneumoniae Carriage, by UN Subregion")

# for the subregions with missing countries, looks like they're plotted as NA's

ggsave(filename = "outputs/subregion_map.pdf", 
       plot = subregion_map, 
       device = cairo_pdf, width = 7, height = 3, units = 'in')

# how old is the covariate value for each study?-----

staleness_socio <- get_staleness_socio(respicar_socio)
# shouldn't there be 439*5=2195 values here?

summary(staleness_socio) # max: -23 (23 years in the future)

staleness_urban <- staleness_socio %>% 
  filter(variable == "urban_percent") %>% 
  filter(staleness >= 5 | staleness<= (-5)) #0 

staleness_gdp <- staleness_socio %>% 
  filter(variable == "gdp_usd") %>% 
  filter(staleness >= 5 | staleness<= (-5)) # 1 

staleness_gini <- staleness_socio %>% 
  filter(variable == "gini") %>% 
  filter(staleness >= 5 | staleness<= (-5)) # 28

staleness_hh <- staleness_socio %>% 
  filter(variable == "mean_hh") %>% 
  filter(staleness >= 5 | staleness<= (-5)) # 97

staleness_female_ed <- staleness_socio %>% 
  filter(variable == "female_ed") %>% 
  filter(staleness >= 5 | staleness<= (-5)) # 22

# exploratory analysis covariates: scatter plots -------





# carriage and urban percent
ggplot(respicar_socio, aes(y = carriage,x = urban_percent)) + 
  geom_point(size=2) + 
  ylab("log Carriage") + 
  xlab("Percent Urban Population") +
  #scale_x_log10() + 
  scale_y_log10() + # when carriage is on the log scale & urban_percent is linear, 
  # there may be some relationship--less carriage in higher urbanized populations?? 
  geom_smooth(method = 'lm')


# carriage and GDP
ggplot(respicar_socio, aes(y = carriage,x = gdp_usd)) + 
  geom_point(size=2) + 
  ylab("log Carriage") + 
  xlab("log GDP per capita") +
  scale_x_log10() + 
  geom_smooth(method = 'lm')

# carriage and Gini??? does it make sense to check? 
ggplot(respicar_socio, aes(y = carriage,x = gini)) +
  geom_point(size=2) +
  ylab("Carriage") +
  xlab("Gini Coefficient of Inequality") +
  #scale_x_log10() +
  #scale_y_log10() + # when gini is higher (more unequal), carriage is higher
  geom_smooth(method = 'lm')

# carriage and HH size 
ggplot(respicar_socio, aes(y = carriage,x = mean_hh)) + 
  geom_point(size=2) + 
  ylab("Carriage") + 
  xlab("Average Household Size") +
  #scale_x_log10() +
  #scale_y_log10() + # when average hh size increases, carriage increases
  geom_smooth(method = 'lm')

# carriage and female education
ggplot(respicar_socio, aes(y = carriage,x = female_ed)) + 
  geom_point(size=2) + 
  ylab("Carriage") + 
  xlab("Female Secondary Education Rate") +
  #scale_x_log10() + 
  #scale_y_log10() + # when female ed increases, carriage decreases
  geom_smooth(method = 'lm')

# carriage and UN subregion (not useful?)
# ggplot(respicar_subregion, aes(y = carriage_subregion,x = subregion)) + 
#     geom_point(size=2) + 
#     ylab("Carriage") + 
#     xlab("UN Subregion") +
#     #scale_x_log10() + 
#     #scale_y_log10() + # when carriage is...
#     geom_smooth(method = 'lm') + 
#     geom_text( aes(label = subregion), hjust = 1, vjust = -1)




# basic model fitting (one covariate at a time)-----


# Fit a logistic GLM of the total carriage as a function of each covariate. 
# As each study has a different total individuals serotyped, we'll need to ensure that we use the 
# total serotyped as though it was a number of "trials", $n$, and the number of cases as a number 
# of "successes", $y$, in our GLM. This is done by specifying the left hand side of the regression 
# formula as `cbind(y, n-y)` with appropriate variable names in place of `n` and `y`.

# add RE for subregions in

# intercept-only model, to assess clustering in the data
intercept_glm <- glmer(
  data = respicar_socio %>% 
    mutate(p = Positive/Total),
  formula = p ~ 1 + (1|subregion),
  family  = "binomial", 
  weights = Total)

print(intercept_glm, corr = FALSE)


# urban_percent 
urban_percent_glm <- glmer(
  data = respicar_socio %>% 
    mutate(p = Positive/Total),
  formula = p ~ urban_percent + (1|subregion),
  family  = "binomial", 
  weights = Total)

print(urban_percent_glm, corr = FALSE)

performance::icc(urban_percent_glm) # 8% of the variation in carriage 
# can be accounted for by clustering of the data by subregion 


# need package to visualize glmer output (tidy doesn't work)
#install.packages("jtools")
jtools::summ(urban_percent_glm, exp=TRUE)

# tidy(urban_percent_glm, 
#      conf.int = T, 
#      exponentiate = TRUE)


# GDP
gdp_glm <- glmer(
  data = respicar_socio %>% 
    mutate(p = Positive/Total),
  formula = p ~ gdp_usd + (1|subregion),
  family  = "binomial", 
  weights = Total)

print(gdp_glm, corr = FALSE)

summ(gdp_glm, exp=TRUE)



# Gini
gini_glm <- glm(data    = respicar_socio %>% mutate(p = Positive/Total),
                formula = p ~ gini,
                family  = "binomial", weights = Total)

tidy(gini_glm, conf.int = T, exponentiate = TRUE)


# HH size
hh_glm <- glm(data    = respicar_socio %>% mutate(p = Positive/Total),
              formula = p ~ mean_hh,
              family  = "binomial", weights = Total)

tidy(hh_glm, conf.int = T, exponentiate = TRUE) # an actual association!


# female ed 
female_ed_glm <- glm(data    = respicar_socio %>% mutate(p = Positive/Total),
                     formula = p ~ female_ed,
                     family  = "binomial", weights = Total)

tidy(female_ed_glm, conf.int = T, exponentiate = TRUE)


# full model (without interaction), with RE for subregion correlation----

# add in RE 
full_glm <- glm(data    = respicar_socio %>% mutate(p = Positive/Total),
                formula = p ~ urban_percent + gdp_usd + gini + mean_hh + female_ed,
                family  = "binomial", weights = Total) 

# full model (with interaction), with RE for subregion correlation----

# add in RE 
# check formatting for interaction term (*??)
full_glm <- glm(data    = respicar_socio %>% mutate(p = Positive/Total),
                formula = p ~ urban_percent + gdp_usd*gini + mean_hh + female_ed,
                family  = "binomial", weights = Total) 
