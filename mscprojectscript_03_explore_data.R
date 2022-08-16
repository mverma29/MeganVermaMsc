# Megan Verma 
# 7/25/2022 

if(!dir.exists('outputs')){dir.create('outputs')}

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

# change certain character variables to factor
respicar_socio$Country             <- as.factor(respicar_socio$Country)
respicar_socio$`ISO 3166-1`        <- as.factor(respicar_socio$`ISO 3166-1`)
respicar_socio$Continent           <- as.factor(respicar_socio$Continent)
respicar_socio$Age                 <- as.factor(respicar_socio$Age)
respicar_socio$`Ethnic Minority`   <- as.factor(respicar_socio$`Ethnic Minority`)
respicar_socio$Design              <- as.factor(respicar_socio$Design)
respicar_socio$`Sampling strategy` <- as.factor(respicar_socio$`Sampling strategy`)
# respicar_socio$`subregion`         <- as.factor(respicar_socio$`subregion`)

summary(respicar_socio)
ethnic_minority <- filter(respicar_socio, `Ethnic Minority`=="Yes")
# 62 studies in ethnic minority groups 

# 376 entries (studies) if we take out ethnic minority
# 438 datasets
# Cross-sectional:312
# Longitudinal   :125
# Unknown design :  1

respicar_israel    <- filter(respicar_socio,`Country` == "Israel")
# 22 studies in Israel, 1993-2009
respicar_palestine <- filter(respicar_socio,`Country` == "Palestinian Territories")
# 5 studies in Palestine, 2009 only

# make carriage variable
respicar_socio     <- respicar_socio %>% 
  mutate(carriage = Positive/Total)

summary(respicar_socio)

# basic map-----

# make weighted mean of carriage by country


respicar_carriage <- respicar_socio %>%
  dplyr::group_by(`ISO 3166-1`) %>%
  dplyr::summarise(carriage_country =
                     weighted.mean(x     = carriage,
                                   w     = Total,
                                   na.rm = T),
                   n                = n())

# because assumed fixed effect, we take a weighted average of the carriage 
# by the population in the study (total individuals)
# can't do the same for covariates-- they change over time 

# plot carriage by country 
world <- ne_countries(scale = "small", returnclass = "sf")
world <- world %>% filter(sovereignt != "Antarctica")

world_with_carriage <- merge (
  x     = respicar_carriage,
  y     = world,
  by.x  = "ISO 3166-1",
  by.y  = "iso_n3",
  all.y = TRUE
)


country_map <- ggplot(data = world_with_carriage) +
  geom_sf(aes(geometry   = geometry, #world map geometry (polygons)
              fill       = carriage_country),
          size = 0.1) + #color map w/ cont. values of total cases
  scale_fill_gradient(
    low                = "yellow",
    high               = "red",
    na.value           = "azure2",
    name               = 'Carriage rate (weighted average)',
    limits             = c(0, 1)
  ) + #set color fill
  theme_bw() + #theme of dark text on light background
  theme(legend.position  = 'bottom') +
  ggtitle("Worldwide Streptococcus pneumoniae Carriage, by Country")



ggsave(
  filename = "outputs/country_map.png",
  plot     = country_map,
  device   = png,
  width    = 7,
  height   = 4,
  units    = 'in',
  res      = 600
)

summary(world_with_carriage$carriage) #108 countries without carriage (not in dataset)

# re-map into UN subregions carriage-----

# make weighted mean of carriage by UN subregion 

# find most recent population of each country 
data(pop)
respicar_subregion <- merge (
  x = respicar_carriage,
  y = pop,
  by.x = "ISO 3166-1",
  by.y = "country_code",
  all.x = TRUE
)

na_pop <- respicar_subregion %>% filter(is.na(2020)) # no missing values

respicar_subregion <- respicar_subregion %>%
  mutate(
    region = countrycode(
      sourcevar = `ISO 3166-1`,
      origin = "iso3n",
      destination = "un.regionsub.name"
    ),
    region = ifelse(
      test = `ISO 3166-1` == 158,
      yes  = "Eastern Asia",
      no   = region
    )
  )

na_subregion <- respicar_subregion %>% filter(is.na(region))

respicar_subregion <- respicar_subregion %>%
  dplyr::group_by(`region`) %>%
  dplyr::summarise(
    carriage_subregion =
      weighted.mean(x = carriage_country,
                    w = `2020`, na.rm = T),
    n = n()
  )


world <- mutate(
  world,
  region = countrycode(
    sourcevar = `iso_a3`,
    origin = "iso3c",
    destination = "un.regionsub.name"
  ),
  region = ifelse(
    test = `iso_n3` == "158",
    yes  = "Eastern Asia",
    no   = region
  )
)

cntries  <- st_read("https://ec.europa.eu/eurostat/cache/GISCO/distribution/v2/countries/geojson/CNTR_RG_20M_2016_3035.geojson",
                    stringsAsFactors = FALSE)


subworld <- cntries %>%
    mutate(region = countrycode::countrycode(id,
                                             origin = 'iso2c',
                                             destination = 'un.regionsub.name')) 

subworld %<>% mutate(
    region = case_when(id == "AQ"    ~ "Antarctica",
                       id == "CP"    ~ "Western Europe",
                       id == "EL"    ~ "Southern Europe",
                       id == "UK"    ~ "Northern Europe",
                       is.na(region) ~ "Disputed Territory",
                       TRUE          ~ region)
)

subworld %<>%
    group_by(region) %>%
    summarise(n = n()) %>%
    filter(!is.na(region)) %>%
    select(-n)

subworld %<>% filter(region != "Antarctica") %>%
    st_transform(crs = '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0')



# merge map data with subregion
world_with_subregion_carriage <-
  merge (x     = respicar_subregion,
         y     = subworld,
         by    = "region",
         all.y = TRUE)




subregion_map <- ggplot(data = world_with_subregion_carriage) +
    geom_sf(aes(geometry       = geometry, #world map geometry (polygons)
                fill           = carriage_subregion),
            size = 0.1) + #color map w/ cont. values of total cases
    scale_fill_gradient(
        low                      = "yellow",
        high                     = "red",
        na.value                 = "azure2",
        name                     = 'Carriage rate (weighted average)',
        limits                   = c(0, 1)
    ) + #set color fill
    theme_bw() + #theme of dark text on light background
    theme(legend.position      = 'bottom') +
    ggtitle("Worldwide Streptococcus pneumoniae Carriage, by UN Subregion") 

    
ggsave(
  filename = "outputs/subregion_map.png",
  plot     = subregion_map,
  device   = png,
  width    = 7,
  height   = 4,
  units    = 'in',
  res      = 600
)


# how old is the covariate value for each study?-----

staleness_socio <- get_staleness_socio(respicar_socio)
# shouldn't there be 438*5=2190 values here?
# the `distinct` 

summary(staleness_socio) # max: -23 (23 years in the future)

# look at the total datasets with vars of 5+ years of staleness, for each var: 

staleness_urban <- covariate_staleness_socio("urban_percent")
staleness_gdp <- covariate_staleness_socio("gdp_usd")
staleness_gini <- covariate_staleness_socio("gini")
staleness_hh <- covariate_staleness_socio("mean_hh")
staleness_female_ed <- covariate_staleness_socio("female_ed")


# exploratory analysis covariates: scatter plots -------





# carriage and urban percent
ggplot(respicar_socio, aes(y = carriage, x = urban_percent)) +
  geom_point(size          = 2) +
  ylab("Carriage") +
  xlab("Urban Population (Percent)") +
  #scale_x_log10() +
  #scale_y_log10() + 
  # there may be some relationship--less carriage in higher urbanized populations??
  geom_smooth(method       = 'lm')

ggsave("outputs/scatter_urban_percent_carriage.png")


# carriage and GDP
ggplot(respicar_socio, aes(y = carriage, x = log_gdp)) +
  geom_point(size = 2) +
  ylab("Carriage") +
  xlab("Log GDP (USD) per capita") +
  # scale_x_log10() +
  # scale_y_log10() + # carriage decreases as gdp inc
  geom_smooth(method = 'lm')

ggsave("outputs/scatter_gdp_carriage.png")

# carriage and Gini
ggplot(respicar_socio, aes(y = carriage, x = gini)) +
  geom_point(size = 2) +
  ylab("Carriage") +
  xlab("Gini Coefficient of Inequality (Percent)") +
  # scale_x_log10() +
  #scale_y_log10() + # when gini is higher (more unequal), carriage is higher
  geom_smooth(method = 'lm')

ggsave("outputs/scatter_gini_carriage.png")


# carriage and HH size 
ggplot(respicar_socio, aes(y = carriage,x = mean_hh)) + 
  geom_point(size=2) + 
  ylab("Carriage") + 
  xlab("Average Household Size") +
  #scale_x_log10() +
  #scale_y_log10() + # when average hh size increases, carriage increases
  geom_smooth(method = 'lm')

ggsave("outputs/scatter_hh_carriage.png")


# carriage and female education
ggplot(respicar_socio, aes(y = carriage,x = female_ed)) + 
  geom_point(size=2) + 
  ylab("Carriage") + 
  xlab("Female Secondary Education Rate (Percent)") +
  #scale_x_log10() + 
  #scale_y_log10() + # when female ed increases, carriage decreases
  geom_smooth(method = 'lm')

ggsave("outputs/scatter_female_ed_carriage.png")


# carriage and UN subregion (not useful?)
# ggplot(respicar_subregion, aes(y = carriage_subregion,x = subregion)) + 
#     geom_point(size=2) + 
#     ylab("Carriage") + 
#     xlab("UN Subregion") +
#     #scale_x_log10() + 
#     #scale_y_log10() + # when carriage is...
#     geom_smooth(method = 'lm') + 
#     geom_text( aes(label = subregion), hjust = 1, vjust = -1)



# explore carriage itself------
# carriage histogram
carriage_hist <- ggplot (respicar_socio, aes(x = carriage)) +
  geom_histogram(binwidth = 0.02, center = 0.02/2)
# fairly normal distribution ?

ggsave(
    filename = "outputs/carriage_hist.png",
    plot     = carriage_hist, 
    device   = png,
    width    = 10,
    height   = 7,
    units    = 'in',
    res      = 600
)

boxplot_subregion <- ggplot(respicar_socio, aes (subregion, carriage)) + 
    geom_boxplot() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) + 
    xlab("UN Subregion") + 
    ylab("Carriage Rate")

ggsave(
    filename = "outputs/boxplot_subregion.png",
    plot     = boxplot_subregion, 
    device   = png,
    width    = 13,
    height   = 7,
    units    = 'in',
    res      = 600
)

# potentially some correlation there


# facetwrap plot of each covariate by UN subregion -------

facet_urban_percent <- ggplot(data = respicar_socio,
       aes(x = urban_percent, y = carriage)) +
    geom_point() +
    facet_wrap(~ subregion, nrow = 3) +
    theme_bw() +
    geom_point(data = select(respicar_socio,-subregion),
               alpha = 0.1,
               pch = 16) + 
    ggtitle("Urban Population and Carriage, by UN Subregion") + 
    theme(plot.title = element_text(hjust = 0.5)) + 
    theme(strip.background = element_rect(fill="lightblue", size=1, color="darkblue")) + 
    xlab("Urban Population (Percent)") + 
    ylab("Carriage Rate")

ggsave(
    filename = "outputs/facet_urban_percent_carriage.png",
    plot     = facet_urban_percent,
    device   = png,
    width    = 11,
    height   = 5,
    units    = 'in',
    res      = 600
)

facet_gdp <- ggplot(data = respicar_socio,
       aes(x = log_gdp, y = carriage)) +
    geom_point() +
    facet_wrap(~ subregion, nrow = 3) +
    theme_bw() +
    geom_point(data = select(respicar_socio,-subregion),
               alpha = 0.1,
               pch = 16) + 
    ggtitle("Log GDP and Carriage, by UN Subregion") + 
    theme(plot.title = element_text(hjust = 0.5)) + 
    theme(strip.background = element_rect(fill="lightblue", size=1, color="darkblue")) + 
    xlab("Log GDP (USD)") + 
    ylab("Carriage Rate")

ggsave(
    filename = "outputs/facet_gdp_carriage.png",
    plot     = facet_gdp,
    device   = png,
    width    = 11,
    height   = 5,
    units    = 'in',
    res      = 600
)

facet_gini <- ggplot(data = respicar_socio,
       aes(x = gini, y = carriage)) +
    geom_point() +
    facet_wrap(~ subregion, nrow = 3) +
    theme_bw() +
    geom_point(data = select(respicar_socio,-subregion),
               alpha = 0.1,
               pch = 16) + 
    ggtitle("Gini Coefficient of Inequality and Carriage, by UN Subregion") + 
    theme(plot.title = element_text(hjust = 0.5)) + 
    theme(strip.background = element_rect(fill="lightblue", size=1, color="darkblue")) + 
    xlab("Gini Coefficient of Inequality (Percent)") + 
    ylab("Carriage Rate")

ggsave(
    filename = "outputs/facet_gini_carriage.png",
    plot     = facet_gini,
    device   = png,
    width    = 11,
    height   = 5,
    units    = 'in',
    res      = 600
)

facet_hh <- ggplot(data = respicar_socio,
       aes(x = mean_hh, y = carriage)) +
    geom_point() +
    facet_wrap(~ subregion, nrow = 3) +
    theme_bw() +
    geom_point(data = select(respicar_socio,-subregion),
               alpha = 0.1,
               pch = 16) + 
    ggtitle("Average Household Size and Carriage, by UN Subregion") + 
    theme(plot.title = element_text(hjust = 0.5)) + 
    theme(strip.background = element_rect(fill="lightblue", size=1, color="darkblue")) + 
    xlab("Average Household Size") + 
    ylab("Carriage Rate")

ggsave(
    filename = "outputs/facet_hh_carriage.png",
    plot     = facet_hh,
    device   = png,
    width    = 11,
    height   = 5,
    units    = 'in',
    res      = 600
)

facet_ed <- ggplot(data = respicar_socio,
       aes(x = female_ed, y = carriage)) +
    geom_point() +
    facet_wrap(~ subregion, nrow = 3) +
    theme_bw() +
    geom_point(data = select(respicar_socio,-subregion),
               alpha = 0.1,
               pch = 16) + 
    ggtitle("Female Secondary Education Enrollment and Carriage, by UN Subregion") + 
    theme(plot.title = element_text(hjust = 0.5)) + 
    theme(strip.background = element_rect(fill="lightblue", size=1, color="darkblue")) + 
    xlab("Female Secondary Education Enrollment (Percent)") + 
    ylab("Carriage Rate")

ggsave(
    filename = "outputs/facet_ed_carriage.png",
    plot     = facet_ed,
    device   = png,
    width    = 11,
    height   = 5,
    units    = 'in',
    res      = 600
)




# descriptive stats by UN subregion------

desc_aus_nz <- filter(respicar_socio, `subregion`=="Australia and New Zealand")
summary(desc_aus_nz)

desc_c_asia <- filter(respicar_socio, `subregion`=="Central Asia")
summary(desc_c_asia)

desc_e_asia <- filter(respicar_socio, `subregion`=="Eastern Asia")
summary(desc_e_asia)

desc_e_europe <- filter(respicar_socio, `subregion`=="Eastern Europe")
summary(desc_e_europe)

desc_l_a <- filter(respicar_socio, `subregion`=="Latin America and the Caribbean")
summary(desc_l_a)

desc_mel <- filter(respicar_socio, `subregion`=="Melanesia")
summary(desc_mel)

desc_n_africa <- filter(respicar_socio, `subregion`=="Northern Africa")
summary(desc_n_africa)

desc_n_amer <- filter(respicar_socio, `subregion`=="Northern America")
summary(desc_n_amer)

desc_n_europe <- filter(respicar_socio, `subregion`=="Northern Europe")
summary(desc_n_europe)

desc_se_asia <- filter(respicar_socio, `subregion`=="South-eastern Asia")
summary(desc_se_asia)

desc_s_asia <- filter(respicar_socio, `subregion`=="Southern Asia")
summary(desc_s_asia)

desc_s_europe <- filter(respicar_socio, `subregion`=="Southern Europe")
summary(desc_s_europe)

desc_ss_africa <- filter(respicar_socio, `subregion`=="Sub-Saharan Africa")
summary(desc_ss_africa)

desc_w_asia <- filter(respicar_socio, `subregion`=="Western Asia")
summary(desc_w_asia)

desc_w_euro <- filter(respicar_socio, `subregion`=="Western Europe")
summary(desc_w_euro)






# descriptive stats for table 1/2-----
summary(respicar_socio)
sum(respicar_socio$Total)

age_under_5 <- filter(respicar_socio, `Age`=="<5y")
sum(age_under_5$Total)

age_5_17 <- filter(respicar_socio, `Age`=="5-17y")
sum(age_5_17$Total)

age_18_plus <- filter(respicar_socio, `Age`=="18+y")
sum(age_18_plus$Total)

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



