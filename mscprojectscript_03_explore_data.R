# Megan Verma 
# 7/25/2022 

if(!dir.exists('outputs')){dir.create('outputs')}

# summaries/tidying------
# view(respicar) 
summary(respicar_socio) 

summary(respicar_socio$female_ed) # max is >1 

excess_ed <- respicar_socio %>% 
    filter(`female_ed` > 1) # 155 datasets with ed > 100% 

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
    dplyr::summarise(carriage_country = weighted.mean
                     (x               = carriage,
                         w            = Total,
                         na.rm        = T),
                     n                = n())

# because assumed fixed effect, we take a weighted average of the carriage 
# by the population in the study (total individuals)
# can't do the same for covariates-- they change over time 

# plot carriage by country 
world <- ne_countries(scale = "small", returnclass = "sf")
world <- world %>% filter(sovereignt != "Antarctica")

world_with_carriage <- merge (
    x = respicar_carriage,
    y = world,
    by.x = "ISO 3166-1",
    by.y = "iso_n3",
    all.y = TRUE
)


country_map <- ggplot(data = world_with_carriage) +
    geom_sf(aes(geometry   = geometry, #world map geometry (polygons)
                fill       = carriage_country)) + #color map w/ cont. values of total cases
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
    height   = 3,
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

na_region <- world %>% filter(is.na(region)) #countries not in study

# aggregate world geometry into subregions
subworld <- world %>% 
  group_by(region) %>%
  # Mock the data field
  summarise(data=n())


# merge map data with subregion
world_with_subregion_carriage <-
    merge (x     = respicar_subregion,
           y     = subworld,
           by    = "region",
           all.y = TRUE)


# re-map onto subregions
subregion_map <- ggplot(data = world_with_subregion_carriage) +
    geom_sf(aes(geometry     = geometry, #world map geometry (polygons)
                fill         = carriage_subregion)) + #color map w/ cont. values of total cases
    scale_fill_gradient(
        low                  = "yellow",
        high                 = "red",
        na.value             = "azure2",
        name                 = 'Carriage rate (weighted average)',
        limits               = c(0, 1)
    ) + #set color fill
    theme_bw() + #theme of dark text on light background
    theme(legend.position    = 'bottom') +
    ggtitle("Worldwide Streptococcus pneumoniae Carriage, by UN Subregion")

# for the subregions with missing countries, looks like there's a cutoff, 
# then they're plotted as NA's


ggsave(
    filename = "outputs/subregion_map.png",
    plot     = subregion_map,
    device   = png,
    width    = 7,
    height   = 3,
    units    = 'in',
    res      = 600
)

# how old is the covariate value for each study?-----

staleness_socio <- get_staleness_socio(respicar_socio)
# shouldn't there be 438*5=2190 values here?

summary(staleness_socio) # max: -23 (23 years in the future)


staleness_urban     <- staleness_socio %>%
    filter(variable == "urban_percent") %>%
    filter(staleness >= 5 | staleness <= (-5)) #0

staleness_gdp       <- staleness_socio %>%
    filter(variable == "gdp_usd") %>%
    filter(staleness >= 5 | staleness <= (-5)) # 1

staleness_gini      <- staleness_socio %>%
    filter(variable == "gini") %>%
    filter(staleness >= 5 | staleness <= (-5)) # 28

staleness_hh        <- staleness_socio %>%
    filter(variable == "mean_hh") %>%
    filter(staleness >= 5 | staleness <= (-5)) # 97

staleness_female_ed <- staleness_socio %>%
    filter(variable == "female_ed") %>%
    filter(staleness >= 5 | staleness <= (-5)) # 22

# exploratory analysis covariates: scatter plots -------





# carriage and urban percent
ggplot(respicar_socio, aes(y = carriage, x = urban_percent)) +
    geom_point(size          = 2) +
    ylab("log Carriage") +
    xlab("Percent Urban Population") +
    #scale_x_log10() +
    scale_y_log10() + # when carriage is on the log scale & urban_percent is linear,
    # there may be some relationship--less carriage in higher urbanized populations??
    geom_smooth(method       = 'lm')

ggsave("outputs/scatter_urban_percent_carriage.png")


# carriage and GDP
ggplot(respicar_socio, aes(y = carriage, x = log_gdp)) +
    geom_point(size = 2) +
    ylab("log Carriage") +
    xlab("log GDP per capita") +
    # scale_x_log10() +
    scale_y_log10() + # carriage decreases as gdp inc
    geom_smooth(method = 'lm')

ggsave("outputs/scatter_gdp_carriage.png")

# carriage and Gini??? does it make sense to check?
ggplot(respicar_socio, aes(y = carriage, x = gini)) +
    geom_point(size = 2) +
    ylab("log Carriage") +
    xlab("Gini Coefficient of Inequality") +
    # scale_x_log10() +
    scale_y_log10() + # when gini is higher (more unequal), carriage is higher
    geom_smooth(method = 'lm')

ggsave("outputs/scatter_gini_carriage.png")


# carriage and HH size 
ggplot(respicar_socio, aes(y = carriage,x = mean_hh)) + 
    geom_point(size=2) + 
    ylab("log Carriage") + 
    xlab("Average Household Size") +
    #scale_x_log10() +
    scale_y_log10() + # when average hh size increases, carriage increases
    geom_smooth(method = 'lm')

ggsave("outputs/scatter_hh_carriage.png")


# carriage and female education
ggplot(respicar_socio, aes(y = carriage,x = female_ed)) + 
    geom_point(size=2) + 
    ylab("log Carriage") + 
    xlab("Female Secondary Education Rate") +
    #scale_x_log10() + 
    scale_y_log10() + # when female ed increases, carriage decreases
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
ggplot (respicar_socio, aes(carriage)) +
    geom_histogram(bins = 75)
# fairly normal distribution ?

boxplot(carriage ~ subregion, data = respicar_socio)
# potentially some correlation there


# how to facetwrap plot ex
library(ggplot2)
data(mtcars)
ggplot(data = mtcars,
       aes(x = wt, y = mpg)) +
    geom_point() +
    facet_wrap(~ cyl, nrow = 1) +
    theme_bw() +
    geom_point(data = select(mtcars,-cyl),
               alpha = 0.2,
               pch = 16)

