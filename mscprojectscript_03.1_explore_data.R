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

# country map carriage-----

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

# UN subregions map carriage-----

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


# explore carriage ------
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



