# Megan Verma 
# 7/25/2022 

### load RESPICAR data ----

#RESPICAR dataset (summary)
respicar <- read_csv("data/appendix_summary.csv")


### sociodemographic variables:

# Percent urbanicity----
# from un populations divison, accessed via world bank
urban_percent <- read_csv("data/urban_pop_percent.csv")  %>% 
    pivot_longer(cols      = "1960":"2021", 
                 names_to  = "year", 
                 values_to = "urban_percent") %>%
    mutate(year = parse_integer(year))

# Taiwan and New Caledonia urban population
# https://www.worldometers.info/world-population/taiwan-population/
# https://www.worldometers.info/world-population/new-caledonia-population/
# using UN WPP 2019

urban_percent_extra <-
    list(`158` = 
             tibble(year = c(seq(1980, 2015, by = 5),
                             seq(2016, 2020, by = 1)),
                    urban_percent = c(48.2,
                                      57.2,
                                      65.7,
                                      68.0,
                                      69.5,
                                      72.0,
                                      74.4,
                                      76.7,
                                      77.1,
                                      77.6,
                                      78.0,
                                      78.5,
                                      78.9),
                    `Country Code` = "TWN",
                    `Country Name` = "Taiwan",
                    `Indicator Name` = "Urban population (% of total population)",
                    `Indicator Code` = "SP.URB.TOTL.IN.ZS"),
         `540` = tibble(year = c(seq(1980, 2015, by = 5),
                                 seq(2016, 2020, by = 1)),
                        urban_percent = c(57.4,
                                          58.8, 
                                          59.4,
                                          59.5,
                                          60.7,
                                          62.9,
                                          66.4,
                                          68.9,
                                          69.4,
                                          70.0,
                                          70.6, 
                                          71.3,
                                          71.9),
                        `Country Code` = "NCL",
                        `Country Name` = "New Caledonia",
                        `Indicator Name` = "Urban population (% of total population)",
                        `Indicator Code` = "SP.URB.TOTL.IN.ZS")) %>%
    bind_rows(.id = "iso_code") %>%
    mutate(iso_code = parse_integer(iso_code))

urban_percent %<>% bind_rows(urban_percent_extra)

# GDP----
WDIsearch("gdp per capita")
# #5 seems right-- GDP per capita, current USD 

gdp_data <- WDI(
    country   = "all",
    indicator = "NY.GDP.PCAP.CD",
    start     = 1987,
    end       = 2016,
    extra     = TRUE,
    cache     = NULL,
    latest    = NULL,
    language  = "en")

class(gdp_data$year) #integer

# Gini----
gini <- WDI(
    country   = "all", 
    indicator = "SI.POV.GINI", 
    start     = 1987,
    end       = 2016, 
    extra     = TRUE,
    language  = "en")

class(gini$year) #integer


# Household size----
# Download the UN Population Division's data on [Household size and composition]
# (https://www.un.org/development/desa/pddata/household-size-and-composition)

hh_data <- readxl::read_xlsx(
    path      = "data/un_hh.xlsx", 
    sheet     = 4, 
    range     = "A5:E819", 
    col_names = TRUE)

names(hh_data)
sum(is.na(hh_data$`Average household size (number of members)`)) #0 NAs

# female education (proxy for maternal)----
# UNESCO data, from world bank site 
# secondary education levels!

female_ed <- read_csv("data/female_secondary_education.csv") %>% 
    pivot_longer(cols      = "1960":"2021", 
                 names_to  = "year", 
                 values_to = "female_ed") %>%
    mutate(year = parse_number(year))

summary(female_ed)

# UN subregion---- 

world <- ne_countries(scale = "medium", returnclass = "sf")

sf::sf_use_s2(FALSE)

un_subregion <- world %>% group_by(subregion) %>% summarise(n = n())

# 5 countries-ish without iso codes (only 2 "sovereign countries", Northern Cyprus & Kosovo)

### merge datasets on each study's `Year started` & numeric iso code:----

# urban percent----
# match on iso code
names(urban_percent)

urban_percent <- mutate(urban_percent, 
                        iso_code = countrycode(sourcevar   = `Country Code`, 
                                               origin      = 'wb',
                                               destination = 'iso3n')) 
urban_percent$iso_code

sum(is.na(unique(urban_percent$iso_code))) # 1 NA
# unmatched values are all summary values (regional or income level), except Kosovo (XKX)
# this is ok bc no RESPICAR studies on Kosovo 

urban_percent <- drop_na(urban_percent, "iso_code")


respicar <- respicar %>% 
    group_by(`ISO 3166-1`,`Country`) %>%
    arrange(.by_group = TRUE)

urban_percent %<>% fill_socio

respicar_socio <- merge(
    x     = respicar, 
    y     = urban_percent, 
    by.x  = c("ISO 3166-1", "Year started"),
    by.y  = c("iso_code", "year"),
    all.x = TRUE) %>% 
    select(!c("Country Name", "Country Code", "Indicator Name", "Indicator Code"))

names(respicar_socio)
sum(is.na(respicar_socio$urban_percent))
# 0/439 missing values for urban percent

# filter(respicar_socio, is.na(urban_percent))
# na_urban_percent <- tibble(filter(respicar_socio, is.na(urban_percent)))
# na_urban_percent %>% distinct(Country)

# GDP---- 
names(gdp_data)

gdp_data <- mutate(gdp_data, 
                   iso_code = countrycode(sourcevar   = `iso2c`, 
                                          origin      = 'iso2c',
                                          destination = 'iso3n'),
                   iso3c    = ifelse(is.na(iso3c), 
                                     countrycode(sourcevar   = iso2c,
                                          origin      = 'iso2c',
                                          destination = 'iso3c'),
                                     iso3c))



gdp_data %>%
    ungroup %>%
    filter(is.na(iso_code)) %>%
    distinct(iso2c, iso3c, country) %>%
    arrange(iso3c)

gdp_data <- gdp_data %>% drop_na(iso_code)
sum(is.na(gdp_data$iso_code)) # 0 NA


gdp_data <- gdp_data %>% 
    rename("gdp_usd"="NY.GDP.PCAP.CD") %>% 
    select("year", "iso_code", "gdp_usd")

# https://www.statista.com/statistics/727589/gross-domestic-product-gdp-in-taiwan/
# population WPP2019
data(pop)

pop_extra <- filter(pop, country_code %in% c(158)) %>%
    rename(iso_code = country_code) %>%
    select(-name) %>%
    gather(year, pop, -iso_code) %>%
    mutate(year = parse_integer(year)) %>%
    split(.$iso_code) %>%
    map(~{list(data  = .x,
          model = approx(x = .x$year, y = .x$pop, xout = full_seq(.x$year, 1)))}) %>%
    map("model") %>%
    map_df(as.data.frame, .id = 'iso_code') %>%
    rename(year = x, pop = y) %>%
    mutate(iso_code = parse_integer(iso_code))


gdp_data_extra <-
    list(`158` = data.frame(year    = 1987:2021,
                            gdp_usd = c(105.04,
                                        126.47,
                                        152.7,
                                        116.62,
                                        187.14,
                                        222.91,
                                        236.34,
                                        256.25,
                                        279.06,
                                        292.49,
                                        303.28,
                                        279.96,
                                        303.83,
                                        330.68,
                                        299.28,
                                        307.44,
                                        317.38,
                                        346.92,
                                        374.06,
                                        386.45,
                                        406.91,
                                        415.9,
                                        390.83,
                                        444.28,
                                        483.97,
                                        495.61,
                                        512.94,
                                        535.33,
                                        534.52,
                                        543.08,
                                        590.73,
                                        609.2,
                                        611.4,
                                        669.25,
                                        789.51)*1e9
                            
    )) %>%
    bind_rows(.id = "iso_code") %>%
    mutate(iso_code = parse_integer(iso_code))

gdp_per_cap_extra <- left_join(gdp_data_extra, pop_extra) %>%
    mutate(gdp_usd = gdp_usd/pop/1e3) %>%
    select(-pop)

gdp_data %<>% bind_rows(gdp_per_cap_extra)

gdp_data %<>% fill_socio

respicar_socio <- merge(x=respicar_socio, 
                        y=gdp_data, 
                        by.x= c("ISO 3166-1", "Year started"),
                        by.y= c("iso_code", "year"),
                        all.x = TRUE)
names(respicar_socio)
sum(is.na(respicar_socio$gdp_usd))
# 12/439 missing values for gdp (3.1%)
na_gdp <- tibble(filter(respicar_socio, is.na(gdp_usd)))
na_gdp %>% distinct(Country)
# Gini---- 
names(gini)
gini <- mutate(gini, 
               iso_code = countrycode(sourcevar   = `iso2c`, 
                                      origin      = 'iso2c',
                                      destination = 'iso3n'))

# gini <- gini %>% drop_na(iso_code)
# sum(is.na(gini$iso_code)) # 0 NA

gini <- gini %>% 
    rename("gini" = "SI.POV.GINI") %>% 
    select("year", "iso_code", "gini")

gini %<>% fill_socio

respicar_socio <- merge(x=respicar_socio, 
                        y=gini, 
                        by.x= c("ISO 3166-1", "Year started"),
                        by.y= c("iso_code", "year"),
                        all.x = TRUE)
names(respicar_socio)
sum(is.na(respicar_socio$gini))
# 17/439 missing values for gini
na_gini <- tibble(filter(respicar_socio, is.na(gini)))
na_gini %>% 
    group_by(Country) %>%
    nest %>%
    mutate(R = map(.x = data, ~range(.x$`Year started`) %>% 
                       setNames(., c("Min", "Max")))) %>%
    unnest_wider(R)

# Household size-----

# fix date to be year only 
names(hh_data)

class(hh_data$`Reference date (dd/mm/yyyy)`)

hh_data <- hh_data %>%
    # group_by(`Country or area`) %>%
    mutate(year = as.Date(`Reference date (dd/mm/yyyy)`,
                          format= "%d/%m/%y"))
class(hh_data$year)

hh_data <- mutate(hh_data, year = year(`year`))
class(hh_data$year)

# average for when there's multiple data sources for the same country-year

hh_data <- mutate(hh_data, 
                  average_hh = parse_number(`Average household size (number of members)`, 
                                            na = c("", "NA", "..")))
hh_data <- hh_data %>% 
    group_by(`Country or area`, year,  iso_code = `ISO Code`) %>%
    summarise(mean_hh = mean(average_hh, na.rm = TRUE), .groups = "drop") 

sum(is.na(hh_data$mean_hh)) #23 NAs in mean_hh

sum(is.na(hh_data$iso_code)) #0 NAs in iso_code

hh_data <- select(hh_data, year, iso_code, mean_hh)

hh_data_extra <-
    list(# Swedish census data
        `752` = read_csv('data/BE0101C¤_20220729-160046.csv', skip = 1) %>%
            mutate(mean_hh = `00 Sweden Number of persons`/`00 Sweden Number of households`),
        
        # Danish census data 
        `208` = read_csv("data/denmark_pop_thousands.csv", skip = 3,
                         col_names = c("year", "pop")) %>%
            full_join(read_csv("data/denmark_households.csv",
                               skip = 2, col_names = c("year", "hh"))) %>%
            arrange(year) %>%
            group_by(year) %>%
            transmute(mean_hh = pop/hh * 1000),
        
        # CEIC data
        `352` = data.frame(year     = seq(2004, 2016),
                           mean_hh  = c(2.6, 2.5, 2.5, rep(2.4, 6), NA, 2.7, 2.7, 2.9)),
        
        # Statista
        `158` = data.frame(year     = seq(1990, 2020),
                           mean_hh  = c(4.19, 4.16, 4.1, 4.1, 4.02, 3.94, 3.92, 3.84, 3.77,
                                        3.63, 3.62, 3.58, 3.65, 3.53, 3.5, 3.42, 3.41,
                                        3.38, 3.35, 3.34, 3.25, 3.29, 3.23, 3.21, 3.15,
                                        3.1, 3.07, 3.07, 3.05, 3.02, 2.92)),
        
        # CEIC
        `144` = data.frame(year     = c(1981, 1986, 1991, 1996, 2001, 2019,
                                        2016, 2013, 2010, 2007, 2004 ),
                           mean_hh  = c(4.9,  5.1,  4.9,  4.5,  4.2,  3.7,
                                        3.8,  3.9,  4.0,  4.1, 4.1)),
        
        # Survey and census https://journals.sagepub.com/doi/10.1177/2158244020914556
        `682` = data.frame(year = c(1992, 2004, 2010, 2000, 2007, 2016),
                           mean_hh = c(6.6, 6, 6.3, 6.8, 6.0, 5.9)),
        
        # http://www.stats.gov.cn/tjsj/ndsj/2021/indexeh.htm table 2-7
        `156` = data.frame(year = c(1982, 1990, 2000, 2010, 2020),
                           mean_hh = c(4.41, 3.96, 3.44, 3.10, 2.62)),
        
        # New Caledonia
        # 1989 World Development Indicators 2008 ISBN 9780821373873
        # 1990 Statistical Handbook on the World's Children (2002) ISBN 9781573563901
        # 2004 UN POPULATION DIVISION file
        # 2014 https://pacificsecurity.net/wp-content/uploads/2021/02/Pacific_Islands_2020_Populations_poster.pdf 
        # 2021 https://www.prb.org/international/indicator/hh-size-av/table/
        `540` = data.frame(year   = c(1989, 
                                      1990, 
                                      2004, 
                                      2014,
                                      2021),
                           mean_hh = c(4.1,
                                       4.0, 
                                       round(weighted.mean(x = c(1, 2.5, 4.5, 6.5),
                                                           w = c(17.45, 38.67, 28.23, 15.65)),digits = 1),
                                       3.1,
                                       3.5))
    ) %>%
    map(~select(.x, year, mean_hh)) %>%
    bind_rows(.id = "iso_code") %>%
    mutate(iso_code = parse_integer(iso_code)) %>%
    arrange(iso_code, year)

hh_data %<>% anti_join(distinct(hh_data_extra, iso_code))

hh_data %<>% bind_rows(hh_data_extra)

hh_data %<>% fill_socio

respicar_socio <- merge(x=respicar_socio, 
                        y=hh_data, 
                        by.x= c("ISO 3166-1", "Year started"),
                        by.y= c("iso_code", "year"),
                        all.x = TRUE)
names(respicar_socio)

sum(is.na(respicar_socio$mean_hh))

# 27/439 are missing (6.2%)
na_hh <- tibble(filter(respicar_socio, is.na(mean_hh)))
na_hh %>% distinct(Country)

# Female education----
names(female_ed)
female_ed <- mutate(female_ed, 
                    iso_code = countrycode(sourcevar   = `Country Code`, 
                                           origin      = 'iso3c',
                                           destination = 'iso3n'))
female_ed <- female_ed %>% drop_na(iso_code)

female_ed <- female_ed %>% 
    select("year", "iso_code", "female_ed")
sum(is.na(female_ed$female_ed))
# 7460/13330 entries are missing 

female_ed %<>% fill_socio

respicar_socio <- merge(x=respicar_socio, 
                        y=female_ed, 
                        by.x= c("ISO 3166-1", "Year started"),
                        by.y= c("iso_code", "year"),
                        all.x = TRUE)
names(respicar_socio)
sum(is.na(respicar_socio$female_ed))
# 8/439 missing values for female ed (1.8%) 
na_female_ed <- tibble(filter(respicar_socio, is.na(female_ed)))
na_female_ed %>% distinct(Country)

# UN subregion------

respicar_socio <- mutate(respicar_socio,
                         subregion = countrycode(sourcevar   = `ISO 3166-1`,
                                                 origin      = 'iso3n', 
                                                 destination = 'un.regionsub.name'),
                         subregion = ifelse(test = `ISO 3166-1` == 158, # Taiwan
                                            yes  = "Eastern Asia",
                                            no   = subregion))


