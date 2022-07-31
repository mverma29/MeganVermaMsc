# Megan Verma 
# 7/25/2022 

### load data ----

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

# GDP----
WDIsearch("gdp per capita")
# #5 seems right-- GDP per capita, current USD 

gdp_data <- WDI(
    country   = "all",
    indicator = "NY.GDP.PCAP.CD",
    start     = 1990,
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
    start     = 1990,
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
# already in world_with_cases as "subregion"


### merge datasets on each study's `Year started` & numeric iso code:----

## urban percent----
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
# 8/439 missing values for urban percent (1.8%)

filter(respicar_socio, is.na(urban_percent))

## GDP---- 
names(gdp_data)

gdp_data <- mutate(gdp_data, 
                   iso_code = countrycode(sourcevar   = `iso3c`, 
                                          origin      = 'iso3c',
                                          destination = 'iso3n'))
gdp_data <- gdp_data %>% drop_na(iso_code)
sum(is.na(gdp_data$iso_code)) # 0 NA


gdp_data <- gdp_data %>% 
    rename("gdp_usd"="NY.GDP.PCAP.CD") %>% 
    select("year", "iso_code", "gdp_usd")

gdp_data %<>% fill_socio

respicar_socio <- merge(x=respicar_socio, 
                        y=gdp_data, 
                        by.x= c("ISO 3166-1", "Year started"),
                        by.y= c("iso_code", "year"),
                        all.x = TRUE)
names(respicar_socio)
sum(is.na(respicar_socio$gdp_usd))
# 0/439 missing values for gdp (0%)

## Gini---- 
names(gini)
gini <- mutate(gini, 
               iso_code = countrycode(sourcevar   = `iso3c`, 
                                      origin      = 'iso3c',
                                      destination = 'iso3n'))
gini <- gini %>% drop_na(iso_code)
sum(is.na(gini$iso_code)) # 0 NA

gini <- gini %>% 
    rename("gini"="SI.POV.GINI") %>% 
    select("year", "iso_code", "gini")

gini %<>% fill_socio

respicar_socio <- merge(x=respicar_socio, 
                        y=gini, 
                        by.x= c("ISO 3166-1", "Year started"),
                        by.y= c("iso_code", "year"),
                        all.x = TRUE)
names(respicar_socio)
sum(is.na(respicar_socio$gini))
# 24/439 missing values for gini (5.5%)

## Household size-----

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
# 40 parsing failures? 

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
                           mean_hh = c(6.6, 6, 6.3, 6.8, 6.0, 5.9))
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
# 42 new entries?? unsure of how to check what's added, only know how to check what's dropped 
names(respicar_socio)

sum(is.na(respicar_socio$mean_hh))
# 2 are still missing



## Female education----
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

respicar_socio <- merge(x=respicar_socio, 
                        y=female_ed, 
                        by.x= c("ISO 3166-1", "Year started"),
                        by.y= c("iso_code", "year"),
                        all.x = TRUE)
names(respicar_socio)
sum(is.na(respicar_socio$female_ed))
# 130/439 missing values for female ed (29.6%) ********
