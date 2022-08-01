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
    start     = 1989,
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
    start     = 1989,
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
un_subregion <- world %>% select(geometry, subregion, iso_n3)

na_world_iso <- tibble(filter(world, is.na(iso_n3)))
na_world_iso %>% distinct(sovereignt)
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
# 12/439 missing values for gdp (3.1%)
na_gdp <- tibble(filter(respicar_socio, is.na(gdp_usd)))
na_gdp %>% distinct(Country)
        # Gini---- 
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
# 22/439 missing values for gini
na_gini <- tibble(filter(respicar_socio, is.na(gini)))
na_gini %>% distinct(Country)

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

respicar_socio <- merge (x=respicar_socio, y=un_subregion, 
                              by.x = "ISO 3166-1", 
                              by.y = "iso_n3", 
                              all.x=TRUE) 
names(respicar_socio)
sum(is.na(respicar_socio$subregion))
#0/439 missing values for subregion 
na_subregion <- tibble(filter(respicar_socio, is.na(subregion)))
na_subregion %>% distinct(Country)
