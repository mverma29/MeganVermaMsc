# Megan Verma 
# 7/25/2022 

### load data ----

#RESPICAR dataset (summary)
respicar <- read_csv("data/appendix_summary.csv")


### sociodemographic variables:

# Percent urbanicity----
# from un populations divison, accessed via world bank
urban_percent <- read_csv("data/urban_pop_percent.csv")

# needs reshaping and ensure that the year has an appropriate class
urban_percent <- urban_percent %>% 
    pivot_longer(cols = "1960":"2021", 
                 names_to = "Year", values_to = "urban_percent")

urban_percent <- mutate(urban_percent, Year = parse_integer(Year))

# GDP----
WDIsearch("gdp per capita")
# #5 seems right-- GDP per capita, current USD 

gdp_data <- WDI(country = "all",
  indicator = "NY.GDP.PCAP.CD",
  start = 1990,
  end = 2016,
  extra = TRUE,
  cache = NULL,
  latest = NULL,
  language = "en")

class(gdp_data$year) #integer

# Gini----
gini<- WDI(country="all", 
           indicator= "SI.POV.GINI", 
           start = 1990,
           end = 2016, 
           extra = TRUE,
           language = "en")

class(gini$year) #integer


# Household size----
# Download the UN Population Division's data on [Household size and composition]
# (https://www.un.org/development/desa/pddata/household-size-and-composition)

hh_data <- readxl::read_xlsx("data/un_hh.xlsx", 
                             sheet= 4, 
                             range= "A5:E819", 
                             col_names = TRUE)

hh_data

# will need to find HH data from each study's year START date-- might be missing 


# female education (proxy for maternal)----
# UNESCO data, from world bank site 
# secondary education levels!

female_ed <- read_csv("data/female_secondary_education.csv")
# needs reshaping and ensure that the year has an appropriate class

female_ed <- female_ed %>% 
    pivot_longer(cols = "1960":"2021", 
                 names_to = "Year", values_to = "female_ed")

female_ed <- mutate(female_ed, Year = parse_number(Year))

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
# unmatched values are all summary values (regional or income level), except Kosovo (XKX)
# this is ok bc no RESPICAR studies on Kosovo 

urban_percent <- drop_na(urban_percent, "iso_code")
urban_percent$iso_code

respicar <- respicar %>% 
    group_by(`ISO 3166-1`,`Country`) %>%
    arrange(.by_group = TRUE)

respicar_socio <- merge(x=respicar, 
                        y=urban_percent, 
                        by.x= c("ISO 3166-1", "Year started"),
                        by.y= c("iso_code", "Year"),
                        all.x = TRUE) %>% 
    select(!c("Country Name", "Country Code", "Indicator Name", "Indicator Code"))
names(respicar_socio)

## GDP---- 
names(gdp_data)
gdp_data <- mutate(gdp_data, 
                        iso_code = countrycode(sourcevar   = `iso3c`, 
                                               origin      = 'iso3c',
                                               destination = 'iso3n'))
gdp_data <- gdp_data %>% drop_na(iso_code)

gdp_data <- gdp_data %>% 
    rename("gdp_usd"="NY.GDP.PCAP.CD") %>% 
    select("year", "iso_code", "gdp_usd")

respicar_socio <- merge(x=respicar_socio, 
                        y=gdp_data, 
                        by.x= c("ISO 3166-1", "Year started"),
                        by.y= c("iso_code", "year"),
                        all.x = TRUE)
names(respicar_socio)
## Gini---- 
names(gini)
gini <- mutate(gini, 
                   iso_code = countrycode(sourcevar   = `iso3c`, 
                                          origin      = 'iso3c',
                                          destination = 'iso3n'))
gini <- gini %>% drop_na(iso_code)

gini <- gini %>% 
    rename("gini"="SI.POV.GINI") %>% 
    select("year", "iso_code", "gini")

respicar_socio <- merge(x=respicar_socio, 
                        y=gini, 
                        by.x= c("ISO 3166-1", "Year started"),
                        by.y= c("iso_code", "year"),
                        all.x = TRUE)
names(respicar_socio)
## Household size
names(hh_data)
class(hh_data$`Reference date (dd/mm/yyyy)`)

hh_data <- hh_data %>% 
    mutate(refyear = as.Date(`Reference date (dd/mm/yyyy)`, 
                             format= "%d/%m/%y"))

hh_data$refyear<- hh_data %>% year(`refyear`)

hh_data <- select(!c("Data source category"))

gini <- gini %>% drop_na(iso_code)

gini <- gini %>% 
    rename("gini"="SI.POV.GINI") %>% 
    select("year", "iso_code", "gini")

respicar_socio <- merge(x=respicar_socio, 
                        y=gini, 
                        by.x= c("ISO 3166-1", "Year started"),
                        by.y= c("iso_code", "year"),
                        all.x = TRUE)
names(respicar_socio)


