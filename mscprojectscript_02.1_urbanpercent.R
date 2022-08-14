# Load Percent urbanicity data----
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

# merge into respicar dataset----
# match on iso code
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
    arrange(.by_group = TRUE) %>%
    ungroup

urban_percent %<>% fill_socio

respicar_socio <- merge_socio(x = respicar, 
                              y = urban_percent)

names(respicar_socio)

sum(is.na(respicar_socio$urban_percent))

na_urban_percent <- tibble(filter(respicar_socio, is.na(urban_percent)))

check_socio_na(na_urban_percent)

respicar_socio %<>% mutate(urban_percent= urban_percent/10)
summary(respicar_socio$urban_percent)

