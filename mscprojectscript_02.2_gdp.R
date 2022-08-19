# load GDP data ----
WDIsearch("gdp per capita") %>%
    as_tibble %>%
    filter(grepl(pattern = "^GDP per capita", x = name))
# GDP per capita, current USD 

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

WDIsearch("gini") %>%
    as_tibble %>%
    filter(grepl(pattern = "^Gini index", x = name))

# merge GDP into respicar---- 
names(gdp_data)

gdp_data <- mutate(gdp_data, 
                   iso_code = countrycode(sourcevar   = iso2c, 
                                          origin      = 'iso2c',
                                          destination = 'iso3n'),
                   iso3c    = ifelse(is.na(iso3c), 
                                     countrycode(sourcevar   = iso2c,
                                                 origin      = 'iso2c',
                                                 destination = 'iso3c'),
                                     iso3c))

# 
# 
# gdp_data %>%
#     ungroup %>%
#     filter(is.na(iso_code)) %>%
#     distinct(iso2c, iso3c, country) %>%
#     arrange(iso3c)

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
               model = approx(x    = .x$year, 
                              y    = .x$pop,
                              xout = full_seq(.x$year, 1)))}) %>%
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


respicar_unfilled <- merge_socio(x = respicar_unfilled, 
                              y = gdp_data)

# fill in most recent year for covariate values that don't match

gdp_data %<>% fill_socio
respicar_socio <- merge_socio(x = respicar_socio, 
                              y = gdp_data)

na_gdp <- tibble(filter(respicar_socio, is.na(gdp_usd)))
check_socio_na(na_gdp)

