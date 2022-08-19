# load UN subregion data---- 

world <- ne_countries(scale = "medium", returnclass = "sf")

sf::sf_use_s2(FALSE)

un_subregion <- world %>% group_by(subregion) %>% summarise(n = n())

# 5 countries-ish without iso codes (only 2 "sovereign countries", Northern Cyprus & Kosovo)

# merge UN subregion into respicar------

respicar_socio <- mutate(respicar_socio,
                         subregion = countrycode(sourcevar   = `ISO 3166-1`,
                                                 origin      = 'iso3n', 
                                                 destination = 'un.regionsub.name'),
                         subregion = ifelse(test = `ISO 3166-1` == 158, # Taiwan
                                            yes  = "Eastern Asia",
                                            no   = subregion))

respicar_unfilled <- mutate(respicar_unfilled,
                         subregion = countrycode(sourcevar   = `ISO 3166-1`,
                                                 origin      = 'iso3n', 
                                                 destination = 'un.regionsub.name'),
                         subregion = ifelse(test = `ISO 3166-1` == 158, # Taiwan
                                            yes  = "Eastern Asia",
                                            no   = subregion))

na_subregion <- tibble(filter(respicar_socio, is.na(subregion)))
check_socio_na(na_subregion)

