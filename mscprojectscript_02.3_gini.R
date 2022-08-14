# load Gini data----
gini <- WDI(
    country   = "all", 
    indicator = "SI.POV.GINI", 
    start     = 1987,
    end       = 2016, 
    extra     = TRUE,
    language  = "en")

class(gini$year) #integer


# merge Gini into respicar---- 
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



gini_data_extra <-
    list(
        # https://www.statista.com/statistics/922574/taiwan-gini-index/(
        `158` = data.frame(year    = 1987:2020,
                           gini = c(29.9,
                                    30.3,
                                    30.3,
                                    31.2,
                                    30.8,
                                    31.2,
                                    31.5,
                                    31.8,
                                    31.7,
                                    31.7,
                                    32,
                                    32.4,
                                    32.5,
                                    32.6,
                                    35,
                                    34.5,
                                    34.3,
                                    33.8,
                                    34,
                                    33.9,
                                    34,
                                    34.1,
                                    34.5,
                                    34.2,
                                    34.2,
                                    33.8,
                                    33.6,
                                    33.6,
                                    33.8,
                                    33.6,
                                    33.7,
                                    33.8,
                                    33.9,
                                    34)
        ),
        
        # https://www.legco.gov.hk/yr04-05/english/sec/library/0405fs07e.pdf
        # https://www.hkeconomy.gov.hk/en/pdf/gini_comparison.pdf
        `344` = data.frame(year = c(1981, 1986, 1991, 1996, 2001, 2006),
                           gini = c(45.1, 45.3, 47.6, 51.8, 52.5, 53.3)),
        
        # https://www.spc.int/DigitalLibrary/Doc/SDD/Meetings/2010/3rd_HOPS_2013/CONF_HOPS_2010_IP_3_Measuring_Poverty.doc
        `540` = data.frame(year = 2008,
                           gini = 45.0),
        
        # 1998 https://tradingeconomics.com/singapore/gini-index-wb-data.html
        # 2014 https://en.wikipedia.org/wiki/List_of_countries_by_income_equality CIA
        # 2019 https://www.worldeconomics.com/Inequality/Gini-Coefficient/Singapore.aspx
        # 2000, 2007, 2008, 2009 https://www.jstor.org/stable/43184870 
        `702` = data.frame(year = c(1998, 2000, 2001, 2002, 
                                    2003, 2004, 2005, 2006,
                                    2007, 2008, 2009, 2014, 2019),
                           gini = c(42.48, 44.4, 45.6, 45.7,
                                    46.0,  46.4, 47.0, 47.6,
                                    48.9, 48.1, 47.8, 46.4, 65.5)),
        
        # Saudi Arabia
        # 2013 https://www.cia.gov/the-world-factbook/field/gini-index-coefficient-distribution-of-family-income/country-comparison
        # 2016 https://ssrn.com/abstract=3465663
        `682` = data.frame(year = c(2013, 2016),
                           gini = c(45.9, 39.97)),
        
        # Cuba
        # 1986-1999 https://www.cepal.org/en/publications/37484-cepal-review-no86
        # 2002, 2013 http://www.espaciolaical.org/contens/38/101104.pdf
        # 2017 Ricardo Torres, “Cuba’s Economy: Reforms and Delays, 2014-2018,” in Cuba at the Crossroads, eds., Philip Brenner, John M. Kirk and William M. LeoGrande (Lanham, MD: Rowman and Littlefield, 2020), 56.
        `192` = data.frame(year = c(1986, 1989, 1995, 1996, 1998, 1999, 2002, 2013, 2017),
                           gini = c(22.0, 25.0, 55.0, 39.0, 38.0, 40.7, 38.0, 40.0, 45.0))
        
        
    ) %>%
    bind_rows(.id = "iso_code") %>%
    mutate(iso_code = parse_integer(iso_code))

gini %<>% anti_join(distinct(gini_data_extra, iso_code))

gini %<>% bind_rows(gini_data_extra)

gini %<>% fill_socio

respicar_socio <- merge_socio(x = respicar_socio, 
                              y = gini)
names(respicar_socio)
sum(is.na(respicar_socio$gini))
# 0/439 missing values for gini
na_gini <- tibble(filter(respicar_socio, is.na(gini)))

check_socio_na(na_gini)

respicar_socio %<>% mutate(gini = gini/10)
summary(respicar_socio$gini)