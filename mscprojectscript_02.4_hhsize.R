# load Household size data----
# Download the UN Population Division's data on [Household size and composition]
# (https://www.un.org/development/desa/pddata/household-size-and-composition)

hh_data <- readxl::read_xlsx(
  path      = "data/un_hh.xlsx", 
  sheet     = 4, 
  range     = "A5:E819", 
  col_names = TRUE)

# names(hh_data)
# sum(is.na(hh_data$`Average household size (number of members)`)) #0 NAs

# merge Household size data into respicar-----

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
      mutate(mean_hh = `00 Sweden Number of persons`/
               `00 Sweden Number of households`),
    
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
                                                       w = c(17.45, 38.67, 28.23, 15.65)),
                                         digits = 1),
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

# 0/439 are missing
na_hh <- tibble(filter(respicar_socio, is.na(mean_hh)))

check_socio_na(na_hh)

