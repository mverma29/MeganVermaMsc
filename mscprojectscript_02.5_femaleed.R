# load female education data (proxy for maternal)----
# UNESCO data, from world bank site 
# secondary education levels!

female_ed <- read_csv("data/female_secondary_education.csv") %>% 
  pivot_longer(cols      = "1960":"2021", 
               names_to  = "year", 
               values_to = "female_ed") %>%
  mutate(year = parse_number(year))

female_ed %<>% bind_rows(female_ed_twn)

summary(female_ed)

# merge Female education data into respicar----
names(female_ed)
female_ed <- mutate(female_ed, 
                    iso_code = countrycode(sourcevar   = `Country Code`, 
                                           origin      = 'iso3c',
                                           destination = 'iso3n'))

female_ed <- female_ed %>% drop_na(iso_code)

female_ed <- female_ed %>% 
  select("year", "iso_code", "female_ed")
sum(is.na(female_ed$female_ed))
# 620/13330 entries are missing 

female_ed_twn <- readODS::read_ods('data/e104-5.ods', sheet = 1, range = 'A3:D21') %>%
  mutate(year = gsub(x = `School Year`, pattern = "SY ", replacement = ""),
         year = parse_integer(sub("[0-9]{4}-", "", year))) %>%
  mutate(iso_code = 158L) %>%
  select(iso_code, year = year, female_ed = Female)

female_ed %<>% bind_rows(female_ed_twn)

female_ed %<>% fill_socio

respicar_socio <- merge(x=respicar_socio, 
                        y=female_ed, 
                        by.x= c("ISO 3166-1", "Year started"),
                        by.y= c("iso_code", "year"),
                        all.x = TRUE)
names(respicar_socio)
sum(is.na(respicar_socio$female_ed))
# 9/443 missing values for female ed (2.0%) 
na_female_ed <- tibble(filter(respicar_socio, is.na(female_ed)))

check_socio_na(na_female_ed)


