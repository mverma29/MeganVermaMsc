library(tidyverse)

owid <- read_csv('data/owid-covid-data.csv') # replace NULL with file name


owid_latest <- filter(owid, date == max(date)) %>%
    filter(!grepl(pattern = '^OWID', x = iso_code)) 

library(rnaturalearth)
library(rnaturalearthdata)
world <- ne_countries(scale = "medium", returnclass = "sf")


world_with_cases <- select(owid_latest,
                           iso_code,
                           total_cases,
                           total_cases_per_million,
                           population,
                           gdp_per_capita) %>%
    inner_join(world, by = c("iso_code" = "iso_a3"))

world_with_cases %>%
    mutate(total_cases_per_million_cat = cut(total_cases_per_million,
                                             breaks = c(0, 10^(0:6)))) %>%
    ggplot(data = .) +
    geom_sf(aes(fill = total_cases_per_million_cat,
                geometry = geometry)) +
    scale_fill_brewer(palette = "Reds")

world_with_cases %>%
    ggplot(data =., aes(x = gdp_per_capita, y = total_cases_per_million)) +
    geom_point(aes(color = continent)) +
    scale_x_log10() +
    scale_y_log10() +
    geom_smooth(method = 'lm') 

case_glm <- glm(data    = world_with_cases,
                formula = cbind(total_cases, population - total_cases) ~ log10(gdp_per_capita),
                family  = "binomial")

confint(case_glm)


library(WDI)

gini <- WDI(country = "all", 
            indicator = "SI.POV.GINI", 
            latest = 1)

world_with_gini <- select(gini, iso_a2 = iso2c, gini = SI.POV.GINI, year.gini = year) %>%
    right_join(world_with_cases) 

ggplot(data = world_with_gini,
       aes(x = gini, y = total_cases_per_million)) +
    geom_point(aes(color = continent)) +
    scale_y_log10() +
    geom_smooth(method = 'lm') 

ggplot(data = world_with_gini,
       aes(x = log10(gdp_per_capita), y = gini)) +
    geom_point(aes(color = continent)) +
    geom_smooth(method = 'lm') 



case_gini <- glm(data    = world_with_gini,
                 formula = cbind(total_cases, population - total_cases) ~ 
                     log10(gdp_per_capita) + gini,
                 family  = "binomial")

confint(case_gini)




households <- readxl::read_xlsx(
    path = "data/undesa_pd_2019_houseshold_size_and_composition_dataset.xlsx",
    range = "A5:E819", sheet = 4)

households %<>% mutate(av_hh = parse_number(`Average household size (number of members)`)) %>%
    filter(!is.na(av_hh))  %>%
    group_by(`ISO Code`) %>%
    rename(date = "Reference date (dd/mm/yyyy)") %>%
    slice_max(date) %>%
    ungroup %>%
    mutate(iso_code = countrycode::countrycode(`ISO Code`, 
                                               origin      = 'iso3n',
                                               destination = 'iso3c')) %>%
    group_by(iso_code, date) %>%
    summarise(av_hh = round(mean(av_hh),2))

library(magrittr)
world_with_gini %<>% left_join(households)

world_with_gini %<>% mutate(log10gpd = log10(gdp_per_capita))

case_hh <- glm(data    = world_with_gini,
               formula = cbind(total_cases, population - total_cases) ~
                   log10gpd + gini + av_hh,
               family  = "binomial")

cor(world_with_gini[, c('gini', 'av_hh', 'log10gpd')],
    use = 'pair', method = 'spearman')

summary(case_hh)
confint(case_hh)


world_with_gini %>%
    ggplot(data=., aes(geometry = geometry)) +
    geom_sf(aes(fill = av_hh)) +
    scale_fill_gradient(low = 'white', high = 'orange')


world_with_gini %>%
    mutate(subregion = countrycode::countrycode(sourcevar = iso_n3,
                                                origin    = 'iso3n',
                                                destination = 'un.regionsub.name')) %>%
    group_by(subregion) %>%
    summarise(av_hh = weighted.mean(x = av_hh,
                                    w = population, 
                                    na.rm=T))

data_for_model <- world_with_gini %>% 
    select(total_cases,
           population,
           gdp_per_capita,
           gini,
           av_hh,
           iso_code) %>%
    na.omit %>%
    mutate(subregion = countrycode::countrycode(sourcevar = iso_code,
                                                origin    = 'iso3c',
                                                destination = 'un.regionsub.name')) 

glm_av_hh <-  glm(data    = data_for_model,
                  formula = cbind(total_cases, population - total_cases) ~ 
                      log10(gdp_per_capita) + gini + av_hh,
                  family  = "binomial") 

glm_no_hh <-  glm(data    = data_for_model,
                  formula = cbind(total_cases, population - total_cases) ~ 
                      log10(gdp_per_capita) + gini,
                  family  = "binomial") 

glm_sr_hh <-  glm(data    = data_for_model,
                  formula = cbind(total_cases, population - total_cases) ~ 
                      log10(gdp_per_capita) + gini + av_hh + subregion, 
                  family  = "binomial") 

anova(glm_no_hh, glm_av_hh, glm_sr_hh)

# megan again

world.hh <- dplyr::select(world, geometry, iso_code_l = iso_a3, pop_est)

un_hh_world <- merge(x=un_hh_iso, y = world.hh, by = 'iso_code_l', all.y = T) # i don't think all.x is what we want. i'd suggest making a new data frame containing geom, pop, isocode.

ggplot(data = un_hh_world) +
    geom_sf(aes(geometry = geometry, fill = mean_hh )) +
    scale_fill_continuous(low = 'white', high = 'red')



un_hh_world_summ <- un_hh_world %>% 
    group_by(`un_subregion`) %>% 
    summarise (hh_pop_reg= weighted.mean(x = mean_hh, w = pop_est, na.rm = T))

un_hh_world_summ ## whyyyyyy is this not working 