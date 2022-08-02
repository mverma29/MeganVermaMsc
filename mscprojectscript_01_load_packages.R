# Megan Verma 
# 7/25/2022 


#setup: load packages

library(mgcv)
library(sf)
library(broom)
library(rnaturalearth)
library(rnaturalearthdata)
library(countrycode)
library(epiDisplay)
library(conflicted)
conflict_prefer('select', 'dplyr')
conflict_prefer('filter', 'dplyr')
conflict_prefer('alpha', 'ggplot2')
conflict_prefer('multinom', 'mgcv')
conflict_prefer('year', 'lubridate')
library(WDI)
library(purrr)
library(lubridate)
library(magrittr)
library(tidyverse)
library(wpp2019)
library(readODS)



fill_socio <- function(x){
    complete(x, iso_code, year) %>%
        arrange(iso_code, year) %>%
        group_by(iso_code) %>%
        fill(!!!vars(-iso_code, -year), .direction = "downup")     
}


check_socio_na <- function(x){
    group_by(x, Country, `ISO 3166-1`) %>%
        nest %>%
        mutate(R = map(.x = data, ~range(.x$`Year started`) %>% 
                           setNames(., c("Min", "Max")))) %>%
        unnest_wider(R) %>%
        mutate(n = map_dbl(data, nrow))
}
