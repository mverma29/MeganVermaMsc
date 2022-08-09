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
library(lme4)


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


merge_socio <- function(x, y){
    argnames <- as.character(as.list(match.call())[-1])

        z <- merge(
        x     = x, 
        y     = y, 
        by.x  = c("ISO 3166-1", "Year started"),
        by.y  = c("iso_code", "year"),
        all.x = TRUE) %>% 
        select(-any_of(c("Country Name", "Country Code",
                         "Indicator Name", "Indicator Code")))
    
    n_check <- list(x = x,
                    z = z) %>%
        map(ungroup) %>%
        map_df(~count(.x, id), .id = 'source') %>%
        spread(source, n) %>%
        filter(x != z) %>%
        select(id) 
    
    if(nrow(n_check) > 0L){
        stop(sprintf("Mismatch in rows coming in (%i) and going out (%i). \nCheck covariates in object \'%s\' for id %s.",
                     nrow(x), nrow(z), argnames[2], paste(n_check$id, collapse = ", ")))
    } else {
        z
    }
    
}
