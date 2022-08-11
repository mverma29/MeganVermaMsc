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
library(WDI)
library(purrr)
library(lubridate)
library(magrittr)
library(tidyverse)
library(wpp2019)
library(readODS)
library(lme4)
library(sjstats)
library(jtools)
library(cAIC4)
library(conflicted)
conflict_prefer('select', 'dplyr')
conflict_prefer('filter', 'dplyr')
conflict_prefer('alpha', 'ggplot2')
conflict_prefer('multinom', 'mgcv')
conflict_prefer('year', 'lubridate')
conflict_prefer("summ", "jtools")

if (!require(GLMMadaptive)){
    devtools::install_github("drizopoulos/GLMMadaptive")
    library(GLMMadaptive)
}


fill_socio <- function(x){
    
    
    z <- complete(x, iso_code, year) %>%
        arrange(iso_code, year) %>%
        select(-any_of(c('Country Name', 'Country Code', 
                         'Indicator Name', 'Indicator Code'))) 
    
    zcols <- colnames(z)[-matches(c("iso_code", "year"),
                                  vars = colnames(z))] %>% setNames(.,.)
    
    z <-  
        mutate_at(z,
                  .vars = zcols,
                  .funs = list(filled_year = ~ifelse(is.na(.),
                                                     NA,
                                                     year)), ) %>%
        group_by(iso_code) %>%
        fill(!!!vars(-iso_code, -year), .direction = "downup")     
    
    z
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

get_staleness_socio <- function(x){
    # find which variables got 'filled_year' values from fill_socio()
    nms <- names(x) %>%
        grep('filled_year', ., value = T) %>%
        c("id", "Year started") 
    
    # find the distinct metadata from each study
    # then reshape as a long data frame
    # if a 2005 study uses 2004 covariate data, staleness is 1
    z   <- select(.data = x, any_of(nms)) %>%
        rename(year_study = `Year started`) %>%
        # distinct %>%
        gather(variable, year_covar, -id, -year_study) %>%
        mutate(variable = sub('_filled_year', '', variable)) %>%
        mutate(staleness = year_study - year_covar)
    
    z
}
