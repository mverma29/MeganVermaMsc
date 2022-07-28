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
library(tidyverse)




