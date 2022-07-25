---
title: "mscprojectscript"
author: "Megan Verma"
date: "7/21/2022"
output: html_document
---
    
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
library(WDI)
library(purrr)
library(tidyverse)




