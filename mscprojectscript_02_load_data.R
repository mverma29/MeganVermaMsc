# Megan Verma 
# 7/25/2022 


source("mscprojectscript_01_load_packages")

### load data ----

#RESPICAR dataset (summary)
respicar <- read_csv("/Users/meganverma/Desktop/git/MeganVermaMsc/data/appendix_summary.csv")

### sociodemographic variables----

# Percent urbanicity----
# from un populations divison, accessed via world bank
urban_percent <- read_csv("/Users/meganverma/Desktop/git/MeganVermaMsc/data/urban_pop_percent.csv")

# GDP----
WDIsearch("gdp per capita")
# #5 seems right-- GDP per capita, current USD 

gdp_data <- WDI(country = "all",
  indicator = "NY.GDP.PCAP.CD",
  start = 1990,
  end = 2016,
  extra = TRUE,
  cache = NULL,
  latest = NULL,
  language = "en")


# Gini----
gini<- WDI(country="all", 
           indicator= "SI.POV.GINI", 
           start = 1990,
           end = 2016, 
           language = "en")



# Household size----
# Download the UN Population Division's data on [Household size and composition]
# (https://www.un.org/development/desa/pd/data/household-size-and-composition)

un_data <- readxl::read_xlsx("/Users/meganverma/Desktop/git/MeganVermaMsc/data/un_hh.xlsx", 
                             sheet= 4, 
                             range= "A5:E819", 
                             col_names = TRUE)

un_data

# will need to find HH data from each study's year end date 
# (for countries with mult studies, we take most recent study end date)


# female education (proxy for maternal)----
# UNESCO data, from world bank site 
# secondary education levels!

female_ed <- read_csv("/Users/meganverma/Desktop/git/MeganVermaMsc/data/female_secondary_education.csv")



# UN subregion---- 
# already in world_with_cases as "subregion"


### merge datasets on year end (& numeric iso code)----