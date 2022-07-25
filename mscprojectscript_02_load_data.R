# Megan Verma 
# 7/25/2022 


source("mscprojectscript_01_load_packages")

# load data 

#RESPICAR dataset (summary)
respicar <- read_csv("/Users/meganverma/Desktop/git/MeganVermaMsc/data/appendix_summary.csv")

#sociodemographic variables:

# Percent urbanicity

# GDP 
WDIsearch("gdp per capita")
gdp_data <- WDI(country = "all",
  indicator = "NY.GDP.PCAP.CD",
  start = 1990,
  end = 2016,
  extra = TRUE,
  cache = NULL,
  latest = NULL,
  language = "en")

# Gini 

# Household size 

# Maternal education? 

# UN subregion 
# NE data? UN Data

# merge datasets on year end (&iso code)