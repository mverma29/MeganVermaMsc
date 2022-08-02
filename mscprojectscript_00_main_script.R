# Megan Verma MSc Project

# Load all packages required for analysis
source("mscprojectscript_01_load_packages.R")

# Load RESPICAR data 
source("mscprojectscript_02_load_data.R")

# load urbanicity data & add to RESPICAR data 
source("mscprojectscript_02.1_urbanpercent.R")

# load GDP data & add to RESPICAR data 
source("mscprojectscript_02.2_gdp.R")

# load Gini data & add to RESPICAR data 
source("mscprojectscript_02.3_gini.R")

# load Household size data & add to RESPICAR data 
source("mscprojectscript_02.4_hhsize.R")

# load female education data & add to RESPICAR data 
source("mscprojectscript_02.5_femaleed.R")

# load UN subregion data & add to RESPICAR data 
source("mscprojectscript_02.6_unsubregion.R")

# Exploratory data analysis
source("mscprojectscript_03_explore_data.R", verbose = TRUE)
