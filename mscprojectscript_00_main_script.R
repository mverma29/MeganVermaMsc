# Megan Verma MSc Project

# Load all packages required for analysis
source("mscprojectscript_01_load_packages.R")

# Load RESPICAR data 
source("mscprojectscript_02_load_data.R")

# Exploratory data analysis (carriage)
source("mscprojectscript_03.1_explore_data.R", verbose = TRUE)

# Descriptive statistics by UN subregion 
source("mscprojectscript_03.2_descriptive_stats_subregion.R", verbose = TRUE)

# Descriptive statistics by covariate  
source("mscprojectscript_03.3_descriptive_stats_covariate.R", verbose = TRUE)

# Univariate mixed effect models 
source("mscprojectscript_04_univariate_models.R", verbose = TRUE)

# Multivariate mixed effect models 
source("mscprojectscript_05_multivariate_models.R")
