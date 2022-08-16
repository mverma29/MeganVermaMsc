if(!dir.exists('outputs')){dir.create('outputs')}

# Exploratory data analysis (carriage)
source("mscprojectscript_03.1_plot_carriage.R", verbose = TRUE)

# Descriptive statistics by UN subregion 
source("mscprojectscript_03.2_descriptive_stats_subregion.R", verbose = TRUE)

# Descriptive statistics by covariate  
source("mscprojectscript_03.3_descriptive_stats_covariate.R", verbose = TRUE)
