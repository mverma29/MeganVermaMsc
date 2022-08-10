# Megan Verma 
# 7/25/2022 

### load RESPICAR data ----

#RESPICAR dataset (summary)
respicar <- read_csv("data/appendix_summary.csv")

# Omit New Caledonia
respicar %<>% filter(`ISO 3166-1` != 540L)

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

# mutate GDP to log GDP 
respicar_socio %<>% mutate(log_gdp = log(gdp_usd))

# mutate all covariate percentages to proportions in RESPICAR dataset
respicar_socio %<>% mutate(
    urban_percent = urban_percent / 100,
    gini = gini / 100,
    female_ed = female_ed / 100
)

