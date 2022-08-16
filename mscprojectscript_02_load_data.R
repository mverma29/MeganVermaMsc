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
respicar_socio %<>% mutate(log_gdp = log10(gdp_usd))


# change certain character variables to factor
respicar_socio$Country             <- as.factor(respicar_socio$Country)
respicar_socio$`ISO 3166-1`        <- as.factor(respicar_socio$`ISO 3166-1`)
respicar_socio$Continent           <- as.factor(respicar_socio$Continent)
respicar_socio$Age                 <- as.factor(respicar_socio$Age)
respicar_socio$`Ethnic Minority`   <- as.factor(respicar_socio$`Ethnic Minority`)
respicar_socio$Design              <- as.factor(respicar_socio$Design)
respicar_socio$`Sampling strategy` <- as.factor(respicar_socio$`Sampling strategy`)