# Megan Verma 
# 7/25/2022 

source("/Users/meganverma/Desktop/git/MeganVermaMsc/mscprojectscript_01_load_packages.R")
source("/Users/meganverma/Desktop/git/MeganVermaMsc/mscprojectscript_02_load_data.R") #broken??

# view(respicar) 
summary(respicar) 
# 429 datasets
t1 <- table(respicar$Country)
df1 <- as.data.frame(t1)
# 73 countries, most from US then portugal/israel/brazil (44 & 22 each)
names(respicar)

# change certain character variables to factor (inelegant but effective code)
respicar$Country <- as.factor(respicar$Country)
respicar$`ISO 3166-1` <- as.factor(respicar$`ISO 3166-1`)
respicar$Continent <- as.factor(respicar$Continent)
respicar$Age <- as.factor(respicar$Age)
respicar$`Ethnic Minority` <- as.factor(respicar$`Ethnic Minority`)
respicar$Design <- as.factor(respicar$Design)
respicar$`Sampling strategy` <- as.factor(respicar$`Sampling strategy`)

summary(respicar)
# 367 entries (studies) if we take out ethnic minority

# make carriage variable
respicar <- respicar %>% mutate(carriage = Positive/Total)

# plot carriage by country 
world <- ne_countries(scale = "medium", returnclass = "sf")
ggplot(data = world) + geom_sf() + theme_void()
world_with_carriage <- merge (x=respicar, y=world, 
                              by.x = "ISO 3166-1", 
                              by.y = "iso_n3")

world_with_carriage <- world_with_carriage %>% 
  select("ISO 3166-1", "carriage", "geometry", "pop_est", "gdp_md_est", "subregion") 
  

ggplot(data = world_with_carriage) +  
  geom_sf(aes(geometry= geometry, #world map geometry (polygons)
              fill=carriage)) + #color map w/ cont. values of total cases
  scale_fill_gradient(low="yellow", high="red") + #set color fill 
  theme_bw() #theme of dark text on light background 
summary(world_with_carriage$carriage)
# something is wrong-- 3 studies have carriage >100%



