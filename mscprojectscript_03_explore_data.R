# Megan Verma 
# 7/25/2022 

# view(respicar) 
summary(respicar_socio) 

t1  <- table(respicar_socio$Country)
study_countries <- as.data.frame(t1)
# 76 countries, most from US then portugal/israel/brazil (44 & 22 each)
names(respicar)

# change certain character variables to factor
respicar_socio$Country             <- as.factor(respicar_socio$Country)
respicar_socio$`ISO 3166-1`        <- as.factor(respicar_socio$`ISO 3166-1`)
respicar_socio$Continent           <- as.factor(respicar_socio$Continent)
respicar_socio$Age                 <- as.factor(respicar_socio$Age)
respicar_socio$`Ethnic Minority`   <- as.factor(respicar_socio$`Ethnic Minority`)
respicar_socio$Design              <- as.factor(respicar_socio$Design)
respicar_socio$`Sampling strategy` <- as.factor(respicar_socio$`Sampling strategy`)

summary(respicar_socio)
ethnic_minority <- filter(respicar_socio, `Ethnic Minority`=="Yes")
# 62 studies in ethnic minority groups 

# 377 entries (studies) if we take out ethnic minority
# 439 datasets
# Cross-sectional:313
# Longitudinal   :125
# Unknown design :  1

respicar_israel    <- filter(respicar_socio,`Country` == "Israel")
# 22 studies in Israel, 1993-2009
respicar_palestine <- filter(respicar_socio,`Country` == "Palestinian Territories")
# 5 studies in Palestine, 2009 only

# make carriage variable
respicar_socio <- respicar_socio %>% 
    mutate(carriage = Positive/Total)

# make weighted mean of carriage by country

respicar_socio <- respicar_socio %>% 
    group_by(`ISO 3166-1`) %>% 
    summarise(carriage_country = weighted.mean(x = carriage, w = Total, na.rm = T),`ISO 3166-1`)

# plot carriage by country 
world <- ne_countries(scale = "medium", returnclass = "sf")
filter(world, `sovereignt`=="Antarctica")
world <- world %>% subset(sovereignt!="Antarctica")

#ggplot(data = world) + geom_sf() + theme_void()

world_with_carriage <- merge (x=respicar_socio, y=world, 
                              by.x = "ISO 3166-1", 
                              by.y = "iso_n3", 
                              all.y=TRUE)

world_with_carriage <- world_with_carriage %>% 
  select("ISO 3166-1", "carriage_country", "geometry", "pop_est", "gdp_md_est", "subregion") 
  
ggplot(data = world_with_carriage) +  
  geom_sf(aes(geometry= geometry, #world map geometry (polygons)
              fill=carriage_country)) + #color map w/ cont. values of total cases
  scale_fill_gradient(low="yellow", high="red", na.value="azure2", 
                      name = 'Weighted carriage rate', 
                      limits = c(0,1)) + #set color fill 
  theme_bw()  #theme of dark text on light background 

# add map title 

summary(world_with_carriage$carriage)



