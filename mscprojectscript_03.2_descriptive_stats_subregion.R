# Megan Verma 
# 8/16/2022 


respicar_israel    <- filter(respicar_socio,`Country` == "Israel")
# 22 studies in Israel, 1993-2009
respicar_palestine <- filter(respicar_socio,`Country` == "Palestinian Territories")
# 5 studies in Palestine, 2009 only

# facetwrap plot of each covariate by UN subregion -------

facet_urban_percent <- ggplot(data = respicar_socio,
                              aes(x = urban_percent, y = carriage)) +
  geom_point() +
  facet_wrap(~ subregion, nrow = 3) +
  theme_bw() +
  geom_point(data = select(respicar_socio,-subregion),
             alpha = 0.1,
             pch = 16) + 
  ggtitle("Urban Population and Carriage, by UN Subregion") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(strip.background = element_rect(fill="lightblue", size=1, color="darkblue")) + 
  xlab("Urban Population (Percent)") + 
  ylab("Carriage Rate")

ggsave(
  filename = "outputs/facet_urban_percent_carriage.png",
  plot     = facet_urban_percent,
  device   = png,
  width    = 11,
  height   = 5,
  units    = 'in',
  res      = 600
)

facet_gdp <- ggplot(data = respicar_socio,
                    aes(x = log_gdp, y = carriage)) +
  geom_point() +
  facet_wrap(~ subregion, nrow = 3) +
  theme_bw() +
  geom_point(data = select(respicar_socio,-subregion),
             alpha = 0.1,
             pch = 16) + 
  ggtitle("Log10 GDP Per Capita and Carriage, by UN Subregion") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(strip.background = element_rect(fill="lightblue", size=1, color="darkblue")) + 
  xlab("Log10 GDP Per Capita (current USD)") + 
  ylab("Carriage Rate")

ggsave(
  filename = "outputs/facet_gdp_carriage.png",
  plot     = facet_gdp,
  device   = png,
  width    = 11,
  height   = 5,
  units    = 'in',
  res      = 600
)

facet_gini <- ggplot(data = respicar_socio,
                     aes(x = gini, y = carriage)) +
  geom_point() +
  facet_wrap(~ subregion, nrow = 3) +
  theme_bw() +
  geom_point(data = select(respicar_socio,-subregion),
             alpha = 0.1,
             pch = 16) + 
  ggtitle("Gini Coefficient of Inequality and Carriage, by UN Subregion") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(strip.background = element_rect(fill="lightblue", size=1, color="darkblue")) + 
  xlab("Gini Coefficient of Inequality (Percent)") + 
  ylab("Carriage Rate")

ggsave(
  filename = "outputs/facet_gini_carriage.png",
  plot     = facet_gini,
  device   = png,
  width    = 11,
  height   = 5,
  units    = 'in',
  res      = 600
)

facet_hh <- ggplot(data = respicar_socio,
                   aes(x = mean_hh, y = carriage)) +
  geom_point() +
  facet_wrap(~ subregion, nrow = 3) +
  theme_bw() +
  geom_point(data = select(respicar_socio,-subregion),
             alpha = 0.1,
             pch = 16) + 
  ggtitle("Average Household Size and Carriage, by UN Subregion") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(strip.background = element_rect(fill="lightblue", size=1, color="darkblue")) + 
  xlab("Average Household Size") + 
  ylab("Carriage Rate")

ggsave(
  filename = "outputs/facet_hh_carriage.png",
  plot     = facet_hh,
  device   = png,
  width    = 11,
  height   = 5,
  units    = 'in',
  res      = 600
)

facet_ed <- ggplot(data = respicar_socio,
                   aes(x = female_ed, y = carriage)) +
  geom_point() +
  facet_wrap(~ subregion, nrow = 3) +
  theme_bw() +
  geom_point(data = select(respicar_socio,-subregion),
             alpha = 0.1,
             pch = 16) + 
  ggtitle("Female Secondary Education Enrollment and Carriage, by UN Subregion") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(strip.background = element_rect(fill="lightblue", size=1, color="darkblue")) + 
  xlab("Female Secondary Education Enrollment (Percent)") + 
  ylab("Carriage Rate")

ggsave(
  filename = "outputs/facet_ed_carriage.png",
  plot     = facet_ed,
  device   = png,
  width    = 11,
  height   = 5,
  units    = 'in',
  res      = 600
)






# descriptive stats by UN subregion------

desc_aus_nz <- filter(respicar_socio, `subregion`=="Australia and New Zealand")
summary(desc_aus_nz)

desc_c_asia <- filter(respicar_socio, `subregion`=="Central Asia")
summary(desc_c_asia)

desc_e_asia <- filter(respicar_socio, `subregion`=="Eastern Asia")
summary(desc_e_asia)

desc_e_europe <- filter(respicar_socio, `subregion`=="Eastern Europe")
summary(desc_e_europe)

desc_l_a <- filter(respicar_socio, `subregion`=="Latin America and the Caribbean")
summary(desc_l_a)

desc_mel <- filter(respicar_socio, `subregion`=="Melanesia")
summary(desc_mel)

desc_n_africa <- filter(respicar_socio, `subregion`=="Northern Africa")
summary(desc_n_africa)

desc_n_amer <- filter(respicar_socio, `subregion`=="Northern America")
summary(desc_n_amer)

desc_n_europe <- filter(respicar_socio, `subregion`=="Northern Europe")
summary(desc_n_europe)

desc_se_asia <- filter(respicar_socio, `subregion`=="South-eastern Asia")
summary(desc_se_asia)

desc_s_asia <- filter(respicar_socio, `subregion`=="Southern Asia")
summary(desc_s_asia)

desc_s_europe <- filter(respicar_socio, `subregion`=="Southern Europe")
summary(desc_s_europe)

desc_ss_africa <- filter(respicar_socio, `subregion`=="Sub-Saharan Africa")
summary(desc_ss_africa)

desc_w_asia <- filter(respicar_socio, `subregion`=="Western Asia")
summary(desc_w_asia)

desc_w_euro <- filter(respicar_socio, `subregion`=="Western Europe")
summary(desc_w_euro)






