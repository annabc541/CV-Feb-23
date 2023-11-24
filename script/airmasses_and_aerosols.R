library(tidyverse)
library(lubridate)
library(janitor)
library(zoo)
library(openair)
library(car)

Sys.setenv(TZ = 'UTC')


# Functions ---------------------------------------------------------------

lm_eqn <- function(df,x,y){
  m <- lm(y ~ x, df);
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                   list(a = format(unname(coef(m)[1]), digits = 2),
                        b = format(unname(coef(m)[2]), digits = 2),
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));
}

# Reading data ------------------------------------------------------------

dat_aerosols = read.csv("data/aerosol_data/nitrate_ammonium_CVAO_12-19.csv") %>%  
  clean_names() %>% 
  rename(date = start_local_time,
         nitrate_ug_m3 = nitrate_mg_m,
         ammonium_ug_m3 = ammonium_mg_m) %>% 
  mutate(date = mdy_hm(date),
         date = round_date(date, "1 day")) %>% 
  select(-c(sample_no,stop_local_time,ammonium_ug_m3))

air_mass = read.csv("data/new_CVAO_sector_%_boxes_1.csv") %>% 
  rename(date = X) %>% 
  mutate(date = ymd_hm(date)) %>% 
  clean_names() %>% 
  timeAverage("1 day")

air_mass_feb23 = air_mass %>% 
  filter(date > "2023-01-31" & date < "2023-03-01")

write.csv(air_mass_feb23,"output/data/air_mass_feb23.csv",row.names = F)

dat = left_join(air_mass,dat_aerosols,by = "date") %>% 
  filter(date > "2012-01-09") %>% 
  remove_constant() %>% 
  remove_empty()

# write.csv(dat,"output/data/air_mass_nitrate.csv",row.names = F)

# Correlation -------------------------------------------------------------

dat %>%
  # filter(date > "2019-01-01" & date < "2020-01-01") %>% 
  # pivot_longer(c(upwelling:south_atlantic)) %>%
  # mutate(value = value *4/100) %>% 
  ggplot() +
  # geom_area(aes(date,value,fill = name)) +
  geom_point(aes(sahara,north_atlantic)) +
  # scale_x_datetime(date_breaks = "1 month",date_labels = "%m/%y") +
  # facet_grid(rows = vars(name)) +
  # scale_fill_viridis_d() +
  NULL

corr = dat %>% 
  mutate(nitrate_bin = ntile(nitrate_ug_m3,n = 4),
         north_atlantic_bin = ntile(north_atlantic,n =4))

corr %>% 
  ggplot(aes(nitrate_ug_m3,north_atlantic_bin)) +
  geom_point()

cor(dat[,c("nitrate_ug_m3","north_atlantic","sahara")],use = "complete.obs")
df = data.frame(cor(dat[,unlist(lapply(dat, is.numeric))],use = "complete.obs"))

feb = dat %>% 
  mutate(month = month(date),
         year = year(date)) %>% 
  filter(month == 2)
  
dat %>% 
  select(-c(ammonium_ug_m3,nitrate_ug_m3)) %>% 
  corPlot()

model = lm(sahara ~ north_america,feb)

lm(sahara ~ nitrate_ug_m3,feb)

summary(model)

dat %>% 
  mutate(month = month(date),
         year = year(date),
         season = case_when(between(month,3,5) ~ "Spring",
                            between(month,6,8) ~ "Summmer",
                            between(month,9,11) ~ "Autumn",
                            TRUE ~ "Winter")) %>% 
  rename("Europe" = europe,"North Atlantic" = north_atlantic,"Sahara" = sahara) %>% 
  # filter(month == 2) %>% 
  pivot_longer(c("Europe","North Atlantic","Sahara")) %>% 
  ggplot(aes(nitrate_ug_m3,value,col = season)) +
  geom_point() +
  theme_bw() +
  geom_smooth(method = "lm") +
  scale_colour_viridis_d() +
  facet_grid(rows = vars(name),scales = "free") +
  theme(legend.position = "top") +
  labs(x = "Air mass %",
       y = expression(Nitrate~(ug~m^{-3})),
       col = "Season")

ggsave('nitrate_eu_na_s.svg',
       path = "output/plots/air_masses",
       width = 32,
       height = 15,
       units = 'cm')

#biggest negative correlation for nitrate is north atlantic air mass
#biggest positive correlation for nitrate is saharan air mass
#how to turn this into something?


# 2023 air mass data ------------------------------------------------------

air_mass_feb23 = air_mass %>% 
  filter(date > "2023-02-01",
         date < "2023-03-01") %>% 
  clean_names(case = "upper_camel") %>% 
  rename("West Africa" = WestAfrica,
         "North Atlantic" = NorthAtlantic,
         "North America" = NorthAmerica,
         "South Atlantic" = SouthAtlantic) %>% 
  remove_empty() %>% 
  remove_constant

air_mass_feb23 %>% 
  pivot_longer(c(Upwelling:"South Atlantic")) %>% 
  ggplot(aes(Date,value)) +
  theme_minimal() +
  geom_area(aes(fill = name)) +
  scale_fill_viridis_d() +
  labs(fill = NULL,
       y = "Air mass %") +
  scale_x_datetime(date_breaks = "2 days",date_labels = "%d %b") +
  theme(legend.position = "top")

nitrate_feb = dat %>% 
  mutate(month = month(date),
         year = year(date)) %>% 
  filter(month == 2) %>% 
  mutate(nitrate = 2.18 - 0.014 * north_america)

nitrate_feb %>% 
  ggplot(aes(nitrate,nitrate_ug_m3,col = as.character(year))) +
  geom_point()

ggsave('feb23_airmass.svg',
       path = "output/plots/air_masses",
       width = 33.87,
       height = 15,
       units = 'cm')


# Multiple linear regression -----------------------------------------------

nitrate_model = lm(formula = nitrate_ug_m3 ~ upwelling + sahel + sahara + west_africa + central_africa +
                     europe + north_america + south_america + north_atlantic + south_atlantic, data = dat)

model_residulas = nitrate_model$residuals

summary(nitrate_model)

vif(nitrate_model)

hist(model_residuals)
reduced_dat = dat %>%  select(-c(nitrate_ug_m3,ammonium_ug_m3,date)) %>% 
  remove_constant()

results = prcomp(reduced_dat,scale = T)

corr_matrix = round(cor(reduced_dat),2)
ggcorrplot(corr_matrix, hc.order = TRUE, type = "lower",
           lab = TRUE)
