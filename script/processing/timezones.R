library(tidyverse)
library(lubridate)
library(openair)
library(janitor)
library(zoo)
library(viridis)

Sys.setenv(TZ = 'UTC')

#spec rad data for 2023 is in UTC-1 so an hour needs to be added to the data to make it UTC
#this code can be used to make plots with the spec rad data to show how it looks better in UTC

# Reading in old spec rad data (2017-21) - known to be in UTC -------------

#spec rad data from 01/2017 to 02/2021 in UTC
spec_rad_adj = read.csv("data/spec_rad/Spec_rad_Hourly_timestamp_adj.csv") %>% 
  mutate(new.date = dmy_hm(new.date)) %>% 
  rename(old_date = date,
         date = new.date,
         jhono = j.hono.,
         jhno3 = j.hno3.) %>% 
  select(date,jhono,jhno3)

#calculated spec rad values from solar radiation, which is in UTC
spec_rad_calc = read.csv("data/spec_rad/spec-rad_calculated.csv") %>% 
  mutate(date = dmy_hm(date)) %>% 
  select(date,solar_radiation = SOLAR_RADIATION,jhono_calc,jhno3_calc)

spec_rad_old = left_join(spec_rad_adj,spec_rad_calc)


# Reading in 2023 spec rad data -------------------------------------------

#spec rad data from 2023, can correct date or not to see difference
spec_rad23 = read.csv("data/spec_rad/Specrad_hour_23_with_calc.csv") %>% 
  mutate(date = dmy_hm(date),
         # date = date + 3600 #UTC correction
  ) %>% 
  clean_names() %>% 
  select(date,solar_radiation,jhono = j_hono,jhno3 = j_hno3,jhono_calc,jhno3_calc)

# Plotting diurnals of species across different years ---------------------

spec_rad = bind_rows(spec_rad_old,spec_rad23)

diurnal = spec_rad %>% 
  mutate(month = month(date),
         year = year(date)) %>% 
  filter(month == 2) %>% 
  pivot_wider(names_from = year,values_from = jhno3) %>%
  timeVariation(c("2017","2018","2019","2020","2021","2023"))

diurnal_dat = diurnal$data$hour

diurnal_dat %>% 
  ggplot(aes(hour,Mean,col = variable)) +
  theme_bw() +
  geom_path(size = 0.8) +
  # facet_grid(rows = vars(variable),scales = "free_y") +
  scale_x_continuous(breaks = c(0:23)) +
  scale_colour_viridis_d() +
  theme(legend.position = "top") +
  labs(x = "Hour of day (UTC?)",
       y = NULL,
       color = NULL) +
  NULL

# ggsave('feb23_corr.svg',
#        path = "output/plots_analysis/spec_rad",
#        # width = 15.37,
#        # height = 13,
#        height = 12,
#        width = 30,
#        units = 'cm')


# Plotting diurnals of data from 2023 -------------------------------------

#solar radiation already in the correct timezone, to compare the corrected data need to create three dfs

diurnal_sr = spec_rad23 %>% 
  timeVariation("solar_radiation")

diurnal_jhono = spec_rad23 %>% 
  mutate(date = date + 3600) %>% 
  timeVariation(pollutant = "jhono")

diurnal_jhno3 = spec_rad23 %>% 
  mutate(date = date + 3600) %>% 
  timeVariation(pollutant = "jhno3")

diurnal_dat_sr = diurnal_sr$data$hour %>% 
  ungroup() %>% 
  rename(solar_radiation = Mean) %>% 
  select(hour,solar_radiation)

diurnal_dat_jhono = diurnal_jhono$data$hour %>% 
  ungroup() %>% 
  rename(jhono = Mean) %>% 
  select(hour,jhono)

diurnal_dat_jhno3 = diurnal_jhno3$data$hour %>% 
  ungroup() %>% 
  rename(jhno3 = Mean) %>% 
  select(hour,jhno3)

df_list = list(diurnal_dat_jhono,diurnal_dat_jhno3,diurnal_dat_sr)
diurnal_comparison = df_list %>% reduce(full_join,by = "hour") %>% 
  arrange(hour)

diurnal_comparison %>% 
  pivot_longer(c(jhono,solar_radiation,jhno3)) %>% 
  ggplot(aes(hour,value,col = name)) +
  theme_bw() +
  geom_path(size = 0.8) +
  facet_grid(rows = vars(name),scales = "free_y") +
  scale_colour_viridis_d() +
  labs(x = "Hour of day (UTC)",
       y = NULL,
       color = NULL) +
  scale_x_continuous(breaks = c(0:23)) +
  # ylim(-0.5,12) +
  theme(legend.position = "top")

# ggsave('feb23_corr.svg',
#        path = "output/plots_analysis/spec_rad",
#        # width = 15.37,
#        # height = 13,
#        height = 12,
#        width = 30,
#        units = 'cm')

# Checking with HONO data -------------------------------------------------

hono23 = read.csv("output/data/hono23_utc.csv") %>% 
  mutate(date = ymd_hms(date)) %>% 
  timeAverage("1 hour")

dat_hono = left_join(spec_rad,hono23,by = "date")

diurnal_hono = dat_hono %>% 
  mutate(month = month(date),
         year = year(date)) %>% 
  filter(month == 2,
         year == 2023) %>%
  timeVariation(c("jhno3","jhono","hono"))


diurnal_dat_hono = diurnal_hono$data$hour

diurnal_dat_hono %>% 
  ggplot(aes(hour,Mean,col = variable)) +
  theme_bw() +
  geom_path(size = 0.8) +
  facet_grid(rows = vars(variable),scales = "free_y") +
  scale_x_continuous(breaks = c(0:23)) +
  scale_colour_viridis_d() +
  theme(legend.position = "top") +
  labs(x = "Hour of day (UTC)",
       y = NULL,
       color = NULL) +
  NULL

ggsave('feb23_hono_uncorr.svg',
       path = "output/plots_analysis/spec_rad",
       width = 15.37,
       height = 13,
       # height = 12,
       # width = 30,
       units = 'cm')
