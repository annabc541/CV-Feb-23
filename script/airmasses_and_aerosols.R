library(tidyverse)
library(lubridate)
library(janitor)
library(zoo)
library(openair)

Sys.setenv(TZ = 'UTC')


# Reading data ------------------------------------------------------------

dat15 = read.csv("data/aerosol_data/cv_data_2015.csv") %>% 
  tibble() %>% 
  mutate(date = ymd_hms(date)) %>% 
  clean_names() %>% 
  remove_constant() %>% 
  select(date,mass_ug_m3:dp_um)

dat19 = read.csv("data/aerosol_data/cv_data_2019.csv") %>% 
  tibble() %>% 
  mutate(date = ymd_hms(date)) %>% 
  clean_names() %>% 
  remove_constant() %>% 
  remove_empty() %>% 
  select(date,dp_um:calcium_ug_m3) %>% 
  timeAverage("6 hour")

dat_aerosols = bind_rows(dat15,dat19)

air_mass = read.csv("data/new_CVAO_sector_%_boxes_1.csv") %>% 
  rename(date = X) %>% 
  mutate(date = ymd_hm(date)) %>% 
  clean_names()

dat = left_join(dat_aerosols,air_mass) %>% 
  remove_constant() %>% 
  remove_empty()

# Correlation tests -------------------------------------------------------

dat_cor = dat %>% 
  select(-date)

shapiro.test(dat$sahara)

cor.test(dat_cor$sahara,dat_cor$nitrate_ug_m3,use = "complete.obs",method = "pearson")

dat_cor %>% 
  filter(sahara > 10) %>%
  ggplot(aes(sahara,calcium_ug_m3)) +
  geom_point()
