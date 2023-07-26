library(lubridate)
library(tidyverse)
library(openair)
library(janitor)
library(viridis)
library(zoo)

# Functions ---------------------------------------------------------------

min_max_norm <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

# Sorting spec rad --------------------------------------------------------

spec_rad = read.csv("data/Specrad_hour_23_with_calc.csv") %>% 
  mutate(date = dmy_hm(date),
         hour = hour(date)) %>% 
  clean_names() %>% 
  mutate(jhono = ifelse(is.na(j_hono),jhono_calc,j_hono),
         jhno3 = ifelse(is.na(j_hno3),jhno3_calc,j_hno3),
         jo1d = ifelse(is.na(j_o1d),jo1d_calc,j_o1d)) %>% 
  select(date,hour,jhono,jhno3,jo1d,jno2 = j_no2)

spec_rad_mean = spec_rad %>% 
  group_by(hour) %>% 
  summarise(jhono_avg = mean(jhono,na.rm = T),
            jhno3_avg = mean(jhno3,na.rm = T),
            jno2_avg = mean(jno2,na.rm = T),
            jo1d_avg = mean(jo1d,na.rm = T))

spec_rad_full = left_join(spec_rad,spec_rad_mean,by = "hour") %>% 
  mutate(jhono = ifelse(is.na(jhono),jhono_avg,jhono),
         jhno3 = ifelse(is.na(jhno3),jhno3_avg,jhno3),
         jno2 = ifelse(is.na(jno2),jno2_avg,jno2),
         jo1d = ifelse(is.na(jo1d),jo1d_avg,jo1d),
         jo1d_norm = min_max_norm(jo1d),
         jo1d_norm = jo1d_norm * max(jhno3)) %>% 
  select(-c(jhono_avg,jhno3_avg,jno2_avg,jo1d_avg))

spec_rad_full %>% 
  filter(date < "2023-03-01") %>%
  pivot_longer(c(jhno3,jo1d_norm)) %>% 
  ggplot(aes(date,value,col = name)) +
  geom_path() +
  scale_color_viridis_d()
  # facet_grid(rows = vars(name),scales = "free_y") +
  NULL

# PSS ------------------------------------------------------------

kp = 3.3 * 10^-11 #rate constant for oh + no -> hono (from Atkinson et al. 2004)
kl = 6 * 10^-12 #rate constant for oh + hono -> h2o + no2 (from Atkinson et al. 2004)
dv = 0.3 #deardfroff velocity, value used by Simone

dat = read.csv("output/data/all_data.csv") %>% 
  mutate(date = ymd_hms(date)) %>% 
  select(-c(jhono,jhno3))

#hono = k[OH][NO]+ jHNO3 * f * pNO3 / jHONO + k[OH] + kdep

#units for calculation are molecule per cm3

#can change f and kdep and other parameters as needed and compare them

pss_calc = left_join(dat,spec_rad_full,by = "date") %>% 
  filter(campaign == "February 2023") %>% 
  mutate(hour = hour(date),
         lifetime = ifelse(hour >= 11 & hour <= 15,1/jhono,NA_real_),
         lifetime = na.approx(lifetime,na.rm = FALSE),
         nitrate = na.approx(nitrate,na.rm = FALSE),
         h = lifetime * dv,
         kdep1 = 0.01/h,
         no_molecules = no * 2.46 * 10^19 * 10^-12,
         pss = (kp*oh*no_molecules + (jhno3 * nitrate * 58)) / (jhono + (kl*oh) + kdep1) / (2.46 * 10^19 * 10^-12),
         pss_red = (kp*oh*no_molecules + (jo1d_norm * nitrate * 58)) / (jhono + (kl*oh) + kdep1) / (2.46 * 10^19 * 10^-12))


pss_calc %>% 
  pivot_longer(c(hono,pss,pss_red)) %>% 
  ggplot(aes(date,value,col = name)) +
  geom_path() +
  scale_color_viridis_d()

# Checking diurnals -------------------------------------------------------

diurnal = pss_calc %>% 
  filter(is.na(hono) == FALSE) %>% 
  timeVariation(pollutant = c("hono","pss","pss_red"))

diurnal_dat = diurnal$data$hour

diurnal_dat %>% 
  ggplot(aes(hour,Mean,col = variable)) +
  geom_line(size = 1) +
  scale_color_manual(values = viridis(3)) +
  theme_bw() +
  labs(x = "Hour of day (UTC)",
       y = "HONO (ppt)",
       color = NULL) +
  scale_x_continuous(breaks = c(0,4,8,12,16,20)) +
  # facet_grid(rows = vars(variable),scales = "free_y") +
  theme(legend.position = "top")
