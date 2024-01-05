library(tidyverse)
library(lubridate)
library(openair)
library(janitor)
library(zoo)
library(viridis)

#used for looking at feb 2023 pss and creating plots with different f and dep velocities
#calculates f based on missing hono production, but doesn't use parameterisation

kp = 3.3 * 10^-11 #rate constant for oh + no -> hono (from Atkinson et al. 2004)
kl = 6 * 10^-12 #rate constant for oh + hono -> h2o + no2 (from Atkinson et al. 2004)
dv = 0.3 #deardfroff velocity, value used by Simone
nitrate = 1.20 * 10^10 #constant value until more recent measurements are received from TROPOS

dat = read.csv("output/data/data_for_pss.csv") %>% 
  mutate(date = ymd_hms(date))

# PSS with f = 70 ---------------------------------------------------------

pss70 = dat %>% 
  mutate(hour = hour(date),
         lifetime = ifelse(hour >= 11 & hour <= 15,1/jhono,NA_real_),
         lifetime = na.approx(lifetime,na.rm = FALSE),
         nitrate = na.approx(nitrate,na.rm = FALSE),
         h = lifetime * dv,
         kdep1 = 0.01/h,
         kdep3 = 0.03/h,
         no_molecules = no * 2.46 * 10^19 * 10^-12,
         pss1 = (kp*oh*no_molecules + (jhno3 * nitrate * 70)) / (jhono + (kl*oh) + kdep1)
                         / (2.46 * 10^19 * 10^-12),
         pss3 = (kp*oh*no_molecules + (jhno3 * nitrate * 70)) / (jhono + (kl*oh) + kdep3)
         / (2.46 * 10^19 * 10^-12))

pss70 %>%  
  filter(date < "2023-02-27" & date > "2023-02-06") %>% 
  rename('Dep velocity = 1' = pss1,
         'Dep velocity = 3' = pss3,
         'Observed' = hono) %>% 
  pivot_longer(c('Dep velocity = 1','Dep velocity = 3','Observed')) %>%
  ggplot(aes(date,value,col = name)) +
  theme_bw() +
  labs(x = "Hour of day (UTC)",
       y = "HONO (ppt)",
       color = NULL) +
  geom_path(size = 0.8) +
  theme(legend.position = "top") +
  scale_x_datetime(breaks = "1 day",date_labels = "%d/%m") +
  scale_color_viridis_d() +
  NULL

ggsave('f70_dep_vel.svg',
       path = "output/plots/pss/feb23",
       width = 30,
       height = 12,
       units = 'cm')


# Calculating f -----------------------------------------------------------

f_calc = pss70 %>%   
  filter(hour >= 11 & hour <= 15) %>%
  mutate(lifetime = 1/jhono,
         h = lifetime * dv,
         kdep1 = 0.01/h,
         kdep3 = 0.03/h,
         production_without_nitrate = kp*oh*no_molecules,
         loss1 = jhono + (kl*oh) + kdep1,
         loss_hono1 = loss1 * hono * 2.46 * 10^19 * 10^-12,
         missing_production1 = loss_hono1 - production_without_nitrate,
         loss3 = jhono + (kl*oh) + kdep3,
         loss_hono3 = loss3 * hono * 2.46 * 10^19 * 10^-12,
         missing_production3 = loss_hono3 - production_without_nitrate)

#finding f - daytime median of missing production and jhno3 used
#specifically between 10 and 15 local time - 11 and 16 UTC
jhno3 = mean(f_calc$jhno3,na.rm = TRUE)
nitrate = mean(f_calc$nitrate,na.rm = TRUE)
missing_production1 = mean(f_calc$missing_production1,na.rm = TRUE)
f1 = missing_production1/(nitrate*jhno3)
missing_production3 = mean(f_calc$missing_production3,na.rm = TRUE)
f3 = missing_production3/(nitrate*jhno3)

# PSS with calculated f ---------------------------------------------------------

pss_calc = dat %>% 
  mutate(hour = hour(date),
         lifetime = ifelse(hour >= 11 & hour <= 15,1/jhono,NA_real_),
         lifetime = na.approx(lifetime,na.rm = FALSE),
         nitrate = na.approx(nitrate,na.rm = FALSE),
         h = lifetime * dv,
         kdep1 = 0.01/h,
         kdep3 = 0.03/h,
         no_molecules = no * 2.46 * 10^19 * 10^-12,
         pss1 = (kp*oh*no_molecules + (jhno3 * nitrate * f1)) / (jhono + (kl*oh) + kdep1)
         / (2.46 * 10^19 * 10^-12),
         pss3 = (kp*oh*no_molecules + (jhno3 * nitrate * f3)) / (jhono + (kl*oh) + kdep3)
         / (2.46 * 10^19 * 10^-12))

pss_calc %>%  
  # filter(date < "2023-02-27" & date > "2023-02-06") %>% 
  rename('f = 63' = pss1,
         'f = 67' = pss3,
         'Observed' = hono) %>% 
  pivot_longer(c('f = 63','f = 67','Observed')) %>%
  ggplot(aes(date,value,col = name)) +
  theme_bw() +
  labs(x = "Hour of day (UTC)",
       y = "HONO (ppt)",
       color = NULL) +
  geom_path(size = 0.8) +
  theme(legend.position = "top") +
  scale_x_datetime(breaks = "1 day",date_labels = "%d/%m") +
  scale_color_viridis_d() +
  NULL

ggsave('fcalc_dep_vel.svg',
       path = "output/plots/pss/feb23",
       width = 30,
       height = 12,
       units = 'cm')


# Diurnals ----------------------------------------------------------------

diurnal = pss_calc %>% 
  filter(is.na(hono) == FALSE) %>% 
  rename(HONO = hono,'f = 62' = pss1, 'f = 66' = pss3) %>% 
  timeVariation(pollutant = c("HONO",'f = 62','f = 66'))

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
  ylim(-1.5,13) + #in order to have same sized axes for diurnals for all three campaigns
  theme(legend.position = "top")

ggsave('diurnal_fcalc_dep_vel.svg',
       path = "output/plots/pss/feb23",
       width = 30,
       height = 12,
       units = 'cm')
