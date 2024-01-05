library(tidyverse)
library(lubridate)
library(janitor)
library(zoo)
library(openair)

Sys.setenv(TZ = 'UTC')

#used to be called agu - was used to generate plots for agu
#now calculates f for 2023 using matt's parameterisation and then plots it nicely
#most up to date code for parameterisation, currently still using ML nitrate

# Constants ---------------------------------------------------------------

kp = 3.3 * 10^-11 #rate constant for oh + no -> hono (from Atkinson et al. 2004) in cm3 molecule-1 s-1
kl = 6 * 10^-12 #rate constant for oh + hono -> h2o + no2 (from Atkinson et al. 2004) in cm3 molecule-1 s-1
dv = 0.3 #deardroff velocity, value used by Simone, in m s-1

# Functions ---------------------------------------------------------------

standardise <- function(x){
  y = (x-mean(x,na.rm = T))/sd(x,na.rm = T)
  
}


# Reading in data -----------------------

#feb 2023 data required for parameterisation
dat = read.csv("output/data/data_parameterisation.csv") %>% 
  mutate(date = ymd_hms(date)) %>% 
  filter(year == 2023) %>% 
  select(-c(mass_ug_m3:calcium_ug_m3,ocean:aerosol))

#hono data from simone's arna campaign
arna_data = read.csv("data/hono/arna_hono/Renoxification_data_for_Anna_v2.csv") %>% 
  mutate(date = dmy_hm(Start_time),
         year = year(date)) %>% 
  select(date,nitrate_ppt = NO3_ppt,
         nitrate_err = Total_NO3_uncertainty_.,
         hono = HONO_pptV,
         hono_err = HONO_Uncertainty_ppt,
         rh = RH,
         no = NO_pptV,
         jhno3 = J_HNO3,
         jhono = J_HONO,
         altitude = Altitude_m)


# Calculating daily enhancement factors -----------------------------------

#all calculations performed in molecules per cm3 and s-1

hourly_f = dat %>%
  rename(hono_ppt = hono,oh_m_cm3 = oh,no_ppt = no,no2_ppt = no2) %>% 
  filter(hour >= 11 & hour <= 15) %>%  #only looking at daytime values
  mutate(lifetime = 1/jhono,
         h = lifetime * dv,
         kdep = 0.01/h,
         nitrate_m_cm3 = (nitrate_ug_m3 * 10^-12 *6.022 * 10^23)/62.004,
         nitrate_nmol_m3 = (nitrate_m_cm3 * 10^15)/(6.022*10^23),
         production_without_nitrate = kp*oh_m_cm3*no_ppt * 2.46 * 10^19 * 10^-12,
         loss = (jhono + (kl*oh_m_cm3) + kdep) * hono_ppt * 2.46 * 10^19 * 10^-12,
         missing_production = (loss - production_without_nitrate), #molecule cm-3 s-1
         f_calc = missing_production/(jhno3 * nitrate_m_cm3),
         # f_para_rh = (10^5/((1+80.07*nitrate_nmol_m3))) * (rh/rh_mean),
         f_para = 103706014.61/(1+83211.37 *nitrate_nmol_m3), #matt rowlinson's parameterisation
         # hono_para_rh = ((production_without_nitrate + (jhno3 * nitrate_m_cm3 * f_para_rh)) / (jhono + (kl*oh_m_cm3) + kdep))
         # / (2.46 * 10^19 * 10^-12),
         hono_para = ((production_without_nitrate + (jhno3 * nitrate_m_cm3 * f_para)) / (jhono + (kl*oh_m_cm3) + kdep))
         / (2.46 * 10^19 * 10^-12),#converts hono from molecules cm-3 s-1 to ppt
         # ratio_hono_rh = hono_ppt/hono_para_rh,
         ratio_hono = hono_ppt/hono_para)

daily_f = hourly_f %>% 
  timeAverage("1 day")
# select(date,nitrate_ug_m3,rh,jhno3,no_ppt,hono_ppt,hono_para,f_calc,f_para,ratio_hono)
# select(date,year,hono_ppt,hono_para,ratio_hono,nitrate_ug_m3,f_calc,f_para,jhono,jhno3,everything())

# write.csv(daily_para,"output/data/parameterised23.csv",row.names = F)

#same as above but with arna data, one f value calculated per flight
arna_f = arna_data %>%
  rename(hono_ppt = hono,no_ppt = no) %>% 
  mutate(lifetime = 1/jhono,
         oh_m_cm3 = 2 * 10^6,
         h = lifetime * dv,
         year = year(date),
         kdep = 0.01/h,
         nitrate_m_cm3 = nitrate_ppt * 2.46 * 10^19 * 10^-12,
         nitrate_nmol_m3 = (nitrate_m_cm3 * 10^15)/(6.022*10^23),
         nitrate_ug_m3 = nitrate_m_cm3 * 62.004 / (10^-12 *6.022 * 10^23),
         production_without_nitrate = kp*oh_m_cm3*no_ppt * 2.46 * 10^19 * 10^-12,
         loss = (jhono + (kl*oh_m_cm3) + kdep) * hono_ppt * 2.46 * 10^19 * 10^-12,
         missing_production = (loss - production_without_nitrate), #molecule cm-3 s-1
         f_calc = missing_production/(jhno3 * nitrate_m_cm3),
         f_para = 103706014.61/(1+83211.37 *nitrate_nmol_m3),
         hono_para = ((production_without_nitrate + (jhno3 * nitrate_m_cm3 * f_para)) / (jhono + (kl*oh_m_cm3) + kdep))
         / (2.46 * 10^19 * 10^-12),#converts hono from molecules cm-3 s-1 to ppt
         ratio_hono = hono_ppt/hono_para) %>% 
  select(date,nitrate_nmol_m3,hono_ppt,hono_err,rh,altitude,f_para,hono_para,ratio_hono,year,f_calc,nitrate_ug_m3)

# Using daily f to calculate pss hono -------------------------------------

#calculating pss hono with daily f

only_daily_f = daily_f %>% 
  select(date,f_para,ratio_hono,)

pss_dat = dat %>%
  rename(hono_ppt = hono,oh_m_cm3 = oh,no_ppt = no,no2_ppt = no2) %>% 
  left_join(only_daily_f,by = "date") %>% 
  fill(c(f_para,nitrate_ug_m3,upwelling:south_atlantic),.direction = "down") %>%
  mutate(lifetime = ifelse(hour >= 11 & hour <= 15,1/jhono,NA_real_),
         # f_para_rh = case_when(day == 7 ~ 45.24,
         #                       TRUE ~ f_para_rh),
         f_para = case_when(day == 7 ~ 55.53,
                            TRUE ~ f_para)) %>% 
  fill(lifetime,.direction = "updown") %>%
  mutate(h = lifetime * dv,
         kdep = 0.01/h,
         nitrate_m_cm3 = (nitrate_ug_m3 * 10^-12 *6.022 * 10^23)/62.004,
         production_without_nitrate = kp*oh_m_cm3*no_ppt * 2.46 * 10^19 * 10^-12,
         loss = (jhono + (kl*oh_m_cm3) + kdep) * hono_ppt * 2.46 * 10^19 * 10^-12,
         missing_production = (loss - production_without_nitrate),
         # hono_para_rh = ((kp*oh_m_cm3*no_ppt * 2.46 * 10^19 * 10^-12 + (jhno3 * nitrate_m_cm3 * f_para_rh)) / (jhono + (kl*oh_m_cm3) + kdep))
         # / (2.46 * 10^19 * 10^-12),
         hono_para = ((kp*oh_m_cm3*no_ppt * 2.46 * 10^19 * 10^-12 + (jhno3 * nitrate_m_cm3 * f_para)) / (jhono + (kl*oh_m_cm3) + kdep))
         / (2.46 * 10^19 * 10^-12))


# Feb 2023 HONO diurnal ---------------------------------------------------

diurnal = dat %>% 
  rename(HONO = hono) %>% 
  filter(is.na(HONO) == FALSE,
         year == 2023) %>% 
  timeVariation(pollutant = c("HONO","hono_err"))

diurnal_dat = diurnal$data$hour %>% 
  ungroup() %>% 
  pivot_wider(names_from = variable,values_from = Mean) %>% 
  group_by(hour) %>% 
  summarise(HONO = mean(HONO,na.rm = T),
            hono_err = mean(hono_err,na.rm = T))
  

diurnal_dat %>%
  ggplot(aes(hour,HONO)) +
  geom_path(size = 0.75,col = "steelblue1") +
  geom_ribbon(aes(ymin = HONO - hono_err,ymax = HONO + hono_err),alpha = 0.25,fill = "steelblue1") +
  # facet_grid(rows = vars(variable),scales = "free_y",labeller = label_parsed) +
  scale_colour_viridis_d() +
  theme_bw() +
  labs(x = "Hour of day (UTC)",
       y = "HONO (ppt)",
       color = NULL) +
  scale_x_continuous(breaks = c(0,4,8,12,16,20)) +
  # ylim(-1,13) +
  theme(legend.position = "top")

ggsave('hono_diurnal23.svg',
       path = "output/plots/agu",
       width = 8,
       height = 13,
       units = 'cm')


# Feb 2023 HONO timeseries ------------------------------------------------

dat %>% 
  ggplot(aes(date,hono)) +
  theme_bw() +
  labs(x = "Datetime (UTC)",
       y = "HONO (ppt)",
       col = NULL) +
  geom_path(size = 0.75,col = "steelblue1") +
  geom_ribbon(aes(ymin = hono - hono_err,ymax = hono + hono_err),alpha = 0.25) +
  theme(legend.position = "top") +
  scale_x_datetime(breaks = "2 day",date_labels = "%d/%m/%y") +
  NULL

ggsave('hono_timeseries23.svg',
       path = "output/plots/agu",
       width = 23,
       height = 13,
       units = 'cm')

# Feb 2023 HONO timeseries with PSS ---------------------------------------

#obs calc and para hono in same facet - timeseries
pss_dat %>%
  mutate(hono_para_err = hono_para *0.1,
         hono_min = hono_ppt - hono_err,
         hono_max = hono_ppt + hono_err,
         hono_para = ifelse(hono_para > -1, hono_para,NA_real_)) %>%
  # rename('Parameterised PSS' = hono_para,
  #        'Measured' = hono_ppt) %>%
  # pivot_longer(c('Parameterised PSS','Measured')) %>%
  ggplot() +
  theme_bw() +
  labs(x = "Datetime (UTC)",
       y = "HONO (ppt)",
       col = "RH (%)") +
  geom_path(aes(date,hono_para),size = 0.8) +
  geom_ribbon(aes(date,ymin = hono_para - hono_para_err,ymax = hono_para + hono_para_err),alpha = 0.25) +
  geom_path(aes(date,hono_ppt,col = rh),size = 0.8) +
  geom_ribbon(aes(x = date,ymin = hono_min,ymax = hono_max),alpha = 0.25) +
  scale_x_datetime(breaks = "2 day",date_labels = "%d/%m/%y") +
  scale_colour_gradient(low = "darkorange",high = "steelblue1") +
  # facet_wrap(~name,ncol = 1) +
  # geom_ribbon(data = subset(pss_dat,name == "Parameterised PSS"),
  #             aes(x = date,ymin = value - hono_err,ymax = value + hono_err),
  #             alpha = 0.5,fill = "steelblue1") +
  # scale_colour_manual(values = c("black","darkorange","steelblue1"),
  #                   breaks = c("Measured","Parameterised PSS","Parameterised PSS with RH")) +
  NULL

ggsave('timeseries_para_rh.svg',
       path = "output/plots/agu",
       width = 31.78,
       height = 13,
       units = 'cm')


# Feb 2023 HONO diurnal with PSS ------------------------------------------

diurnal = pss_dat %>% 
  mutate(hono_para_err = hono_para *0.1) %>% 
  rename(HONO = hono_ppt) %>% 
  filter(is.na(HONO) == FALSE,
         year == 2023) %>% 
  timeVariation(pollutant = c("HONO","hono_err","hono_para","hono_para_err"))

diurnal_dat = diurnal$data$hour %>% 
  ungroup() %>% 
  pivot_wider(names_from = variable,values_from = Mean) %>% 
  group_by(hour) %>% 
  summarise(HONO = mean(HONO,na.rm = T),
            hono_err = mean(hono_err,na.rm = T),
            hono_para = mean(hono_para,na.rm = T),
            hono_para_err = mean(hono_para_err,na.rm = T))


diurnal_dat %>%
  ggplot(aes(hour,HONO)) +
  geom_path(aes(hour,HONO),size = 0.75,col = "steelblue1") +
  geom_ribbon(aes(ymin = HONO - hono_err,ymax = HONO + hono_err),alpha = 0.25,fill = "steelblue1") +
  geom_path(aes(hour,hono_para),size = 0.75,col = "black") +
  geom_ribbon(aes(ymin = hono_para - hono_para_err,ymax = hono_para + hono_para_err),alpha = 0.25,fill = "black") +
  # facet_grid(rows = vars(variable),scales = "free_y",labeller = label_parsed) +
  theme_bw() +
  labs(x = "Hour of day (UTC)",
       y = "HONO (ppt)",
       color = NULL) +
  scale_x_continuous(breaks = c(0,4,8,12,16,20)) +
  # ylim(-1,13) +
  theme(legend.position = "top")

ggsave('hono_para_diurnal.svg',
       path = "output/plots/agu",
       width = 8,
       height = 12.28,
       units = 'cm')

# Feb23 and ARNA data comparison ------------------------------------------

arna_f_mbl = arna_f %>% 
  filter(year == 2020)

model = lm(ratio_hono ~ rh,arna_f_mbl)
summary(model)

arna_feb23 = hourly_f %>% 
  select(date,nitrate_ug_m3,hono_ppt,hono_err,rh,f_para,hono_para,ratio_hono,year,f_calc) %>% 
  bind_rows(arna_f) %>% 
  mutate(campaign = case_when(year == 2023 ~ "February 2023 campaign",
                              TRUE ~ "Previous campaigns at the CVAO"),
         rh_se = rh * 2 /100,
         hono_para_err = hono_para *0.1)

arna_feb23 %>% 
  mutate(f_calc_err = f_calc * 0.1) %>% 
  ggplot(aes(rh,f_calc,col = nitrate_ug_m3)) +
  theme_bw() +
  facet_wrap(~campaign,scale = "free_x") +
  geom_point() +
  geom_smooth(data = subset(arna_feb23,campaign == "Previous campaigns at the CVAO"),method = "lm",se=F,col = "black") +
  labs(x = "RH (%)",
       y = "EF from missing HONO",
       col = "Nitrate") +
  geom_linerange(aes(xmax = rh + 2,xmin = rh - 2)) +
  geom_linerange(aes(ymax = f_calc + f_calc_err,ymin = f_calc - f_calc_err)) +
  scale_colour_gradient2(low = "darkorange",mid = "slategray1",high = "steelblue1",midpoint = 2) +
  # geom_linerange(aes(ymax = ratio_hono + ratio_err,ymin = ratio_hono - ratio_err)) +
  # scale_colour_manual(values = c("darkorange","black","steelblue1"),
  #                     breaks = c("ARNA 2019 flight campaign","ARNA 2020 flight campaign","February 2023 ground campaign")) +
  NULL

ggsave('f_calc_vs_rh.svg',
       path = "output/plots/agu",
       width = 23.43,
       height = 10,
       units = 'cm')


# Daytime hourly rh vs hono ratio -----------------------------------------
 
#not used in agu presentation

hourly_f %>% 
  mutate(rh_se = rh * 2 /100,
         hono_para_err = hono_para *0.1,
         ratio_err = ratio_hono * sqrt((hono_err/hono_ppt)^2 + (hono_para_err/hono_para)^2)) %>% 
  ggplot(aes(rh,ratio_hono)) +
  theme_bw() +
  theme(legend.position = "top") +
  geom_point(col = "steelblue1") +
  geom_linerange(aes(xmax = rh + rh_se,xmin = rh - rh_se),col = "steelblue1") +
  geom_linerange(aes(ymax = ratio_hono + ratio_err,ymin = ratio_hono - ratio_err),col = "steelblue1") +
  geom_smooth(method = "lm",se=F,col = "black") +
  labs(x = "RH (%)",
       y = "Measured HONO / Parameterised HONO",
       col = NULL)

arna_f %>% 
  mutate(rh_se = rh * 2 /100,
         hono_para_err = hono_para *0.1,
         ratio_err = ratio_hono * sqrt((hono_err/hono_ppt)^2 + (hono_para_err/hono_para)^2)) %>% 
  ggplot(aes(rh,ratio_hono)) +
  theme_bw() +
  theme(legend.position = "top") +
  geom_point(col = "darkorange") +
  geom_linerange(aes(xmax = rh + rh_se,xmin = rh - rh_se),col = "darkorange") +
  geom_linerange(aes(ymax = ratio_hono + ratio_err,ymin = ratio_hono - ratio_err),col = "darkorange") +
  geom_smooth(method = "lm",se=F,col = "black") +
  labs(x = "RH (%)",
       y = "Measured HONO / Parameterised HONO",
       col = NULL)

ggsave('ratio_vs_rh_feb23.svg',
       path = "output/plots/agu",
       width = 11.75,
       height = 10,
       units = 'cm')

# HONO ratio --------------------------------------------------------------

#not used in agu presentation

daily_f %>% 
  ggplot(aes(rh,ratio_hono)) +
  theme_bw() +
  geom_smooth(method = "lm",se=F,col = "darkorange") +
  geom_point() +
  labs(x = "RH (%)",
       y = "Measured HONO / Parameterised HONO",
       col = expression(Nitrate~(ug~m^{-3}))) +
  scale_colour_gradient(low = "darkorange",high = "steelblue1") +
  NULL

daily_f %>%
  pivot_longer(c(nitrate_ug_m3,rh)) %>% 
  ggplot(aes(value,ratio_hono)) +
  theme_bw() +
  # geom_smooth(method = "lm",se=F,col = "black") +
  geom_point() +
  labs(y = "Measured HONO / Parameterised HONO") +
  scale_colour_gradient(low = "darkorange",high = "steelblue1") +
  facet_wrap(~name,scales = "free_x",
             strip.position = "bottom", 
             labeller = as_labeller(c(nitrate_ug_m3 = "Nitrate (ug/m3)", rh = "RH (%)") ) )  +
  xlab(NULL) +
  theme(strip.background = element_blank(),
        strip.placement = "outside") +
  NULL

model = lm(ratio_hono ~ rh,daily_f)
summary(model)

ggsave('ratio_vs_nitrate_rh.svg',
       path = "output/plots/agu",
       width = 16,
       height = 13,
       units = 'cm')

# Enhancement factors -----------------------------------------------------

#not used in agu

daily_f %>% 
  rename("Parameteriesed f" = f_para,
         "f calculated from missing HONO" = f_calc) %>%
  pivot_longer(c("Parameteriesed f","f calculated from missing HONO")) %>% 
  ggplot(aes(nitrate_ug_m3,value,col = rh)) +
  theme_bw() +
  geom_point() +
  labs(y = "Enhancement factor",
       x = expression(Nitrate~(ug~m^{-3})),
       col = "RH (%)") +
  facet_wrap(vars(name),scales = "free_y",ncol = 1) +
  scale_colour_gradient(low = "darkorange",high = "steelblue1") +
  NULL

ggsave('f_vs_nitrate.svg',
       path = "output/plots/agu",
       width = 17,
       height = 9.6,
       units = 'cm')