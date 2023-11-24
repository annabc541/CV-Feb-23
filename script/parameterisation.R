library(tidyverse)
library(lubridate)
library(janitor)
library(zoo)
library(openair)

Sys.setenv(TZ = 'UTC')

#calculating daily f both from measured hono data and from parameterisation

# Constants ---------------------------------------------------------------

kp = 3.3 * 10^-11 #rate constant for oh + no -> hono (from Atkinson et al. 2004) in cm3 molecule-1 s-1
kl = 6 * 10^-12 #rate constant for oh + hono -> h2o + no2 (from Atkinson et al. 2004) in cm3 molecule-1 s-1
dv = 0.3 #deardroff velocity, value used by Simone, in m s-1

# Functions ---------------------------------------------------------------

standardise <- function(x){
  y = (x-mean(x,na.rm = T))/sd(x,na.rm = T)

}


# Reading in data - created in bottom section of df -----------------------

dat = read.csv("output/data/data_parameterisation.csv") %>% 
  mutate(date = ymd_hms(date))

# Calculating daily enhancement factors -----------------------------------

#all calculations performed in molecules per cm3 and s-1

daily_f = dat %>% 
  rename(hono_ppt = hono,oh_m_cm3 = oh,no_ppt = no,no2_ppt = no2) %>% 
  filter(hour >= 11 & hour <= 15) %>% #only looking at daytime values
  mutate(lifetime = 1/jhono,
         h = lifetime * dv,
         kdep = 0.01/h,
         nitrate_m_cm3 = (nitrate_ug_m3 * 10^-12 *6.022 * 10^23)/62.004,
         production_without_nitrate = kp*oh_m_cm3*no_ppt * 2.46 * 10^19 * 10^-12,
         loss = (jhono + (kl*oh_m_cm3) + kdep) * hono_ppt * 2.46 * 10^19 * 10^-12,
         missing_production = (loss - production_without_nitrate), #molecule cm-3 s-1
         f_calc = missing_production/(jhno3 * nitrate_m_cm3),
         f_para = 10^5/((1+80.07*nitrate_ug_m3)*rh),
         ratio_f = f_calc/f_para,
         hono_para = ((production_without_nitrate + (jhno3 * nitrate_m_cm3 * f_para)) / (jhono + (kl*oh_m_cm3) + kdep))
         / (2.46 * 10^19 * 10^-12), #converts hono from molecules cm-3 s-1 to ppt
         ratio_hono = hono_ppt/hono_para) %>% 
  timeAverage("1 day") %>%
  filter(is.na(hono_ppt) == F) %>% 
  # select(date,nitrate_ug_m3,rh,jhno3,no_ppt,hono_ppt,hono_para,f_calc,f_para,ratio_hono)
  select(date,year,hono_ppt,hono_para,ratio_hono,nitrate_ug_m3,f_calc,f_para,jhono,jhno3,everything())

# write.csv(daily_para,"output/data/parameterised23.csv",row.names = F)

# Using daily f to calculate pss hono -------------------------------------

only_daily_f = daily_f %>% 
  select(date,f_calc,f_para,ratio_hono)
  
#using daily f to see hono timeseries
pss_dat = dat %>%
  rename(hono_ppt = hono,oh_m_cm3 = oh,no_ppt = no,no2_ppt = no2) %>% 
  left_join(only_daily_f,by = "date") %>% 
  fill(c(f_calc,f_para,nitrate_ug_m3,ratio_hono,upwelling:south_atlantic),.direction = "down") %>%
  mutate(lifetime = ifelse(hour >= 11 & hour <= 15,1/jhono,NA_real_)) %>% 
  fill(lifetime,.direction = "updown") %>%
  mutate(h = lifetime * dv,
         kdep = 0.01/h,
         nitrate_m_cm3 = (nitrate_ug_m3 * 10^-12 *6.022 * 10^23)/62.004,
         production_without_nitrate = kp*oh_m_cm3*no_ppt * 2.46 * 10^19 * 10^-12,
         loss = (jhono + (kl*oh_m_cm3) + kdep) * hono_ppt * 2.46 * 10^19 * 10^-12,
         missing_production = (loss - production_without_nitrate),
         hono_para = ((kp*oh_m_cm3*no_ppt * 2.46 * 10^19 * 10^-12 + (jhno3 * nitrate_m_cm3 * f_para)) / (jhono + (kl*oh_m_cm3) + kdep))
         / (2.46 * 10^19 * 10^-12),
         hono_calc = ((kp*oh_m_cm3*no_ppt * 2.46 * 10^19 * 10^-12 + (jhno3 * nitrate_m_cm3 * f_calc)) / (jhono + (kl*oh_m_cm3) + kdep))
         / (2.46 * 10^19 * 10^-12)) %>% 
  select(date,hono_ppt,hono_para,hono_calc,ratio_hono,jhno3,jhono,nitrate_ug_m3,f_calc,f_para,everything())

# Diurnals ----------------------------------------------------------------

#one campaign per diurnal

diurnal = pss_dat %>% 
  rename(Obs = hono_ppt,Parameterised = hono_para,Calculated = hono_calc) %>% 
  filter(is.na(Obs) == FALSE) %>% 
  timeVariation(pollutant = c("Obs","Parameterised","Calculated"),type = "year")

diurnal_dat = diurnal$data$hour

diurnal_dat %>% 
  ggplot(aes(hour,Mean,col = variable)) +
  geom_line(size = 1) +
  scale_colour_viridis_d() +
  theme_bw() +
  labs(x = "Hour of day (UTC)",
       y = "HONO (ppt)",
       color = NULL) +
  scale_x_continuous(breaks = c(0,4,8,12,16,20)) +
  theme(legend.position = "top") +
  facet_wrap(~year)

# ggsave('hono_diurnals_para_calc.svg',
#        path = "output/plots/parameterisation/diurnals",
#        width = 32,
#        height = 15,
#        units = 'cm')

#all campaigns in one plot

diurnal = pss_dat %>% 
  rename(HONO = hono_ppt) %>% 
  filter(is.na(HONO) == FALSE) %>% 
  timeVariation(pollutant = "hono_calc",type = "year")

diurnal_dat = diurnal$data$hour

diurnal_dat %>% 
  ggplot(aes(hour,Mean,col = year)) +
  geom_line(size = 1) +
  scale_colour_viridis_d() +
  theme_bw() +
  labs(x = "Hour of day (UTC)",
       y = "HONO (ppt)",
       color = NULL) +
  scale_x_continuous(breaks = c(0,4,8,12,16,20)) +
  theme(legend.position = "top")

# ggsave('hono_diurnals19.svg',
#        path = "output/plots/parameterisation/diurnals",
#        width = 11,
#        height = 12,
#        units = 'cm')


# Timeseries --------------------------------------------------------------

#obs calc and para hono in same facet - timeseries
hono_dat %>%
  mutate(hono_ppt = case_when(day == 18 & year == 2019 ~ NA_real_,
                              day == 27 & year == 2019 ~ NA_real_,
                              TRUE ~ hono_ppt),
         hono_para = case_when(day == 18 & year == 2019 ~ NA_real_,
                               day == 27 & year == 2019 ~ NA_real_,
                               TRUE ~ hono_para),
         hono_calc = case_when(day == 18 & year == 2019 ~ NA_real_,
                               day == 27 & year == 2019 ~ NA_real_,
                               TRUE ~ hono_calc)) %>% 
  filter(year == 2023) %>%
  # filter(date > "2015-11-25") %>%
  rename('Parameterised PSS' = hono_para,
         'Observed' = hono_ppt,
         'Calculated PSS' = hono_calc) %>%
  pivot_longer(c('Parameterised PSS','Observed','Calculated PSS')) %>%
  ggplot(aes(date,value,col = name)) +
  theme_bw() +
  labs(x = "Datetime (UTC)",
       y = "HONO (ppt)",
       col = NULL) +
  geom_path(size = 0.8) +
  theme(legend.position = "top") +
  scale_x_datetime(breaks = "1 day",date_labels = "%d/%m/%y") +
  # scale_color_viridis_d() +
  # facet_wrap(vars(year),ncol = 1,scales = "free") +
  NULL

# ggsave('hono_para23.svg',
#        path = "output/plots/parameterisation",
#        width = 32,
#        height = 15,
#        units = 'cm')


# Correlation between hono ratio and others -------------------------------

#standardising hono ratio for each campaign

standard23 = daily_f %>% 
  filter(year == 2023) %>% 
  mutate(ratio_standard = standardise(ratio_hono))
standard19 = daily_f %>% 
  filter(year == 2019) %>% 
  mutate(ratio_standard = standardise(ratio_hono))
standard15 = daily_f %>% 
  filter(year == 2015) %>% 
  mutate(ratio_standard = standardise(ratio_hono))

ratio_standard = bind_rows(standard15,standard19,standard23)
#remove(standard23,standard19,standard15)

#df for correlation plot
cor = ratio_standard %>% 
  select(date,year,ratio_standard,upwelling:south_atlantic,no_ppt,no2_ppt,nitrate_ug_m3,rh,o3_ppb,co_ppb)

corPlot(cor)

ratio_standard %>% 
  ggplot(aes(co_ppb,ratio_standard,col = as.character(year))) +
  geom_point()

# ggsave('rh_hono_ratio.svg',
#        path = "output/plots/parameterisation/all_campaigns",
#        width = 32,
#        height = 15,
#        units = 'cm')


# NO2 uptake --------------------------------------------------------------

v = 367 #NO2 mean thermal velocity, in ms-1 calculated as sqrt(8RT/piM)
gamma = 10^-4 #uptake coefficient for NO2 hydrolysis, 10^-4 for sea-salt/dust aerosols, 10^-5 for biomass burning/soot

surface_area = read.csv("data/arna_hono/Renoxification_data_for_Anna.csv") %>% 
  mutate(date = ymd_hms(Start_time)) %>% 
  select(date,sa = Surface_area.um.2.cc.,altitude = Altitude_m) %>% 
  filter(altitude < 1000) %>%
  mutate(sa_m = sa*10^-6) #sorting units

sa = max(surface_area$sa_m) #maximum surface area measured
k_hydro = (gamma * sa * v)/4

no2_hydrolysis_pss = dat %>% 
  mutate(no2_hydro = ifelse(year == 2019 & day > 26,NA_real_,k_hydro * no2 * 3600))

#plotting what the max hono produced form this per hour would be
no2_hydrolysis_pss %>% 
  filter(is.na(hono) == F) %>% 
  # mutate(hono_ppt = case_when(day == 18 ~ NA_real_,
  #                             day == 27 & year == 2019 ~ NA_real_,
  #                             TRUE ~ hono_ppt))
  ggplot(aes(date,no2_hydro,col = hono)) +
  theme_bw() +
  geom_point() +
  facet_wrap(vars(year),ncol = 1,scales = "free_x") +
  scale_colour_viridis_c() +
  labs(x = "Datetime (UTC)",
       y = "k[NO2] (ppt h-1)",
       col = "HONO (ppt)") +
  scale_x_datetime(breaks = "2 day",date_labels = "%d/%m")

# ggsave('surface_area.svg',
#        path = "output/plots",
#        width = 30,
#        height = 12,
#        units = 'cm')


# Using jno2 instead of jhno3 ---------------------------------------------

jnorm = dat %>% 
  rename(hono_ppt = hono,oh_m_cm3 = oh,no_ppt = no,no2_ppt = no2) %>%
  mutate(a = (max(jhno3) - min(jhno3))/(max(jno2) - min(jno2)),
         b = max(jhno3) - (a * max(jno2)),
         jno2_norm = a * jno2 + b) %>% 
  filter(hour >= 11 & hour <= 15) %>% #only looking at daytime values
  mutate(lifetime = 1/jhono,
         h = lifetime * dv,
         kdep = 0.01/h,
         nitrate_m_cm3 = (nitrate_ug_m3 * 10^-12 *6.022 * 10^23)/62.004,
         production_without_nitrate = kp*oh_m_cm3*no_ppt * 2.46 * 10^19 * 10^-12,
         loss = (jhono + (kl*oh_m_cm3) + kdep) * hono_ppt * 2.46 * 10^19 * 10^-12,
         missing_production = (loss - production_without_nitrate), #molecule cm-3 s-1
         f_calc_no2 = missing_production/(jno2_norm * nitrate_m_cm3),
         f_calc_hno3 = missing_production/(jhno3 * nitrate_m_cm3),
         f_para = 10^5/((1+80.07*nitrate_ug_m3)*rh),
         ratio_f_no2 = f_calc_no2/f_para,
         ratio_f_hno3 = f_calc_hno3/f_para,
         hono_para_no2 = ((production_without_nitrate + (jno2_norm * nitrate_m_cm3 * f_para)) / (jhono + (kl*oh_m_cm3) + kdep))
         / (2.46 * 10^19 * 10^-12),
         hono_para_hno3 = ((production_without_nitrate + (jhno3 * nitrate_m_cm3 * f_para)) / (jhono + (kl*oh_m_cm3) + kdep))
         / (2.46 * 10^19 * 10^-12), #converts hono from molecules cm-3 s-1 to ppt
         hono_ratio_no2 = hono_ppt/hono_para_no2,
         hono_ratio_hno3 = hono_ppt/hono_para_hno3) %>% 
  timeAverage("1 day") %>%
  filter(is.na(hono_ppt) == F) %>% 
  select(date,jhno3,jno2_norm,f_calc_no2,f_calc_hno3,f_para,hono_ppt,hono_para_hno3,hono_para_no2,hono_ratio_no2,hono_ratio_hno3,everything())

# write.csv(jnorm,"output/data/red_parameterised19.csv",row.names = F)

jnorm %>% 
  mutate(f_ratio = f_calc_no2/f_calc_hno3) %>% 
  rename("HONO PSS with jhno3" = hono_para_hno3,
         "HONO PSS with jno2" = hono_para_no2,
         "Observed HONO" = hono_ppt) %>% 
  pivot_longer(c("HONO PSS with jhno3","HONO PSS with jno2",hono_calc_no2,hono_calc)) %>% 
  ggplot(aes(date,value,col = name)) +
  geom_path() +
  theme_bw() +
  facet_wrap(vars(year),ncol = 1,scales = "free") +
  scale_colour_viridis_d() +
  labs(x = "Date (UTC)",
       y = "HONO (ppt)",
       col = NULL) +
  theme(legend.position = "top")


f_para = jnorm %>% 
  select(date,f_calc_no2,f_calc_hno3,f_para)

#using daily f to see hono timeseries
hono_dat = dat %>%
  rename(hono_ppt = hono,oh_m_cm3 = oh,no_ppt = no,no2_ppt = no2) %>% 
  left_join(f_para,by = "date") %>% 
  fill(c(f_calc_no2,f_calc_hno3,f_para,nitrate_ug_m3,upwelling:south_atlantic),.direction = "down") %>%
  mutate(lifetime = ifelse(hour >= 11 & hour <= 15,1/jhono,NA_real_)) %>% 
  fill(lifetime,.direction = "updown") %>%
  mutate(a = (max(jhno3) - min(jhno3))/(max(jno2) - min(jno2)),
         b = max(jhno3) - (a * max(jno2)),
         jno2_norm = a * jno2 + b,
         h = lifetime * dv,
         kdep = 0.01/h,
         nitrate_m_cm3 = (nitrate_ug_m3 * 10^-12 *6.022 * 10^23)/62.004,
         production_without_nitrate = kp*oh_m_cm3*no_ppt * 2.46 * 10^19 * 10^-12,
         loss = (jhono + (kl*oh_m_cm3) + kdep) * hono_ppt * 2.46 * 10^19 * 10^-12,
         missing_production = (loss - production_without_nitrate),
         hono_para = ((kp*oh_m_cm3*no_ppt * 2.46 * 10^19 * 10^-12 + (jhno3 * nitrate_m_cm3 * f_para)) / (jhono + (kl*oh_m_cm3) + kdep))
         / (2.46 * 10^19 * 10^-12),
         hono_para_no2 = ((kp*oh_m_cm3*no_ppt * 2.46 * 10^19 * 10^-12 + (jno2_norm * nitrate_m_cm3 * f_para)) / (jhono + (kl*oh_m_cm3) + kdep))
         / (2.46 * 10^19 * 10^-12),
         hono_calc = ((kp*oh_m_cm3*no_ppt * 2.46 * 10^19 * 10^-12 + (jhno3 * nitrate_m_cm3 * f_calc_hno3)) / (jhono + (kl*oh_m_cm3) + kdep))
         / (2.46 * 10^19 * 10^-12),
         hono_calc_no2 = ((kp*oh_m_cm3*no_ppt * 2.46 * 10^19 * 10^-12 + (jno2_norm * nitrate_m_cm3 * f_calc_no2)) / (jhono + (kl*oh_m_cm3) + kdep))
         / (2.46 * 10^19 * 10^-12)) %>% 
  select(date,hono_ppt,hono_calc,hono_calc_no2,hono_para,hono_para_no2,jhno3,jno2_norm,jhono,nitrate_ug_m3,f_calc_no2,f_calc_hno3,f_para,everything())

hono_dat %>%
  mutate(hono_ppt = case_when(day == 18 ~ NA_real_,
                              day == 27 & year == 2019 ~ NA_real_,
                              TRUE ~ hono_ppt),
         hono_para = case_when(day == 18 ~ NA_real_,
                               day == 27 & year == 2019 ~ NA_real_,
                               TRUE ~ hono_para),
         hono_calc = case_when(day == 18 ~ NA_real_,
                               day == 27 & year == 2019 ~ NA_real_,
                               TRUE ~ hono_calc),
         hono_calc_no2 = case_when(day == 18 ~ NA_real_,
                               day == 27 & year == 2019 ~ NA_real_,
                               TRUE ~ hono_calc_no2),
         hono_para_no2 = case_when(day == 18 ~ NA_real_,
                               day == 27 & year == 2019 ~ NA_real_,
                               TRUE ~ hono_para_no2)) %>% 
  filter(date > "2015-11-25") %>%
  rename('Parameterised PSS with normalisation' = hono_para,
         'Parameterised PSS' = hono_para_no2,
         'Observed' = hono_ppt,
         'Calculated PSS with normalisation' = hono_calc_no2,
         'Calculated PSS' = hono_calc) %>%
  pivot_longer(c('Observed','Parameterised PSS with normalisation','Calculated PSS','Parameterised PSS')) %>%
  ggplot(aes(date,value,col = name)) +
  theme_bw() +
  labs(x = "Datetime (UTC)",
       y = "HONO (ppt)",
       col = NULL) +
  geom_path(size = 0.8) +
  theme(legend.position = "top") +
  scale_x_datetime(breaks = "1 day",date_labels = "%d/%m/%y") +
  scale_color_viridis_d() +
  facet_wrap(vars(year),ncol = 1,scales = "free") +
  NULL

jnorm %>% 
  filter(year == 2015) %>% 
  pivot_longer(c(jno2_norm,jhno3)) %>% 
  ggplot(aes(date,value,col = name)) +
  geom_path()

# Photo-production --------------------------------------------------------

photo_production = dat %>% 
  rename(hono_ppt = hono,oh_m_cm3 = oh,no_ppt = no,no2_ppt = no2) %>% 
  arrange(date) %>% 
  filter(hour >= 11 & hour <= 15) %>% #only looking at daytime values
  mutate(lifetime = 1/jhono,
         h = lifetime * dv,
         kdep = 0.01/h,
         nitrate_m_cm3 = (nitrate_ug_m3 * 10^-12 *6.022 * 10^23)/62.004,
         production_without_nitrate = kp*oh_m_cm3*no_ppt * 2.46 * 10^19 * 10^-12,
         loss_no_hono = jhono + (kl*oh_m_cm3) + kdep,
         loss = loss_no_hono * hono_ppt * 2.46 * 10^19 * 10^-12,
         missing_production = ifelse(loss > production_without_nitrate,loss - production_without_nitrate,NA_real_),
         f_calc = missing_production/(jhno3 * nitrate_m_cm3),
         f_para = 10^5/((1+80.07*nitrate_ug_m3)*rh),
         ratio_f = f_calc/f_para,
         photo_production_calc = jhno3 * nitrate_m_cm3 * f_calc,
         photo_production_para = jhno3 * nitrate_m_cm3 * f_para,
         production_calc = production_without_nitrate + photo_production_calc,
         production_para = production_without_nitrate + photo_production_para,
         production_perc_calc = photo_production_calc/production_calc,
         hono_para = (production_para / loss_no_hono)
         / (2.46 * 10^19 * 10^-12),
         production_perc_para = photo_production_para/production_para,
         loss_perc = jhono/loss_no_hono,
         ratio_hono = hono_ppt/hono_para) %>% 
  timeAverage("1 day") %>%
  filter(is.na(hono_ppt) == F) %>% 
  select(date,hono_ppt,hono_para,no_ppt,ratio_hono,f_para,f_calc,nitrate_m_cm3:loss_perc,everything(),
       # -c(uva_rad_wm2:co_mean_ppb,mass_ug_m3:dp_um,no2_ppt:day,month:kdep)
       )

photo_production %>% 
  # filter(year == 2015) %>%
  # mutate(diff = ifelse(production_perc_calc>production_perc_para,"1","0")) %>% 
  rename(Calculated = production_perc_calc,
         Parameterised = production_perc_para) %>% 
  pivot_longer(c(Calculated,Parameterised)) %>%
  ggplot(aes(date,value,col = name)) +
  geom_point() +
  theme_bw() +
  theme(legend.position = "top") +
  labs(x = "Datetime (UTC)",
       y = "% production due to photochemistry",
       col = NULL) +
  scale_x_datetime(breaks = "1 day",date_labels = "%d/%m") +
  scale_colour_viridis_d() +
  facet_wrap(vars(year),ncol = 1,scales = "free") +
  NULL




# Reading in and tidying data ---------------------------------------------
# 
# #dataset from Simone, who got it from Roberto (?),contains majority of info needed for 2015 f parameterisation
# #no data is different from current 2015 df - use more up to date data
# og_dat15 = read.csv("data/aerosol_data/cv_data_2015.csv") %>% 
#   tibble() %>% 
#   mutate(date = ymd_hms(date)) %>% 
#   clean_names() %>% 
#   rename(o3_ppb = o3_mean_ppb,
#          co_ppb = co_mean_ppb) %>% 
#   select(-c(hono_ppt,no_ppt:jhno3,flag_met,o3_std_ppb,co_std_ppb,flag_o3,flag_co))
# 
# #getting oh, nox and air mass data from master df - nox data wrong in dataset above
# newer_dat15 = read.csv("output/data/all_data_utc.csv") %>% 
#   tibble() %>% 
#   mutate(date = ymd_hms(date)) %>% 
#   filter(date > "2015-11-22 23:00" & date < "2015-12-05") %>% 
#   select(date,hono,no:oh,jhono:jo1d,upwelling:south_atlantic)
# 
# #creating final dataset for f parameterisation for 2015
# final_dat15 = left_join(og_dat15,newer_dat15,by = "date") %>% 
#   mutate(hour = hour(date),
#          day = day(date),
#          year = year(date),
#          month = month(date)) %>% 
#   select(date,hono,no,no2,oh,nitrate_ug_m3,jhono,jhno3,jno2,jo1d,o3_ppb,co_ppb,rh,upwelling:south_atlantic)
# 
# #dataset from Simone, who got it from Roberto (?),contains majority of info needed for 2019 f parameterisation
# og_dat19 = read.csv("data/aerosol_data/cv_data_2019.csv") %>% 
#   tibble() %>% 
#   mutate(date = ymd_hms(date)) %>% 
#   clean_names() %>% 
#   rename(o3_ppb = o3_mean_ppb,
#          co_ppb = co_mean_ppb) %>%
#   select(-c(hono_ppt,no_ppt:jhno3,flag_met,o3_std_ppb,co_std_ppb,flag_o3,flag_co))
# 
# #getting oh, nox and air mass data from master df - nox data wrong in dataset above
# newer_dat19 = read.csv("output/data/all_data_utc.csv") %>% 
#   tibble() %>% 
#   mutate(date = ymd_hms(date)) %>% 
#   filter(campaign == "August 2019") %>% 
#   select(date,hono,no:oh,jhono:jo1d,upwelling:south_atlantic)
# 
# #read in ozone data for 2019
# # ozone_met = read.csv("data/2006-2022_Met_O3_data.csv") %>% 
# #   tibble() %>% 
# #   mutate(date = ymd_hms(date)) %>% 
# #   clean_names() %>% 
# #   select(date,o3_mean_ppb = o3)
# # 
# # df_list = list(newer_dat19,og_dat19,ozone_met)
# 
# final_dat19 = left_join(og_dat19,newer_dat19,by = "date") %>% 
#   # df_list %>% 
#   # reduce(left_join,by = "date") %>% 
#   mutate(hour = hour(date),
#          day = day(date),
#          year = year(date),
#          month = month(date)) %>% 
#   select(date,hono,no,no2,oh,nitrate_ug_m3,jhono,jhno3,jno2,jo1d,o3_ppb,co_ppb,rh,upwelling:south_atlantic)
# 
# dat23 = read.csv("output/data/all_data_utc.csv") %>% 
#   tibble() %>% 
#   fill(nitrate_ug_m3,.direction = "updown") %>%
#   mutate(date = ymd_hms(date)) %>% 
#   filter(campaign == "February 2023") %>% 
#   select(date,hono,no,no2,oh,nitrate_ug_m3,jhono,jhno3,jno2,jo1d,rh,upwelling:south_atlantic)
# 
# o3_co_23 = read.csv("data/CO_O3_Feb23.csv") %>% 
#   tibble() %>% 
#   mutate(date = dmy_hm(date),
#          date = round_date(date,"1 hour")) %>% 
#   select(date,o3_ppb = O3_ppbV,co_ppb = CO_ppbV) %>% 
#   filter(date > "2023-02-07" & date < "2023-02-27")
# 
# all23 = dat23 %>% left_join(o3_co_23,by = "date")
# 
# dat = bind_rows(final_dat15,final_dat19,all23) %>% arrange(date) %>% 
#   mutate(hour = hour(date),
#          day = day(date),
#          year = year(date),
#          month = month(date))
# 
# write.csv(dat,"output/data/data_parameterisation.csv",row.names = F)
