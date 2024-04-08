library(tidyverse)
library(lubridate)
library(janitor)
library(zoo)
library(openair)

Sys.setenv(TZ = 'UTC')

#creates the parameterisatin df - df that has all the info necessary to look at parameterised f
#then looking at f for all three campaigns (2015, 2019 and 2023)
#the most up to date code for the parameterisation is in parameterisation23
#but it is only used for 2023 data

# Constants ---------------------------------------------------------------

kp = 3.3 * 10^-11 #rate constant for oh + no -> hono (from Atkinson et al. 2004) in cm3 molecule-1 s-1
kl = 6 * 10^-12 #rate constant for oh + hono -> h2o + no2 (from Atkinson et al. 2004) in cm3 molecule-1 s-1
dv = 0.3 #deardroff velocity, value used by Simone, in m s-1

# Functions ---------------------------------------------------------------

standardise <- function(x){
  y = (x-mean(x,na.rm = T))/sd(x,na.rm = T)

}

# Creating parameterisation df ---------------------------------------------

# dataset from Simone, who got it from Roberto,contains majority of info needed for 2015 parameterisation
# no data is different from current 2015 df
og_dat15 = read.csv("data/aerosol_data/cv_data_2015.csv") %>%
  tibble() %>%
  mutate(date = ymd_hms(date)) %>%
  clean_names() %>%
  rename(o3_ppb = o3_mean_ppb,
         co_ppb = co_mean_ppb) %>%
  select(-c(hono_ppt,no_ppt:jhno3,flag_met,o3_std_ppb,co_std_ppb,flag_o3,flag_co))

#getting oh, nox and air mass data from master df - nox data wrong in dataset above
newer_dat15 = read.csv("output/data/all_data_utc.csv") %>%
  tibble() %>%
  mutate(date = ymd_hms(date)) %>%
  filter(date > "2015-11-22 23:00" & date < "2015-12-05") %>%
  select(date,hono,no:oh,jhono:jo1d,upwelling:south_atlantic)

#creating final dataset for parameterisation for 2015
final_dat15 = left_join(og_dat15,newer_dat15,by = "date") %>%
  mutate(hour = hour(date),
         day = day(date),
         year = year(date),
         month = month(date)) %>%
  select(date,hono,no,no2,oh,nitrate_ug_m3,jhono,jhno3,jno2,jo1d,o3_ppb,co_ppb,rh,upwelling:south_atlantic,mass_ug_m3:calcium_ug_m3)

#dataset from Simone, who got it from Roberto, contains majority of info needed for 2019 parameterisation
og_dat19 = read.csv("data/aerosol_data/cv_data_2019.csv") %>%
  tibble() %>%
  filter(date > "2019-08-15") %>%
  mutate(date = ymd_hms(date)) %>%
  clean_names() %>%
  rename(o3_ppb = o3_mean_ppb,
         co_ppb = co_mean_ppb) %>%
  select(-c(hono_ppt,no_ppt:jhno3,flag_met,o3_std_ppb,co_std_ppb,flag_o3,flag_co))

#getting oh, nox and air mass data from master df - nox data wrong in dataset above
newer_dat19 = read.csv("output/data/all_data_utc.csv") %>%
  tibble() %>%
  mutate(date = ymd_hms(date)) %>%
  filter(campaign == "August 2019") %>%
  select(date,hono,no:oh,jhono:jo1d,upwelling:south_atlantic)

#read in ozone data for 2019
# ozone_met = read.csv("data/2006-2022_Met_O3_data.csv") %>%
#   tibble() %>%
#   mutate(date = ymd_hms(date)) %>%
#   clean_names() %>%
#   select(date,o3_mean_ppb = o3)
#
# df_list = list(newer_dat19,og_dat19,ozone_met)

final_dat19 = left_join(newer_dat19,og_dat19,by = "date") %>%
  # df_list %>%
  # reduce(left_join,by = "date") %>%
  mutate(hour = hour(date),
         day = day(date),
         year = year(date),
         month = month(date)) %>%
  select(date,hono,no,no2,oh,nitrate_ug_m3,jhono,jhno3,jno2,jo1d,o3_ppb,co_ppb,rh,upwelling:south_atlantic,mass_ug_m3:calcium_ug_m3)

dat23 = read.csv("output/data/all_data_utc.csv") %>%
  tibble() %>%
  fill(nitrate_ug_m3,.direction = "updown") %>%
  mutate(date = ymd_hms(date)) %>%
  filter(campaign == "February 2023") %>%
  select(date,hono,hono_err,no,no2,oh,nitrate_ug_m3,jhono,jhno3,jno2,jo1d,rh,upwelling:south_atlantic)

o3_co_23 = read.csv("data/CO_O3_Feb23.csv") %>%
  tibble() %>%
  mutate(date = dmy_hm(date),
         date = round_date(date,"1 hour")) %>%
  select(date,o3_ppb = O3_ppbV,co_ppb = CO_ppbV) %>%
  filter(date > "2023-02-07" & date < "2023-02-27")

all23 = dat23 %>% left_join(o3_co_23,by = "date")

#calculating seasalt and non-seasalt aerosol composition based on values Rosie gave me
dat = bind_rows(final_dat15,final_dat19,all23) %>% arrange(date) %>%
  fill(c(upwelling:south_atlantic),.direction = "down") %>%
  mutate(hour = hour(date),
         day = day(date),
         year = year(date),
         month = month(date),
         ocean = north_atlantic + south_atlantic + upwelling,
         ss_ca = sodium_ug_m3 * (10.2/468),
         nss_ca = calcium_ug_m3 - ss_ca,
         ss_k = sodium_ug_m3 * (10.2/468),
         nss_k = potassium_ug_m3- ss_k,
         ss_mg = sodium_ug_m3 * (53.2/468),
         nss_mg = magnesium_ug_m3 - ss_mg,
         ss_so4 = sodium_ug_m3 * (28.2/468),
         nss_so4 = sulfate_ug_m3 - ss_so4,
         sea_salt = 1.17 * (sodium_ug_m3 + chloride_ug_m3),
         bb_ratio_k_ca = nss_k/nss_ca,
         bb_ratio_so4_no3 = nitrate_ug_m3/nss_so4,
         aerosol = case_when(nss_ca > 4 & sahara > 0 ~ "dust",
                             bb_ratio_k_ca > 0.24 & bb_ratio_so4_no3 > 1.4 ~ "biomass burning",
                             sea_salt > 11 & ocean > 70 ~ "marine",
                             TRUE ~ "other"))

# write.csv(dat,"output/data/data_parameterisation.csv",row.names = F)

# Reading in data - created in previous section of code -----------------------

dat = read.csv("output/data/data_parameterisation.csv") %>% 
  mutate(date = ymd_hms(date))


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
         f_simone = 385.7/(1+0.19*nitrate_nmol_m3),
         f_para_rh = (10^5/((1+80.07*nitrate_nmol_m3))) * (rh),
         f_para = 103706014.61/(1+83211.37 *nitrate_nmol_m3),
         log_f = log(302297003.43/(1+412107.64*nitrate_nmol_m3)),
         hono_para_rh = ((production_without_nitrate + (jhno3 * nitrate_m_cm3 * f_para_rh)) / (jhono + (kl*oh_m_cm3) + kdep))
         / (2.46 * 10^19 * 10^-12),
         hono_para_simone = ((production_without_nitrate + (jhno3 * nitrate_m_cm3 * f_simone)) / (jhono + (kl*oh_m_cm3) + kdep))
         / (2.46 * 10^19 * 10^-12),
         hono_para = ((production_without_nitrate + (jhno3 * nitrate_m_cm3 * f_para)) / (jhono + (kl*oh_m_cm3) + kdep))
         / (2.46 * 10^19 * 10^-12),#converts hono from molecules cm-3 s-1 to ppt
         ratio_hono_rh = hono_ppt/hono_para_rh,
         ratio_hono = hono_ppt/hono_para,
         ratio_hono_simone = hono_ppt/hono_para_simone) 

daily_f = hourly_f %>% 
  timeAverage("1 day")
  # select(date,nitrate_ug_m3,rh,jhno3,no_ppt,hono_ppt,hono_para,f_calc,f_para,ratio_hono)
  # select(date,year,hono_ppt,hono_para,ratio_hono,nitrate_ug_m3,f_calc,f_para,jhono,jhno3,everything())

# write.csv(daily_para,"output/data/parameterised23.csv",row.names = F)

# Using daily f to calculate pss hono -------------------------------------

only_daily_f = daily_f %>% 
  select(date,f_calc,f_para_rh,f_para,ratio_hono_rh,ratio_hono,f_simone,ratio_hono_simone)
  
#using daily f to see hono timeseries
pss_dat = dat %>%
  filter(year == 2023) %>%
  rename(hono_ppt = hono,oh_m_cm3 = oh,no_ppt = no,no2_ppt = no2) %>% 
  left_join(only_daily_f,by = "date") %>% 
  fill(c(f_calc,f_para_rh,f_para,f_simone,nitrate_ug_m3,upwelling:south_atlantic),.direction = "updown") %>%
  mutate(lifetime = ifelse(hour >= 11 & hour <= 15,1/jhono,NA_real_)) %>% 
  fill(lifetime,.direction = "updown") %>%
  mutate(h = lifetime * dv,
         kdep = 0.01/h,
         nitrate_m_cm3 = (nitrate_ug_m3 * 10^-12 *6.022 * 10^23)/62.004,
         production_without_nitrate = kp*oh_m_cm3*no_ppt * 2.46 * 10^19 * 10^-12,
         loss = (jhono + (kl*oh_m_cm3) + kdep) * hono_ppt * 2.46 * 10^19 * 10^-12,
         missing_production = (loss - production_without_nitrate),
         hono_para_rh = ((kp*oh_m_cm3*no_ppt * 2.46 * 10^19 * 10^-12 + (jhno3 * nitrate_m_cm3 * f_para_rh)) / (jhono + (kl*oh_m_cm3) + kdep))
         / (2.46 * 10^19 * 10^-12),
         hono_para = ((kp*oh_m_cm3*no_ppt * 2.46 * 10^19 * 10^-12 + (jhno3 * nitrate_m_cm3 * f_para)) / (jhono + (kl*oh_m_cm3) + kdep))
         / (2.46 * 10^19 * 10^-12),
         hono_para_simone = ((kp*oh_m_cm3*no_ppt * 2.46 * 10^19 * 10^-12 + (jhno3 * nitrate_m_cm3 * f_simone)) / (jhono + (kl*oh_m_cm3) + kdep))
         / (2.46 * 10^19 * 10^-12),
         hono_calc = ((kp*oh_m_cm3*no_ppt * 2.46 * 10^19 * 10^-12 + (jhno3 * nitrate_m_cm3 * f_calc)) / (jhono + (kl*oh_m_cm3) + kdep))
         / (2.46 * 10^19 * 10^-12))
  # select(date,hono_ppt,new_hono_para,old_hono_para,hono_calc,jhno3,jhono,nitrate_ug_m3,f_calc,everything())

# Diurnals ----------------------------------------------------------------

#one campaign per diurnal

diurnal = pss_dat %>% 
  rename(Obs = hono_ppt,Parameterised_old = old_hono_para,Parameterised_new = new_hono_para,Calculated = hono_calc) %>% 
  filter(is.na(Obs) == FALSE) %>% 
  timeVariation(pollutant = c("Obs","Parameterised_new","Parameterised_old","Calculated"),type = "year")

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

ggsave('hono_diurnals_new_para_calc.svg',
       path = "output/plots/parameterisation/diurnals",
       width = 32,
       height = 15,
       units = 'cm')

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
pss_dat %>%
  # mutate(hono_ppt = case_when(day == 18 & year == 2019 ~ NA_real_,
  #                             day == 27 & year == 2019 ~ NA_real_,
  #                             TRUE ~ hono_ppt),
  #        hono_para = case_when(day == 18 & year == 2019 ~ NA_real_,
  #                              day == 27 & year == 2019 ~ NA_real_,
  #                              TRUE ~ hono_para),
  #        hono_para_rh = case_when(day == 18 & year == 2019 ~ NA_real_,
  #                                  day == 27 & year == 2019 ~ NA_real_,
  #                                  TRUE ~ hono_para_rh),
  #        hono_calc = case_when(day == 18 & year == 2019 ~ NA_real_,
  #                              day == 27 & year == 2019 ~ NA_real_,
  #                              TRUE ~ hono_calc)) %>%
  # filter(date > "2015-11-25") %>%
  rename('Parameterised with RH' = hono_para_rh,
         'Parameterised PSS' = hono_para,
         'Measured' = hono_ppt,
         'Calculated PSS' = hono_calc) %>%
  pivot_longer(c("Measured","Parameterised PSS")) %>%
  ggplot(aes(date,value,col = name)) +
  theme_bw() +
  labs(x = "Datetime (UTC)",
       y = "HONO (ppt)",
       col = NULL) +
  geom_path(size = 0.8) +
  theme(legend.position = "top") +
  scale_x_datetime(breaks = "2 day",date_labels = "%d/%m/%y") +
  # scale_color_viridis_c() +
  # facet_wrap(vars(name),ncol = 1,scales = "free") +
  NULL

ggsave('hono_pss_23.svg',
       path = "output/plots/parameterisation/timeseries",
       width = 32,
       height = 15,
       units = 'cm')

# February 2023 data ------------------------------------------------------

daily_f_feb23 = daily_f %>% 
  filter(year == 2023)

daily_f_feb23 %>% 
  rename("Parameterised f" = f_para,
         "From missing HONO" = f_calc) %>% 
  pivot_longer(c("Parameterised f","From missing HONO")) %>% 
  ggplot(aes(rh,ratio_hono)) +
  theme_bw() +
  geom_point() +
  geom_smooth(method = "lm",se=F) +
  labs(col = expression(Nitrate~(ug~m^{-3})),
       y = "Measured HONO / Parameterised HONO",
       x = "RH (%)") +
  # facet_wrap(vars(name),ncol = 1) +
  # theme(legend.position = "top") +
  scale_colour_viridis_c()

ggsave('rh_hono_ratio.svg',
       path = "output/plots/parameterisation/feb23",
       width = 32,
       height = 15,
       units = 'cm')

model = lm(ratio_hono ~ rh,daily_f_feb23)

summary(model)

daily_f_feb23 %>% 
  pivot_longer(c(f_para,f_para_rh,f_simone)) %>% 
  ggplot(aes(nitrate_ug_m3,value,col = rh,shape = name)) +
  geom_point() +
  scale_colour_viridis_c()

# Correlation between standardised HONO ratio and dust tracers --------

standard19 = daily_f %>% 
  filter(year == 2019) %>% 
  mutate(ratio_standard = standardise(ratio_hono),
         hono_para_err = hono_para *0.1,
         ratio_err = ratio_hono * sqrt((hono_err/hono_ppt)^2 + (hono_para_err/hono_para)^2),
         err_standard = standardise(ratio_err))
standard15 = daily_f %>% 
  filter(year == 2015) %>% 
  mutate(ratio_standard = standardise(ratio_hono),
         hono_para_err = hono_para *0.1,
         ratio_err = ratio_hono * sqrt((hono_err/hono_ppt)^2 + (hono_para_err/hono_para)^2),
         err_standard = standardise(ratio_err))

dust_tracers = bind_rows(standard15,standard19) %>% 
  mutate()

dust_tracers %>% 
  mutate(year = as.character(year)) %>% 
  rename("NSS Ca" = nss_ca,
         "NSS K" = nss_k,
         "Sea salt" = sea_salt) %>% 
  pivot_longer(c("NSS Ca","NSS K","Sea salt")) %>% 
  ggplot(aes(value,ratio_standard,fill = year,shape = year,col = year)) +
  geom_point() +
  scale_colour_manual(values = c("darkorange","steelblue1"),
                    breaks = c("2019","2015")) +
  scale_shape_manual(breaks = c("2019","2015"),values = c(21,24)) +
  scale_fill_manual(values = c("darkorange","steelblue1"),
                    breaks = c("2019","2015")) +
  labs(x = expression(Aerosol~composition~(ug/m^{3})),
       y = "Normalised HONO measured / HONO parameterised",
       col = NULL,
       shape = NULL,
       fill = NULL) +
  theme_bw() +
  theme(legend.position = "top") +
  # geom_linerange(aes(ymax = ratio_standard + ratio_err,ymin = ratio_standard - ratio_err)) +
  facet_wrap(vars(name),scales = "free_x") +
  NULL

ggsave('hono_ratio_dust_tracers.svg',
       path = "output/plots/agu",
       width = 31.78,
       height = 11.42,
       units = 'cm')

# Correlation between hono ratio and others -------------------------------

#standardising hono ratio for each campaign

standard23 = daily_f %>% 
  filter(year == 2023) %>% 
  mutate(ratio_standard = standardise(new_ratio_hono))
standard19 = daily_f %>% 
  filter(year == 2019) %>% 
  mutate(ratio_standard = standardise(new_ratio_hono))
standard15 = daily_f %>% 
  filter(year == 2015) %>% 
  mutate(ratio_standard = standardise(new_ratio_hono))

ratio_standard = bind_rows(standard15,standard19,standard23)
#remove(standard23,standard19,standard15)

#df for correlation plot
cor = daily_f %>% 
  filter(year == 2023) %>% 
  select(date,ratio_hono,jhono,upwelling:south_atlantic,no_ppt,no2_ppt,nitrate_ug_m3,rh,o3_ppb,co_ppb,jhno3)

corPlot(cor)

cor %>% 
  pivot_longer(c(no_ppt,no2_ppt)) %>% 
  ggplot(aes(rh,new_ratio_hono)) +
  geom_point() +
  # facet_wrap(vars(name),scales = "free",ncol = 1) +
  NULL

ggsave('rh_hono_ratio.svg',
       path = "output/plots/parameterisation/feb23",
       width = 32,
       height = 15,
       units = 'cm')


# Comparing normalised values ---------------------------------------------

standard = daily_f %>% 
  mutate(f_calc = standardise(f_calc),
         new_f_para = standardise(new_f_para),
         old_f_para = standardise(old_f_para))

standard %>% 
  pivot_longer(c(f_calc,new_f_para,old_f_para)) %>%
  ggplot(aes(date,value,col =  name)) +
  geom_path() +
  geom_point() +
  facet_wrap(vars(year),ncol = 1,scales = "free_x")

ggsave('normalised_f.svg',
       path = "output/plots/parameterisation",
       width = 32,
       height = 15,
       units = 'cm')

# NO2 uptake --------------------------------------------------------------

v = 367 #NO2 mean thermal velocity, in ms-1 calculated as sqrt(8RT/piM)
gamma = 10^-4 #uptake coefficient for NO2 hydrolysis, 10^-4 for sea-salt/dust aerosols, 10^-5 for biomass burning/soot

surface_area = read.csv("data/arna_hono/Renoxification_data_for_Anna.csv") %>% 
  mutate(date = ymd_hms(Start_time)) %>% 
  select(date,sa = Surface_area.um.2.cc.,altitude = Altitude_m) %>% 
  filter(altitude < 1000) %>%
  mutate(sa_m = sa*10^-6) #sorting units
https://github.com/wacl-york/WACLaTeX/blob/main/thesis/thesis_master.tex
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

ggsave('no2_timeseries.svg',
       path = "output/plots/timeseries",
       width = 30,
       height = 12,
       units = 'cm')


# Using jno2 instead of jhno3 ---------------------------------------------

jnorm = dat %>% 
  rename(hono_ppt = hono,oh_m_cm3 = oh,no_ppt = no,no2_ppt = no2) %>%
  mutate(a = (max(jhno3,na.rm = T) - min(jhno3,na.rm = T))/(max(jno2,na.rm = T) - min(jno2,na.rm = T)),
         b = max(jhno3,na.rm = T) - (a * max(jno2,na.rm = T)),
         jno2_norm = a * jno2 + b) %>% 
  filter(hour >= 11 & hour <= 15) %>%  #only looking at daytime values
  mutate(lifetime = 1/jhono,
         h = lifetime * dv,
         kdep = 0.01/h,
         nitrate_m_cm3 = (nitrate_ug_m3 * 10^-12 *6.022 * 10^23)/62.004,
         production_without_nitrate = kp*oh_m_cm3*no_ppt * 2.46 * 10^19 * 10^-12,
         loss = (jhono + (kl*oh_m_cm3) + kdep) * hono_ppt * 2.46 * 10^19 * 10^-12,
         missing_production = (loss - production_without_nitrate), #molecule cm-3 s-1
         f_para = 10^5/((1+80.07*nitrate_ug_m3)*rh),
         hono_para_no2 = ((production_without_nitrate + (jno2_norm * nitrate_m_cm3 * f_para)) / (jhono + (kl*oh_m_cm3) + kdep))
         / (2.46 * 10^19 * 10^-12),
         hono_para_hno3 = ((production_without_nitrate + (jhno3 * nitrate_m_cm3 * f_para)) / (jhono + (kl*oh_m_cm3) + kdep))
         / (2.46 * 10^19 * 10^-12), #converts hono from molecules cm-3 s-1 to ppt
         hono_ratio_no2 = hono_ppt/hono_para_no2,
         hono_ratio_hno3 = hono_ppt/hono_para_hno3) %>%
  timeAverage("1 day") %>% 
  filter(is.na(hono_ppt) == F) %>% 
  select(date,jhno3,jno2_norm,f_para,hono_ppt,hono_para_hno3,hono_para_no2,hono_ratio_no2,hono_ratio_hno3,everything())

# write.csv(jnorm,"output/data/red_parameterised19.csv",row.names = F)

f_para_no2 = jnorm %>% 
  select(date,f_para)

#using daily f to see hono timeseries
hono_dat = dat %>%
  mutate(a = (max(jhno3,na.rm = T) - min(jhno3,na.rm = T))/(max(jno2,na.rm = T) - min(jno2,na.rm = T)),
         b = max(jhno3,na.rm = T) - (a * max(jno2,na.rm = T)),
         jno2_norm = a * jno2 + b) %>% 
  rename(hono_ppt = hono,oh_m_cm3 = oh,no_ppt = no,no2_ppt = no2) %>% 
  left_join(f_para_no2,by = "date") %>% 
  fill(c(f_para,nitrate_ug_m3,upwelling:south_atlantic),.direction = "down") %>%
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
         hono_para_no2 = ((kp*oh_m_cm3*no_ppt * 2.46 * 10^19 * 10^-12 + (jno2_norm * nitrate_m_cm3 * f_para)) / (jhono + (kl*oh_m_cm3) + kdep))
         / (2.46 * 10^19 * 10^-12)) %>% 
  select(date,hono_ppt,hono_para,hono_para_no2,jhno3,jno2_norm,jhono,nitrate_ug_m3,f_para,everything())

hono_dat %>%
  filter(year == 2023) %>%
  rename('Parameterised PSS with normalisation' = hono_para_no2,
         'Parameterised PSS' = hono_para,
         'Observed' = hono_ppt) %>%
  pivot_longer(c('Observed','Parameterised PSS with normalisation',"Observed",'Parameterised PSS')) %>%
  ggplot(aes(date,value,col = name)) +
  theme_bw() +
  labs(x = "Datetime (UTC)",
       y = "HONO (ppt)",
       col = NULL) +
  geom_path() +
  theme(legend.position = "top") +
  scale_x_datetime(breaks = "1 day",date_labels = "%d/%m/%y") +
  scale_color_viridis_d() +
  facet_wrap(vars(year),ncol = 1,scales = "free") +
  NULL

ggsave('diurnal_jno2norm.svg',
       path = "output/plots/parameterisation/feb23",
       width = 30,
       height = 12,
       units = 'cm')

#diurnal

diurnal = hono_dat %>% 
  filter(year == 2023,
         is.na(hono_ppt) == FALSE) %>%
  rename('Parameterised PSS with normalisation' = hono_para_no2,
         'Parameterised PSS' = hono_para,
         'Observed' = hono_ppt) %>%
  timeVariation(pollutant = c("Parameterised PSS with normalisation","Parameterised PSS","Observed"))

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
  theme(legend.position = "top")

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





