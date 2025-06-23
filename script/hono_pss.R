library(tidyverse)
library(lubridate)
library(janitor)
library(zoo)
library(openair)
library(ggh4x)
library(ggpubr)
library(plotly)

Sys.setenv(TZ = 'UTC')
setwd("~/Cape Verde/peroxy_campaign")
# conflict_prefer("select",winner = "dplyr")
# conflict_prefer("filter",winner = "dplyr")

#calculates HONO using Matt's parameterisation for enhancement factor (f)

# Constants ---------------------------------------------------------------

kp = 3.3 * 10^-11 #rate constant for oh + no -> hono (from Atkinson et al. 2004) in cm3 molecule-1 s-1
kl = 6 * 10^-12 #rate constant for oh + hono -> h2o + no2 (from Atkinson et al. 2004) in cm3 molecule-1 s-1
dv = 0.3 #deardroff velocity, value used by Simone, in m s-1
oh_error = 0.21
j_error = 0.06
nitrate_error = 0.4

molar_mass = data.frame(
  no3 = 62.004,
  so4 = 96.06,
  cl = 35.45,
  na = 22.99,
  nh4 = 18.039,
  mg = 24.305,
  ca = 40.078,
  k = 39.098
)

# c("no3","so4","cl","na","nh4","mg","ca","k"),
# molar_mass = c(62.004,96.06,35.45,22.99,18.039,24.305,40.078,39.098)

# Functions ---------------------------------------------------------------

standardise <- function(x){
  y = (x-mean(x,na.rm = T))/sd(x,na.rm = T)
  
}
#n/V =  p/RT = 1atm / (0.08206 L atm K-1 mol-1 * 298 K) = 0.0409 mol L-1 = 0.0409 * 10^-3 mol cm-3
#nmol mol-1 * 10^-12 *  6.022 * 10^23 molecules mol-1 * 0.0409 * 10^-3 mol cm-3
#2.46 * 10^7 molecules cm-3 conversion factor
ppt_to_molecules_cm3 <- function(x){y = x * 2.46 * 10^19 * 10^-12}
molecules_cm3_to_ppt <- function(x){y = x / ( 2.46 * 10^19 * 10^-12)}
ppt_to_ug_m3 <- function(x,molar_mass){
  y = x * 2.46 * 10^19 * 10^-12
  z = (y * molar_mass * 10^12) / (6.022 * 10^23)
  z
  }
 ss_aerosol <- function(na,x){y = na * (x/468)}
 nss_aerosol <- function(ss,x){y = x - ss}

NormalisedMeanBias <- function(modelled, observations, na.rm){
  mod <- modelled
  obs <- observations
  
  NMB <- (sum(mod - obs, na.rm = na.rm)/sum(obs, na.rm = na.rm))*100
  NMB
}

# Reading in data -----------------------

#can choose whether to filter by year or to calculate for whole available dataset

dat_sep24 = read.csv("~/Cape Verde/cvao_hono_sep24/output/processed_data/sept24_all_dat.csv") %>% 
  mutate(date = ymd_hms(date),
         year = year(date),
         pollution_flag = case_when(ws <= 2 | wd >= 100 & wd <= 340 ~ "Local pollution (met)",
                                    date > "2024-09-11 03:00" & date < "2024-09-13 15:00" ~ "Local pollution",
                                    TRUE ~ "Baseline"),
         hono = ifelse(pollution_flag == "Baseline",hono,NA_real_),
         hono_err = ifelse(pollution_flag == "Baseline",hono_err,NA_real_)) %>% 
  clean_names() %>% 
  filter(date > "2024-09-08" & date < "2024-09-19") %>% 
  select(-c(no_lod_ppt,no2_lod_ppt,no_flag,no2_flag,oh_molecules_cm3,hono_err_percent))
  

dat = read.csv("output/data/all_data_utc_updated_nox_hono.csv") %>% 
  # filter(year == 2023) %>%
  mutate(date = ymd_hms(date)) %>% 
  bind_rows(dat_sep24) %>% 
  mutate(pollution_flag = ifelse(is.na(pollution_flag),"Baseline",pollution_flag))

#unit conversions and data filled in where necessary
dat_parameterisation = dat %>% 
  group_by(year) %>% 
  fill(pH:phosphate_ug_m3,upwelling:south_atlantic,.direction = "down") %>% 
  ungroup() %>% 
  mutate(oh = case_when(year != 2023 ~ 2 * 10^6,
                        year == 2023 ~ na.approx(oh, na.rm = F)),
         oh_precision = na.approx(oh_precision, na.rm = F),
         nitrate_molecules_cm3 = case_when(year == 2020 ~ 1.20 * 10^10,
                             TRUE ~ (nitrate_ug_m3 * 10^-12 *6.022 * 10^23)/62.004),
         nitrate_nmol_m3 = (nitrate_molecules_cm3 * 10^15)/(6.022*10^23),
         hour = hour(date),
         campaign = case_when(date > "2015-11-24" & date < "2015-12-04" ~ "November 2015",
                              date > "2019-08-15" & date < "2019-08-29" ~ "August 2019",
                              date > "2020-02-14" & date < "2020-02-27" ~ "February 2020",
                              date > "2023-02-07" & date < "2023-02-27" ~ "February 2023",
                              date > "2024-09-08" & date < "2024-09-19" ~ "September 2024")) %>% 
  fill(oh, oh_precision) %>% 
  select(date,hour,month,year,everything()) %>% 
  rename(hono_ppt = hono,oh_molecules_cm3 = oh)

# Calculating enhancement factors and PSS HONO ----------------------------

#all calculations performed in molecules per cm3 and s-1
#specify all units to keep track of things

hourly_pss = dat_parameterisation %>%
  filter(hour >= 11 & hour <= 15) %>%  #only looking at daytime values
  mutate(lifetime = 1/jhono,
         no_percent_error = ifelse(no_ppt >0 & no_u_ppt >0,no_u_ppt/no_ppt,NA_real_),
         hono_percent_error = ifelse(hono_ppt >0 & hono_err >0,hono_err/hono_ppt,NA_real_),
         h = lifetime * dv,
         kdep = 0.01/h,
         production_without_nitrate = kp * oh_molecules_cm3 * ppt_to_molecules_cm3(no_ppt),
         pwc_error = sqrt(oh_error^2 + no_percent_error^2),
         loss = (jhono + (kl*oh_molecules_cm3) + kdep) * ppt_to_molecules_cm3(hono_ppt),
         loss_error = sqrt((j_error)^2 + (oh_error)^2 + (j_error)^2 + (hono_err/hono_ppt)^2),
         missing_production = (loss - production_without_nitrate), #molecule cm-3 s-1
         mp_error = (sqrt((loss*loss_error)^2 + (production_without_nitrate * pwc_error)^2))/missing_production,
         f_calc = missing_production/(jhno3 * nitrate_molecules_cm3),
         f_calc_error = sqrt((mp_error)^2+(j_error)^2 + (nitrate_error)^2),
         f_para_simone = (385.7)/(1 + (0.19 * nitrate_nmol_m3)),
         f_para_matt = (5.02*10^8)/(1 + (7.19 * 10^5 * nitrate_nmol_m3)) #matt's parameterisation new in paper
         # hono_para = molecules_cm3_to_ppt((production_without_nitrate + (jhno3 * nitrate_molecules_cm3 * f_para))
                                          # / (jhono + (kl*oh_molecules_cm3) + kdep))
         )

error_mean = mean(hourly_pss$pwc_error,na.rm = T)
lifetime_median = median(hourly_pss$lifetime,na.rm = T) /60

daily_f = hourly_pss %>% 
  timeAverage("1 day") %>% 
  select(date,f_para_simone,f_para_matt,f_calc,f_calc_error,lifetime) %>% 
  filter(is.na(f_para_simone) == F & is.na(f_para_matt) == F)

daily_pss = dat_parameterisation %>%
  left_join(daily_f,by = "date") %>% 
  # filter(year == 2023) %>% 
  fill(f_para_simone,f_para_matt,f_calc,lifetime,.direction = "down") %>% 
  mutate(h = lifetime * dv,
         kdep = 0.01/h,
         no_percent_error = ifelse(no_ppt >0 & no_u_ppt >0,no_u_ppt/no_ppt,NA_real_),
         hono_percent_error = ifelse(hono_ppt >0 & hono_err >0,hono_err/hono_ppt,NA_real_),
         production_without_nitrate = kp*oh_molecules_cm3 * ppt_to_molecules_cm3(no_ppt),
         pwc_error = sqrt(oh_error^2 + no_percent_error^2),
         loss = (jhono + (kl*oh_molecules_cm3) + kdep) * ppt_to_molecules_cm3(hono_ppt),
         loss_error = sqrt((j_error)^2 + (oh_error)^2 + (j_error)^2 + (hono_err/hono_ppt)^2),
         loss_error1 = sqrt((j_error)^2 + (oh_error)^2 + (j_error)^2),
         missing_production = (loss - production_without_nitrate), #molecule cm-3 s-1
         mp_error = (sqrt((loss*loss_error)^2 + (production_without_nitrate * pwc_error)^2))/missing_production,
         hono_para_simone = molecules_cm3_to_ppt((production_without_nitrate + (jhno3 * nitrate_molecules_cm3 * f_para_simone))
                                          / (jhono + (kl*oh_molecules_cm3) + kdep)),
         hono_para_matt = molecules_cm3_to_ppt((production_without_nitrate + (jhno3 * nitrate_molecules_cm3 * f_para_matt))
                                          / (jhono + (kl*oh_molecules_cm3) + kdep)),
         hono_wn_error = sqrt(pwc_error^2 + loss_error1^2),
         hono_para_error = sqrt(pwc_error^2 + loss_error1^2 + hono_percent_error^2),
         hono_without_nitrate = molecules_cm3_to_ppt((production_without_nitrate + (jhno3 * nitrate_molecules_cm3))
                                                     / (jhono + (kl*oh_molecules_cm3) + kdep)),
         nmb_simone = ifelse(hour >= 11 & hour <= 15,NormalisedMeanBias(hono_para_simone,hono_ppt,na.rm = T),NA_real_),
         nmb_matt = ifelse(hour >= 11 & hour <= 15,NormalisedMeanBias(hono_para_matt,hono_ppt,na.rm = T),NA_real_))

# Plotting timeseries without renoxification -----------------------------------------------------

#hono timeseries - feb23 and sep24 hono with no and pss without renoxification
daily_pss %>% 
  filter(campaign == "February 2023" | campaign == "September 2024") %>%
  mutate(hono_without_nitrate = ifelse(date > "2024-09-11 03:00" & date < "2024-09-12 14:00" | hono_without_nitrate < 0,
                                       NA_real_,hono_without_nitrate),
         no_ppt = ifelse(date > "2024-09-11 03:00" & date < "2024-09-12 14:00",
                                       NA_real_,no_ppt),
         hono_wn_error = hono_without_nitrate * hono_wn_error,
         hono_err_plot_max = hono_ppt + 2 * hono_err,
         hono_err_plot_min = hono_ppt - 2 * hono_err,
         no_err_plot_max = no_ppt + no_u_ppt,
         no_err_plot_min = no_ppt - no_u_ppt,
         hono_wn_err_plot_max = hono_without_nitrate + hono_wn_error,
         hono_wn_err_plot_min = hono_without_nitrate - hono_wn_error) %>% 
  rename(`Measured HONO` = hono_ppt,
         NO = no_ppt,
         `PSS HONO` = hono_without_nitrate) %>% 
  pivot_longer(c(`Measured HONO`,NO,`PSS HONO`)) %>% 
  pivot_longer(cols = c(hono_err_plot_max,no_err_plot_max,hono_wn_err_plot_max),
               values_to = "max_err_v",names_to = "max_err_n") %>% 
  pivot_longer(cols = c(hono_err_plot_min,no_err_plot_min,hono_wn_err_plot_min),
               values_to = "min_err_v",names_to = "min_err_n") %>% 
  mutate(flag = case_when(name == "Measured HONO" & min_err_n == "hono_err_plot_min" & max_err_n == "hono_err_plot_max" ~ "hono",
                          name == "NO" & min_err_n == "no_err_plot_min" & max_err_n == "no_err_plot_max" ~ "no",
                          name == "PSS HONO" & min_err_n == "hono_wn_err_plot_min" & max_err_n == "hono_wn_err_plot_max" ~ "no2")) %>% 
  filter(is.na(flag) == F) %>%
  ggplot(aes(date,value,col = name)) +
  theme_bw() +
  facet_wrap(~campaign,ncol = 1,scales = "free") +
  geom_path(size = 1) +
  labs(x = NULL,
       y = "Mixing ratio (ppt)",
       col = NULL,
       fill = NULL) +
  geom_path(aes(date,value,col = name),size = 0.75) +
  geom_ribbon(aes(date,ymin = min_err_v,ymax = max_err_v,fill = name,col = NULL),alpha = 0.4) +
  scale_colour_manual(values = c("darkorange","steelblue1","navy")) +
  scale_fill_manual(values = c("darkorange","steelblue1","navy")) +
  scale_x_datetime(date_breaks = "2 days",date_labels = "%d %b") +
  theme(legend.position = "top",
        text = element_text(size =  20)) +
  NULL

ggsave('hono_timeseries_pss_no.pdf',
       path = "~/Writing/hono_paper",
       width = 32,
       height = 11,
       units = 'cm')

# Plotting timeseries with para -------------------------------------------

#hono timeseries - feb23 and sep24 pss hono with andersen and rowlinson para pss
#error used for parametrisation is average of hono error from each campaign, 18% for Feb and 61% for Sep
daily_pss %>% 
  filter(campaign == "February 2023" | campaign == "September 2024") %>%
  mutate(hono_para_simone = ifelse(date > "2024-09-11 03:00" & date < "2024-09-12 14:00",
                                       NA_real_,hono_para_simone),
         hono_para_simone = ifelse(hono_para_simone < 0,0,hono_para_simone),
         hono_para_matt = ifelse(hono_para_matt < 0,0,hono_para_matt),
         hono_para_matt = ifelse(date > "2024-09-11 03:00" & date < "2024-09-12 14:00",
                                   NA_real_,hono_para_matt),
         hono_ps_error = ifelse(campaign == "Feburary 2023",
                                hono_para_simone * 0.18,hono_para_simone * 0.61),
         hono_pm_error = ifelse(campaign == "Feburary 2023",
                                hono_para_matt * 0.18,hono_para_matt * 0.61),
         hono_err_plot_max = hono_ppt + 2 * hono_err,
         hono_err_plot_min = hono_ppt - 2 * hono_err,
         hono_ps_err_plot_max = hono_para_simone + hono_ps_error,
         hono_ps_err_plot_min = hono_para_simone - hono_ps_error,
         hono_pm_err_plot_max = hono_para_matt + hono_pm_error,
         hono_pm_err_plot_min = hono_para_matt - hono_pm_error,
         campaign = case_when(campaign == "February 2023" ~ "Feb 2023",
                              campaign == "September 2024" ~ "Sep 2024")) %>% 
  rename(`Measured HONO` = hono_ppt,
         `PSS HONO (Andersen parameterisation)` = hono_para_simone,
         `PSS HONO (Rowlinson parameterisation)` = hono_para_matt) %>% 
  pivot_longer(c(`Measured HONO`,`PSS HONO (Rowlinson parameterisation)`,`PSS HONO (Andersen parameterisation)`)) %>% 
  pivot_longer(cols = c(hono_err_plot_max,hono_pm_err_plot_max,hono_ps_err_plot_max),
               values_to = "max_err_v",names_to = "max_err_n") %>%
  pivot_longer(cols = c(hono_err_plot_min,hono_pm_err_plot_min,hono_ps_err_plot_min),
               values_to = "min_err_v",names_to = "min_err_n") %>%
  mutate(flag = case_when(name == "Measured HONO" & min_err_n == "hono_err_plot_min" & max_err_n == "hono_err_plot_max" ~ "hono",
                          name == "PSS HONO (Rowlinson parameterisation)" & min_err_n == "hono_pm_err_plot_min" & max_err_n == "hono_pm_err_plot_max" ~ "matt",
                          name == "PSS HONO (Andersen parameterisation)" & min_err_n == "hono_ps_err_plot_min" & max_err_n == "hono_ps_err_plot_max" ~ "simone")) %>%
  filter(is.na(flag) == F) %>%
  ggplot(aes(date,value,col = name)) +
  theme_bw() +
  facet_wrap(~campaign,ncol = 1,scales = "free") +
  geom_path(size = 1) +
  labs(x = "Datetime",
       y = "Mixing ratio (ppt)",
       col = NULL,
       fill = NULL) +
  geom_path(aes(date,value,col = name),size = 0.75) +
  geom_ribbon(aes(date,ymin = min_err_v,ymax = max_err_v,fill = name,col = NULL),alpha = 0.1) +
  scale_colour_manual(values = c("darkorange","steelblue1","navy")) +
  scale_fill_manual(values = c("darkorange","steelblue1","navy")) +
  scale_x_datetime(date_breaks = "3 days",date_labels = "%d %b") +
  theme(legend.position = "None",
        # text = element_text(size =  20)
        ) +
  # ylim(0,60) +
  NULL

ggsave('hono_timeseries_pss_para.png',
       path = "~/Writing/hono_paper",
       width = 24,
       height = 10,
       units = 'cm')

# Plotting timeseries -----------------------------------------------------

#hono timeseries - either for comparison with pss or as a stand-alone
daily_pss %>% 
  filter(campaign == "February 2023" | campaign == "September 2024") %>%
  mutate(hono_para_rowlinson = ifelse(hono_para_matt < 0,0,hono_para_matt),
         hono_para_andersen = ifelse(hono_para_simone < 0,0,hono_para_simone),
         hono_without_nitrate = ifelse(hono_without_nitrate < 0,0,hono_without_nitrate)) %>%
  rename(hono_measured = hono_ppt) %>% 
  pivot_longer(c(hono_measured,hono_without_nitrate,no_ppt)) %>%
  ggplot(aes(date,value,col = name)) +
  theme_bw() +
  facet_wrap(~campaign,ncol = 1,scales = "free") +
  geom_path(size = 1) +
  labs(x = NULL,
       y = "Mixing ratio (ppt)",
       col = NULL) +
  # geom_path(aes(date,hono_para),size = 1,col = "darkorange") +
  # geom_ribbon(aes(date,ymin = hono_para - hono_para_err,ymax = hono_para + hono_para_err),alpha = 0.25) +
  # geom_path(aes(date,hono_ppt),
  #           col = "steelblue1",
  #           size = 1) +
  # geom_ribbon(aes(date,ymin = hono_ppt - hono_err,ymax = hono_ppt + hono_err),alpha = 0.25,fill = "steelblue1") +
  # geom_path(aes(date,hono_without_nitrate),size = 1,col = "navyblue") +
  scale_x_datetime(breaks = "1 day",date_labels = "%d/%m") +
  # scale_colour_manual(values = c("darkorange","navyblue","steelblue1"),
  # labels = c("Measured HONO","PSS HONO (Andersen parameterisation)","PSS HONO (Rowlinson parameterisation)")) +
  theme(legend.position = "top",
        text = element_text(size =  20)
        # axis.title = element_text(size = 28),
        # strip.text = element_text(size = 28),
        # axis.text = element_text(size = 20),
        # legend.text = element_text(size = 20)
  ) +
  # facet_grid(rows = vars(name)) +
  # scale_colour_gradient(low = "darkorange",high = "steelblue1") +
  # facet_wrap(~year,ncol = 1,scales = "free_x") +
  NULL

# ggsave('hono_timeseries_pss_no.png',
#        path = "~/Writing/Thesis/Chapter 4 (HONO in CVAO)/Images",
#        width = 37,
#        height = 14,
#        units = 'cm')

# Plotting diurnal --------------------------------------------------------

diurnal = daily_pss %>%
  filter(campaign == "February 2023" | campaign == "September 2024") %>%
  filter(is.na(hono_ppt) == FALSE) %>%
  group_by(hour,campaign) %>%
  select(hono = hono_ppt,simone = hono_para_simone,matt = hono_para_matt) %>% 
  summarise(across(where(is.numeric),list(mean = ~mean(.,na.rm = T),sd = ~sd(.,na.rm = T),se = ~sd(., na.rm = TRUE) / sqrt(length(.))))) %>% 
  ungroup()

diurnal %>% 
  mutate(hono_err_plot_max = hono_mean + hono_se,
         hono_err_plot_min = hono_mean - hono_se,
         simone_err_plot_max = simone_mean + simone_se,
         simone_err_plot_min = simone_mean - simone_se,
         matt_err_plot_max = matt_mean + matt_se,
         matt_err_plot_min = matt_mean - matt_se,
         campaign = case_when(campaign == "February 2023" ~ "Feb 2023",
                              campaign == "September 2024" ~ "Sep 2024")) %>% 
  rename("Measured HONO" = hono_mean,
         "PSS HONO (Andersen parametrisation)" = simone_mean,
         "PSS HONO (Rowlinson parametrisation)" = matt_mean) %>%
  pivot_longer(c("Measured HONO","PSS HONO (Andersen parametrisation)","PSS HONO (Rowlinson parametrisation)")) %>% 
  pivot_longer(cols = c(hono_err_plot_max,simone_err_plot_max,matt_err_plot_max),values_to = "max_err_v",names_to = "max_err_n") %>% 
  pivot_longer(cols = c(hono_err_plot_min,simone_err_plot_min,matt_err_plot_min),values_to = "min_err_v",names_to = "min_err_n") %>% 
  mutate(flag = case_when(name == "Measured HONO" & min_err_n == "hono_err_plot_min" & max_err_n == "hono_err_plot_max" ~ "hono",
                          name == "PSS HONO (Andersen parametrisation)" & min_err_n == "simone_err_plot_min" & max_err_n == "simone_err_plot_max" ~ "simone",
                          name == "PSS HONO (Rowlinson parametrisation)" & min_err_n == "matt_err_plot_min" & max_err_n == "matt_err_plot_max" ~ "matt")) %>% 
  filter(is.na(flag) == F) %>% 
  ggplot() +
  theme_bw() +
  facet_wrap(~campaign,scales = "free") +
  geom_path(aes(hour,value,col = name),size = 0.75) +
  geom_ribbon(aes(hour,ymin = min_err_v,ymax = max_err_v,fill = name),alpha = 0.25) +
  # facet_wrap(~name,scales = "free") +
  scale_colour_manual(values = c("darkorange","steelblue1","navy")) +
  scale_fill_manual(values = c("darkorange","steelblue1","navy")) +
  # scale_x_datetime(date_breaks = "2 days",date_labels = "%d %b") +
  theme(legend.position = "None",
        # text = element_text(size =  20)
        ) +
  scale_x_continuous(breaks = c(0,4,8,12,16,20)) +
  labs(x = "Hour of day (UTC)",
       y = NULL,
       col = NULL,
       fill = NULL)

ggsave('hono_para_diurnal.png',
       path = "~/Writing/hono_paper",
       width = 10,
       height = 10,
       units = 'cm')

# Doing this for ARNA -----------------------------------------------------

#hono data from simone's arna campaign
arna_data = read.csv("data/hono/arna_hono/Renoxification_data_for_Anna_v2.csv") %>% 
  mutate(date = dmy_hm(Start_time),
         year = year(date)) %>% 
  select(date,
         hono = HONO_pptV,
         hono_err = HONO_Uncertainty_ppt,
         no2_ppt = NO2_pptV,
         rh = RH,
         no = NO_pptV,
         jhno3 = J_HNO3,
         jhono = J_HONO,
         altitude = Altitude_m,
         temp_k = Temp_K) %>% 
  mutate(temp = temp_k - 273.15)

arna_aerosol = read.csv("data/aerosol_data/arna_aerosols.csv") %>% 
  clean_names() %>% 
  mutate(date = mdy_hm(start_time)) %>% 
  mutate(nitrate_ug_m3 = ppt_to_ug_m3(no3_ppt,molar_mass$no3),
         nitrate_err = ppt_to_ug_m3(total_no3_uncertainty,molar_mass$no3),
         sulfate_ug_m3 = ppt_to_ug_m3(so4_ppt,molar_mass$so4),
         sulfate_err = ppt_to_ug_m3(total_so4_uncertainty,molar_mass$so4),
         chloride_ug_m3 = ppt_to_ug_m3(cl_ppt,molar_mass$cl),
         chloride_err = ppt_to_ug_m3(total_cl_uncertainty,molar_mass$cl),
         sodium_ug_m3 = ppt_to_ug_m3(na_ppt,molar_mass$na),
         sodium_err = ppt_to_ug_m3(total_na_uncertainty,molar_mass$na),
         ammonium_ug_m3 = ppt_to_ug_m3(nh4_ppt,molar_mass$nh4),
         ammonium_err = ppt_to_ug_m3(total_nh4_uncertainty,molar_mass$nh4),
         magnesium_ug_m3 = ppt_to_ug_m3(mg_ppt,molar_mass$mg),
         magnesium_err = ppt_to_ug_m3(total_mg_uncertainty,molar_mass$mg),
         calcium_ug_m3 = ppt_to_ug_m3(ca_ppt,molar_mass$ca),
         calcium_err = ppt_to_ug_m3(total_ca_uncertainty,molar_mass$ca),
         potassium_ug_m3 = ppt_to_ug_m3(k_ppt,molar_mass$k),
         potassium_err = ppt_to_ug_m3(total_k_uncertainty,molar_mass$k)) %>% 
  select(date,nitrate_ug_m3:potassium_err)

arna = arna_data %>% left_join(arna_aerosol)

#PSS with arna data, one f value calculated per slr
arna_pss = arna %>%
  rename(hono_ppt = hono,no_ppt = no) %>% 
  mutate(lifetime = 1/jhono,
         oh_molecules_cm3 = 2 * 10^6,
         h = lifetime * dv,
         year = year(date),
         kdep = 0.01/h,
         # nitrate_ug_m3 = ppt_to_ug_m3(no3_ppt,molar_mass$no3),
         nitrate_molecules_cm3 = (nitrate_ug_m3 * 10^-12 *6.022 * 10^23)/62.004,
         nitrate_nmol_m3 = (nitrate_molecules_cm3 * 10^15)/(6.022*10^23),
         production_without_nitrate = kp* oh_molecules_cm3 * ppt_to_molecules_cm3(no_ppt),
         loss = (jhono + (kl*oh_molecules_cm3) + kdep) * ppt_to_molecules_cm3(hono_ppt),
         missing_production = (loss - production_without_nitrate), 
         f_calc = missing_production/(jhno3 * nitrate_molecules_cm3),
         f_para_simone = (385.7)/(1 + (0.19 * nitrate_nmol_m3)),
         f_para_matt = (5.02*10^8)/(1 + (7.19 * 10^5 * nitrate_nmol_m3)),
         hono_para_matt = molecules_cm3_to_ppt((production_without_nitrate + (jhno3 * nitrate_molecules_cm3 * f_para_matt)) 
                                          / (jhono + (kl*oh_molecules_cm3) + kdep)),
         hono_para_simone = molecules_cm3_to_ppt((production_without_nitrate + (jhno3 * nitrate_molecules_cm3 * f_para_simone)) 
                                               / (jhono + (kl*oh_molecules_cm3) + kdep)),
         campaign = ifelse(date < "2020-01-01","ARNA 2019","ARNA 2020"))

# arna_pss %>% 
#   select(f_obs = f_calc,nitrate_ug_m3:potassium_ug_m3) %>% 
#   corPlot()

arna_pss %>% 
  mutate(year = year(date)) %>% 
  rename_with(~str_remove(.,"_ug_m3")) %>% 
  rename_with(~str_to_title(.),.cols = c("nitrate":"potassium")) %>% 
  pivot_longer(Nitrate:Potassium) %>% 
  ggplot(aes(value,f_calc,col = as.character(year))) +
  geom_point() +
  facet_wrap(~name,scales = "free") +
  theme_bw() +
  labs(x = NULL,
       y = expression(f[obs]),
       col = NULL) +
  theme(legend.position = "top")

# ggsave('fobs_vs_aerosols_arna.svg',
#        path = "output/plots",
#        width = 30,
#        height = 12.7,
#        units = 'cm')


# ARNA and ground f ratio with aerosol composition ------------------------------------------

arna_ground = daily_pss %>% 
  timeAverage("1 day") %>%
  mutate(campaign = case_when(year == 2015 ~ "November 2015",
                              year == 2019 ~ "August 2019",
                              year == 2020 ~ "February 2020",
                              year == 2023 ~ "February 2023",
                              year == 2024 ~ "September 2024")) %>% 
  # rename_with(~str_remove(.,"_ug_m3")) %>% 
  # select(date,year,hono_ppt,nitrate_nmol_m3,nitrate_molecules_cm3,jhno3,missing_production,f_calc,f_para,campaign,rh) %>%
  bind_rows(arna_pss) %>% 
  mutate(f_ratio_matt = f_calc/f_para_matt,
         f_ratio_simone = f_calc/f_para_simone) %>% 
  arrange(date)
  
# arna_ground %>% 
#   select(chloride_ug_m3:sulfate_ug_m3,sodium_ug_m3:calcium_ug_m3,rh,f_obs = f_calc,f_para_matt,f_para_simone,f_ratio_matt,f_ratio_simone) %>% 
#   corPlot()

breaks_fun <- function(x) {
  if(max(x) > 10) {
    seq(0,12,3)
  } else if(max(x) < 0.5) {
    seq(0,0.5,0.1)
  }
  else {pretty(x)}
}

arna_ground %>% 
  filter(is.na(campaign) == F,
         campaign == "February 2023" | campaign == "ARNA 2019" | campaign == "ARNA 2020" | campaign == "September 2024",
         altitude < 500 | is.na(altitude) == T) %>% 
  select(-c(fluoride_ug_m3,msa_ug_m3)) %>%
  rename_with(~str_remove(.,"_ug_m3")) %>%
  mutate(hono_err_percent = hono_err/hono_ppt,
         f_ratio_err_matt = f_ratio_matt * hono_err_percent,
         f_rem_max = f_ratio_matt + f_ratio_err_matt,
         f_rem_min = f_ratio_matt - f_ratio_err_matt,
         f_ratio_err_simone = f_ratio_simone * hono_err_percent,
         f_res_max = f_ratio_simone + f_ratio_err_simone,
         f_res_min = f_ratio_simone - f_ratio_err_simone,
         across(c(nitrate_err:magnesium_err,potassium_err),~ifelse(is.na(.),0.002,.)),
         calcium_err = ifelse(is.na(calcium_err),0.02,calcium_err),
         oxalate_err = 0.002,
         across(.cols = c(chloride:calcium),
                .fns = ~ . + get(paste0(cur_column(),"_err")),
                .names = "{col}_max_err"),
         across(.cols = c(chloride:calcium),
                .fns = ~ . - get(paste0(cur_column(),"_err")),
                .names = "{col}_min_err")) %>%
  filter(is.na(campaign) == F,
         campaign == "February 2023" | campaign == "ARNA 2019" | campaign == "ARNA 2020" | campaign == "September 2024",
         altitude < 500 | is.na(altitude) == T) %>%
  rename(`f[Andersen]` = f_ratio_simone,
         `f[Rowlinson]` = f_ratio_matt) %>% 
  pivot_longer(c(`f[Andersen]`,`f[Rowlinson]`),
               values_to = "f_v",names_to = "f_n") %>% 
  pivot_longer(cols = c(f_rem_max,f_res_max),
               values_to = "f_max_err_v",names_to = "f_max_err_n") %>%
  pivot_longer(cols = c(f_rem_min,f_res_min),
               values_to = "f_min_err_v",names_to = "f_min_err_n") %>%
  mutate(f_flag = case_when(f_n == "f[Andersen]" & f_min_err_n == "f_res_min" & f_max_err_n == "f_res_max" ~ "s",
                          f_n == "f[Rowlinson]" & f_min_err_n == "f_rem_min" & f_max_err_n == "f_rem_max" ~ "m")) %>%
  filter(is.na(f_flag) == F) %>%
  rename_with(~str_to_title(.),.cols = c("chloride":"calcium")) %>% 
  pivot_longer(c(Ammonium,Calcium,Chloride,Magnesium,Nitrate,Oxalate,Potassium,Sodium,Sulfate),
               values_to = "aerosol_v") %>%
  pivot_longer(cols = c(chloride_max_err:calcium_max_err),
               values_to = "max_err_v",names_to = "max_err_n") %>%
  pivot_longer(cols = c(chloride_min_err:calcium_min_err),
               values_to = "min_err_v",names_to = "min_err_n") %>%
  mutate(flag = case_when(name == "Ammonium" & min_err_n == "ammonium_min_err" & max_err_n == "ammonium_max_err" ~ "a",
                          name == "Calcium" & min_err_n == "calcium_min_err" & max_err_n == "calcium_max_err" ~ "ca",
                          name == "Chloride" & min_err_n == "chloride_min_err" & max_err_n == "chloride_max_err" ~ "ch",
                          name == "Magnesium" & min_err_n == "magnesium_min_err" & max_err_n == "magnesium_max_err" ~ "mg",
                          name == "Nitrate" & min_err_n == "nitrate_min_err" & max_err_n == "nitrate_max_err" ~ "ni",
                          name == "Oxalate" & min_err_n == "oxalate_min_err" & max_err_n == "oxalate_max_err" ~ "ox",
                          name == "Potassium" & min_err_n == "potassium_min_err" & max_err_n == "potassium_max_err" ~ "k",
                          name == "Sodium" & min_err_n == "sodium_min_err" & max_err_n == "sodium_max_err" ~ "na",
                          name == "Sulfate" & min_err_n == "sulfate_min_err" & max_err_n == "sulfate_max_err" ~ "s")) %>%
  filter(is.na(flag) == F) %>%
  ggplot() +
  geom_point(aes(aerosol_v,f_v,col = campaign),size = 2.5) +
  geom_errorbar(aes(x = aerosol_v,ymin = f_min_err_v,ymax = f_max_err_v,col = campaign)) +
  geom_errorbar(aes(y = f_v,xmin = min_err_v,xmax = max_err_v,col = campaign)) +
  facet_nested_wrap(vars(name,f_n),
                    scales = "free",
                    labeller = label_parsed,
                    ncol = 6,
                    axes = "margins") +
  # facet_wrap(~name,scales = "free_x") +
  theme_bw() +
  scale_colour_manual(values = c("steelblue1","navy","darkorange","goldenrod1"),
                      breaks = c("ARNA 2019","ARNA 2020","February 2023","September 2024")) +
  scale_x_continuous(breaks = breaks_fun) +
  labs(x = expression(Aerosol~(ug/m^3)),
       y = expression(f[obs]/f[parametrised]),
       col = NULL) +
  stat_cor(aes(aerosol_v,f_v,label = after_stat(rr.label)),
             label.x = 0, label.y = 5.8, # set label position if needed
           r.digits = 2 ) +
  theme(legend.position = "top",
        text = element_text(size = 20),
        panel.spacing = unit(1,"lines")) +
  geom_hline(yintercept = 1,linetype = "dashed") +
  ylim(-2.75,6) +
  # xlim(1,1.1)
  # scale_colour_viridis_d() +
  NULL

dat_for_r_squared = arna_ground %>% 
  filter(is.na(campaign) == F,
         campaign == "February 2023" | campaign == "ARNA 2019" | campaign == "ARNA 2020",
         altitude < 500 | is.na(altitude) == T,)

model <- lm(f_ratio_matt ~ sodium_ug_m3, data = dat_for_r_squared)
r_squared <- summary(model)$r.squared

# ggsave('f_ratio_vs_aerosols.png',
#        path = "~/Writing/Thesis/Chapter 4 (HONO in CVAO)/Images",
#        width = 40,
#        height = 30,
#        units = 'cm')

# f ratio with RH and pH --------------------------------------------------

#edit to make it about RH or pH
arna_ground %>% 
  mutate(altitude = ifelse(campaign != "ARNA 2019" & campaign != "ARNA 2020",3,altitude),
         temp_k = temp + 273.15,
         e_s = 611.21 * exp((17.52 * temp) / (temp + 240.97)),
         absolute_humidity = 1000 * ((rh * e_s) / (461.5 * temp_k * 100)),
         humidity_error = rh * 0.02,
         h_e_max = rh + humidity_error,
         h_e_min = rh - humidity_error,
         hono_err_percent = hono_err/hono_ppt,
         f_ratio_err_matt = f_ratio_matt * hono_err_percent,
         f_rem_max = f_ratio_matt + f_ratio_err_matt,
         f_rem_min = f_ratio_matt - f_ratio_err_matt,
         f_ratio_err_simone = f_ratio_simone * hono_err_percent,
         f_res_max = f_ratio_simone + f_ratio_err_simone,
         f_res_min = f_ratio_simone - f_ratio_err_simone) %>% 
  filter(is.na(campaign) == F,
         # campaign == "February 2023",
         campaign != "February 2020" & campaign != "August 2019" & campaign != "November 2015",
         altitude < 500
         ) %>%
  rename(`f[Rowlinson]` = f_ratio_matt,
         `f[Andersen]` = f_ratio_simone) %>% 
  pivot_longer(c(`f[Andersen]`,`f[Rowlinson]`)) %>% 
  pivot_longer(cols = c(f_rem_max,f_res_max),
               values_to = "max_err_v",names_to = "max_err_n") %>% 
  pivot_longer(cols = c(f_rem_min,f_res_min),
               values_to = "min_err_v",names_to = "min_err_n") %>% 
  mutate(flag = case_when(name == "f[Andersen]" & min_err_n == "f_res_min" & max_err_n == "f_res_max" ~ "s",
                          name == "f[Rowlinson]" & min_err_n == "f_rem_min" & max_err_n == "f_rem_max" ~ "m")) %>% 
  filter(is.na(flag) == F) %>%
  ggplot() +
  geom_errorbar(aes(rh,ymin = min_err_v,ymax = max_err_v,col = campaign),
                width =0) +
  geom_errorbarh(aes(y = value,xmax = h_e_max,xmin = h_e_min,col = campaign)) +
  geom_point(aes(rh,value,col = campaign),size = 2.5) +
  # stat_cor(aes(pH,value,label = after_stat(rr.label)),
  #              label.x.npc = 0.8,label.y.npc = 0.95) +
  theme_bw() +
  facet_wrap(~name,scales = "free",
             labeller = label_parsed) +
  scale_colour_manual(values = c("steelblue1","navy","darkorange","goldenrod1"),
                      breaks = c("ARNA 2019","ARNA 2020","February 2023","September 2024")) +
  labs(y = expression(f[obs]/f[para]),
       # x = "pH",
       x = "RH (%)",
       # x = expression(Absolute~humidity~(g~m^{-3})),
       col = NULL) +
  geom_hline(yintercept = 1,linetype = "dashed") +
  theme(legend.position = "top",
        text = element_text(size = 20)) +
  # scale_colour_viridis_c(breaks = seq(60,90,by = 10)) +
  NULL

ggsave('f_ratio_vs_rh.pdf',
       path = "~/Writing/hono_paper",
       width = 29,
       height = 15,
       units = 'cm')

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

# Relationship between missing HONO source and various --------------------

arna_ground %>% 
  mutate(jhno3_hour = jhno3 * 3600,
         nitrate_ppt = molecules_cm3_to_ppt(nitrate_molecules_cm3),
         jhno3_nitrate = jhno3_hour * nitrate_ppt,
         missing_production_ppt_hour = molecules_cm3_to_ppt(missing_production) * 3600,
         ss_ca = ss_aerosol(sodium_ug_m3,ca),
         nss_ca = nss_aerosol(ss_ca,calcium_ug_m3),
         ss_cl = ss_aerosol(sodium_ug_m3,cl),
         nss_cl = nss_aerosol(ss_cl,chloride_ug_m3),
         ss_k = ss_aerosol(sodium_ug_m3,k),
         nss_k = nss_aerosol(ss_k,potassium_ug_m3),
         ss_mg = ss_aerosol(sodium_ug_m3,mg),
         nss_mg = nss_aerosol(ss_mg,magnesium_ug_m3),
         ss_so4 = ss_aerosol(sodium_ug_m3,so4),
         nss_so4 = nss_aerosol(ss_so4,sulfate_ug_m3),
         aerosol_type = case_when(nss_ca > 5 ~ "Dust/Sea salt",
                                  nss_k/nss_ca > 0.24 & nitrate_ug_m3/nss_so4 > 1.4 ~ "Biomass burning",
                                  sodium_ug_m3 > 1 & chloride_ug_m3 > 4 ~ "Sea salt"),
         cl_no = chloride_ug_m3/nitrate_ug_m3,
         na_no = sodium_ug_m3/nitrate_ug_m3) %>% 
  filter(
    # altitude <= 500 | is.na(altitude) == T,
    campaign == "ARNA 2019" | campaign == "ARNA 2020" |
      campaign == "February 2023" | campaign == "September 2024"
  ) %>% 
  rename(Calcium = calcium_ug_m3,
         Potassium = potassium_ug_m3,
         Magnesium = magnesium_ug_m3) %>% 
  # pivot_longer(c(Calcium,Potassium,Magnesium)) %>% 
  ggplot(aes(jhno3_nitrate,missing_production_ppt_hour,col = aerosol_type,fill = aerosol_type,shape = campaign)) +
  theme_bw() +
  geom_point(size = 3) +
  scale_shape_manual(values = c(24,25,19,20)) +
  labs(x = expression(pNO[3]~x~j[HNO[3]]~(ug~m^{-3})),
       y = expression(Missing~HONO~production~(ppt~h^{-1})),
       col = NULL,
       fill = NULL,
       shape = NULL) +
  # facet_wrap(~name,scales = "free_x") +
  theme(legend.position = "top",
        text = element_text(size = 16)
  ) +
  # scale_colour_viridis_c() +
  NULL

ggsave('cvao_arna_dust_tracers_mbl.png',
       path = "output/plots/pre-thesis_plots",
       width = 30,
       height = 12,
       units = 'cm')

# Enhancement diurnals ----------------------------------------------------

dat_f_diurnals = dat_parameterisation %>%
  filter(campaign == "February 2023" | campaign == "September 2024") %>%
  mutate(lifetime = 1/jhono,
         h = lifetime * dv,
         kdep = 0.01/h,
         production_without_nitrate = kp * oh_molecules_cm3 * ppt_to_molecules_cm3(no_ppt),
         loss = (jhono + (kl*oh_molecules_cm3) + kdep) * ppt_to_molecules_cm3(hono_ppt),
         missing_production = (loss - production_without_nitrate), #molecule cm-3 s-1
         f_calc = missing_production/(jhno3 * nitrate_molecules_cm3),
         f_para_simone = (385.7)/(1 + (0.19 * nitrate_nmol_m3)),
         f_para_matt = (5.02*10^8)/(1 + (7.19 * 10^5 * nitrate_nmol_m3)),
         f_calc_jhno3 = f_calc * jhno3,
         f_a_jhno3 = f_para_simone * jhno3,
         f_r_jhno3 = f_para_matt *jhno3) %>% 
  select(date,hour,jhno3,f_calc:f_r_jhno3,missing_production,loss,jhono,production_without_nitrate,campaign)

diurnal_f = dat_f_diurnals %>%
  group_by(hour,campaign) %>%
  select(-date) %>%
  summarise(across(where(is.numeric),list(mean = ~mean(.,na.rm = T),sd = ~sd(.,na.rm = T),se = ~sd(., na.rm = TRUE) / sqrt(length(.))))) %>%
  ungroup()

diurnal_f %>% 
  mutate(f_calc_plot_max = f_calc_mean + f_calc_se,
         f_calc_plot_min = f_calc_mean - f_calc_se) %>% 
  filter(hour >= 8 & hour <= 19) %>% 
  ggplot() +
  theme_bw() +
  geom_path(aes(hour,f_calc_mean,col = campaign),size = 0.75) +
  geom_ribbon(aes(hour,ymin = f_calc_plot_min,ymax = f_calc_plot_max,fill = campaign),alpha = 0.25) +
  theme(text = element_text(size =  20),
        legend.position = "top") +
  scale_colour_manual(values = c("darkorange","goldenrod1")) +
  scale_fill_manual(values = c("darkorange","goldenrod1")) +
  scale_x_continuous(breaks = c(8:19)) +
  labs(x = "Hour of day (UTC)",
       y = expression(f[obs]),
       col = NULL,
       fill = NULL)

pno3_diurnal = read.csv("data/aerosol_data/GEOSChem.pNO3.diurnal-data.csv") %>% 
  clean_names() %>% 
  mutate(date = ymd_hms(time),
         year = year(date),
         hour = hour(date)) %>% 
  select(date,hour,year,base,pno3_photolysis = this_study,no_p_no_3_hv)

pno3_diurnal1 = pno3_diurnal %>% 
  group_by(hour,year) %>% 
  filter(year == 2023) %>% 
  summarise(base = mean(base),
            pno3_photolysis = mean(pno3_photolysis),
            no_p_no_3_hv = mean(no_p_no_3_hv))

diurnal_f %>% 
  # filter(campaign == "February 2023") %>% 
  left_join(pno3_diurnal1) %>% 
  mutate(pno3_photolysis = pno3_photolysis * 1500,
         Nitrate = "Nitrate") %>%
  # pivot_longer(c(f_calc_mean,base)) %>% 
  filter(hour >= 8 & hour <= 19) %>% 
  ggplot(aes(hour)) +
  theme_bw() +
  geom_path(aes(y = f_calc_mean,col = campaign),size = 1) +
  geom_path(aes(y = pno3_photolysis,col = Nitrate),size = 1) +
  scale_x_continuous(breaks = c(8:19)) +
  scale_y_continuous(name = expression(f[obs]),
                     sec.axis = sec_axis(~./1500,name = expression(pNO[3]))) +
  theme(legend.position = "top",
        text = element_text(size = 20)) +
  scale_colour_manual(values = c("black","darkorange","goldenrod1"),
                      breaks = c("Nitrate","February 2023","September 2024")) +
  labs(x = "Hour of day (UTC)",
       col = NULL,
       fill = NULL)

diurnal_f %>%
  mutate(f_calc_plot_max = (f_calc_jhno3_mean + f_calc_jhno3_se)* 10^5,
         f_calc_plot_min = (f_calc_jhno3_mean - f_calc_jhno3_se) * 10^5,
         f_simone_plot_max = (f_a_jhno3_mean + f_a_jhno3_se) * 10^5,
         f_simone_plot_min = (f_a_jhno3_mean - f_a_jhno3_se) * 10^5,
         f_matt_plot_max = (f_r_jhno3_mean + f_r_jhno3_se) * 10^5,
         f_matt_plot_min = (f_r_jhno3_mean - f_r_jhno3_se) * 10^5,
         "f[obs]" = f_calc_jhno3_mean * 10^5,
         "f[Andersen]" = f_a_jhno3_mean * 10^5,
         "f[Rowlinson]" = f_r_jhno3_mean * 10^5) %>% 
  pivot_longer(c("f[obs]","f[Andersen]","f[Rowlinson]")) %>% 
  pivot_longer(cols = c(f_calc_plot_max,f_simone_plot_max,f_matt_plot_max),values_to = "max_err_v",names_to = "max_err_n") %>% 
  pivot_longer(cols = c(f_calc_plot_min,f_simone_plot_min,f_matt_plot_min),values_to = "min_err_v",names_to = "min_err_n") %>% 
  mutate(flag = case_when(name == "f[obs]" & min_err_n == "f_calc_plot_min" & max_err_n == "f_calc_plot_max" ~ "obs",
                          name == "f[Andersen]" & min_err_n == "f_simone_plot_min" & max_err_n == "f_simone_plot_max" ~ "simone",
                          name == "f[Rowlinson]" & min_err_n == "f_matt_plot_min" & max_err_n == "f_matt_plot_max" ~ "matt")) %>%
  filter(hour >= 8 & hour <= 19, is.na(flag) == F) %>% 
  ggplot() +
  theme_bw() +
  geom_path(aes(hour,value,col = name),size = 0.75) +
  geom_ribbon(aes(hour,ymin = min_err_v,ymax = max_err_v,fill = name),alpha = 0.25) +
  # facet_wrap(~name,scales = "free") +
  scale_colour_manual(values = c("darkorange","navy","steelblue1"),
                      breaks = c("f[obs]","f[Andersen]","f[Rowlinson]"),
                      labels = c(expression(f[obs]),expression(f[Andersen]),expression(f[Rowlinson]))) +
  scale_fill_manual(values = c("darkorange","navy","steelblue1"),
                    breaks = c("f[obs]","f[Andersen]","f[Rowlinson]"),
                    labels = c(expression(f[obs]),expression(f[Andersen]),expression(f[Rowlinson]))) +
  # scale_x_datetime(date_breaks = "2 days",date_labels = "%d %b") +
  theme(legend.position = "top",
        text = element_text(size =  20)) +
  scale_x_continuous(breaks = c(8:19)) +
  labs(x = "Hour of day (UTC)",
       y = expression(f~"\u00D7"~j[HNO[3]]~("\u00D7"~10^{-5})),
       col = NULL,
       fill = NULL)

ggsave('f_obs_nitrate_diurnal.pdf',
       path = "~/Writing/hono_paper",
       width = 25,
       height = 10,
       units = 'cm')

# Boxplot of f ratio and aerosols (and RH) --------------------------------

library(forcats)

dat_boxplot = arna_ground %>% 
  mutate(ammonium_bins = case_when(ammonium_ug_m3 < 0.1 ~ "<0.1",
                                   ammonium_ug_m3 >= 0.1 & ammonium_ug_m3 < 0.5 ~ "0.1-0.5",
                                   ammonium_ug_m3 > 0.5 & ammonium_ug_m3 < 1 ~ "0.5-1.0",
                                   ammonium_ug_m3 > 1 & ammonium_ug_m3 <= 1.5 ~ "1.0-1.5",
                                   ammonium_ug_m3 > 1.5 ~ ">1.5"),
         calcium_bins = case_when(calcium_ug_m3 < 0.5 ~ "<0.5",
                                  calcium_ug_m3 > 0.5 & calcium_ug_m3 < 1 ~ "0.5-1.0",
                                  calcium_ug_m3 > 1 & calcium_ug_m3 < 1.5 ~ "1.0-1.5",
                                  calcium_ug_m3 > 1.5 & calcium_ug_m3 < 2 ~ "1.5-2.0",
                                  calcium_ug_m3 > 2 ~ ">2.0"),
         chloride_bins = case_when(chloride_ug_m3 < 0.5 ~ "<0.5",
                                   chloride_ug_m3 > 0.5 & chloride_ug_m3 < 2.0 ~ "0.5-2.0",
                                   chloride_ug_m3 > 2.0 & chloride_ug_m3 < 4 ~ "2.0-4.0",
                                   chloride_ug_m3 > 4.0 & chloride_ug_m3 < 6 ~ "4.0-6.0",
                                   chloride_ug_m3 > 6.0 ~ ">6.0"),
         magnesium_bins = case_when(magnesium_ug_m3 <= 0.1 ~ "<0.1",
                                    magnesium_ug_m3 > 0.1 & magnesium_ug_m3 <= 0.2 ~ "0.1-0.2",
                                    magnesium_ug_m3 > 0.2 & magnesium_ug_m3 <= 0.3 ~ "0.2-0.3",
                                    magnesium_ug_m3 > 0.3 & magnesium_ug_m3 < 0.4 ~ "0.3-0.4",
                                    magnesium_ug_m3 > 0.4 ~ ">0.4"),
         nitrate_bins = case_when(nitrate_ug_m3 <= 1 ~ "<1.0",
                                  nitrate_ug_m3 > 1 & nitrate_ug_m3 < 1.5 ~ "1.0-1.5",
                                  nitrate_ug_m3 > 1.5 & nitrate_ug_m3 < 2.0 ~ "1.5-2.0",
                                  nitrate_ug_m3 > 2 & nitrate_ug_m3 < 2.5 ~ "2.0-2.5",
                                  nitrate_ug_m3 > 2.5 ~ ">2.5"),
         potassium_bins = case_when(potassium_ug_m3 <= 0.1 ~ "<0.1",
                                    potassium_ug_m3 > 0.1 & potassium_ug_m3 < 0.2 ~ "0.1-0.2",
                                    potassium_ug_m3 >= 0.2 & potassium_ug_m3 < 0.3 ~ "0.2-0.3",
                                    potassium_ug_m3 >= 0.3 & potassium_ug_m3 < 0.4 ~ "0.3-0.4",
                                    potassium_ug_m3 > 0.4 ~ ">0.4"),
         sodium_bins = case_when(sodium_ug_m3 <= 0.5 ~ "<0.5",
                                 sodium_ug_m3 > 0.5 & sodium_ug_m3 < 1.0 ~ "0.5-1.0",
                                 sodium_ug_m3 > 1.0 & sodium_ug_m3 < 2 ~ "1.0-2.0",
                                 sodium_ug_m3 > 2.0 & sodium_ug_m3 < 3 ~ "2.0-3.0",
                                 sodium_ug_m3 > 3 ~ ">3.0"),
         sulfate_bins = case_when(sulfate_ug_m3 <= 1.0 ~ "<1.0",
                                  sulfate_ug_m3 > 1.0 & sulfate_ug_m3 < 1.5 ~ "1.0-1.5",
                                  sulfate_ug_m3 > 1.5 & sulfate_ug_m3 < 2.0 ~ "1.5-2.0",
                                  sulfate_ug_m3 > 2.0 & sulfate_ug_m3 < 3.0 ~ "2.0-3.0",
                                  sulfate_ug_m3 > 3.0 ~ ">3.0"),
         rh_bins = case_when(rh <=20 ~ "<20",
                             rh > 20 & rh < 40 ~ "20-40",
                             rh > 40 & rh < 60 ~ "40-60",
                             rh > 60 & rh < 80 ~ "60-80",
                             rh > 80 ~ ">80")) %>% 
  select(date:hono_err,chloride_ug_m3:sulfate_ug_m3,ammonium_ug_m3:calcium_ug_m3,rh,f_ratio_matt:rh_bins)
  # select(rh,rh_bins)


dat_boxplot$ammonium_bins <- fct_relevel(dat_boxplot$ammonium_bins,"<0.1","0.1-0.5","0.5-1.0","1.0-1.5",">1.5")
dat_boxplot$calcium_bins <- fct_relevel(dat_boxplot$calcium_bins,"<0.5","0.5-1.0","1.0-1.5","1.5-2.0",">2.0")
dat_boxplot$chloride_bins <- fct_relevel(dat_boxplot$chloride_bins,"<0.5","0.5-2.0","2.0-4.0","4.0-6.0",">6.0")
dat_boxplot$magnesium_bins <- fct_relevel(dat_boxplot$magnesium_bins,"<0.1","0.1-0.2","0.2-0.3","0.3-0.4",">0.4")
dat_boxplot$nitrate_bins <- fct_relevel(dat_boxplot$nitrate_bins,"<1.0","1.0-1.5","1.5-2.0","2.0-2.5",">2.5")
dat_boxplot$potassium_bins <- fct_relevel(dat_boxplot$potassium_bins,"<0.1","0.1-0.2","0.2-0.3","0.3-0.4",">0.4")
dat_boxplot$sodium_bins <- fct_relevel(dat_boxplot$sodium_bins,"<0.5","0.5-1.0","1.0-2.0","2.0-3.0",">3.0")
dat_boxplot$sulfate_bins <- fct_relevel(dat_boxplot$sulfate_bins,"<1.0","1.0-1.5","1.5-2.0","2.0-3.0",">3.0")
dat_boxplot$rh_bins <- fct_relevel(dat_boxplot$rh_bins,"<20","20-40","40-60","60-80",">80")


dat_boxplot %>% 
  pivot_longer(c(ammonium_bins:rh_bins)) %>% 
  # rename(bins = rh_bins) %>%
  # filter(is.na(bins) == FALSE) %>% 
  ggplot(aes(value,f_ratio_simone)) +
  geom_boxplot() +
  geom_abline(slope = 0, intercept = 1,linetype = "dashed") +
  theme_bw() +
  facet_wrap(~name,scales = "free") +
  labs(# x = expression(Sulfate~(ug~m^-3)),
       y = expression(f[obs]/f[Andersen])) 

# ggsave('rh.svg',
#        path = "output/plots/boxplot",
#        width = 11.08,
#        height = 5.29,
#        units = 'cm')


# Comparing HONO PSS with measured and machine learning nitrate -----------

#nitrate data for Feb 2023 from machine learning
nitrate_dat_ml = read.csv("data/aerosol_data/CVAO_Nitrate_Prediction_Feb2023.csv") %>% 
  mutate(date = ymd(date)) %>% 
  select(date,nitrate_ml = nitrate_ug_m3)

dat_ml = dat_parameterisation %>% 
  filter(year == 2023) %>% 
  left_join(nitrate_dat_ml,by = "date") %>% 
  fill(nitrate_ml) %>% 
  mutate(nitrate_molecules_cm3_ml = (nitrate_ml * 10^-12 *6.022 * 10^23)/62.004,
         nitrate_nmol_m3_ml = (nitrate_molecules_cm3_ml * 10^15)/(6.022*10^23))

dat_ml %>% 
  pivot_longer(c(nitrate_ml,nitrate_ug_m3)) %>% 
  ggplot(aes(date,value,col = name)) +
  geom_point()

hourly_pss_ml = dat_ml %>%
  filter(hour >= 11 & hour <= 15) %>%  #only looking at daytime values
  mutate(lifetime = 1/jhono,
         h = lifetime * dv,
         kdep = 0.01/h,
         production_without_nitrate = kp * oh_molecules_cm3 * ppt_to_molecules_cm3(no_ppt),
         loss = (jhono + (kl*oh_molecules_cm3) + kdep) * ppt_to_molecules_cm3(hono_ppt),
         missing_production = (loss - production_without_nitrate), #molecule cm-3 s-1
         f_calc = missing_production/(jhno3 * nitrate_molecules_cm3),
         f_para_simone = (385.7)/(1 + (0.19 * nitrate_nmol_m3)),
         f_para_matt = (5.02*10^8)/(1 + (7.19 * 10^5 * nitrate_nmol_m3)),
         f_calc_ml = missing_production/(jhno3 * nitrate_molecules_cm3_ml),
         f_para_simone_ml = (385.7)/(1 + (0.19 * nitrate_nmol_m3_ml)),
         f_para_matt_ml = (5.02*10^8)/(1 + (7.19 * 10^5 * nitrate_nmol_m3_ml)))

#checking how different the ratios are if we're using ML or measured nitrate
hourly_pss_ml %>% 
  mutate(ratio_simone = f_calc/f_para_simone,
         ratio_simone_ml = f_calc_ml/f_para_simone_ml) %>% 
  ggplot(aes(ratio_simone,ratio_simone_ml)) +
  geom_point()

daily_fml = hourly_pss_ml %>% 
  timeAverage("1 day") %>% 
  select(date,f_para,f_calc,lifetime,f_calc_ml,f_para_ml,nitrate_molecules_cm3_ml,nitrate_ml)

daily_pss_ml = dat_parameterisation %>%
  filter(year == 2023) %>%
  left_join(daily_fml,by = "date") %>% 
  fill(f_para,f_calc,lifetime,f_calc_ml,f_para_ml,nitrate_molecules_cm3_ml,nitrate_ml,.direction = "down") %>% 
  mutate(h = lifetime * dv,
         kdep = 0.01/h,
         production_without_nitrate = kp*oh_molecules_cm3 * ppt_to_molecules_cm3(no_ppt),
         loss = (jhono + (kl*oh_molecules_cm3) + kdep) * ppt_to_molecules_cm3(hono_ppt),
         missing_production = (loss - production_without_nitrate), #molecule cm-3 s-1
         hono_para = molecules_cm3_to_ppt((production_without_nitrate + (jhno3 * nitrate_molecules_cm3 * f_para))
                                          / (jhono + (kl*oh_molecules_cm3) + kdep)),
         hono_para_ml = molecules_cm3_to_ppt((production_without_nitrate + (jhno3 * nitrate_molecules_cm3_ml * f_para_ml))
                                                     / (jhono + (kl*oh_molecules_cm3) + kdep)))
  

daily_pss_ml %>% 
  filter(date > "2023-02-07" & date < "2023-02-27") %>% 
  mutate(hono_para = ifelse(hono_para < 0,NA_real_,hono_para),
         hono_para_ml = ifelse(hono_para < 0,NA_real_,hono_para_ml)) %>% 
  # timeAverage("1 day") %>% 
  rename("Measured HONO" = hono_ppt,
         "PSS HONO" = hono_para,
         "ML PSS HONO" = hono_para_ml,
         f_obs = f_calc,
         f_obs_ml = f_calc_ml) %>% 
  # pivot_longer(c(f_para,f_para_ml)) %>% 
  ggplot(aes(f_para,f_para_ml)) +
  geom_point() +
  # geom_path(size = 0.75) +
  theme_bw() +
  theme(legend.position = "none") +
  geom_abline(slope = 1, intercept = 0,col = "red") +
  xlim(40,250) +
  ylim(40,250) +
  labs(x = "Parameterised f with measured nitrate",
       y = "Parameterised f with machine learning nitrate",
       col = NULL) +
  # scale_colour_viridis_c() +
  NULL

ggsave('f_comparison_ml_nitrate.svg',
       path = "output/plots/comparing_measured_ml",
       width = 12.7,
       height = 12.7,
       units = 'cm')

# Correlation between f and aerosol composition ---------------------------

#seawater concentrations of ions are used to calculate ss and non-ss contribution to aersosol ions
#assumption is that all na aerosol is from seawater
#ss_k = na_aerosol * (k_seawater/na_seawater)
#mM conc (from Stumm and Morgan):
#na = 468
mg = 53.2
k = 10.2
ca = 10.2
cl = 545
so4 = 28.2

aerosol_composition = daily_pss %>%
  filter(year == 2023) %>% 
  mutate(ss_ca = ss_aerosol(sodium_ug_m3,ca),
         nss_ca = nss_aerosol(ss_ca,calcium_ug_m3),
         ss_cl = ss_aerosol(sodium_ug_m3,cl),
         nss_cl = nss_aerosol(ss_cl,chloride_ug_m3),
         ss_k = ss_aerosol(sodium_ug_m3,k),
         nss_k = nss_aerosol(ss_k,potassium_ug_m3),
         ss_mg = ss_aerosol(sodium_ug_m3,mg),
         nss_mg = nss_aerosol(ss_mg,magnesium_ug_m3),
         ss_so4 = ss_aerosol(sodium_ug_m3,so4),
         nss_so4 = nss_aerosol(ss_so4,sulfate_ug_m3),
         sea_salt = 1.17 * (sodium_ug_m3 + chloride_ug_m3),
         ratio_k_ca = nss_k/nss_ca,
         ratio_no3_so4 = nitrate_ug_m3/nss_so4)

aerosol_composition %>% 
  timeAverage("1 day") %>% 
  mutate(f_ratio = f_para/f_calc) %>% 
  dplyr::select(f_obs = f_calc,f_para,f_ratio,pH:tc_ug_m3,rh) %>%
  corPlot()

aerosol_composition %>% 
  timeAverage("1 day") %>% 
  mutate(f_ratio = f_para/f_calc) %>%
  ggplot(aes(f_ratio,rh)) +
  geom_point()

aerosol_composition %>% 
  timeAverage("1 day") %>%
  dplyr::select(-c(fluoride_ug_m3,msa_ug_m3,tc_ug_m3,ammonium_ug_m3)) %>% 
  rename_with(~str_remove(.,"_ug_m3")) %>% 
  rename_with(~str_to_title(.),.cols = c("mass":"calcium")) %>% 
  rename(OC = oc, EC = ec,f_obs = f_calc,RH =rh) %>%
  # pivot_longer(c(pH:EC,RH)) %>% 
  # pivot_longer(c(f_para,f_obs),names_to = "f",values_to = "f_values") %>% 
  ggplot(aes(f_obs,f_para)) +
  geom_point() +
  geom_abline(slope = 1,intercept = 0,col = "red") +
  xlim(0,275) +
  ylim(0,275) +
  # facet_wrap(~name,scales = "free") +
  theme_bw() +
  labs(x = expression(f[obs]),
       y = expression(f[para]),
       col = NULL) +
  theme(legend.position = "top")

ggsave('fobs_vs_fpara.svg',
       path = "output/plots/plots_with_measured_aerosol",
       width = 30,
       height = 12,
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
# NO2 uptake --------------------------------------------------------------

#waiting on aerosol surface area from tropos to update this for cvao in feb 2023

v = 367 #NO2 mean thermal velocity, in ms-1 calculated as sqrt(8RT/piM)
gamma = 10^-4 #uptake coefficient for NO2 hydrolysis, 10^-4 for sea-salt/dust aerosols, 10^-5 for biomass burning/soot

surface_area = read.csv("data/hono/arna_hono/Renoxification_data_for_Anna.csv") %>% 
  mutate(date = ymd_hms(Start_time)) %>% 
  select(date,sa = Surface_area.um.2.cc.,altitude = Altitude_m) %>% 
  filter(altitude < 1000) %>%
  mutate(sa_m = sa*10^-6) #sorting units

sa = max(surface_area$sa_m) #maximum surface area measured
sa_paper = 215 *10^-6
k_hydro = (gamma * sa * v)/4

no2_hydrolysis_pss = dat_parameterisation %>% 
  mutate(pollution_flag = case_when(ws <= 2 | wd >= 100 & wd <= 340 ~ "Local pollution (met)",
                                    date > "2024-09-11 03:00" & date < "2024-09-12 14:00" ~ "Local pollution",
                                    TRUE ~ "Baseline"),
    #no2_hydro = ifelse(year == 2019 & day > 26,NA_real_,k_hydro * no2 * 3600),
         no2_hydro = k_hydro * no2_ppt * 3600) %>% 
  filter(campaign == "ARNA 2019" | campaign == "ARNA 2020" | campaign == "February 2023" | campaign == "September 2024",
         pollution_flag == "Baseline") %>% 
  group_by(campaign) %>% 
  summarise(no2_hydro_max = max(no2_hydro,na.rm = T),
            no2_hydro_min = min(no2_hydro,na.rm = T),
            no2_hydro_mean = mean(no2_hydro,na.rm = T))

no2_hydro_max = max(no2_hydrolysis_pss$no2_hydro,na.rm = T)

#calculating what gamma would need to be to account for missing HONO production
test = arna_ground %>% 
  filter(campaign == "ARNA 2019" | campaign == "ARNA 2020" |
           campaign == "February 2023" | campaign == "September 2024") %>% 
  mutate(missing_production_ppt = molecules_cm3_to_ppt(missing_production)) %>% 
  select(date,campaign,missing_production_ppt,no2_ppt) %>% 
  # group_by(campaign) %>% 
  # summarise(across(c(missing_production_ppt,no2_ppt),
  #                  list(mean = ~mean(.,na.rm = T),
  #                       max = ~max(.,na.rm = T)))) %>% 
  mutate(gamma = (4 * missing_production_ppt)/(sa * v * no2_ppt)) %>% 
  group_by(campaign) %>% 
  summarise(across(c(missing_production_ppt,no2_ppt,gamma),
                   list(mean = ~mean(.,na.rm = T),
                        max = ~max(.,na.rm = T),
                        min = ~min(.,na.rm = T))))

#plotting what the max hono produced form this per hour would be
no2_hydrolysis_pss %>% 
  # filter(is.na(hono) == F) %>% 
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