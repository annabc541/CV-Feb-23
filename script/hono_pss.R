library(tidyverse)
library(lubridate)
library(janitor)
library(zoo)
library(openair)

Sys.setenv(TZ = 'UTC')

#calculates HONO using Matt's parameterisation for enhancement factor (f)

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
#n/V =  p/RT = 1atm / (0.08206 L atm K-1 mol-1 * 298 K) = 0.0409 mol L-1 = 0.0409 * 10^-3 mol cm-3
#nmol mol-1 * 10^-12 *  6.022 * 10^23 molecules mol-1 * 0.0409 * 10^-3 mol cm-3
#2.46 * 10^7 molecules cm-3 conversion factor
ppt_to_molecules_cm3 <- function(x){y = x * 2.46 * 10^19 * 10^-12}
molecules_cm3_to_ppt <- function(x){y = x / ( 2.46 * 10^19 * 10^-12)}
ss_aerosol <- function(na,x){y = na * (x/468)}
nss_aerosol <- function(ss,x){y = x - ss}

# Reading in data -----------------------

#can choose whether to filter by year or to calculate for whole available dataset

dat = read.csv("output/data/all_data_utc.csv") %>% 
  # filter(year == 2023) %>%
  mutate(date = ymd_hms(date))

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
         hour = hour(date)) %>% 
  fill(oh, oh_precision) %>% 
  select(date,hour,month,year,everything()) %>% 
  rename(hono_ppt = hono,oh_molecules_cm3 = oh,no_ppt = no,no2_ppt = no2)

# Calculating enhancement factors and PSS HONO ----------------------------

#all calculations performed in molecules per cm3 and s-1
#specify all units to keep track of things

hourly_pss = dat_parameterisation %>%
  filter(hour >= 11 & hour <= 15) %>%  #only looking at daytime values
  mutate(lifetime = 1/jhono,
         h = lifetime * dv,
         kdep = 0.01/h,
         production_without_nitrate = kp * oh_molecules_cm3 * ppt_to_molecules_cm3(no_ppt),
         loss = (jhono + (kl*oh_molecules_cm3) + kdep) * ppt_to_molecules_cm3(hono_ppt),
         missing_production = (loss - production_without_nitrate), #molecule cm-3 s-1
         f_calc = missing_production/(jhno3 * nitrate_molecules_cm3),
         f_para = 103706014.61/(1 + (83211.37 * nitrate_nmol_m3)), #matt's parameterisation
         # hono_para = molecules_cm3_to_ppt((production_without_nitrate + (jhno3 * nitrate_molecules_cm3 * f_para))
                                          # / (jhono + (kl*oh_molecules_cm3) + kdep))
         )

daily_f = hourly_pss %>% 
  timeAverage("1 day") %>% 
  select(date,f_para,f_calc,lifetime)

daily_pss = dat_parameterisation %>%
  left_join(daily_f,by = "date") %>% 
  fill(f_para,f_calc,lifetime,.direction = "down") %>% 
  mutate(h = lifetime * dv,
         kdep = 0.01/h,
         production_without_nitrate = kp*oh_molecules_cm3 * ppt_to_molecules_cm3(no_ppt),
         loss = (jhono + (kl*oh_molecules_cm3) + kdep) * ppt_to_molecules_cm3(hono_ppt),
         missing_production = (loss - production_without_nitrate), #molecule cm-3 s-1
         hono_para = molecules_cm3_to_ppt((production_without_nitrate + (jhno3 * nitrate_molecules_cm3 * f_para))
                                          / (jhono + (kl*oh_molecules_cm3) + kdep)),
         hono_without_nitrate = molecules_cm3_to_ppt((production_without_nitrate + (jhno3 * nitrate_molecules_cm3))
                                                     / (jhono + (kl*oh_molecules_cm3) + kdep))) 


# Plotting timeseries -----------------------------------------------------

#hono timeseries - either for comparison with pss or as a stand-alone
daily_pss %>% 
  # filter(date > "2015-11-24" & date < "2015-12-04") %>%
  mutate(hono_para_err = hono_para * 0.1,
         hono_para = ifelse(hono_para < 0,0,hono_para),
         hono_without_nitrate = ifelse(hono_without_nitrate < 0,0,hono_without_nitrate)) %>% 
  ggplot() +
  theme_bw() +
  labs(x = "Datetime (UTC)",
       y = "HONO (ppt)",
       col = "RH (%)") +
  geom_path(aes(date,hono_para),size = 1,col = "darkorange") +
  # geom_ribbon(aes(date,ymin = hono_para - hono_para_err,ymax = hono_para + hono_para_err),alpha = 0.25) +
  geom_path(aes(date,hono_ppt),
            col = "steelblue1",
            size = 1) +
  # geom_ribbon(aes(date,ymin = hono_ppt - hono_err,ymax = hono_ppt + hono_err),alpha = 0.25,fill = "steelblue1") +
  # geom_path(aes(date,hono_without_nitrate),size = 1,col = "navyblue") +
  scale_x_datetime(breaks = "2 day",date_labels = "%d/%m/%y") +
  scale_colour_gradient(low = "darkorange",high = "steelblue1") +
  facet_wrap(~year,ncol = 1,scales = "free_x")
  NULL

# ggsave('hono_timeseries23_para_rh.svg',
#        path = "output/plots/departmental_poster",
#        width = 55.6,
#        height = 12.58,
#        units = 'cm')

# Plotting diurnal --------------------------------------------------------

diurnal = daily_pss %>% 
  mutate(hono_para = ifelse(hono_para < 0,0,hono_para),
         hono_para_err = hono_para *0.1) %>% 
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
  ggplot() +
  geom_path(aes(hour,HONO),size = 2,col = "steelblue1") +
  geom_ribbon(aes(hour,ymin = HONO - hono_err,ymax = HONO + hono_err),alpha = 0.25,fill = "steelblue1") +
  geom_path(aes(hour,hono_para),size = 2,col = "black") +
  geom_ribbon(aes(hour,ymin = hono_para - hono_para_err,ymax = hono_para + hono_para_err),alpha = 0.25,fill = "black") +
  theme_bw() +
  labs(x = "Hour of day (UTC)",
       y = "HONO (ppt)",
       color = NULL) +
  scale_x_continuous(breaks = c(0,4,8,12,16,20)) +
  theme(axis.title = element_text(size = 28),
        axis.text = element_text(size = 20)) +
  NULL

# ggsave('hono_para_diurnal.svg',
#        path = "output/plots/departmental_poster",
#        width = 22.28,
#        height = 12.58,
#        units = 'cm')

# Doing this for ARNA -----------------------------------------------------

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

#PSS with arna data, one f value calculated per flight
arna_pss = arna_data %>%
  rename(hono_ppt = hono,no_ppt = no) %>% 
  mutate(lifetime = 1/jhono,
         oh_molecules_cm3 = 2 * 10^6,
         h = lifetime * dv,
         year = year(date),
         kdep = 0.01/h,
         nitrate_molecules_cm3 = ppt_to_molecules_cm3(nitrate_ppt),
         nitrate_nmol_m3 = (nitrate_molecules_cm3 * 10^15)/(6.022*10^23),
         nitrate_ug_m3 = nitrate_molecules_cm3 * 62.004 / (10^-12 *6.022 * 10^23),
         production_without_nitrate = kp* oh_molecules_cm3 * ppt_to_molecules_cm3(no_ppt),
         loss = (jhono + (kl*oh_molecules_cm3) + kdep) * ppt_to_molecules_cm3(hono_ppt),
         missing_production = (loss - production_without_nitrate), 
         f_calc = missing_production/(jhno3 * nitrate_molecules_cm3),
         f_para = 103706014.61/(1+83211.37 *nitrate_nmol_m3),
         hono_para = molecules_cm3_to_ppt((production_without_nitrate + (jhno3 * nitrate_molecules_cm3 * f_para)) 
                                          / (jhono + (kl*oh_molecules_cm3) + kdep)))

# Ground-campaigns and ARNA data comparison ------------------------------------------

daily_pss %>% 
  mutate(campaign = case_when(year == 2015 ~ "November 2015 campaign",
                              year == 2019 ~ "August 2019 campaign",
                              year == 2020 ~ "February 2020 campaign",
                              year == 2023 ~ "February 2023 campaign")) %>% 
  # timeAverage("1 day") %>% 
  select(date,nitrate_nmol_m3,year,f_calc,campaign) %>% 
  bind_rows(arna_pss) %>% 
  mutate(campaign = case_when(is.na(campaign) & year == 2019 ~ "ARNA campaign 2019",
                              is.na(campaign) & year == 2020 ~ "ARNA campaign 2020",
                              TRUE ~ campaign)) %>% 
  filter(campaign != "November 2015 campaign",
         campaign != "August 2019 campaign",
         campaign != "February 2020 campaign") %>% 
  ggplot(aes(nitrate_nmol_m3,f_calc,col = campaign)) +
  geom_point()

arna_feb23_comparison %>%  
  timeAverage("1 day") %>% 
  pivot_longer(c(nitrate_ug_m3:tc)) %>% 
  ggplot(aes(nitrate_nmol_m3,f_calc)) +
  geom_point()
  



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


# Correlation between f and aerosol composition ---------------------------

#seawater concentrations of ions are used to calculate ss and non-ss contribution to aersosol ions
#assumption is that all na aerosol is from seawater
#ss_k = na_aerosol * (k_seawater/na_seawater)
#mM conc (from Stumm and Morgan):
na = 468
mg = 53.2
k = 10.2
ca = 10.2
cl = 545
so4 = 28.2

daily_pss %>%
  mutate(ss_ca = ss_aerosol(),
         nss_ca = calcium - ss_ca,
         ss_k = sodium * (10.2/468),
         nss_k = potassium- ss_k,
         ss_mg = sodium * (53.2/468),
         nss_mg = magnesium - ss_mg,
         ss_so4 = sodium * (28.2/468),
         nss_so4 = sulfate - ss_so4,
         sea_salt = 1.17 * (sodium + chloride)) %>% 
  select(hono_ppt,f_para,f_calc,nitrate_ug_m3:tc,ss_ca:sea_salt) %>% 
  corPlot()

daily_pss %>% 
  timeAverage("1 day") %>% 
  pivot_longer(c(nitrate_ug_m3:tc)) %>% 
  ggplot(aes(nitrate_nmol_m3,f_calc)) +
  geom_point()
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