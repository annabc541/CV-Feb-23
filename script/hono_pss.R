library(tidyverse)
library(lubridate)
library(janitor)
library(zoo)
library(openair)

Sys.setenv(TZ = 'UTC')
# conflict_prefer("select",winner = "dplyr")
# conflict_prefer("filter",winner = "dplyr")

#calculates HONO using Matt's parameterisation for enhancement factor (f)


# Constants ---------------------------------------------------------------

kp = 3.3 * 10^-11 #rate constant for oh + no -> hono (from Atkinson et al. 2004) in cm3 molecule-1 s-1
kl = 6 * 10^-12 #rate constant for oh + hono -> h2o + no2 (from Atkinson et al. 2004) in cm3 molecule-1 s-1
dv = 0.3 #deardroff velocity, value used by Simone, in m s-1
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

# Reading in data -----------------------

#can choose whether to filter by year or to calculate for whole available dataset

dat = read.csv("output/data/all_data_utc.csv") %>% 
  filter(year == 2023) %>%
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
  filter(date > "2023-02-07" & date < "2023-02-27") %>%
  mutate(hono_para_err = hono_para * 0.1,
         hono_para = ifelse(hono_para < 0,0,hono_para),
         hono_without_nitrate = ifelse(hono_without_nitrate < 0,0,hono_without_nitrate)) %>% 
  pivot_longer(c(hono_ppt,hono_para,hono_without_nitrate)) %>%
  ggplot(aes(date,value,col = name)) +
  theme_bw() +
  geom_path(size = 1) +
  labs(x = "Datetime (UTC)",
       y = "HONO (ppt)",
       col = NULL) +
  # geom_path(aes(date,hono_para),size = 1,col = "darkorange") +
  # geom_ribbon(aes(date,ymin = hono_para - hono_para_err,ymax = hono_para + hono_para_err),alpha = 0.25) +
  # geom_path(aes(date,hono_ppt),
  #           col = "steelblue1",
  #           size = 1) +
  # geom_ribbon(aes(date,ymin = hono_ppt - hono_err,ymax = hono_ppt + hono_err),alpha = 0.25,fill = "steelblue1") +
  # geom_path(aes(date,hono_without_nitrate),size = 1,col = "navyblue") +
  scale_x_datetime(breaks = "5 day",date_labels = "%d/%m/%y") +
  scale_colour_manual(values = c("steelblue1","darkorange","navyblue"),
                      labels = c("PSS HONO with f","Measured HONO","PSS HONO without f")) +
  theme(legend.position = "top",
        # axis.title = element_text(size = 28),
        # strip.text = element_text(size = 28),
        # axis.text = element_text(size = 20),
        # legend.text = element_text(size = 20)
        ) +
  # scale_colour_gradient(low = "darkorange",high = "steelblue1") +
  # facet_wrap(~year,ncol = 1,scales = "free_x") +
  NULL

ggsave('hono_timeseries23_para.svg',
       path = "output/plots/more_final",
       width = 33.87,
       height = 7.7,
       units = 'cm')

# Plotting diurnal --------------------------------------------------------

diurnal = daily_pss %>% 
  mutate(hono_para = ifelse(hono_para < 0,0,hono_para),
         hono_para_err = hono_para *0.1,
         hono_without_nitrate = ifelse(hono_without_nitrate < 0,0,hono_without_nitrate)) %>% 
  rename(HONO = hono_ppt) %>% 
  filter(is.na(HONO) == FALSE,
         year == 2023) %>% 
  timeVariation(pollutant = c("HONO","hono_err","hono_para","hono_para_err","hono_without_nitrate"))

diurnal_dat = diurnal$data$hour %>% 
  ungroup() %>% 
  pivot_wider(names_from = variable,values_from = Mean) %>% 
  group_by(hour) %>% 
  summarise(HONO = mean(HONO,na.rm = T),
            hono_err = mean(hono_err,na.rm = T),
            hono_para = mean(hono_para,na.rm = T),
            hono_para_err = mean(hono_para_err,na.rm = T),
            hono_without_nitrate = mean(hono_without_nitrate,na.rm = T))

diurnal_dat %>%
  pivot_longer(c(HONO,hono_para,hono_without_nitrate)) %>% 
  ggplot(aes(hour,value,col = name)) +
  geom_path(size = 1) +
  scale_colour_manual(values = c("darkorange","steelblue1","navyblue"),
                      labels = c("Measured HONO","PSS HONO with f","PSS HONO without f")) +
  # geom_path(aes(hour,HONO),size = 2,col = "steelblue1") +
  # geom_ribbon(aes(hour,ymin = HONO - hono_err,ymax = HONO + hono_err),alpha = 0.25,fill = "steelblue1") +
  # geom_path(aes(hour,hono_para),size = 2,col = "black") +
  # geom_ribbon(aes(hour,ymin = hono_para - hono_para_err,ymax = hono_para + hono_para_err),alpha = 0.25,fill = "black") +
  theme_bw() +
  labs(x = "Hour of day (UTC)",
       y = "HONO (ppt)",
       color = NULL) +
  scale_x_continuous(breaks = c(0,4,8,12,16,20)) +
  theme(legend.position = "top",
        # axis.title = element_text(size = 28),
        # axis.text = element_text(size = 20)
        ) +
  NULL

ggsave('hono_para_diurnal.svg',
       path = "output/plots/more_final",
       width = 16.06,
       height = 13.88,
       units = 'cm')

# Doing this for ARNA -----------------------------------------------------

#hono data from simone's arna campaign
arna_data = read.csv("data/hono/arna_hono/Renoxification_data_for_Anna_v2.csv") %>% 
  mutate(date = dmy_hm(Start_time),
         year = year(date)) %>% 
  select(date,
         hono = HONO_pptV,
         hono_err = HONO_Uncertainty_ppt,
         rh = RH,
         no = NO_pptV,
         jhno3 = J_HNO3,
         jhono = J_HONO,
         altitude = Altitude_m)

arna_aerosol = read.csv("data/aerosol_data/arna_aerosols.csv") %>% 
  clean_names() %>% 
  mutate(date = mdy_hm(start_time)) %>%  
  mutate(nitrate_ug_m3 = ppt_to_ug_m3(no3_ppt,molar_mass$no3),
         sulfate_ug_m3 = ppt_to_ug_m3(so4_ppt,molar_mass$so4),
         chloride_ug_m3 = ppt_to_ug_m3(cl_ppt,molar_mass$cl),
         sodium_ug_m3 = ppt_to_ug_m3(na_ppt,molar_mass$na),
         ammonium_ug_m3 = ppt_to_ug_m3(nh4_ppt,molar_mass$nh4),
         magnesium_ug_m3 = ppt_to_ug_m3(mg_ppt,molar_mass$mg),
         calcium_ug_m3 = ppt_to_ug_m3(ca_ppt,molar_mass$ca),
         potassium_ug_m3 = ppt_to_ug_m3(k_ppt,molar_mass$k)) %>% 
  select(date:potassium_ug_m3)

arna = arna_data %>% left_join(arna_aerosol)

#PSS with arna data, one f value calculated per slr
arna_pss = arna %>%
  rename(hono_ppt = hono,no_ppt = no) %>% 
  mutate(lifetime = 1/jhono,
         oh_molecules_cm3 = 2 * 10^6,
         h = lifetime * dv,
         year = year(date),
         kdep = 0.01/h,
         nitrate_molecules_cm3 = (nitrate_ug_m3 * 10^-12 *6.022 * 10^23)/62.004,
         nitrate_nmol_m3 = (nitrate_molecules_cm3 * 10^15)/(6.022*10^23),
         production_without_nitrate = kp* oh_molecules_cm3 * ppt_to_molecules_cm3(no_ppt),
         loss = (jhono + (kl*oh_molecules_cm3) + kdep) * ppt_to_molecules_cm3(hono_ppt),
         missing_production = (loss - production_without_nitrate), 
         f_calc = missing_production/(jhno3 * nitrate_molecules_cm3),
         f_para = 103706014.61/(1+83211.37 *nitrate_nmol_m3),
         hono_para = molecules_cm3_to_ppt((production_without_nitrate + (jhno3 * nitrate_molecules_cm3 * f_para)) 
                                          / (jhono + (kl*oh_molecules_cm3) + kdep)))

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


# Ground-campaigns and ARNA data comparison ------------------------------------------

arna_ground = hourly_pss %>% 
  timeAverage("1 day") %>%
  mutate(campaign = case_when(year == 2015 ~ "November 2015 campaign",
                              year == 2019 ~ "August 2019 campaign",
                              year == 2020 ~ "February 2020 campaign",
                              year == 2023 ~ "February 2023 campaign")) %>% 
  # select(date,year,hono_ppt,nitrate_nmol_m3,nitrate_molecules_cm3,jhno3,missing_production,f_calc,f_para,campaign,rh) %>% 
  bind_rows(arna_pss) %>% 
  mutate(campaign = case_when(is.na(campaign) & year == 2019 ~ "ARNA campaign 2019",
                              is.na(campaign) & year == 2020 ~ "ARNA campaign 2020",
                              TRUE ~ campaign),
         nitrate_ppt = molecules_cm3_to_ppt(nitrate_molecules_cm3),
         f_ratio = f_calc/f_para) %>% 
  arrange(date)

arna_ground %>% 
  filter(is.na(campaign) == FALSE) %>% 
  rename("f[para]" = f_para,"f[obs]" = f_calc,"f[obs]/f[para]" = f_ratio) %>% 
  pivot_longer(c("f[para]","f[obs]","f[obs]/f[para]")) %>% 
  ggplot(aes(rh,value,col = campaign)) +
  geom_point() +
  theme_bw() +
  labs(col = NULL,
       x = "RH (%)",
       y = NULL) +
  theme(legend.position = "top") +
  facet_grid(rows = vars(name),labeller = label_parsed,scales = "free") +
  scale_colour_viridis_d() +
  NULL
# 
# ggsave('f_vs_rh.svg',
#        path = "output/plots/plots_with_measured_aerosol/arna_ground",
#        width = 30,
#        height = 12.7,
#        units = 'cm')
  
arna_ground %>% 
  select(chloride_ug_m3:sulfate_ug_m3,sodium_ug_m3:calcium_ug_m3,rh,f_obs = f_calc,f_para,f_ratio) %>% 
  corPlot()

arna_ground %>% 
  select(-c(oxalate_ug_m3:msa_ug_m3)) %>%
  rename_with(~str_remove(.,"_ug_m3")) %>% 
  rename_with(~str_to_title(.),.cols = c("mass":"calcium")) %>% 
  rename(OC = oc, EC = ec,f_obs = f_calc,RH =rh) %>%
  pivot_longer(c(Chloride:Calcium,RH)) %>%
  # pivot_longer(c(f_para,f_obs),names_to = "f",values_to = "f_values") %>% 
  ggplot(aes(value,f_ratio,col = campaign)) +
  geom_point() +
  facet_wrap(~name,scales = "free") +
  theme_bw() +
  labs(x = NULL,
       y = expression(f[obs]/f[para]),
       col = NULL) +
  theme(legend.position = "top") +
  scale_colour_viridis_d() +
  NULL

# ggsave('fpara_vs_aerosols.svg',
#        path = "output/plots/plots_with_measured_aerosol",
#        width = 30,
#        height = 12,
#        units = 'cm')


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
  select(date:hono_err,chloride_ug_m3:sulfate_ug_m3,ammonium_ug_m3:calcium_ug_m3,rh,f_ratio:rh_bins)
  # select(rh,rh_bins)


# dat_boxplot$ammonium_bins <- fct_relevel(dat_boxplot$ammonium_bins,"<0.1","0.1-0.5","0.5-1.0","1.0-1.5",">1.5")
# dat_boxplot$calcium_bins <- fct_relevel(dat_boxplot$calcium_bins,"<0.5","0.5-1.0","1.0-1.5","1.5-2.0",">2.0")
# dat_boxplot$chloride_bins <- fct_relevel(dat_boxplot$chloride_bins,"<0.5","0.5-2.0","2.0-4.0","4.0-6.0",">6.0")
# dat_boxplot$magnesium_bins <- fct_relevel(dat_boxplot$magnesium_bins,"<0.1","0.1-0.2","0.2-0.3","0.3-0.4",">0.4")
# dat_boxplot$nitrate_bins <- fct_relevel(dat_boxplot$nitrate_bins,"<1.0","1.0-1.5","1.5-2.0","2.0-2.5",">2.5")
# dat_boxplot$potassium_bins <- fct_relevel(dat_boxplot$potassium_bins,"<0.1","0.1-0.2","0.2-0.3","0.3-0.4",">0.4")
# dat_boxplot$sodium_bins <- fct_relevel(dat_boxplot$sodium_bins,"<0.5","0.5-1.0","1.0-2.0","2.0-3.0",">3.0")
# dat_boxplot$sulfate_bins <- fct_relevel(dat_boxplot$sulfate_bins,"<1.0","1.0-1.5","1.5-2.0","2.0-3.0",">3.0")
dat_boxplot$rh_bins <- fct_relevel(dat_boxplot$rh_bins,"<20","20-40","40-60","60-80",">80")


dat_boxplot %>% 
  rename(bins = rh_bins) %>% 
  filter(is.na(bins) == FALSE) %>% 
  ggplot(aes(bins,f_ratio)) +
  geom_boxplot() +
  geom_abline(slope = 0, intercept = 1,linetype = "dashed") +
  theme_bw() +
  labs(x = "RH (%)",
       # x = expression(Sulfate~(ug~m^-3)),
       # y = expression(f[obs]/f[para]),
       y = NULL
  ) 

ggsave('rh.svg',
       path = "output/plots/boxplot",
       width = 11.08,
       height = 5.29,
       units = 'cm')


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
         f_para = 103706014.61/(1 + (83211.37 * nitrate_nmol_m3)), #matt's parameterisation
         f_calc_ml = missing_production/(jhno3 * nitrate_molecules_cm3_ml),
         f_para_ml = 103706014.61/(1 + (83211.37 * nitrate_nmol_m3_ml)))

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

ggsave('no2_timeseries.svg',
       path = "output/plots/timeseries",
       width = 30,
       height = 12,
       units = 'cm')