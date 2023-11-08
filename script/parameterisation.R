library(tidyverse)
library(lubridate)
library(janitor)
library(zoo)
library(openair)

Sys.setenv(TZ = 'UTC')

#calculating daily f both from measured hono data and from parameterisation
#need to figure out error bars
#need to see when the difference between f_para and f_calc is statistically significant
#need to determine correlation between aerosol composition and air masses

# Constants ---------------------------------------------------------------

kp = 3.3 * 10^-11 #rate constant for oh + no -> hono (from Atkinson et al. 2004) in cm3 molecule-1 s-1
kl = 6 * 10^-12 #rate constant for oh + hono -> h2o + no2 (from Atkinson et al. 2004) in cm3 molecule-1 s-1
dv = 0.3 #deardroff velocity, value used by Simone, in m s-1

# Reading in and tidying data ---------------------------------------------

#dataset from Simone, who got it from Roberto (?),contains majority of info needed for 2015 f parameterisation
#no data is different from current 2015 df - use more up to date data
og_dat15 = read.csv("data/aerosol_data/cv_data_2015.csv") %>% 
  tibble() %>% 
  mutate(date = ymd_hms(date)) %>% 
  clean_names() %>% 
  select(-c(no_ppt:jno2,flag_met,o3_std_ppb,co_std_ppb,flag_o3,flag_co))

#getting oh, nox and air mass data from master df - nox data wrong in dataset above
newer_dat15 = read.csv("output/data/all_data_utc.csv") %>% 
  tibble() %>% 
  mutate(date = ymd_hms(date)) %>% 
  filter(date > "2015-11-22 23:00" & date < "2015-12-05") %>% 
  select(date,oh,no,no2,jhono1 = jhono,jhno31 = jhno3,upwelling,sahel,sahara,west_africa,central_africa,europe,north_america,south_america,north_atlantic,south_atlantic)

#creating final dataset for f parameterisation for 2015
final_dat15 = left_join(og_dat15,newer_dat15,by = "date") %>% 
  mutate(jhono = ifelse(is.na(jhono),jhono1,jhono),
         jhno3 = ifelse(is.na(jhno3),jhno31,jhno3),
         hour = hour(date),
         day = day(date),
         year = year(date),
         month = month(date)) %>% 
  select(-c(jhono1,jhno31))

#dataset from Simone, who got it from Roberto (?),contains majority of info needed for 2019 f parameterisation
og_dat19 = read.csv("data/aerosol_data/cv_data_2019.csv") %>% 
  tibble() %>% 
  mutate(date = ymd_hms(date)) %>% 
  clean_names() %>% 
  select(date:error_ppt,
         uva_rad_wm2:ws,
         o3_mean_ppb,co_mean_ppb,
         jhono:calcium_ug_m3)

#getting oh, nox and air mass data from master df - nox data wrong in dataset above
newer_dat19 = read.csv("output/data/all_data_utc.csv") %>% 
  tibble() %>% 
  mutate(date = ymd_hms(date)) %>% 
  filter(campaign == "August 2019") %>% 
  select(date,oh,no,no2,upwelling,sahel,sahara,west_africa,central_africa,europe,north_america,south_america,north_atlantic,south_atlantic)

final_dat19 = left_join(newer_dat19,og_dat19,by = "date") %>% 
  mutate(hour = hour(date),
         day = day(date),
         year = year(date),
         month = month(date))

# Joining data and calculations -------------------------------------------

#all calculations performed in molecules per cm3 and s-1
dat = bind_rows(final_dat15,final_dat19) %>% 
  rename(oh_m_cm3 = oh,no_ppt = no,no2_ppt = no2) %>% 
  arrange(date) %>% 
  filter(hour >= 11 & hour <= 16) %>% #only looking at daytime values
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
         hono_para = ((kp*oh_m_cm3*no_ppt * 2.46 * 10^19 * 10^-12 + (jhno3 * nitrate_m_cm3 * f_para)) / (jhono + (kl*oh_m_cm3) + kdep))
         / (2.46 * 10^19 * 10^-12), #converts hono from molecules cm-3 s-1 to ppt
         ratio_hono = hono_ppt/hono_para) %>% 
  timeAverage("1 day") %>%
  filter(is.na(hono_ppt) == F) %>% 
  select(date,year,hono_ppt,hono_para,nitrate_ug_m3,f_calc,f_para,ratio_f,ratio_hono,missing_production,everything(),
         -c(hour,nitrate_m_cm3,uva_rad_wm2,press_hpa,dp_um,day,month:loss))

#remove(final_dat15,final_dat19,newer_dat15,newer_dat19,og_dat15,og_dat19)
  
# Missing HONO coloured by f ----------------------------------------------

missing_hono = dat %>% 
  mutate(missing_production = missing_production * 3600 /(2.46 * 10^19 * 10^-12), #in ppt per hour
         jhno3 = jhno3 * 3600, #per hour
         nitrate_ppt = nitrate_ug_m3 * 10^6/62.004 * 8.314*293.15/101325, #ppt
         nitrate_jhno3 = jhno3 * nitrate_ppt)

missing_hono %>%
  # mutate(across(c(upwelling:south_atlantic), ~ na.approx(.x,na.rm =F))) %>%
  filter(is.na(hono_para) == F) %>%
  ggplot(aes(mass_ug_m3,missing_production,shape = as.character(year))) +
  geom_point() +
  theme_bw() +
  # facet_wrap(~year) +
  # theme(legend.position = "top") +
  labs(x = expression(j[HNO[3]]~"*"~nitrate~(ppt~"/"~hour)),
       y = "Missing HONO source (ppt/hour)",
       shape = NULL) +
  scale_colour_viridis_c()

ggsave('daily_missing_hono_jhno3_nitrate.svg',
       path = "output/plots/pss/missing_hono_jhno3",
       width = 30,
       height = 12,
       units = 'cm')

# Looking at hono ---------------------------------------------------------

dat %>% 
  filter(is.na(hono_para) == F) %>% 
  select(-c(central_africa,south_america,bromide_ug_m3,fluoride_ug_m3,phosphate_ug_m3)) %>%
  # pivot_longer(c(upwelling:south_atlantic)) %>%
  # pivot_longer(c(mass_ug_m3:calcium_ug_m3,nitrate_ug_m3)) %>%
  # pivot_longer(c(hono_ppt,hono_para)) %>% 
  ggplot(aes(mass_ug_m3,african,col = as.character(year))) +
  theme_bw() +
  # geom_abline(intercept = 0,slope = 1) +
  geom_point() +
  # geom_smooth(method = "lm",se=F) +
  # facet_wrap(~name,scales = "free") +
  # labs(shape = NULL,
  #      # x = "Concentration (ug/m3)",
  #      y = "HONO ratio") +
  # theme(legend.position = "top") +
  scale_colour_viridis_d() +
  # scale_shape_manual(values=c(2,3)) +
  NULL

ggsave('aerosols_hono_ratio.svg',
       path = "output/plots/parameterisation/ratio",
       width = 31,
       height = 15,
       units = 'cm')

# Plotting ----------------------------------------------------------------

#plotting ratio and various factors that could affect it
dat %>%
  filter(ratio > 1) %>% 
  select(-c(central_africa,south_america)) %>% 
  mutate(flag = case_when(ratio > all_mean + all_sd | ratio < all_mean - all_sd ~ "normal",
                          TRUE ~ "extreme"),
         continental = europe + north_america,
         marine = north_atlantic + south_atlantic + upwelling,
         african = sahara + sahel + west_africa) %>%
  pivot_longer(c(upwelling:south_atlantic)) %>%
  # pivot_longer(c(mass_ug_m3:calcium_ug_m3)) %>%
  # ggplot(aes(hono_ppt,value,shape = as.character(year),col = name)) +
  ggplot(aes(value,ratio,shape = as.character(year),col = flag)) +
  geom_point() +
  facet_wrap(~name,scales = "free") +
  # scale_colour_viridis_c()+
  labs(shape = NULL,
       col = NULL,
       x = NULL) +
  theme(legend.position = "top")

# ggsave('air_masses_ratio.svg',
#        path = "output/plots/f/ratio",
#        width = 32,
#        height = 15.27,
#        units = 'cm')

all_mean = mean(dat$ratio,na.rm=T)
all_sd = sd(dat$ratio,na.rm=T)
mean15 = mean(f15$ratio,na.rm=T)
sd15 = sd(f15$ratio,na.rm=T)
mean19 = mean(f19$ratio,na.rm=T)
sd19 = sd(f19$ratio,na.rm=T)

test = dat %>% 
  select(day,month,year,ratio) %>% 
  mutate(flag = case_when(ratio < all_mean + all_sd & ratio > all_mean-all_sd ~ 0,
                          TRUE ~ 1)) %>% 
  filter(flag == 1)

test %>% 
  ggplot(aes(nitrate_ug_m3,ratio,shape = as.character(year),col = flag)) +
  geom_point()

# Correlation testing between ratio and other factors ---------------------

#test for the normality of the data
shapiro.test(dat$hono_ppt)

#use pearson if data has a normal distribution
#use spearman if data doesn't have a normal distribution
cor.test(dat$ratio,dat$hono_ppt,use = "complete.obs",method = "pearson")

ggqqplot(dat$ratio)

#create df for correlating, with only columns that are going to be used for correlation
dat_cor = dat %>% 
  filter(ratio > 1) %>% 
  select(-c(day:year,lifetime:f_para,bromide_ug_m3,phosphate_ug_m3,fluoride_ug_m3,
            error_ppt,south_america,central_africa,nitrate,dp_um,co_std_ppb,o3_std_ppb,ws,wd,co_mean_ppb)) %>% 
  # select(hono_ppt,rh,jhono,jhno3,chloride_ug_m3,sulfate_ug_m3,ratio) %>% 
  remove_constant()

#correlates all the columns in the above df with a set column
dat_cor1 = as.data.frame(cor(dat_cor[,colnames(dat_cor) != "ratio"],dat_cor$ratio,use = "complete.obs",method = "pearson"))

