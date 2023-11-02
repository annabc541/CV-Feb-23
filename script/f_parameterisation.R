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

kp = 3.3 * 10^-11 #rate constant for oh + no -> hono (from Atkinson et al. 2004)
kl = 6 * 10^-12 #rate constant for oh + hono -> h2o + no2 (from Atkinson et al. 2004)
dv = 0.3 #deardroff velocity, value used by Simone


# November 2015 -----------------------------------------------------------

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

#calculating daily calculated and parameterised f, using only daytime values
f15 = final_dat15 %>%  
  mutate() %>%
  filter(hour >= 11 & hour <= 16) %>% #only looking at daytime values
  mutate(lifetime = 1/jhono,
         h = lifetime * dv,
         kdep = 0.01/h,
         nitrate_molecules = (nitrate_ug_m3 * 10^-12 *6.022 * 10^23)/62.004,
         no_molecules = no * 2.46 * 10^19 * 10^-12,
         production_without_nitrate = kp*oh*no_molecules,
         loss = jhono + (kl*oh) + kdep,
         loss_hono = loss * hono_ppt * 2.46 * 10^19 * 10^-12,
         missing_production = loss_hono - production_without_nitrate) %>% 
  group_by(day) %>%
  summarise_all(mean,na.rm = T) %>% 
  ungroup() %>% 
  mutate(f_calc = missing_production/(nitrate_molecules * jhno3),
         f_para = 10^5/((1+80.07*nitrate_ug_m3)*rh),
         ratio_f = f_calc/f_para,
         hono_para = ((kp*oh*no_molecules + (jhno3 * nitrate_molecules * f_para)) / (jhono + (kl*oh) + kdep))
         / (2.46 * 10^19 * 10^-12),
         ratio_hono = hono_ppt/hono_para) %>% 
  select(date,hono_ppt,hono_para,nitrate_ug_m3,f_calc,f_para,ratio_f,ratio_hono,everything(),
         -c(hour,no_molecules,nitrate_molecules)) %>% 
  arrange(date)

# write.csv(f15,"output/data/f_parameterised15_all_data.csv",row.names = F)

# August 2019 -------------------------------------------------------------

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

#calculating daily calculated and parameterised f
f19 = final_dat19 %>%
  filter(hour >= 11 & hour <= 16) %>% #only looking at daytime values
  mutate(lifetime = 1/jhono,
         h = lifetime * dv,
         kdep = 0.01/h,
         nitrate_molecules = (nitrate_ug_m3 * 10^-12 *6.022 * 10^23)/62.004,
         no_molecules = no * 2.46 * 10^19 * 10^-12,
         production_without_nitrate = kp*oh*no_molecules,
         loss = jhono + (kl*oh) + kdep,
         loss_hono = loss * hono_ppt * 2.46 * 10^19 * 10^-12,
         missing_production = loss_hono - production_without_nitrate) %>% 
  group_by(day) %>%
  summarise_all(mean,na.rm = T) %>% 
  ungroup() %>% 
  mutate(f_calc = missing_production/(nitrate_molecules * jhno3),
         f_para = 10^5/((1+80.07*nitrate_ug_m3)*rh),
         ratio_f = f_calc/f_para,
         hono_para = ((kp*oh*no_molecules + (jhno3 * nitrate_molecules * f_para)) / (jhono + (kl*oh) + kdep))
         / (2.46 * 10^19 * 10^-12),
         ratio_hono = hono_ppt/hono_para) %>% 
  select(date,hono_ppt,hono_para,nitrate_ug_m3,f_calc,f_para,ratio_f,ratio_hono,everything(),
         -c(hour,no_molecules,nitrate_molecules)) %>% 
  arrange(date)
  

# write.csv(f19,"output/data/f_parameterised19.csv",row.names = F)

# Joining data ------------------------------------------------------------

dat = bind_rows(f15,f19)

# Missing HONO coloured by f ----------------------------------------------

missing_dat = bind_rows(final_dat15,final_dat19)

missing_hono = missing_dat %>%
  filter(hour >= 11 & hour <= 16) %>% #only looking at daytime values
  mutate(lifetime = 1/jhono,
         h = lifetime * dv,
         kdep = 0.01/h,
         no_molecules = no * 2.46 * 10^19 * 10^-12,
         nitrate_molecules = (nitrate_ug_m3 * 10^-12 *6.022 * 10^23)/62.004,
         oh_ppt = oh/(2.46 * 10^19 * 10^-12),
         production_without_nitrate = kp * oh_ppt * no * 3600,
         loss = jhono + (kl * oh_ppt) + kdep,
         loss_hono = loss * hono_ppt * 3600,
         missing_production = loss_hono - production_without_nitrate,
         nitrate_moles = nitrate_ug_m3 * 10^6/62.004,
         nitrate_ppt = (nitrate_moles * 8.314*293.15)/101325,
         jhno3_hr = jhno3 * 3600,
         jhno3_nitrate = jhno3_hr * nitrate_ppt,
         f_calc = missing_production/(jhno3_nitrate),
         f_para = 10^5/((1+80.07*nitrate_ug_m3)*rh),
         ratio_f = f_calc/f_para,
         hono_para = ((kp*oh*no_molecules + (jhno3 * nitrate_molecules * f_para)) / (jhono + (kl*oh) + kdep))
         / (2.46 * 10^19 * 10^-12),
         ratio_hono = hono_ppt/hono_para) %>% 
  select(date,hono_ppt,hono_para,nitrate_ug_m3,f_calc,f_para,ratio_f,ratio_hono,everything(),
         -c(hour,no_molecules,nitrate_molecules)) %>% 
  arrange(date)

missing_hono %>% 
  timeAverage("1 day") %>% 
  filter(is.na(hono_ppt) == F) %>%
  mutate(across(c(upwelling:south_atlantic), ~ na.approx(.x,na.rm =F)),
         polluted_air = north_america+europe,
         african_air = west_africa+sahara+upwelling+sahel+central_africa,
         clean_air = north_atlantic+south_atlantic+upwelling,
         jhno3 = jhno3 *10^3) %>%
  ggplot(aes(ratio_f,ratio_hono)) +
  geom_point(aes(col = as.character(year))) +
  # geom_abline(slope = 1) +
  # geom_smooth(method = "lm",se=F) +
  # geom_text(aes(x = 0.75, y = 50, label = lm_eqn(missing_hono,jhno3,missing_production)), parse = TRUE) +
  theme_bw() +
  theme(legend.position = "top") +
  labs(x = expression(j[HNO[3]]~"*"~nitrate~(ppt~"/"~hour)),
       y = "Missing HONO source (ppt/hour)",
       colour = NULL) +
  scale_colour_viridis_d()

ggsave('missing_hono_jhno3_nitrate.svg',
       path = "output/plots/pss/missing_hono_jhno3",
       width = 30,
       height = 12,
       units = 'cm')

# Looking at hono ---------------------------------------------------------

dat %>% 
  filter(is.na(hono_ppt) == F,
         is.na(hono_para) == F) %>% 
  select(-c(central_africa,south_america,bromide_ug_m3,fluoride_ug_m3,phosphate_ug_m3)) %>%
  # pivot_longer(c(upwelling:south_atlantic)) %>%
  # pivot_longer(c(chloride_ug_m3:calcium_ug_m3,nitrate_ug_m3)) %>%
  # pivot_longer(c(hono_ppt,hono_para)) %>% 
  # ggplot(aes(hono_ppt,value,shape = as.character(year),col = name)) +
  ggplot(aes(north_atlantic,ratio_hono,col = magnesium_ug_m3)) +
  theme_bw() +
  # geom_abline(intercept = 0,slope = 1) +
  geom_point() +
  # geom_smooth(method = "lm",se=F) +
  # facet_wrap(~name,scales="free") +
  labs(shape = NULL,
       # col = "RH (%)",
       x = "North Atlantic air mass (%)",
       y = "HONO ratio") +
  # theme(legend.position = "top") +
  scale_colour_viridis_c() +
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

