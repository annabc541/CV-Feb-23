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
  select(-c(no_ppt,no_lod_ppt,no_err_ppt,flag_no,no2_ppt,flag_met,flag_o3,flag_co,jo1d,jno2))

#getting oh, nox and air mass data from master df - nox data wrong in dataset above
newer_dat15 = read.csv("output/data/all_data_utc.csv") %>% 
  tibble() %>% 
  mutate(date = ymd_hms(date)) %>% 
  filter(date > "2015-11-22 23:00" & date < "2015-12-05") %>% 
  select(date,oh,nitrate,no,no2,jhono1 = jhono,jhno31 = jhno3,upwelling,sahel,sahara,west_africa,central_africa,europe,north_america,south_america,north_atlantic,south_atlantic)

#creating final dataset for f parameterisation for 2015
final_dat15 = left_join(og_dat15,newer_dat15,by = "date") %>% 
  mutate(jhono = ifelse(is.na(jhono),jhono1,jhono),
         jhno3 = ifelse(is.na(jhno3),jhno31,jhno3),
         hour = hour(date),
         day = day(date),
         year = year(date),
         month = month(date)) %>% 
  select(-c(jhono1,jhno31))

#calculating daily calculated and parameterised f
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
  select(day,month,year,everything(),
         -c(date,hour,no_molecules)) %>% 
  group_by(day) %>%
  summarise_all(mean,na.rm = T) %>% 
  ungroup() %>% 
  mutate(f_calc = missing_production/(nitrate_molecules * jhno3),
         f_para = 10^5/((1+80.07*nitrate_ug_m3)*rh),
         ratio = f_calc/f_para) %>% 
  select(-nitrate_molecules)

# write.csv(f15,"output/data/f_parameterised15_all_data.csv",row.names = F)

# August 2019 -------------------------------------------------------------

#dataset from Simone, who got it from Roberto (?),contains majority of info needed for 2019 f parameterisation
og_dat19 = read.csv("data/aerosol_data/cv_data_2019.csv") %>% 
  tibble() %>% 
  mutate(date = ymd_hms(date)) %>% 
  clean_names() %>% 
  select(date,hono_ppt,error_ppt,
         uva_rad_wm2,press_hpa,rh,temp_c,wd,ws,
         o3_mean_ppb,o3_std_ppb,co_mean_ppb,co_std_ppb,
         jhono,jhno3,
         mass_ug_m3,chloride_ug_m3,nitrate_ug_m3,sulfate_ug_m3,oxalate_ug_m3,msa_ug_m3,sodium_ug_m3,ammonium_ug_m3,potassium_ug_m3,magnesium_ug_m3,calcium_ug_m3,dp_um)

#getting oh, nox and air mass data from master df - nox data wrong in dataset above
newer_dat19 = read.csv("output/data/all_data_utc.csv") %>% 
  tibble() %>% 
  mutate(date = ymd_hms(date)) %>% 
  filter(campaign == "August 2019") %>% 
  select(date,oh,nitrate,no,no2,upwelling,sahel,sahara,west_africa,central_africa,europe,north_america,south_america,north_atlantic,south_atlantic)

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
  select(day,month,year,everything(),
         -c(date,hour,no_molecules)) %>% 
  group_by(day) %>%
  summarise_all(mean,na.rm = T) %>% 
  ungroup() %>% 
  mutate(f_calc = missing_production/(nitrate_molecules * jhno3),
         f_para = 10^5/((1+80.07*nitrate_ug_m3)*rh),
         ratio = f_calc/f_para) %>% 
  select(-nitrate_molecules)

# write.csv(f19,"output/data/f_parameterised19_all_data.csv",row.names = F)



# Joining data ------------------------------------------------------------

dat = bind_rows(f15,f19)

#when the ratio is above 1 the calculated f is larger than the parameterised f

# Plotting ----------------------------------------------------------------

#plotting ratio and various factors that could affect it
dat %>% 
  # filter(no2 < 40) %>%
  mutate(year = as.character(year),
         africa = sahel + sahara + west_africa + central_africa,
         ocean = upwelling + north_atlantic + south_atlantic,
         america = north_america,south_america) %>% 
  ggplot(aes(europe,ratio,shape = year)) +
  geom_point() +
  scale_colour_viridis_c()


