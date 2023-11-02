library(tidyverse)
library(lubridate)
library(openair)
library(janitor)
library(zoo)
library(viridis)

Sys.setenv(TZ = 'UTC')

#for creating a dataframe with HONO,NOx,nitrate,air masses,met data,OH
#as far as I know, all the data is in UTC
#HONO 2023 data has been corrected before being read in
#spec rad data from 2023 is corrected when it is read in
#HONO 2015 data is corrected when it is read in
#the three above sets of data are in LT rather than UTC
#all other data is in UTC

# Nitrate,air masses,OH ----------------------------------------------------

nitrate_dat = read.csv("data/aerosol_data/nitrate_ammonium_CVAO_12-19.csv") %>% 
  clean_names() %>%
  mutate(date = mdy_hm(start_local_time),
         month = month(date),
         date = round_date(date, "6 hour")) %>% 
  select(date,nitrate_ug_m3 = nitrate_mg_m)

air_mass = read.csv("data/new_CVAO_sector_%_boxes_1.csv") %>% 
  rename(date = X) %>% 
  mutate(date = ymd_hm(date)) %>% 
  clean_names()

oh_dat = read.csv("data/OH_provisional.csv") %>% 
  clean_names() %>% 
  mutate(date = dmy_hm(time),
         oh = na.approx(oh_molecules_cm_3,na.rm = F)) %>% #interpolate missing values
  select(date,oh) %>% 
  timeAverage("1 hour")

# Spec rad ----------------------------------------------------------------

#using calculated values rather than measured values because of weird cal (see presentation from Katie)
spec_rad15 = read.csv("data/spec_rad/jrates_all_new_2015-2020.csv") %>% 
  clean_names() %>%
  mutate(date = dmy_hm(date)) %>% 
  # filter(date > "2015-11-23" & date < "2015-12-04") %>%
  timeAverage("1 hour") %>% 
  select(date,jhono = jhono_calc,jhno3 = jhno3_calc)

spec_rad1920 = read.csv("data/spec_rad/2016_2020_Spec_rad_Hourly.csv") %>% 
  clean_names() %>% 
  mutate(date = dmy_hm(date),
         jhono = case_when(date > "2020-02-14 01:00" & date < "2020-02-26 02:56" ~ jhono_calc,
                           is.na(j_hono) ~ jhono_calc,
                           TRUE ~ j_hono),
         jhno3 = case_when(date > "2020-02-14 01:00" & date < "2020-02-26 02:56" ~ jhno3_calc,
                           is.na(j_hno3) ~ jhno3_calc,
                           TRUE ~ j_hno3)) %>% 
  select(date,jhono,jhno3)

spec_rad23 = read.csv("data/spec_rad/Specrad_hour_23_with_calc_UPDATED.csv") %>% 
  mutate(date = dmy_hm(date),
         hour = hour(date),
         doy = yday(date)) %>% 
  filter(date >= "2023-02-07 08:35" & date < "2023-02-27") %>% 
  clean_names() %>% 
  mutate(jhono = ifelse(is.na(j_hono),jhono_calc,j_hono),
         jhno3 = ifelse(is.na(j_hno3),jhno3_calc,j_hno3)) %>% 
  select(date,hour,jhono,jhno3)

#fill NAs with averages from hours where spec rad data is missing when reading data in
#should only be used for nighttime values, since calculated spec rad values are only for daytime
#missing daytime values should be replaced by calculated values
spec_rad_to_fix = bind_rows(spec_rad15,spec_rad1920,spec_rad23) %>% 
  mutate(hour = hour(date))

#find average jhono and jhno3 values for each hour
spec_rad_mean = spec_rad_to_fix %>% 
  group_by(hour) %>% 
  summarise(jhono_avg = mean(jhono,na.rm = T),
            jhno3_avg = mean(jhno3,na.rm = T))

#replace NAs with average value for that hour
spec_rad = left_join(spec_rad_to_fix,spec_rad_mean,by = "hour") %>% 
  mutate(jhono = ifelse(is.na(jhono),jhono_avg,jhono),
         jhno3 = ifelse(is.na(jhno3),jhno3_avg,jhno3)) %>% 
  select(-c(jhono_avg,jhno3_avg))

remove(spec_rad1920,spec_rad23,spec_rad_mean,spec_rad_to_fix,spec_rad15)

# Met data ----------------------------------------------------------------

met_data_historic = read.csv("data/met_data/2006-2021_Met_O3_data.csv") %>% 
  mutate(date = dmy_hm(date)) %>% 
  select(date,ws = WINDSPD_10M,wd = WINDDIR_10M,temp = TEMP_10M,rh = RH_10M)

met_data23 = read.csv("data/met_data/met2023.csv") %>% 
  mutate(date = ymd_hms(date)) %>% 
  timeAverage("1 hour",vector.ws = T) %>% #data in minute averages, averaged to an hour to combine with prev met data
  select(date,ws,wd,temp,rh = RH)

met_data = bind_rows(met_data_historic,met_data23)

remove(met_data23,met_data_historic)


# NOx data ----------------------------------------------------------------

nox15 = read.csv("data/nox_data/nox15.csv") %>% 
  mutate(date = ymd_hms(X)) %>% 
  filter(date > "2015-11-01") %>% 
  timeAverage("1 hour") %>% 
  select(date,no = NO_Conc_art_corrected,no2 = NO2_Conc_art_corrected)

nox19 = read.csv("data/nox_data/nox19.csv") %>% 
  mutate(date = ymd_hms(X)) %>% 
  filter(date > "2019-08-14" & date < "2019-08-30") %>% 
  timeAverage("5 min") %>% 
  select(date,no = NO_Conc_art_corrected,no2 = NO2_Conc_diode)

nox20 = read.csv("data/nox_data/nox20.csv") %>% 
  mutate(date = ymd_hms(X)) %>% 
  filter(date > "2020-02-13" & date < "2020-02-28") %>% 
  timeAverage("5 min") %>% 
  select(date,no = NO_Conc_art_corrected,no2 = NO2_Conc_diode)

nox23 = read.csv("data/nox_data/nox23.csv") %>% 
  mutate(date = ymd_hms(X)) %>% 
  filter(date > "2023-02-07" & date < "2023-02-27") %>% 
  timeAverage("5 min") %>%
  select(date,no = NO_Conc_art_corrected,no2 = NO2_Conc_diode)

nox = bind_rows(nox15,nox19,nox20,nox23)

remove(nox15,nox19,nox20,nox23)


# HONO data ---------------------------------------------------------------

#hourly data
hono15 = read.csv("data/hono2015.csv") %>% 
  mutate(date = dmy_hm(date),
         date = date + 3600) %>% #changing date to UTC 
  rename(hono = HONO_adj_v2) %>% 
  select(date,hono)

#5 min average
hono19 = read.csv("data/roberto_data/lopap_aug2019.csv") %>% 
  mutate(date = dmy_hms(start.gmt)) %>% 
  select(date,hono = hono.ppt,hono_err = error.ppt)

#5 min average
hono20 = read.csv("data/roberto_data/lopap_feb2020.csv") %>% 
  mutate(date = dmy_hms(start.gmt),
         sus_flag = case_when(date > "2020-02-16 07:30" & date < "2020-02-16 12:00" ~ 1,
                              TRUE ~ 0),
         hono = ifelse(sus_flag == 1,NA_real_,hono.ppt)) %>% 
  select(date,hono)

#hourly data for errors
hono23 = read.csv("output/data/hono23_hourly_utc.csv") %>% 
  mutate(date = ymd_hms(date)) %>% 
  select(date,hono,hono_err)

hono = bind_rows(hono15,hono19,hono20,hono23)
remove(hono15,hono19,hono20,hono23)

# Joining data together ---------------------------------------------------

df_list = list(hono,nox,oh_dat,nitrate_dat,spec_rad,met_data,air_mass)

dat = df_list %>% reduce(full_join,by = "date") %>% 
  arrange(date) %>% 
  filter(date < "2023-02-28",
         date > "2015-11-23") %>% 
  timeAverage("1 hour") %>% 
  mutate(campaign = case_when (date > "2015-11-24 17:00" & date < "2015-12-03 19:00" ~ "November 2015",
                               date > "2019-08-15 12:29" & date < "2019-08-29 01:00" ~ "August 2019",
                               date > "2020-02-14 01:00" & date < "2020-02-26 02:56" ~ "February 2020",
                               date >= "2023-02-07 08:35" ~ "February 2023",
                               TRUE ~ "no campaign"),
         oh = ifelse(campaign != "February 2023",2 * 10^6,oh), #molecules cm-3
         nitrate_molecules_cm3 = case_when(campaign == "February 2020" ~ 1.20 * 10^10,
                             campaign == "February 2023" ~ 1.20 * 10^10,
                             TRUE ~ (nitrate_ug_m3 * 10^-12 *6.022 * 10^23)/62.004))#molecules cm-3

write.csv(dat,"output/data/all_data_utc.csv",row.names = F)

# nitrate_dat %>% 
#   filter(date > "2015-11-23" & date < "2015-12-03 19:00") %>% 
#   # timeAverage("1 hour") %>% 
#   ggplot(aes(date,nitrate)) +
#   geom_point()
