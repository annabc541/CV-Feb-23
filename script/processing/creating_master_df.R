library(tidyverse)
library(lubridate)
library(openair)
library(janitor)
library(zoo)
library(viridis)

Sys.setenv(TZ = 'UTC')

#for creating a dataframe with HONO,NOx,nitrate,air masses,met data,OH,j-values
#as far as I know, all the data is in UTC
#HONO 2023 data has been corrected before being read in
#spec rad data from 2023 is corrected when it is read in
#HONO 2015 data is corrected when it is read in
#the three above sets of data are in LT rather than UTC
#all other data is in UTC

# Aerosols ------------------------------------

#2015 and 2019 nitrate data from matt rowlinson
#2015 and 2019 other aerosol data from Roberto via Simone, for 2015 data changeover at midday, changed to midnight
#for consistancy. In 2019 changeover at 01:18ish, data hourly average, then converted to utc, I think the change 
#is at 01:00 rather than midnight because of BST? So have moved the changeover to be at midnight
#TLDR: edits to time to ensure that both sets changeover at midnight

nitrate1519 = read.csv("data/aerosol_data/nitrate_ammonium_CVAO_12-19.csv") %>% 
  clean_names() %>% 
  mutate(date = mdy_hm(start_local_time),
         date = round_date(date,"1 hour"),
         year = year(date),
         month = month(date),
         nitrate_ug_m3 = case_when(year == 2015 & month >= 11 ~ nitrate_mg_m,
                                   year == 2019 & month == 8 ~ nitrate_mg_m,
                                   TRUE ~ NA_real_)) %>% 
  filter(is.na(nitrate_ug_m3) == FALSE) %>% 
  select(date,nitrate_ug_m3)

others15 = read.csv("data/aerosol_data/cv_data_2015.csv") %>%
  tibble() %>%
  mutate(date = ymd_hms(date)) %>%
  clean_names() %>%
  select(date,mass_ug_m3,chloride_ug_m3,sulfate_ug_m3:calcium_ug_m3)

aerosols15 = nitrate1519 %>% 
  filter(date < "2016-01-01") %>% 
  timeAverage("1 day") %>% 
  full_join(others15) %>% 
  arrange(date) %>% 
  mutate(hour = hour(date)) %>% 
  filter(hour == 0) %>% 
  select(-hour)

others19 = read.csv("data/aerosol_data/cv_data_2019.csv") %>%
  tibble() %>%
  mutate(date = ymd_hms(date,tz = "Europe/London")) %>%
  clean_names() %>% 
  select(date,mass_ug_m3,chloride_ug_m3,sulfate_ug_m3:calcium_ug_m3) %>% 
  timeAverage("1 hour") %>% 
  with_tz("UTC")

aerosols19 = nitrate1519 %>% 
  filter(date > "2016-01-01") %>% 
  full_join(others19) %>% 
  arrange(date) %>% 
  mutate(hour = hour(date)) %>% 
  filter(hour == 0) %>% 
  select(-hour)

aerosols23 = read.csv("data/aerosol_data/cvao_aerosols23.csv") %>% 
  mutate(date = dmy_hm(Start)) %>%
  clean_names() %>% 
  rename_with(.fn = function(.x){paste0(.x,"_ug_m3")},
              .cols=-c(sample_id:stop,p_h,date)) %>% 
  select(date,pH = p_h,mass_ug_m3 = mass_conc_ug_m3,everything(),-c(sample_id,start,stop))

all_aerosols = bind_rows(aerosols23,aerosols15,aerosols19) %>% 
  arrange(date)

remove(aerosols23,aerosols15,aerosols19,nitrate1519,others15,others19)

#nitrate data for Feb 2023 from machine learning
# nitrate_dat_ml = read.csv("data/aerosol_data/CVAO_Nitrate_Prediction_Feb2023.csv") %>%
#   mutate(date = ymd(date)) %>%
#   select(date,nitrate_ml = nitrate_ug_m3)

# Air masses, OH and spec rad ---------------------------------------------

air_mass = read.csv("data/new_CVAO_sector_%_boxes_1.csv") %>% 
  rename(date = X) %>% 
  mutate(date = ymd_hm(date)) %>% 
  clean_names()

oh_dat = read.csv("data/OH_Precision_051223.csv") %>% 
  clean_names() %>% 
  mutate(date = dmy_hm(time)) %>%
  timeAverage("1 hour")

spec_rad = read.csv("data/spec_rad/spec_rad_processed.csv") %>% 
  tibble() %>% 
  mutate(date = ymd_hms(date))

# Met data ----------------------------------------------------------------

met_data_historic = read.csv("data/met_data/2006-2022_Met_O3_data.csv") %>% 
  mutate(date = ymd_hms(date),
         date = round_date(date,"1 hour")) %>% 
  filter(date > "2015-11-01" & date < "2023-01-01") %>% 
  select(date,ws = WINDSPD_10M,wd = WINDDIR_10M,temp = TEMP_10M,rh = RH_10M)

met_data23 = read.csv("data/met_data/met2023.csv") %>% 
  mutate(date = ymd_hms(date)) %>% 
  timeAverage("1 hour",vector.ws = T) %>% #data in minute averages, averaged to an hour to combine with prev met data
  select(date,ws,wd,temp,rh = RH)

met_data = bind_rows(met_data_historic,met_data23)

remove(met_data23,met_data_historic)

# NOx data ----------------------------------------------------------------

#reading in nox data with uncertainties
#from ebas data wrangling R script

no15 = read.csv("data/nox_data_thesis/no15.csv") %>% 
  mutate(date = ymd_hms(date)) %>% 
  select(date,no_ppt = no_ppt_ebas,no_u_ppt)

nox19 = read.csv("data/nox_data_thesis/nox19.csv") %>% 
  mutate(date = ymd_hms(date))

nox20 = read.csv("data/nox_data_thesis/nox20.csv") %>% 
  mutate(date = ymd_hms(date))

nox23 = read.csv("~/Cape Verde/nox/processing/ozone_correction/processed_data/nox2023.csv") %>% 
  mutate(date = ymd_hms(date))

nox = bind_rows(no15,nox19,nox20,nox23) %>% 
  select(-c(no_flag,no2_flag,no_lod_ppt,no2_lod_ppt))

remove(no15,nox19,nox20,nox23)

# HONO data ---------------------------------------------------------------

#hourly data
hono15 = read.csv("data/hono/hono2015.csv") %>% 
  mutate(date = dmy_hm(date),
         date = date + 3600) %>% #changing date to UTC 
  rename(hono = HONO_adj_v2) %>% 
  select(date,hono)

#5 min average
hono19 = read.csv("data/hono/roberto_data/lopap_aug2019.csv") %>% 
  mutate(date = dmy_hms(start.gmt)) %>% 
  select(date,hono = hono.ppt,hono_err = error.ppt) %>% 
  timeAverage("1 hour")

#5 min average
hono20 = read.csv("data/hono/roberto_data/lopap_feb2020.csv") %>% 
  mutate(date = dmy_hms(start.gmt),
         sus_flag = case_when(date > "2020-02-16 07:30" & date < "2020-02-16 12:00" ~ 1,
                              TRUE ~ 0),
         hono = ifelse(sus_flag == 1,NA_real_,hono.ppt)) %>% 
  select(date,hono) %>% 
  timeAverage("1 hour")

#hourly data for errors
hono23 = read.csv("output/data/hono23_hourly_utc.csv") %>% 
  mutate(date = ymd_hms(date)) %>% 
  select(date,hono,hono_err)

hono = bind_rows(hono15,hono19,hono20,hono23)
remove(hono15,hono19,hono20,hono23)

# Joining data together ---------------------------------------------------

df_list = list(hono,nox,oh_dat,all_aerosols,spec_rad,met_data,air_mass)

dat = df_list %>% reduce(full_join,by = "date") %>% 
  arrange(date) %>% 
  timeAverage("1 hour") %>% #for some reason 2019 and 2020 data is duplicated, averaging to fix that
  mutate(year = year(date),
         month = month(date),
         date = format(date, "%Y-%m-%d %H:%M:%S"),
         keep_data = case_when(year == 2015 & month >= 11 ~ 1,
                               year == 2019 & month == 8 ~ 1,
                               year == 2020 & month == 2 ~ 1,
                               year == 2023 & month == 2 ~ 1)) %>% 
  filter(keep_data == 1) %>% 
  select(date,month,year,everything(),-keep_data) 

#currently not modifying, filling or converting any of the data assembled, leave that to analysis codes

  # mutate(oh = ifelse(campaign != "February 2023",2 * 10^6,oh), #molecules cm-3
  #        nitrate_molecules_cm3 = case_when(campaign == "February 2020" ~ 1.20 * 10^10,
  #                            TRUE ~ (nitrate_ug_m3 * 10^-12 *6.022 * 10^23)/62.004))

#with measured nitrate from 2023
write.csv(dat,"output/data/all_data_utc_updated_nox_hono.csv",row.names = F)
