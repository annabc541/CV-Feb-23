library(tidyverse)
library(lubridate)
library(janitor)
library(plotly)
library(openair)
library(zoo)
library(imputeTS)

Sys.setenv(TZ = 'UTC')

#creating data in the format to be used as constraints for box modelling
#photolysis rates from Simone's work can probably be kept constant
#output from box model will be NOx
#constraints will be environmental (temp,pressure, BL height (= 1 in Simone's box model), RH)
#species constraints: HONO, O3, BrO, IO & VOCs, CO, CH4? For Feb23 have measurements of OH, HO2 and RO2
#IO and BrO won't be different from what Simone used


# Functions ---------------------------------------------------------------

ppt_to_molecules_cm3 <- function(x){y = x * 2.46 * 10^19 * 10^-12}
ppb_to_molecules_cm3 <- function(x){y = x * 2.46 * 10^19 * 10^-9}

# Reading in data ---------------------------------------------------------

hono_nox_oh = read.csv("output/data/all_data_utc_updated_nox_hono.csv") %>% 
  remove_constant() %>% 
  remove_empty() %>% 
  mutate(date = ymd_hms(date),
         hour = hour(date)) %>% 
  filter(date >= "2023-02-07" & date < "2023-02-27") %>% #dates when HONO measurements are available
  select(date,hour,hono,no_ppt,no2_ppt,oh)

rox = read.csv("data/ROx.csv") %>% 
  mutate(date = dmy_hm(Time)) %>% 
  filter(date >= "2023-02-07" & date < "2023-02-27") %>% 
  select(date,everything(),-c(Time,Rox)) %>% 
  clean_names() %>% 
  timeAverage("1 hour")

#in merged data don't have toluene, which was used to constrain Simone's model
#in merged data have isoprene and ethanol, which weren't included in Simone's model
cv_merge = read_csv("data/20240507_CV_merge.csv") %>% 
  mutate(date = dmy_hm(date)) %>% 
  filter(date >= "2023-02-07" & date < "2023-02-27") %>% 
  remove_constant() %>% 
  remove_empty() %>% 
  clean_names() %>% 
  mutate(j_hono = ifelse(is.na(j_hono),jhono_calc,j_hono),
         across(c(j_o1d:j_oio),
                ~ ifelse(date >= "2023-02-20" & date < "2023-02-21",NA_real_,.x))) %>% 
  select(date,temp_10m_deg_c,rh_10m_percent,atmospheric_pressure_h_pa,o3_ppb_v,co_ppb_v,ch4_all_ppb_v,ethane:isoprene,
         acetaldehyde:ethanol,j_o1d:j_oio)

df_list = list(hono_nox_oh,rox,cv_merge)

dat = df_list %>% reduce(full_join,by = "date") %>% 
  arrange(date) %>% 
  rename(temp = temp_10m_deg_c,#in celsius
         rh = rh_10m_percent,
         pressure = atmospheric_pressure_h_pa,#in hPa
         o3_ppb = o3_ppb_v,
         co_ppb = co_ppb_v,
         ch4_ppb = ch4_all_ppb_v) %>% 
  rename_with(.cols = c(hono,ethane:ethanol),.fn = ~ paste0(., "_ppt")) %>% 
  rename_with(.cols = c(oh:ro2),.fn = ~ paste0(., "_molecules_cm3"))

remove(cv_merge,df_list,hono_nox_oh,rox)

# Filling NAs -------------------------------------------------------------

#met data (ws,wd,temp,rh,pressure) and o3 don't have any NAs
#ch4 and co only have a small gap and are not diurnal - used interpolation to fill NAs
#for HONO, NOx and ROx filled the NAs using the monthly average for the missing hour
#for VOCs took the mean for each missing hour from the previous and following days, then used rolling mean to fill remaining NAs

diurnals = dat %>% 
  group_by(hour) %>% 
  summarise(across(c(hono_ppt:ro2_molecules_cm3,j_o1d:j_oio),
                   ~ mean(.,na.rm = T),
                   .names = "{col}_hourly_mean"))
# rename_with(.cols = -hour,.fn = ~ paste0(.,"_hourly_mean"))

dat_filled = dat %>% 
  left_join(diurnals,by = "hour") %>% 
  mutate(across(c(hono_ppt:ro2_molecules_cm3,j_o1d:j_oio),
                ~ ifelse(is.na(.x),get(paste0(cur_column(),"_hourly_mean")),.x)),
         # ,.names = "{.col}_filled"),
         across(c(co_ppb,ch4_ppb),
                ~ na.approx(.x,na.rm = F)),
         #.names = "{.col}_filled"),
         only_date = as.Date(date)) %>% 
  left_join(dat %>% 
              mutate(only_date = as.Date(date)) %>% 
              dplyr::transmute(only_date = only_date + 1,hour,across(c(ethane_ppt:ethanol_ppt),
                                                                     .names = "{.col}_prev")),
            by = c("only_date","hour")) %>% 
  left_join(dat %>% 
              mutate(only_date = as.Date(date)) %>% 
              dplyr::transmute(only_date = only_date - 1,hour,across(c(ethane_ppt:ethanol_ppt),
                                                                     .names = "{.col}_next")),
            by = c("only_date","hour")) %>% 
  rowwise() %>% 
  mutate(across(c(ethane_ppt:ethanol_ppt),~mean(c_across(c(paste0(cur_column(),"_prev"),
                                                           paste0(cur_column(),"_next"))),
                                                na.rm = T),
                .names = "{.col}_avg")) %>% 
  ungroup() %>% 
  mutate(across(c(ethane_ppt:ethanol_ppt),
                ~ifelse(is.na(.x),get(paste0(cur_column(),"_avg")),.x)),
         #.names = "{.col}_avg_filled"),
         #removing artificially high hour filled through previous method - not a problem for ethanol and methanol
         across(c(ethane_ppt:acetaldehyde_ppt,acetone_ppt),
                ~ ifelse(date > "2023-02-10 10:00" & date < "2023-02-10 12:00",NA_real_,.x)),
         across(c(ethane_ppt:ethanol_ppt),~{
           roll_mean = rollapply(.x,width = 15,FUN = mean, fill = NA,align = "center",na.rm = T)
           ifelse(is.na(.x),roll_mean,.x)}),
         methanol_ppt = na.approx(methanol_ppt,na.rm = F)) %>% 
  #.names = "{.col}_rollmean")) %>% 
  select(date:j_oio)

remove(diurnals)

# Units -------------------------------------------------------------------

#converting mixing ratios into molecules cm-3
#temperature into K
#also removing any negative values by setting them to 0 (can't have a negative photolysis rate, so have set these to 10^-10)

dat_units = dat_filled %>% 
  mutate(across(c(hono_ppt,no_ppt,no2_ppt,ethane_ppt:ethanol_ppt),
                ~ ppt_to_molecules_cm3(.)),
         across(c(o3_ppb:ch4_ppb),
                ~ ppb_to_molecules_cm3(.)),
         temp_k = temp + 273.15,
         across(c(hono_ppt:ethanol_ppt),
                ~ifelse(.x<0,0,.x)),
         across(c(j_o1d:j_oio),
                ~ifelse(.x<0,10^-10,.x)),
         index = row_number()) %>% 
  rename_with(~ sub("_pp[bt]$","_molecules_cm3",.)) %>% 
  select(-c(temp,hour))

# Formatting for atmchem2 -------------------------------------------------

#making a "seconds from 0" column, increasing in increments of 3600 (seconds in an hour)
x = seq(from = 0, to = 1724400, by = 3600)
x = as.data.frame(x) %>% 
  rename(seconds_since_0 = x) %>% 
  mutate(index = row_number())

#joining this to main df
dat_sec_since_0 = dat_units %>% 
  left_join(x,by = "index") %>% 
  select(date,seconds_since_0,everything(),-index)

#extract the first 24 rows (first day of data - for model spin up)
first_day = dat_atchem2 %>% slice(1:24)
#repeat the first day 3 times
prepended_data = first_day %>% 
  slice(rep(1:n(),times = 3))
#slice doesn't work with 3.5, so to make the model start at midday, need to take the second half of the first day and append it
half_day = first_day %>% slice(13:24)
prepended_data = bind_rows(half_day,prepended_data) %>% 
  mutate(seconds_since_0 = seq(from = 0,by = 3600,length.out = n()))

#adjust the seconds_since_0 column to accomodate the prepended data
dat_atchem2 = dat_sec_since_0 %>% 
  mutate(seconds_since_0 = seconds_since_0 + max(prepended_data$seconds_since_0) + 3600)

dat_atchem2_final = bind_rows(prepended_data,dat_atchem2)

# atchem2 - species -------------------------------------------------------

atchem2_species = dat_atchem2_final %>% 
  select(seconds_since_0:ro2_molecules_cm3,o3_molecules_cm3:ethanol_molecules_cm3) %>% 
  rename_with(.fn = ~ str_remove(.,"_molecules_cm3"),.cols = -seconds_since_0) %>% 
  rename(C2H6 = ethane,
         C2H4 = ethene,
         C3H8 = propane,
         IC4H10 = iso_butane,
         NC4H10 = n_butane,
         C2H2 = acetylene,
         IC5H12 = iso_pentane,
         NC5H12 = n_pentane,
         C5H8 = isoprene,
         BENZENE = benzene,
         CH3COCH3 = acetone,
         CH3OH = methanol,
         C2H5OH = ethanol,
         CH3CHO = acetaldehyde) %>% 
  rename_with(.fn = ~ str_to_upper(.),.cols = -seconds_since_0) %>% 
  pivot_longer(c(HONO:C2H5OH),names_to = "variable",values_to = "conc")

setwd("C:/Users/anna_/Documents/Cape Verde/peroxy_campaign/box_modelling/constraints/species")

for(spc in unique(atchem2_species$variable)){
  output = atchem2_species[atchem2_species$variable == spc,] %>%
    select(-variable)
  
  write.table(output,paste0(spc),row.names = F, col.names = F,sep = ",")
}


# atchem2 - environment ---------------------------------------------------

atchem2_environment = dat_atchem2_final %>% 
  select(seconds_since_0,RH = rh,PRESS = pressure,TEMP = temp_k) %>% 
  pivot_longer(-seconds_since_0,names_to = "variable",values_to = "value")

setwd("C:/Users/anna_/Documents/Cape Verde/peroxy_campaign/box_modelling/constraints/environment")

for(spc in unique(atchem2_environment$variable)){
  output = atchem2_species[atchem2_species$variable == spc,] %>%
    select(-variable)
  
  write.table(output,paste0(spc),row.names = F, col.names = F,sep = ",")
}


# atchem2 - photolysis ----------------------------------------------------

atchem2_photolysis = dat_atchem2_final %>% 
  select(seconds_since_0,j_o1d:j_oio) %>% 
  pivot_longer(-seconds_since_0,names_to = "variable",values_to = "value")

setwd("C:/Users/anna_/Documents/Cape Verde/peroxy_campaign/box_modelling/constraints/photolysis")

for(spc in unique(atchem2_photolysis$variable)){
  output = atchem2_species[atchem2_species$variable == spc,] %>%
    select(-variable)
  
  write.table(output,paste0(spc),row.names = F, col.names = F,sep = ",")
}
