library(tidyverse)
library(lubridate)
library(openair)
library(janitor)
library(zoo)
library(viridis)

#defining constant values used for PSS calculations

kp = 3.3 * 10^-11 #rate constant for oh + no -> hono (from Atkinson et al. 2004)
kl = 6 * 10^-12 #rate constant for oh + hono -> h2o + no2 (from Atkinson et al. 2004)
dv = 0.3 #deardfroff velocity, value used by Simone
nitrate = 1.20 * 10^10 #constant value until more recent measurements are received from TROPOS

#calculated daytime enhancement factor (if I calculated it correctly) is either 60 (dep velocity = 0.03)
#or 58 (dep velocity = 0.01) for February 2023 campaign
#f = 20 for August 2019 campaign and f = 11 for February 2020 campaign (dep velocity = 0.01 for both of those calculations)

# Importing data ----------------------------------------------------------

# dat = read.csv("output/data/all_data.csv") %>% 
#   mutate(date = ymd_hms(date))

dat_all = read.csv("output/data/all_data_updated.csv") %>% 
    mutate(date = ymd_hms(date))

# Photostationary state calculations --------------------------------------

#hono = k[OH][NO]+ jHNO3 * f * pNO3 / jHONO + k[OH] + kdep

#units for calculation are molecule per cm3

#can change f and kdep and other parameters as needed and compare them

pss_calc = dat_all %>% 
  mutate(hour = hour(date),
         lifetime = ifelse(hour >= 11 & hour <= 15,1/jhono,NA_real_),
         lifetime = na.approx(lifetime,na.rm = FALSE),
         nitrate = na.approx(nitrate,na.rm = FALSE),
         h = lifetime * dv,
         kdep1 = 0.01/h,
         kdep3 = 0.03/h,
         no_molecules = no * 2.46 * 10^19 * 10^-12,
         pss1 = case_when(campaign == "August 2019" ~ ((kp*oh*no_molecules + (jhno3 * nitrate * 20)) / (jhono + (kl*oh) + kdep1))
                         / (2.46 * 10^19 * 10^-12),
                         campaign == "February 2020" ~ ((kp*oh*no_molecules + (jhno3 * nitrate * 11)) / (jhono + (kl*oh) + kdep1))
                         / (2.46 * 10^19 * 10^-12),
                         campaign == "February 2023" ~ ((kp*oh*no_molecules + (jhno3 * nitrate * 63)) / (jhono + (kl*oh) + kdep1))
                         / (2.46 * 10^19 * 10^-12)),
         pss3 = ((kp*oh*no_molecules + (jhno3 * nitrate * 67)) / (jhono + (kl*oh) + kdep3))
         / (2.46 * 10^19 * 10^-12))

pss_calc %>% 
  filter(campaign == "February 2023") %>% 
  rename('Dep velocity = 1' = pss1,
         'Dep velocity = 3' = pss3,
         'Observed' = hono) %>% 
  pivot_longer(c('Dep velocity = 1','Dep velocity = 3','Observed')) %>%
  ggplot(aes(date,value,col = name)) +
  theme_bw() +
  labs(x = "Datetime (UTC)",
       y = "HONO (ppt)",
       color = NULL) +
  geom_path(size = 0.8) +
  theme(legend.position = "top") +
  scale_x_datetime(breaks = "1 day",date_labels = "%d/%m") +
  scale_color_viridis_d() +
  # facet_grid(rows = vars(name)) +
  # facet_wrap(vars(campaign),scales = "free", ncol =1) +
  NULL

ggsave('fcalc_dep_vel.svg',
       path = "output/plots/pss/feb23",
       width = 30,
       height = 12,
       units = 'cm')


# Diurnals ----------------------------------------------------------------

#can change what campaign we are seeing the diurnals for
diurnal = pss_calc %>% 
  filter(campaign == "February 2023",
         is.na(hono) == FALSE) %>% 
  rename(HONO = hono,'f = 63' = pss1, 'f = 68' = pss3) %>% 
  timeVariation(pollutant = c("HONO",'f = 63','f = 68'))

diurnal_dat = diurnal$data$hour

diurnal_dat %>% 
  ggplot(aes(hour,Mean,col = variable)) +
  geom_line(size = 1) +
  scale_color_manual(values = viridis(3)) +
  theme_bw() +
  labs(x = "Hour of day (UTC)",
       y = "HONO (ppt)",
       color = NULL) +
  scale_x_continuous(breaks = c(0,4,8,12,16,20)) +
  ylim(-1.5,13) + #in order to have same sized axes for diurnals for all three campaigns
  theme(legend.position = "top")

ggsave('diurnal_f_calc_dep_vel.svg',
       path = "output/plots/pss/feb23",
       width = 11,
       height = 13,
       units = 'cm')


# Diurnals 15 min average -------------------------------------------------

#importing all the data individually and then creating one dataframe - data is averaged to 15 minutes
#interpolation for spec rad data as it is hourly

spec_rad = read.csv("data/Specrad_hour_23_with_calc.csv") %>% 
  mutate(date = dmy_hm(date),
         hour = hour(date)) %>% 
  clean_names() %>% 
  mutate(jhono = ifelse(is.na(j_hono),jhono_calc,j_hono),
         jhno3 = ifelse(is.na(j_hno3),jhno3_calc,j_hno3)) %>% 
  select(date,hour,jhono,jhno3)

#to remove NAs in jhono and jhno3 values

#find average jhono and jhno3 values for each hour
spec_rad_mean = spec_rad %>% 
  group_by(hour) %>% 
  summarise(jhono_avg = mean(jhono,na.rm = T),
            jhno3_avg = mean(jhno3,na.rm = T))

#replace NAs with average value for that hour
spec_rad_full = left_join(spec_rad,spec_rad_mean,by = "hour") %>% 
  mutate(jhono = ifelse(is.na(jhono),jhono_avg,jhono),
         jhno3 = ifelse(is.na(jhno3),jhno3_avg,jhno3)) %>% 
  select(-c(jhono_avg,jhno3_avg)) %>% 
  timeAverage("15 min") %>% 
  mutate(jhono = na.approx(jhono,na.rm = FALSE),
         jhno3 = na.approx(jhno3,na.rm = FALSE))

oh_dat = read.csv("data/OH_provisional.csv") %>% 
  clean_names() %>% 
  mutate(date = dmy_hm(time),
         oh = na.approx(oh_molecules_cm_3,na.rm = F)) %>% #interpolate missing values
  select(date,oh) %>% 
  timeAverage("15 min")

nox_dat = read.csv("data/nox_data/nox23.csv") %>% 
  mutate(date = ymd_hms(X)) %>% 
  filter(date > "2023-02-01" & date < "2023-03-01") %>% 
  select(date,no = NO_Conc_art_corrected) %>%  
  timeAverage("15 min")

hono_dat = read.csv("output/data/processed_in_r2.csv") %>% 
  mutate(date = dmy_hms(date)) %>% 
  select(date,hono) %>% 
  timeAverage("15 min")

df_list = list(nox_dat,oh_dat,hono_dat,spec_rad_full)


dat = df_list %>% reduce(full_join,by = "date") %>% arrange(date) %>% 
  filter(date > "2023-02-07 08:35",
         date < "2023-02-28") %>% 
  mutate(hour = hour(date))

pss_calc = dat %>% 
  mutate(hour = hour(date),
         lifetime = ifelse(hour >= 11 & hour <= 15,1/jhono,NA_real_),
         lifetime = na.approx(lifetime,na.rm = FALSE),
         h = lifetime * dv,
         kdep1 = 0.01/h,
         no_molecules = no * 2.46 * 10^19 * 10^-12,
         pss = ((kp*oh*no_molecules + (jhno3 * nitrate * 58)) / (jhono + (kl*oh) + kdep1))
                         / (2.46 * 10^19 * 10^-12))

diurnal = pss_calc %>% 
  filter(is.na(hono) == FALSE) %>% 
  mutate(minute = minute(date),
         hour = hour + (minute/60)) %>% 
  group_by(hour) %>% 
  summarise(hono = mean(hono,na.rm = TRUE),
            pss = mean(pss,na.rm = TRUE)) %>% 
  ungroup()

diurnal %>% 
  rename(HONO = hono,PSS = pss) %>% 
  pivot_longer(c(HONO,PSS)) %>% 
  ggplot(aes(hour,value,col = name)) +
  geom_line(size = 1) +
  scale_color_manual(values = viridis(2)) +
  theme_bw() +
  labs(x = "Hour of day (UTC)",
       y = "HONO (ppt)",
       color = NULL) +
  scale_x_continuous(breaks = c(0,4,8,12,16,20)) +
  ylim(-1.5,12) + #in order to have same sized axes for diurnals for all three campaigns
  theme(legend.position = "top")

ggsave('15min_diurnal_hono_pss.svg',
       path = "output/plots/red_shift",
       width = 11,
       height = 13,
       units = 'cm')

# Calculating enhancement factor ------------------------------------------

#can change anything in code below to find f for different parameters

#for deposition velocity = 0.01 f = 63 (used to be f = 57 before mfc cal)
#for deposition velocity = 0.03 f = 68 (used to be f = 61 before mfc cal)

#for August 2019 f = 20 (deposition velocity = 0.01)
#for February 2020 f = 11 (deposition velocity = 0.01)

#finding enhancement factor for different campaigns
f_calc_all = pss_calc %>%   
  filter(campaign == "February 2023",
         # date < "2020-02-26",
         hour >= 11 & hour <= 15) %>%
  mutate(lifetime = 1/jhono,
         h = lifetime * dv,
         kdep = 0.03/h, #change as needed
         production_without_nitrate = kp*oh*no_molecules,
         loss = jhono + (kl*oh) + kdep,
         loss_hono = loss * hono * 2.46 * 10^19 * 10^-12,
         missing_production = loss_hono - production_without_nitrate)

#finding f - daytime median of missing production and jhno3 used
#specifically between 10 and 15 local time - 11 and 16 UTC
missing_production23 = mean(f_calc_all$missing_production,na.rm = TRUE)
jhno3_23 = mean(f_calc_all$jhno3,na.rm = TRUE)
nitrate_23 = mean(f_calc_all$nitrate,na.rm = TRUE)
f_feb23 = missing_production23/(nitrate_23*jhno3_23)




# Calculating daily enhancement factor ------------------------------------

#finding f for each day of the campaign and seeing how it changes with air masses and aerosol composition (eventually)

f_daily = dat %>% 
  mutate(hour = hour(date),
         day = day(date)) %>% 
  filter(campaign == "February 2023",
         hour >= 11 & hour <= 15) %>% 
  mutate(lifetime = 1/jhono,
         h = lifetime * dv,
         kdep = 0.01/h,
         no_molecules = no * 2.46 * 10^19 * 10^-12,
         production_without_nitrate = kp*oh*no_molecules,
         loss = jhono + (kl*oh) + kdep,
         loss_hono = loss * hono * 2.46 * 10^19 * 10^-12,
         missing_production = loss_hono - production_without_nitrate,
         f = missing_production/(nitrate*jhno3)) %>% 
  group_by(day) %>% 
  summarise(f = mean(f,na.rm = TRUE),
            hono = mean(hono,na.rm = TRUE),
            sahara = mean(sahara,na.rm = TRUE))

f_daily %>% 
  filter(is.na(f) == FALSE) %>% 
  ggplot(aes(hono,f,col = sahara)) + 
  geom_point() +
  scale_colour_viridis()

# Production and loss mechanisms ------------------------------------------

#diurnal cycle were we see how much each reaction contributes to HONO production and loss
#and we can see if the production and loss mechanisms balance out
#remember to change the value of f depending on campaign we are looking at

prod_loss = pss_calc %>% 
  mutate(oh = oh / (2.46 * 10^19 * 10^-12), #convert to ppt
         nitrate = nitrate / (2.46 * 10^19 * 10^-12), #convert to ppt
         hono_photolysis = jhono * hono,
         hono_oh = kl * hono * oh,
         hono_deposition = kdep1 * hono,
         pno3_photolysis = jhno3 * 20 * nitrate,
         no_oh = kp * no * oh,
         loss = hono_photolysis + hono_deposition + hono_oh,
         production = pno3_photolysis + no_oh)

prod_loss %>% 
  filter(campaign == "August 2019") %>%
  # pivot_longer(c(pno3_photolysis,no_oh)) %>%
  ggplot(aes(date,pno3_photolysis,col = nitrate)) +
  geom_path(size = 0.8) +
  # scale_x_datetime(breaks = "1 day",date_labels = "%d/%m")
  scale_color_viridis() +
  # facet_grid(rows = vars(name),scales = "free") +
  # facet_wrap(vars(campaign),scales = "free", ncol =1) +
  NULL

ggsave('production19_nitrate.svg',
       path = "output/plots/pss/production_loss",
       width = 30,
       height = 12,
       units = 'cm')

diurnal_prod_loss = prod_loss %>% 
  filter(campaign == "August 2019",
         is.na(hono) == FALSE) %>% 
  timeVariation(pollutant = c("production","loss"))

diurnal_dat_prod_loss = diurnal_prod_loss$data$hour

diurnal_dat_prod_loss %>% 
  ggplot(aes(hour,Mean,col = variable)) +
  geom_line(size = 1) +
  scale_color_manual(values = viridis(3)) +
  theme_bw() +
  labs(x = "Hour of day (UTC)",
       y = "HONO (ppt)",
       color = NULL) +
  scale_x_continuous(breaks = c(0,4,8,12,16,20)) +
  theme(legend.position = "top")

ggsave('production_loss19.svg',
       path = "output/plots/pss/production_loss",
       width = 30,
       height = 12,
       units = 'cm')

# Not in use atm ----------------------------------------------------------


# Historical nitrate ------------------------------------------------------

#finding average nitrate values for february
nitrate_dat = read.csv("data/nitrate_ammonium_CVAO_12-19.csv") %>% 
  clean_names() %>%
  mutate(date = mdy_hm(start_local_time),
         month = month(date),
         year = year(date)) %>% 
  filter(month == 2) %>% 
  group_by(year) %>% 
  summarise(nitrate = mean(nitrate_mg_m))
nitrate = mean(nitrate_dat$nitrate,na.rm = TRUE) #in micrograms per meter cubed
#multiplied by 10^-12 to convert to g and to convert to cm-3 (10^-6 for both)
#divided by nitrate molar mass 62.004 g mol-1
#multiplied by Avogadro's number -> 6.022 * 10^23 molecule mol-1
nitrate = (nitrate * 10^-12 *6.022 * 10^23)/62.004 #molecule cm-3


# Creating dataframe for February 2023 ------------------------------------

#importing all the data individually and then creating one dataframe

spec_rad = read.csv("data/Specrad_hour_23_with_calc.csv") %>% 
  mutate(date = dmy_hm(date),
         hour = hour(date)) %>% 
  clean_names() %>% 
  mutate(jhono = ifelse(is.na(j_hono),jhono_calc,j_hono),
         jhno3 = ifelse(is.na(j_hno3),jhno3_calc,j_hno3)) %>% 
  select(date,hour,jhono,jhno3)

#to remove NAs in jhono and jhno3 values

#find average jhono and jhno3 values for each hour
spec_rad_mean = spec_rad %>% 
  group_by(hour) %>% 
  summarise(jhono_avg = mean(jhono,na.rm = T),
            jhno3_avg = mean(jhno3,na.rm = T))

#replace NAs with average value for that hour
spec_rad_full = left_join(spec_rad,spec_rad_mean,by = "hour") %>% 
  mutate(jhono = ifelse(is.na(jhono),jhono_avg,jhono),
         jhno3 = ifelse(is.na(jhno3),jhno3_avg,jhno3)) %>% 
  select(-c(jhono_avg,jhno3_avg))

oh_dat = read.csv("data/OH_provisional.csv") %>% 
  clean_names() %>% 
  mutate(date = dmy_hm(time),
         oh = na.approx(oh_molecules_cm_3,na.rm = F)) %>% #interpolate missing values
  select(date,oh) %>% 
  timeAverage("1 hour")

nox_dat = read.csv("data/nox_data/nox23.csv") %>% 
  mutate(date = ymd_hms(X)) %>% 
  filter(date > "2023-02-01" & date < "2023-03-01") %>% 
  select(date,no = NO_Conc_art_corrected) %>%  
  timeAverage("1 hour")

hono_dat = read.csv("output/data/processed_in_r4.csv") %>% 
  mutate(date = ymd_hms(date)) %>% 
  select(date,hono) %>% 
  timeAverage("1 hour")

df_list = list(nox_dat,oh_dat,hono_dat,spec_rad)
dat = df_list %>% reduce(full_join,by = "date")
write.csv(dat,"output/data/data_for_pss.csv",row.names = FALSE) #saving as .csv


# Creating dataframe for all campaigns (apart from 2015) ------------------------------------

#getting all data in one dataset for hono and various campaigns

#NOx
files = list.files("data/nox_data",full.names=TRUE)
datList = list()

for(index in 1:length(files)) {
  datList[[index]] = read.csv(files[index],header=TRUE,na.strings= c('NA','missing'))%>%
    tibble()
  
}

nox_dat = bind_rows(datList) %>% 
  mutate(date = ymd_hms(X)) %>% 
  select(date,no = NO_Conc_art_corrected,no2 = NO2_Conc_diode) %>% 
  timeAverage("1 hour")

#HONO
files = list.files("data/roberto_data",full.names=TRUE)
datList = list()

for(index in 1:length(files)) {
  datList[[index]] = read.csv(files[index],header=TRUE,na.strings= c('NA','missing'))%>%
    tibble()
  
}

hono = bind_rows(datList) %>% 
  mutate(date = dmy_hms(start.gmt)) %>% 
  select(date,hono = hono.ppt)

hono23 = read.csv("output/data/processed_in_r4.csv") %>% 
  mutate(date = dmy_hms(date))

hono_dat = bind_rows(hono,hono23) %>% 
  timeAverage("1 hour")

#Air masses
air_mass = read.csv("data/new_CVAO_sector_%_boxes_1.csv") %>% 
  rename(date = X) %>% 
  mutate(date = ymd_hm(date)) %>% 
  clean_names()

#Spec rad
spec_rad23 = read.csv("data/Specrad_hour_23_with_calc.csv") %>% 
  mutate(date = dmy_hm(date),
         hour = hour(date)) %>% 
  clean_names() %>% 
  mutate(jhono = ifelse(is.na(j_hono),jhono_calc,j_hono),
         jhno3 = ifelse(is.na(j_hno3),jhno3_calc,j_hno3)) %>% 
  select(date,hour,jhono,jhno3)

spec_rad_historic = read.csv("data/2016_2020_Spec_rad_Hourly.csv") %>% 
  mutate(date = dmy_hm(date),
         hour = hour(date)) %>% 
  clean_names() %>% 
  mutate(jhono = ifelse(is.na(j_hono),jhono_calc,j_hono),
         jhno3 = ifelse(is.na(j_hno3),jhno3_calc,j_hno3)) %>% 
  select(date,hour,jhono,jhno3)

#have used code above to fill NAs with averages from hours where spec rad data is missing
spec_rad = bind_rows(spec_rad_historic,spec_rad23) 

#find average jhono and jhno3 values for each hour
spec_rad_mean = spec_rad %>% 
  group_by(hour) %>% 
  summarise(jhono_avg = mean(jhono,na.rm = T),
            jhno3_avg = mean(jhno3,na.rm = T))

#replace NAs with average value for that hour
spec_rad_full = left_join(spec_rad,spec_rad_mean,by = "hour") %>% 
  mutate(jhono = ifelse(is.na(jhono),jhono_avg,jhono),
         jhno3 = ifelse(is.na(jhno3),jhno3_avg,jhno3)) %>% 
  select(-c(jhono_avg,jhno3_avg))

#Nitrate
nitrate_dat = read.csv("data/nitrate_ammonium_CVAO_12-19.csv") %>% 
  clean_names() %>%
  mutate(date = mdy_hm(start_local_time),
         month = month(date),
         year = year(date),
         date = round_date(date, "1 hour")) %>% 
  select(date,nitrate = nitrate_mg_m) %>% 
  timeAverage("1 hour")

#OH
oh_dat = read.csv("data/OH_provisional.csv") %>% 
  clean_names() %>% 
  mutate(date = dmy_hm(time),
         oh = na.approx(oh_molecules_cm_3,na.rm = F)) %>% #interpolate missing values
  select(date,oh) %>% 
  timeAverage("1 hour")

df_list = list(hono_dat,nox_dat,spec_rad_full,nitrate_dat,oh_dat)
  

dat = df_list %>% reduce(full_join,by = "date") %>% 
  arrange(date) %>% 
  filter(date < "2023-02-28",
         date > "2019-08-15 11:00") %>% 
  mutate(campaign = case_when (date <= "2019-08-29 00:55" ~ "August 2019",
                               between(date,as.POSIXct("2020-02-14 01:00"),as.POSIXct("2020-02-27 00:55")) ~ "February 2020",
                               date >= "2023-02-07 08:35" ~ "February 2023",
                               TRUE ~ "no campaign"),
         oh = ifelse(campaign != "February 2023",2 * 10^6,oh), #molecules cm-3
         nitrate = case_when(campaign == "February 2020" ~ 1.20 * 10^10,
                             campaign == "February 2023" ~ 1.20 * 10^10,
                             TRUE ~ (nitrate* 10^-12 *6.022 * 10^23)/62.004),#molecules cm-3
         # nitrate = ifelse(campaign == "February 2020" | campaign == "February 2023",1.20 * 10^10,
         #                  (nitrate* 10^-12 *6.022 * 10^23)/62.004),
         nitrate = na.approx(nitrate,na.rm = FALSE)) %>%  
  select(-hour)

write.csv(dat,"output/data/all_data.csv",row.names = FALSE) #saving as .csv

# Creating df for all campaigns (with 2015 and air masses) ----------------

#read in data from air_mass_nox_analysis with hono,nox,air masses and nitrate
dat = read.csv("output/data/hono_nox_airmass_all_campaigns.csv") %>% 
  mutate(date = ymd_hms(date))

#Spec rad (not for 2015 yet, still missing that data)
spec_rad23 = read.csv("data/Specrad_hour_23_with_calc.csv") %>% 
  mutate(date = dmy_hm(date),
         hour = hour(date)) %>% 
  clean_names() %>% 
  mutate(jhono = ifelse(is.na(j_hono),jhono_calc,j_hono),
         jhno3 = ifelse(is.na(j_hno3),jhno3_calc,j_hno3)) %>% 
  select(date,hour,jhono,jhno3)

spec_rad_historic = read.csv("data/2016_2020_Spec_rad_Hourly.csv") %>% 
  mutate(date = dmy_hm(date),
         hour = hour(date)) %>% 
  clean_names() %>% 
  mutate(jhono = ifelse(is.na(j_hono),jhono_calc,j_hono),
         jhno3 = ifelse(is.na(j_hno3),jhno3_calc,j_hno3)) %>% 
  select(date,hour,jhono,jhno3)

#have used code above to fill NAs with averages from hours where spec rad data is missing
spec_rad = bind_rows(spec_rad_historic,spec_rad23) 

#find average jhono and jhno3 values for each hour
spec_rad_mean = spec_rad %>% 
  group_by(hour) %>% 
  summarise(jhono_avg = mean(jhono,na.rm = T),
            jhno3_avg = mean(jhno3,na.rm = T))

#replace NAs with average value for that hour
spec_rad_full = left_join(spec_rad,spec_rad_mean,by = "hour") %>% 
  mutate(jhono = ifelse(is.na(jhono),jhono_avg,jhono),
         jhno3 = ifelse(is.na(jhno3),jhno3_avg,jhno3)) %>% 
  select(-c(jhono_avg,jhno3_avg))

#OH
oh_dat = read.csv("data/OH_provisional.csv") %>% 
  clean_names() %>% 
  mutate(date = dmy_hm(time),
         oh = na.approx(oh_molecules_cm_3,na.rm = F)) %>% #interpolate missing values
  select(date,oh) %>% 
  timeAverage("1 hour")

df_list = list(dat,spec_rad_full,oh_dat)

dat2 = df_list %>% reduce(full_join,by = "date") %>% 
  arrange(date) %>% 
  mutate(oh = ifelse(campaign != "February 2023",2 * 10^6,oh), #molecules cm-3
         nitrate = case_when(campaign == "February 2020" ~ 1.20 * 10^10,
                             campaign == "February 2023" ~ 1.20 * 10^10,
                             TRUE ~ (nitrate* 10^-12 *6.022 * 10^23)/62.004),#molecules cm-3
         # nitrate = ifelse(campaign == "February 2020" | campaign == "February 2023",1.20 * 10^10,
         #                  (nitrate* 10^-12 *6.022 * 10^23)/62.004),
         nitrate = na.approx(nitrate,na.rm = FALSE)) %>%  
  select(-hour)

write.csv(dat2,"output/data/all_data_updated.csv",row.names = FALSE) #saving as .csv

dat2 %>% 
  filter(campaign == "February 2023") %>% 
  mutate(sahara = na.approx(sahara,na.rm = FALSE)) %>% 
  pivot_longer(c(hono,no,nitrate,jhono,jhno3,oh)) %>% 
  ggplot(aes(date,value,col = sahara)) +
  facet_grid(rows = vars(name),scales = "free_y") +
  geom_path(size = 0.8) +
  scale_colour_viridis_c()

# Deposition rate ---------------------------------------------------------

#looking at the deposition rate and figuring out which hours should be used to calculate it
#currently using 10 to 15 (which in UTC time is 11 to 16)

#plotting the spread of h, HONO lifetime and kdep for those hours
spec_rad23 %>% 
  mutate(hour = hour(date),
         lifetime = ifelse(hour >= 11 & hour <= 15,1/jhono,NA_real_),
         lifetime = na.approx(lifetime,na.rm = F),
         h = lifetime * 0.3,
         kdep = 3*10^-2/h) %>% 
  filter(date < "2023-03-01",
         hour >= 11 & hour <= 15) %>% 
  mutate(hour = as.character(hour)) %>% 
  pivot_longer(c(lifetime,h,kdep)) %>% 
  ggplot(aes(date,value,col = hour)) +
  # geom_path(aes(date,value)) +
  geom_point() +
  labs(x = NULL,
       y = NULL) +
  theme_bw() +
  facet_grid(rows = vars(name),scales = "free_y")

ggsave('kdep.png',
       path = "output/plots/pss",
       width = 30,
       height = 12,
       units = 'cm')

#looking at how big an effect the choice of hours for kdep has
#seeing what happens if I set kdep to zero outside afternoon hours -> don't do this
pss_calc = pss %>% 
  rename(measured = hono) %>%
  mutate(hour = hour(date),
         lifetime = ifelse(hour > 10 & hour < 15,1/jhono,NA_real_),
         lifetime = na.approx(lifetime,na.rm = FALSE),
         h = lifetime * dv,
         kdep1 = ifelse(hour >= 11 & hour <= 16,3*10^-2/h,0),
         kdep3 = dep_velocity/h,
         no_molecules = no * 2.46 * 10^19 * 10^-12,
         # without_nitrate = ( kp*oh*no_molecules) / (jhono + (kl*oh) + kdep),
         # without_enhancement = (kp*oh*no_molecules + (jhno3 * nitrate)) / (jhono + (kl*oh) + kdep),
         pss1 = (kp*oh*no_molecules + (jhno3 * nitrate * f)) / (jhono + (kl*oh) + kdep1),
         pss2 = (kp*oh*no_molecules + (jhno3 * nitrate * f)) / (jhono + (kl*oh) + kdep3),
         # without_nitrate = without_nitrate / (2.46 * 10^19 * 10^-12),
         # without_enhancement = without_enhancement / (2.46 * 10^19 * 10^-12),
         pss1 = pss1/ (2.46 * 10^19 * 10^-12),
         pss2 = pss2/ (2.46 * 10^19 * 10^-12))

pss_calc %>% pivot_longer(c(pss1,pss2)) %>% 
  filter(date < "2023-02-25" & date > "2023-02-07") %>% 
  ggplot(aes(date,value,col = name)) +
  geom_path() +
  # facet_grid(rows = vars(name),scales = "free_y")
  NULL


#finding r squared for plot of missing production source vs jhno3 (units are ppt h-1)
model <- lm(finding_f$missing_production_ppt_h~finding_f$jhno3_h)
lm(formula = finding_f$missing_production_ppt_h~finding_f$jhno3_h)
summary(model)

#Calculating median loss and production through various pathways

loss_production = dat %>% 
  mutate(hour = hour(date)) %>% 
  filter(date > "2023-02-06" & date < "2023-02-27",
         hour > 9 & hour < 16) %>%
  mutate(lifetime = ifelse(hour > 10 & hour < 15,1/jhono,NA_real_),
         lifetime = na.approx(lifetime,na.rm = FALSE),
         h = lifetime * dv,
         kdep1 = 0.01/h,
         kdep3 = 0.03/h,
         no_molecules = no * 2.46 * 10^19 * 10^-12,
         production_without_nitrate = (kp*oh*no_molecules)/(2.46 * 10^19 * 10^-12),
         loss_photolysis = jhono * hono,
         loss_reaction = (kl*oh*hono* 2.46 * 10^19 * 10^-12)/(2.46 * 10^19 * 10^-12),
         loss_deposition3 = kdep3 * hono,
         loss_deposition1 = kdep1 * hono)

loss_photolysis = median(loss_production$loss_photolysis,na.rm =TRUE)
loss_reaction = median(loss_production$loss_reaction,na.rm =TRUE)
loss_deposition1 = median(loss_production$loss_deposition1,na.rm =TRUE)
loss_deposition3 = median(loss_production$loss_deposition3,na.rm =TRUE)
production = median(loss_production$production_without_nitrate,na.rm =TRUE)