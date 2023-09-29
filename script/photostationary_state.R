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

#read in full data (data joined in creating_master_df)
#all_data2 has the correction for spec_rad timezone applied for 2023

dat = read.csv("output/data/all_data.csv") %>% 
  mutate(date = ymd_hms(date))

# Calculating enhancement factor ------------------------------------------

#can change anything in code below to find f for different parameters

#for deposition velocity = 0.01 f1 = 63 (used to be f = 57 before mfc cal)
#for deposition velocity = 0.03 f3 = 67 (used to be f = 61 before mfc cal)
#depending on timezones, if spec rad timezones are changed f1 = 66 and f3 = 70

#for November 2015 f1 = 7, f3 = 8
#for August 2019 f1 = 21,f3 = 22
#for February 2020 f1 = 10,f3 = 11

#finding enhancement factor for different campaigns
f_calc_all = dat %>%   
  filter(campaign == "February 2023",
         # date < "2020-02-26",
         hour >= 11 & hour <= 15
         ) %>%
  mutate(lifetime = 1/jhono,
         h = lifetime * dv,
         kdep1 = 0.01/h,
         kdep3 = 0.03/h,
         no_molecules = no * 2.46 * 10^19 * 10^-12,
         production_without_nitrate = kp*oh*no_molecules,
         loss1 = jhono + (kl*oh) + kdep1,
         loss_hono1 = loss1 * hono * 2.46 * 10^19 * 10^-12,
         loss3 = jhono + (kl*oh) + kdep3,
         loss_hono3 = loss3 * hono * 2.46 * 10^19 * 10^-12,
         missing_production1 = loss_hono1 - production_without_nitrate,
         missing_production3 = loss_hono3 - production_without_nitrate)

#finding f - daytime median of missing production and jhno3 used
#specifically between 10 and 15 local time - 11 and 16 UTC
nitrate = mean(f_calc_all$nitrate,na.rm = TRUE)

missing_production1 = mean(f_calc_all$missing_production1,na.rm = TRUE)
missing_production3 = mean(f_calc_all$missing_production3,na.rm = TRUE)
jhno3 = mean(f_calc_all$jhno3,na.rm = TRUE)
f1 = missing_production1/(nitrate*jhno3)
f3 = missing_production3/(nitrate*jhno3)

# Photostationary state calculations --------------------------------------

#hono = k[OH][NO]+ jHNO3 * f * pNO3 / jHONO + k[OH] + kdep

#units for calculation are molecule per cm3

#can change f and kdep and other parameters as needed and compare them

pss_calc = dat %>% 
  mutate(hour = hour(date),
         lifetime = ifelse(hour >= 11 & hour <= 15,1/jhono,NA_real_),
         lifetime = na.approx(lifetime,na.rm = FALSE),
         nitrate = na.approx(nitrate,na.rm = FALSE),
         h = lifetime * dv,
         kdep1 = 0.01/h,
         # kdep3 = 0.03/h,
         no_molecules = no * 2.46 * 10^19 * 10^-12,
         pss = case_when(campaign == "November 2015" ~ ((kp*oh*no_molecules + (jhno3 * nitrate * 7)) / (jhono + (kl*oh) + kdep1))
                         / (2.46 * 10^19 * 10^-12),
                         campaign == "August 2019" ~ ((kp*oh*no_molecules + (jhno3 * nitrate * 21)) / (jhono + (kl*oh) + kdep1))
                         / (2.46 * 10^19 * 10^-12),
                         campaign == "February 2020" ~ ((kp*oh*no_molecules + (jhno3 * nitrate * 10)) / (jhono + (kl*oh) + kdep1))
                         / (2.46 * 10^19 * 10^-12),
                         campaign == "February 2023" ~ ((kp*oh*no_molecules + (jhno3 * nitrate * f1)) / (jhono + (kl*oh) + kdep1))
                         / (2.46 * 10^19 * 10^-12)))

pss_calc %>% 
  filter(campaign == "February 2023") %>%
  rename('PSS' = pss,
         'Observed' = hono) %>%
  pivot_longer(c('PSS','Observed',jhono,jhno3)) %>%
  ggplot(aes(date,value,col = name)) +
  theme_bw() +
  labs(x = "Datetime (UTC)",
       y = "HONO (ppt)",
       color = NULL) +
  geom_path(size = 0.8) +
  theme(legend.position = "top") +
  scale_x_datetime(breaks = "1 day",date_labels = "%d/%m") +
  # facet_wrap(~factor(campaign,levels = c("November 2015","August 2019","February 2020","February 2023")),
  #            scales = "free_x",ncol = 1) +
  scale_color_viridis_d() +
  facet_grid(rows = vars(name),scales = "free") +
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
  mutate(hono = min_max_norm(hono),
         pss = min_max_norm(pss),
         jhono = min_max_norm(jhono),
         jhno3 = min_max_norm(jhno3)) %>% 
  rename(HONO = hono,PSS = pss) %>% 
  timeVariation(pollutant = c("HONO",'PSS',"jhono","jhno3"))

diurnal_dat = diurnal$data$hour

diurnal_dat %>% 
  ggplot(aes(hour,Mean,col = variable)) +
  geom_line(size = 1) +
  scale_color_viridis_d() +
  theme_bw() +
  labs(x = "Hour of day (UTC)",
       y = "HONO (ppt)",
       color = NULL) +
  scale_x_continuous(breaks = c(0:23)) +
  # facet_grid(rows = vars(variable),scales = "free") +
  # ylim(-1.5,13) + #in order to have same sized axes for diurnals for all three campaigns
  theme(legend.position = "top")

ggsave('pss23.svg',
       path = "output/plots_analysis/spec_rad",
       width = 15.37,
       height = 13,
       units = 'cm')


# Diurnals 15 min average -------------------------------------------------

#importing all the data individually and then creating one dataframe - data is averaged to 15 minutes
#interpolation for spec rad data as it is hourly

spec_rad23 = read.csv("data/spec_rad/Specrad_hour_23_with_calc.csv") %>% 
  mutate(date = dmy_hm(date),
         hour = hour(date),
         doy = yday(date)) %>% 
  filter(date >= "2023-02-07 08:35" & date < "2023-02-27") %>% 
  clean_names() %>% 
  mutate(jhono = case_when(is.na(j_hono) ~ jhono_calc,
                           doy == 43 ~ NA_real_,
                           doy == 51 ~ NA_real_,
                           TRUE ~ j_hono),
         jhno3 = case_when(is.na(j_hno3) ~ jhno3_calc,
                           doy == 43 ~ NA_real_,
                           doy == 51 ~ NA_real_,
                           TRUE ~ j_hno3)) %>% 
  select(date,hour,jhono,jhno3)

#fill NAs with averages from hours where spec rad data is missing when reading data in
#find average jhono and jhno3 values for each hour
spec_rad_mean = spec_rad23 %>% 
  group_by(hour) %>% 
  summarise(jhono_avg = mean(jhono,na.rm = T),
            jhno3_avg = mean(jhno3,na.rm = T))

#replace NAs with average value for that hour
spec_rad23_corr = left_join(spec_rad23,spec_rad_mean,by = "hour") %>% 
  mutate(jhono = ifelse(is.na(jhono),jhono_avg,jhono),
         jhno3 = ifelse(is.na(jhno3),jhno3_avg,jhno3),
         date = date + 3600, #changing data to utc
  ) %>% 
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

hono_dat = read.csv("output/data/hono23.csv") %>% 
  mutate(date = ymd_hms(date)) %>% 
  select(date,hono) %>% 
  timeAverage("15 min")

df_list = list(nox_dat,oh_dat,hono_dat,spec_rad23_corr)

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
         pss = ((kp*oh*no_molecules + (jhno3 * nitrate * 62)) / (jhono + (kl*oh) + kdep1))
                         / (2.46 * 10^19 * 10^-12))

diurnal = pss_calc %>% 
  filter(is.na(hono) == FALSE) %>% 
  mutate(minute = minute(date),
         hour = hour + (minute/60)) %>% 
  group_by(hour) %>% 
  summarise(hono = mean(hono,na.rm = TRUE),
            pss = mean(pss,na.rm = TRUE),
            jhono = mean(jhono,na.rm = TRUE),
            jhno3 = mean(jhno3,na.rm = TRUE)) %>% 
  ungroup()

diurnal %>% 
  rename(HONO = hono,PSS = pss) %>% 
  pivot_longer(c(HONO,PSS,jhono,jhno3)) %>% 
  ggplot(aes(hour,value,col = name)) +
  geom_line(size = 1) +
  scale_color_viridis_d() +
  theme_bw() +
  labs(x = "Hour of day (UTC)",
       y = "HONO (ppt)",
       color = NULL) +
  scale_x_continuous(breaks = c(0:23)) +
  facet_grid(rows = vars(name),scales = "free")
  theme(legend.position = "top")

ggsave('15min_diurnal_hono_pss.svg',
       path = "output/plots/red_shift",
       width = 11,
       height = 13,
       units = 'cm')

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