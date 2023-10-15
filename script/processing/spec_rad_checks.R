library(tidyverse)
library(lubridate)
library(openair)
library(janitor)
library(zoo)
library(viridis)

Sys.setenv(TZ = 'UTC')

#2023 spec rad data is recorded in CV local time, so needs to be changed to UTC by adding an hour
#still unsure about timezone or quality of 2015 spec rad data

# 2015 data ---------------------------------------------------------------

#I think this should be ignored for now because it does look a bit off in terms of magnitude too

spec_rad15 = read.csv("data/spec_rad/jrates_all_new_2015-2020.csv") %>% 
  clean_names() %>%
  mutate(date = dmy_hm(date)) %>% 
  filter(date > "2015-11-23" & date < "2015-12-04") %>%
  timeAverage("1 hour") %>% 
  select(date,jhono = jhono_calc,jhno3 = jhno3_calc)


# 2019 and 2020 data ------------------------------------------------------

spec_rad_19_20 = read.csv("data/spec_rad/2016_2020_Spec_rad_Hourly.csv") %>% 
  mutate(date = dmy_hm(date)) %>% 
  clean_names() %>% 
  mutate(jhono = ifelse(is.na(j_hono),jhono_calc,j_hono),
         jhno3 = ifelse(is.na(j_hno3),jhno3_calc,j_hno3),
         hour = hour(date)) %>% 
  select(date,hour,jhono,jhno3)

#fill NAs with averages from hours where spec rad data is missing when reading data in
#find average jhono and jhno3 values for each hour
spec_rad_mean = spec_rad_19_20 %>% 
  group_by(hour) %>% 
  summarise(jhono_avg = mean(jhono,na.rm = T),
            jhno3_avg = mean(jhno3,na.rm = T))

#replace NAs with average value for that hour
spec_rad_19_20 = left_join(spec_rad_19_20,spec_rad_mean,by = "hour") %>% 
  mutate(jhono = ifelse(is.na(jhono),jhono_avg,jhono),
         jhno3 = ifelse(is.na(jhno3),jhno3_avg,jhno3),
         year = year(date),
         campaign = case_when (date > "2019-08-15 12:29" & date < "2019-08-29 01:00" ~ "August 2019",
                               date > "2020-02-14 01:00" & date < "2020-02-26 02:56" ~ "February 2020",
                               TRUE ~ "no campaign")) %>% 
  select(-c(jhono_avg,jhno3_avg))



# 2019 and 2020 diurnals --------------------------------------------------

diurnals = spec_rad_19_20 %>% 
  filter(campaign == "February 2020")
  # pivot_wider(names_from = campaign,values_from = jhno3)

diurnal = diurnals %>% 
  timeVariation(pollutant = "jhono")

diurnal20_dat = diurnal$data$hour %>% 
  ungroup() %>% 
  rename(feb20 = Mean) %>% 
  select(hour,feb20)

diurnal_dat %>% 
  ggplot(aes(hour,Mean,col = variable)) +
  theme_bw() +
  geom_path(size = 0.8) +
  # facet_grid(cols = vars(variable),scales = "free_y") +
  scale_colour_viridis_d() +
  labs(x = "Hour of day (UTC?)",
       y = "jhno3",
       color = NULL) +
  scale_x_continuous(breaks = c(0:23)) +
  # ylim(-0.5,12) +
  theme(legend.position = "top")

ggsave('jhno3_19_20_diurnal.svg',
       path = "output/plots_analysis/spec_rad",
       width = 15.37,
       height = 13,
       units = 'cm')

# 2023 data ---------------------------------------------------------------

#adding two data columns correcting for if date is in CV or UK time

spec_rad23 = read.csv("data/spec_rad/Specrad_hour_23_with_calc.csv") %>% 
  mutate(date = dmy_hm(date),
         hour = hour(date),
         doy = yday(date)) %>% 
  clean_names() %>% 
  mutate(jhono = case_when(is.na(j_hono) ~ jhono_calc,
                           doy == 43 ~ NA_real_,
                           doy == 51 ~ NA_real_,
                           TRUE ~ j_hono),
         jhno3 = case_when(is.na(j_hno3) ~ jhno3_calc,
                           doy == 43 ~ NA_real_,
                           doy == 51 ~ NA_real_,
                           TRUE ~ j_hno3))
  select(date,hour,jhono,jhno3)

#fill NAs with averages from hours where spec rad data is missing when reading data in
#find average jhono and jhno3 values for each hour
spec_rad_mean = spec_rad23 %>% 
  group_by(hour) %>% 
  summarise(jhono_avg = mean(jhono,na.rm = T),
            jhno3_avg = mean(jhno3,na.rm = T))

#replace NAs with average value for that hour
spec_rad = left_join(spec_rad23,spec_rad_mean,by = "hour") %>% 
  mutate(jhono = ifelse(is.na(jhono),jhono_avg,jhono),
         jhno3 = ifelse(is.na(jhno3),jhno3_avg,jhno3),
         date = date + 3600, #if data is in Cape Verde time, add an hour for it to be UTC
         # date_mod_uk = date - 3600, #if data is in UK time, subtract an hour (I think - depends on GMT and BST)
         campaign = case_when (date >= "2023-02-07 08:35" & date < "2023-02-27" ~ "February 2023",
                               TRUE ~ "no campaign")) %>% 
  select(-c(jhono_avg,jhno3_avg))


# 2023 diurnals with different timezones ----------------------------------

diurnals_cv = spec_rad23 %>% 
  select(-date) %>% 
  rename(date = date_mod_cv)

diurnals_uk = spec_rad23 %>% 
  select(-date) %>% 
  rename(date = date_mod_uk)

diurnal1 = spec_rad23 %>% 
  timeVariation(pollutant = "jhono")
diurnal2 = diurnals_cv %>% 
  timeVariation(pollutant = "jhono")
diurnal3 = diurnals_uk %>% 
  timeVariation(pollutant = "jhono")

diurnal1_dat = diurnal1$data$hour %>% 
  ungroup() %>% 
  rename(jhono1 = Mean) %>% 
  select(hour,jhono1)
diurnal2_dat = diurnal2$data$hour %>% 
  ungroup() %>% 
  rename(jhono_cv = Mean) %>% 
  select(hour,jhono_cv)
diurnal3_dat = diurnal3$data$hour %>% 
  ungroup() %>% 
  rename(jhono_uk = Mean) %>% 
  select(hour,jhono_uk)

#pulls in diurnal data from February 2020 as a comparison
df_list = list(diurnal1_dat,diurnal2_dat,diurnal3_dat,diurnal20_dat)
diurnal_comparison = df_list %>% reduce(full_join,by = "hour")

diurnal_comparison %>% 
  pivot_longer(c(jhono1,jhono_cv,jhono_uk,feb20)) %>% 
  ggplot(aes(hour,value,col = name)) +
  theme_bw() +
  geom_path(size = 0.8) +
  # facet_grid(cols = vars(variable),scales = "free_y") +
  scale_colour_viridis_d() +
  labs(x = "Hour of day (UTC?)",
       y = "jhono",
       color = NULL) +
  scale_x_continuous(breaks = c(0:23)) +
  # ylim(-0.5,12) +
  theme(legend.position = "top")

ggsave('jhono_diurnal_mods20.svg',
       path = "output/plots_analysis/spec_rad",
       width = 15.37,
       height = 13,
       units = 'cm')
 

# Plotting diurnals -------------------------------------------------------

#corrections for 2015 data which is only from 8 to 17 or something - other data should be corrected
#separately so that the corrections come from their time period
#this might be a bit extra because I think at night it's always 0 pretty much
spec_rad_to_fix = bind_rows(spec_rad15,spec_rad_19_20,spec_rad23) %>% 
  mutate(hour = hour(date))

#find average jhono and jhno3 values for each hour
spec_rad_mean = spec_rad_to_fix %>% 
  group_by(hour) %>% 
  summarise(jhono_avg = mean(jhono,na.rm = T),
            jhno3_avg = mean(jhno3,na.rm = T))

#replace NAs with average value for that hour
spec_rad = left_join(spec_rad_to_fix,spec_rad_mean,by = "hour") %>% 
  mutate(jhono = ifelse(is.na(jhono),jhono_avg,jhono),
         jhno3 = ifelse(is.na(jhno3),jhno3_avg,jhno3),
         campaign = case_when (date > "2015-11-24 17:00" & date < "2015-12-03 19:00" ~ "November 2015",
                               date > "2019-08-15 12:29" & date < "2019-08-29 01:00" ~ "August 2019",
                               date > "2020-02-14 01:00" & date < "2020-02-26 02:56" ~ "February 2020",
                               date >= "2023-02-07 08:35" ~ "February 2023",
                               TRUE ~ "no campaign")) %>% 
  select(-c(jhono_avg,jhno3_avg))


diurnals = spec_rad %>% 
  filter(campaign != "no campaign") %>% 
  pivot_wider(names_from = campaign,values_from = jhono)

diurnal = diurnals %>% 
  rename("Nov 2015"="November 2015","Aug 2019"="August 2019","Feb 2020"="February 2020","Feb 2023"="February 2023") %>% 
  timeVariation(pollutant = c("Nov 2015","Aug 2019","Feb 2020","Feb 2023"))

diurnal_dat = diurnal$data$hour

diurnal_dat %>% 
  ggplot(aes(hour,Mean,col = variable)) +
  theme_bw() +
  geom_path(size = 0.8) +
  # facet_grid(cols = vars(variable),scales = "free_y") +
  scale_colour_viridis_d() +
  labs(x = "Hour of day (UTC?)",
       y = "jhono",
       color = NULL) +
  scale_x_continuous(breaks = c(0,2,4,6,8,10,12,14,16,18,20,22)) +
  # ylim(-0.5,12) +
  theme(legend.position = "top")

ggsave('jhono_diurnal.svg',
       path = "output/plots/spec_rad",
       width = 15.37,
       height = 13,
       units = 'cm')


# Using just 2015 to 2020 dataset -----------------------------------------

#the data from 2019 and 2020 can be found in excel file from 2015 to 2020
#as well as in the excel file from 2016 to 2020, which is what I've used in my analysis so far

spec_rad15 = read.csv("data/spec_rad/jrates_all_new_2015-2020.csv") %>% 
  clean_names() %>%
  mutate(date = dmy_hm(date),
         jhono = ifelse(date > "2015-11-23" & date < "2015-12-04",jhono_calc,j_hono),
         jhno3 = ifelse(date > "2015-11-23" & date < "2015-12-04",jhno3_calc,j_hno3)) %>%
  # filter(date > "2015-11-23" & date < "2015-12-04") %>%
  timeAverage("1 hour") %>%
  select(date,jhono,jhno3)

spec_rad23 = read.csv("data/spec_rad/Specrad_hour_23_with_calc.csv") %>% 
  mutate(date = dmy_hm(date),
         date = date) %>% 
  clean_names() %>% 
  mutate(jhono = ifelse(is.na(j_hono),jhono_calc,j_hono),
         jhno3 = ifelse(is.na(j_hno3),jhno3_calc,j_hno3)) %>% 
  select(date,jhono,jhno3)

#fill NAs with averages from hours where spec rad data is missing when reading data in
spec_rad_to_fix = bind_rows(spec_rad15,spec_rad23) %>% 
  mutate(hour = hour(date))

#find average jhono and jhno3 values for each hour
spec_rad_mean = spec_rad_to_fix %>% 
  group_by(hour) %>% 
  summarise(jhono_avg = mean(jhono,na.rm = T),
            jhno3_avg = mean(jhno3,na.rm = T))

#replace NAs with average value for that hour
spec_rad = left_join(spec_rad_to_fix,spec_rad_mean,by = "hour") %>% 
  mutate(jhono = ifelse(is.na(jhono),jhono_avg,jhono),
         jhno3 = ifelse(is.na(jhno3),jhno3_avg,jhno3),
         year = year(date),
         campaign = case_when (date > "2015-11-24 17:00" & date < "2015-12-03 19:00" ~ "November 2015",
                               date > "2019-08-15 12:29" & date < "2019-08-29 01:00" ~ "August 2019",
                               date > "2020-02-14 01:00" & date < "2020-02-26 02:56" ~ "February 2020",
                               date >= "2023-02-07 08:35" ~ "February 2023",
                               TRUE ~ "no campaign")) %>% 
  select(-c(jhono_avg,jhno3_avg))

diurnals = spec_rad %>% 
  filter(campaign != "no campaign") %>% 
  pivot_wider(names_from = campaign,values_from = jhno3)

diurnal = diurnals %>% 
  rename("Nov 2015"="November 2015","Aug 2019"="August 2019","Feb 2020"="February 2020","Feb 2023"="February 2023") %>% 
  timeVariation(pollutant = c("Nov 2015","Aug 2019","Feb 2020","Feb 2023"))

diurnal_dat = diurnal$data$hour

diurnal_dat %>% 
  ggplot(aes(hour,Mean,col = variable)) +
  theme_bw() +
  geom_path(size = 0.8) +
  # facet_grid(cols = vars(variable),scales = "free_y") +
  scale_colour_viridis_d() +
  labs(x = "Hour of day (UTC?)",
       y = "jhno3",
       color = NULL) +
  scale_x_continuous(breaks = c(0,2,4,6,8,10,12,14,16,18,20,22)) +
  # ylim(-0.5,12) +
  theme(legend.position = "top")

ggsave('jhno3_diurnal_old_data.svg',
       path = "output/plots_analysis/spec_rad",
       width = 15.37,
       height = 13,
       units = 'cm')



# Checking 2023 spec rad data used for pss calcs --------------------------

#looking at the deposition rate and figuring out which hours should be used to calculate it
#currently using 10 to 15 (which in UTC time is 11 to 16)

#plotting the spread of h, HONO lifetime and kdep for those hours
spec_rad %>% 
  filter(campaign == "February 2023",
         # date > "2023-02-20"
         ) %>%
  mutate(hour = hour(date),
         lifetime = ifelse(between(hour,11,15),1/jhono,NA_real_),
         # lifetime = na.approx(lifetime,na.rm = F),
         h = lifetime * dv,
         kdep = 0.01/h) %>% 
  filter(between(hour,11,15),
         # jhono > 0.0005
         ) %>%
  mutate(hour = as.character(hour)) %>% 
  pivot_longer(c(lifetime,h,kdep,jhono)) %>% 
  ggplot(aes(date,value,col = hour)) +
  # geom_path(aes(date,value)) +
  geom_point() +
  labs(x = NULL,
       y = NULL) +
  theme_bw() +
  facet_grid(rows = vars(name),scales = "free_y") +
  scale_colour_viridis_d()

spec_rad %>% 
  filter(date > "2023-02-07" & date < "2023-02-27") %>%
  mutate(doy = yday(date)) %>% 
  # pivot_longer(c(jhono_calc,j_hono)) %>%
  # pivot_longer(c(jhno3_calc,j_hno3)) %>%
  ggplot(aes(date,jhno3)) +
  facet_wrap(vars(doy),scales = "free") +
  scale_x_datetime(breaks = "4 hours",date_labels = "%H:%M") +
  geom_point()

