library(tidyverse)
library(lubridate)
library(openair)
library(janitor)
library(zoo)
library(viridis)

Sys.setenv(TZ = 'UTC')

#most recent change: filtered out HONO on 16/02/20 because it looked very suspicious

# Universal dataframes ----------------------------------------------------

nitrate_dat = read.csv("data/nitrate_ammonium_CVAO_12-19.csv") %>% 
  clean_names() %>%
  mutate(date = mdy_hm(start_local_time),
         month = month(date),
         date = round_date(date, "6 hour")) %>% 
  select(sample_no,date,month,nitrate = nitrate_mg_m,ammonium = ammonium_mg_m)

air_mass = read.csv("data/new_CVAO_sector_%_boxes_1.csv") %>% 
  rename(date = X) %>% 
  mutate(date = ymd_hm(date)) %>% 
  clean_names()

# November 2015 -----------------------------------------------------------

#hourly data
hono15 = read.csv("data/hono2015.csv") %>% 
  mutate(date = dmy_hm(date),
         date = date + 3600) %>% #changing date to UTC 
  rename(hono = HONO_adj_v2)

nox15 = read.csv("data/nox_data/nox15.csv") %>% 
  mutate(date = ymd_hms(X)) %>% 
  filter(date > "2015-11-01") %>% 
  timeAverage("1 hour") %>% 
  select(date,no = NO_Conc_art_corrected,no2 = NO2_Conc_art_corrected)

df_list = list(nox15,hono15,air_mass,nitrate_dat)

dat15 = df_list %>% reduce(left_join,by = "date") %>% 
  arrange(date)

dat15 %>% 
  mutate(sahara = na.approx(sahara,na.rm = FALSE),
         nitrate = na.approx(nitrate,na.rm = FALSE)) %>% 
  rename(NO = no, HONO = hono, 'NO[2]' = no2) %>% 
  pivot_longer(c(HONO,NO,'NO[2]')) %>% 
  filter(date > "2015-11-24 17:00" & date < "2015-12-03 19:00") %>% 
  ggplot(aes(date,value,col = nitrate)) +
  theme_bw() +
  geom_path(size = 0.8) +
  facet_grid(rows = vars(name),scales = "free_y",labeller = label_parsed) +
  scale_colour_viridis_c() +
  labs(x = "Datetime (UTC)",
       y = "ppt",
       color = expression(Nitrate~(ug~m^-3))) +
  scale_x_datetime(date_breaks = "1 day",date_labels = "%d/%m/%y") +
  NULL

#expression(Nitrate~(ug~m^-3)) for nitrate colour scheme legend

ggsave('hono_nox_nitrate_nov15.svg',
       path = "output/plots/timeseries",
       width = 30,
       height = 12,
       units = 'cm')

# August 2019 -------------------------------------------------------------

#five-minute average
hono19 = read.csv("data/roberto_data/lopap_aug2019.csv") %>% 
  mutate(date = dmy_hms(start.gmt)) %>% 
  select(date,hono = hono.ppt)

nox19 = read.csv("data/nox_data/nox19.csv") %>% 
  mutate(date = ymd_hms(X)) %>% 
  filter(date > "2019-08-14" & date < "2019-08-30") %>% 
  timeAverage("5 min") %>% 
  select(date,no = NO_Conc_art_corrected,no2 = NO2_Conc_diode)

df_list = list(nox19,hono19,air_mass,nitrate_dat)

dat19 = df_list %>% reduce(left_join,by = "date") %>% 
  arrange(date)

dat19 %>% 
  timeAverage("1 hour") %>% 
  mutate(sahara = na.approx(sahara,na.rm = FALSE),
         nitrate = na.approx(nitrate,na.rm = FALSE)) %>% 
  rename(NO = no, HONO = hono, 'NO[2]' = no2,) %>% 
  pivot_longer(c(HONO,NO,'NO[2]')) %>% 
  filter(date > "2019-08-15 12:29" & date < "2019-08-29 01:00") %>% 
  ggplot(aes(date,value,col = sahara)) +
  theme_bw() +
  geom_path(size = 0.8) +
  facet_grid(rows = vars(name),scales = "free_y",labeller = label_parsed) +
  scale_colour_viridis_c() +
  labs(x = "Datetime (UTC)",
       y = "ppt",
       color = "Sahara %") +
  scale_x_datetime(date_breaks = "1 day",date_labels = "%d/%m/%y") +
  NULL

ggsave('hono_nox_sahara_aug19_all.svg',
       path = "output/plots/timeseries",
       width = 30,
       height = 12,
       units = 'cm')


# February 2020 -----------------------------------------------------------

#five-minute average
hono20 = read.csv("data/roberto_data/lopap_feb2020.csv") %>% 
  mutate(date = dmy_hms(start.gmt)) %>% 
  select(date,hono = hono.ppt) %>% 
  mutate(sus_flag = case_when(date > "2020-02-16 07:30" & date < "2020-02-16 12:00" ~ 1,
                              TRUE ~ 0),
         hono = ifelse(sus_flag == 1,NA_real_,hono))

nox20 = read.csv("data/nox_data/nox20.csv") %>% 
  mutate(date = ymd_hms(X)) %>% 
  filter(date > "2020-02-13" & date < "2020-02-28") %>% 
  timeAverage("5 min") %>% 
  select(date,no = NO_Conc_art_corrected,no2 = NO2_Conc_diode)

df_list = list(nox20,hono20,air_mass)

dat20 = df_list %>% reduce(left_join,by = "date") %>% 
  arrange(date)

dat20 %>% 
  timeAverage("1 hour") %>% 
  mutate(sahara = na.approx(sahara,na.rm = FALSE)) %>% 
  rename(NO = no, HONO = hono, 'NO[2]' = no2,) %>% 
  pivot_longer(c(HONO,NO,'NO[2]')) %>% 
  filter(date > "2020-02-14 01:00" & date < "2020-02-26 02:56") %>% 
  ggplot(aes(date,value,col = sahara)) +
  theme_bw() +
  geom_path(size = 0.8) +
  facet_grid(rows = vars(name),scales = "free_y",labeller = label_parsed) +
  scale_colour_viridis_c() +
  labs(x = "Datetime (UTC)",
       y = "ppt",
       color = "Sahara %") +
  scale_x_datetime(date_breaks = "1 day",date_labels = "%d/%m/%y") +
  NULL

ggsave('hono_nox_sahara_feb20.svg',
       path = "output/plots/timeseries",
       width = 30,
       height = 12,
       units = 'cm')

# February 2023 -----------------------------------------------------------

#five-minute average
hono23 = read.csv("output/data/processed_in_r3.csv") %>% 
  mutate(date = ymd_hms(date)) %>% 
  timeAverage("5 min") %>%
  select(date,hono)

nox23 = read.csv("data/nox_data/nox23.csv") %>% 
  mutate(date = ymd_hms(X)) %>% 
  filter(date > "2023-02-07" & date < "2023-02-27") %>% 
  timeAverage("5 min") %>%
  select(date,no = NO_Conc_art_corrected,no2 = NO2_Conc_diode)

df_list = list(nox23,hono23,air_mass)

dat23 = df_list %>% reduce(left_join,by = "date") %>% 
  arrange(date)

dat23 %>% 
  timeAverage("1 hour") %>% 
  mutate(sahara = na.approx(sahara,na.rm = FALSE)) %>% 
  rename(NO = no, HONO = hono, 'NO[2]' = no2,) %>% 
  # pivot_longer(c(HONO,NO,'NO[2]')) %>% 
  filter(date > "2023-02-07 11:00" & date < "2023-02-26 19:00") %>% 
  ggplot(aes(date,HONO,col = sahara)) +
  theme_bw() +
  geom_path(size = 0.8) +
  # facet_grid(rows = vars(name),scales = "free_y",labeller = label_parsed) +
  scale_colour_viridis_c() +
  labs(x = "Datetime (UTC)",
       y = "HONO (ppt)",
       color = "Sahara %") +
  scale_x_datetime(date_breaks = "1 day",date_labels = "%d/%m/%y") +
  NULL

ggsave('hono_sahara_feb23.svg',
       path = "output/plots/timeseries",
       width = 30,
       height = 12,
       units = 'cm')


# Joining data ------------------------------------------------------------

dat = bind_rows(dat15,dat19,dat20,dat23) %>% 
  arrange(date) %>% 
  timeAverage("1 hour") %>%
  mutate(campaign = case_when (date > "2015-11-24 17:00" & date < "2015-12-03 19:00" ~ "November 2015",
                               date > "2019-08-15 12:29" & date < "2019-08-29 01:00" ~ "August 2019",
                               date > "2020-02-14 01:00" & date < "2020-02-26 02:56" ~ "February 2020",
                               date >= "2023-02-07 08:35" ~ "February 2023",
                               TRUE ~ "no campaign"))

write.csv(dat,"output/data/hono_nox_airmass_all_campaigns.csv",row.names = FALSE)

dat %>% 
  filter(campaign != "no campaign") %>% 
  mutate(sahara = na.approx(sahara,na.rm = F)) %>%
  fill(sahara,.direction = "up") %>%
  ggplot(aes(date,hono,col = sahara)) +
  geom_path(size = 0.8) +
  scale_color_viridis() +
  labs(x = NULL,
       y = "HONO (ppt)",
       color = "Sahara %") +
  theme_bw() +
  facet_wrap(~factor(campaign,levels = c("November 2015","August 2019","February 2020","February 2023")),
             scales = "free",ncol = 1) +
  # theme(legend.position = "top") +
  scale_x_datetime(date_breaks = "1 day",date_labels = "%d/%m") +
  NULL

ggsave('hono_across_the_years_sahara.svg',
       path = "output/plots/timeseries",
       width = 30,
       height = 12,
       units = 'cm')

# Diurnals ----------------------------------------------------------------

#all hono data in one plot, colour coded by campaign
diurnals = dat %>% 
  filter(campaign != "no campaign",
         is.na(hono) == FALSE) %>% 
  pivot_wider(names_from = campaign,values_from = hono)

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
  labs(x = "Hour of day (UTC)",
       y = expression(NO[2]~(ppt)),
       color = NULL) +
  scale_x_continuous(breaks = c(0,4,8,12,16,20)) +
  # ylim(-0.5,12) +
  theme(legend.position = "top")

ggsave('hono_all_campaigns.svg',
       path = "output/plots/diurnals",
       width = 11,
       height = 13,
       units = 'cm')


# Campaign diurnals -------------------------------------------------------

diurnal_campaigns = dat %>% 
  mutate(NOx = no + no2) %>%
  rename(HONO = hono,NO = no,'NO[2]' = no2) %>% 
  filter(is.na(HONO) == FALSE,
         campaign == "November 2015") %>% 
  timeVariation(pollutant = c("HONO","NOx"))

diurnal_campaigns_dat = diurnal_campaigns$data$hour

diurnal_campaigns_dat %>% 
  ggplot(aes(hour,Mean,col = variable)) +
  geom_line(size = 1) +
  facet_grid(rows = vars(variable),scales = "free_y") +
  scale_colour_viridis_d() +
  theme_bw() +
  labs(x = "Hour of day (UTC)",
       y = "Mixing ratio (ppt)",
       color = NULL) +
  scale_x_continuous(breaks = c(0,4,8,12,16,20)) +
  # ylim(-1,12) +
  theme(legend.position = "top")

ggsave('hono_nox_nov15.svg',
       path = "output/plots/diurnals",
       width = 11,
       height = 13,
       units = 'cm')
