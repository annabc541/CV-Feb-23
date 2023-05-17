library(tidyverse)
library(lubridate)
library(openair)

Sys.setenv(TZ = "UTC")


# Code used for initial NOx df set to Lisa (12/02/23) ---------------------

dat = read.csv("data/nox_data/NOx_2023_calc_df.csv",header=TRUE,na.strings= c('NA','missing')) %>% 
  tibble() %>% 
  rename(date = X) %>%
  mutate(date = ymd_hms(date),
         date = round_date(date, "1 sec"))

dat %>% 
  filter(date > "2023-02-14" & date < "2023-02-16") %>% 
  timeAverage(avg.time = "5 min") %>% 
  pivot_longer(c(NO_Conc_art_corrected,NO2_Conc,NO2_Conc_diode)) %>% 
  ggplot(aes(date,value,col = name)) +
  geom_point() +
  facet_grid(rows = vars(name),scales = "free_y")

#filterting for campaing period
#only using NO2 from diodes - corrected BLC NO2 didn't process -> LOOK INTO THIS!
nox_dat = dat %>% 
  filter(date > "2023-02-01" & date < "2023-03-02") %>% 
  select(date,no = NO_Conc_art_corrected, no2 = NO2_Conc_diode)

#5 min time average so that no and no2 measurements align (5 min measurement cycle)
#remove rows with NAs in both no and no2
nox = nox_dat %>% 
  timeAverage(avg.time = "5 min") %>% 
  subset(is.na(no) == FALSE & is.na(no2) == FALSE)

nox %>% 
  pivot_longer(c(no,no2)) %>%
  ggplot(aes(date,value)) +
  facet_grid(rows = vars(name),scales = "free_y")+
  geom_point()

write.csv(nox,"output/data/nox_data.csv",row.names = FALSE)  




# NOx data where spikes haven't been removed ------------------------------

dat_needed = dat %>% 
  filter(date > "2023-02-03",date < "2023-02-27")

dat_needed %>%  
  timeAverage("5 min") %>% 
  pivot_longer(c(NO_Conc_art_corrected,NO2_Conc_diode)) %>%
  # filter(NO_Conc_art_corrected < 10) %>% 
  ggplot(aes(date,value)) +
  facet_grid(rows = vars(name),scales = "free_y")+
  geom_point() +
  scale_x_datetime(date_breaks = "1 day",date_labels = "%b %d")

dat_spikes = read.csv("data/nox_data/NOx_2023_calc_df_spikes.csv",header=TRUE,na.strings= c('NA','missing')) %>% 
  tibble() %>% 
  rename(date = X) %>%
  mutate(date = ymd_hms(date),
         date = round_date(date, "1 sec"))

dat_spikes %>% 
  filter(date > "2023-02-03",date < "2023-02-27") %>% 
  timeAverage("1 hour") %>% 
  pivot_longer(c(NO_Conc_art_corrected,NO2_Conc_diode)) %>%
  # filter(NO_Conc_art_corrected < 10) %>% 
  ggplot(aes(date,value,col = name)) +
  scale_color_taylor() +
  theme(legend.position = "top") +
  # facet_grid(rows = vars(name),scales = "free_y")+
  geom_path() +
  scale_x_datetime(date_breaks = "1 day",date_labels = "%b %d")


# Looking at hono and nox together ----------------------------------------

hono = despiked_dat_night %>% 
  select(date,flag,hono) %>% 
  mutate(date = round_date(date,"1 min"),
         date = date + 3600)

nox = dat_spikes %>% 
  select(date,no = NO_Conc_art_corrected,no2 = NO2_Conc_diode) %>% 
  mutate(date = round_date(date,"1 min"))


nox_hono = hono %>% left_join(nox)

nox_hono %>%
  mutate(hono = ifelse(flag == 0,hono,NA_real_),
         # no = ifelse(no < 100,no,NA_real_),
         # no2 = ifelse(no2 < 100,no2,NA_real_),
         # no2 = ifelse(no2 > -10,no2,NA_real_)
  ) %>% 
  timeAverage("1 hour") %>%
  pivot_longer(c(hono,no,no2)) %>%
  ggplot(aes(date,value,col = name)) +
  facet_grid(rows = vars(name),scales = "free_y") +
  geom_path() +
  scale_x_datetime(date_breaks = "1 day",date_labels = "%b %d") +
  scale_color_taylor() +
  theme(legend.position = "top")
