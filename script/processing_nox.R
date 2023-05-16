library(tidyverse)
library(lubridate)
library(openair)

setwd("~/Cape Verde/Processing code/code v3/processed_data")
Sys.setenv(TZ = "UTC")


# Code used for initial NOx df set to Lisa (12/02/23) ---------------------

dat = read.csv("NOx_2023_calc_df.csv",header=TRUE,na.strings= c('NA','missing')) %>% 
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

write.csv(nox,"~/CV Feb 23/processed_data/nox_data.csv",row.names = FALSE)  
