library(tidyverse)
library(lubridate)
library(openair)
library(janitor)
library(zoo)
library(viridis)

Sys.setenv(TZ = 'UTC')

#plots data faceted based on campaigns - look in parameterisation_df_all and parameterisation23 for pss plots

#read in full data (data joined in creating_master_df)

dat_full = read.csv("output/data/all_data_utc.csv") %>% 
  mutate(date = ymd_hms(date),
         campaign = case_when(date > "2015-11-24" & date < "2015-12-04" ~ "November 2015",
                              date > "2019-08-15" & date < "2019-08-29" ~ "August 2019",
                              date > "2020-02-14" & date < "2020-02-27" ~ "February 2020",
                              date > "2023-02-07" & date < "2023-02-27" ~ "February 2023"))

# Timeseries --------------------------------------------------------------

#plot data in facets colour coded by different parameters

dat_full %>% 
  filter(campaign != "no campaign") %>%
  mutate(sahara = na.approx(sahara,na.rm = F)) %>%
  # timeAverage("1 hour") %>%
  # fill(sahara,.direction = "up") %>%
  ggplot(aes(date,hono)) +
  geom_path(size = 0.8) +
  scale_color_viridis() +
  labs(x = NULL,
       y = "HONO (ppt)") +
  theme_bw() +
  facet_wrap(~factor(campaign,levels = c("November 2015","August 2019","February 2020","February 2023")),
             scales = "free_x",ncol = 1) +
  # theme(legend.position = "top") +
  scale_x_datetime(date_breaks = "1 day",date_labels = "%d/%m") +
  NULL

ggsave('hono_timeseries_ground_campaigns.svg',
       path = "output/plots/timeseries",
       width = 33.29,
       height = 14.8,
       units = 'cm')

# Data faceted by campaign and parameters of interest ---------------------

dat %>% 
  filter(campaign != "no campaign") %>% 
  mutate(sahara = na.approx(sahara,na.rm = F),
         no = ifelse(no < 30,no,NA_real_),
         no2 = ifelse(no2 < 150,no2,NA_real_)) %>%
  fill(sahara,.direction = "up") %>%
  pivot_longer(c(hono,no,no2,rh,wd,ws)) %>% 
  ggplot(aes(date,value)) +
  geom_path(size = 0.8) +
  scale_color_viridis() +
  labs(x = NULL,
       y = NULL,
       color = "RH %") +
  theme_bw() +
  facet_grid(cols = vars(factor(campaign,levels = c("November 2015","August 2019","February 2020","February 2023"))),
             scales = "free",rows = vars(name)) +
  # theme(legend.position = "top") +
  scale_x_datetime(date_breaks = "2 day",date_labels = "%d/%m") +
  NULL

ggsave('hono_nox_met.svg',
       path = "~/Cape Verde/peroxy_campaign/output/plots/timeseries",
       width = 33,
       height = 19,
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
       y = " HONO (ppt)",
       color = NULL) +
  scale_x_continuous(breaks = c(0,4,8,12,16,20)) +
  # ylim(-0.5,12) +
  theme(legend.position = "top")

ggsave('hono_all_campaigns.svg',
       path = "output/plots/diurnals",
       width = 11,
       height = 12,
       units = 'cm')


# Campaign diurnals -------------------------------------------------------

diurnal_campaigns = dat_full %>% 
  mutate('NO[x]' = no + no2) %>%
  rename(HONO = hono,NO = no,'NO[2]' = no2) %>% 
  filter(is.na(HONO) == FALSE,
         campaign == "February 2023") %>% 
  timeVariation(pollutant = "HONO")

diurnal_campaigns_dat = diurnal_campaigns$data$hour

diurnal_dat = diurnal_campaigns_dat %>% 
  ungroup() %>% 
  select(hour,hono = Mean)

# write.csv(diurnal_dat,"output/data/diurnal_hono_feb23.csv",row.names = F)

diurnal_campaigns_dat %>% 
  ggplot(aes(hour,Mean)) +
  geom_line(size = 1) +
  # facet_grid(rows = vars(variable),scales = "free_y",labeller = label_parsed) +
  scale_colour_viridis_d() +
  theme_bw() +
  labs(x = "Hour of day (UTC)",
       y = "HONO (ppt)",
       color = NULL) +
  scale_x_continuous(breaks = c(0,4,8,12,16,20)) +
  # ylim(-1,13) +
  theme(legend.position = "top")

ggsave('hono_diurnal23.svg',
       path = "output/plots/agu",
       width = 7,
       height = 10,
       units = 'cm')
