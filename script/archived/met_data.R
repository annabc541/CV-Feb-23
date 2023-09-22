
setwd("D:/Cape Verde/data")

met_data = read.csv("2006-2021_Met_O3_data.csv") %>% 
  mutate(date = dmy_hm(date)) %>% 
  select(date,ws = WINDSPD_10M,wd = WINDDIR_10M,temp = TEMP_10M,rh = RH_10M,o3 = O3)

met_data23 = read.csv("met2023.csv") %>% 
  mutate(date = ymd_hms(date)) %>% 
  select(date,ws,wd,temp,rh = RH)


# 2023 --------------------------------------------------------------------

dat23_met = left_join(dat23,met_data23)

dat23_met %>% 
  mutate(hour = hour(date)) %>% 
  # filter(hour >= 11 & hour <= 15) %>% 
  ggplot(aes(rh,hono)) +
  geom_point() +
  scale_colour_viridis_c()
    

dat23_met %>% 
  # timeAverage("1 hour") %>% 
  mutate(sahara = na.approx(sahara,na.rm = FALSE)) %>% 
  rename(NO = no, HONO = hono, 'NO[2]' = no2,) %>% 
  pivot_longer(c(HONO,rh,wd,ws)) %>%
  filter(date > "2023-02-07 11:00" & date < "2023-02-26 19:00") %>% 
  ggplot(aes(date,value)) +
  theme_bw() +
  geom_path(size = 0.8) +
  facet_grid(rows = vars(name),scales = "free_y",labeller = label_parsed) +
  scale_colour_viridis_d() +
  labs(x = "Datetime (UTC)",
       y = "ppt",
       color = "ws") +
  scale_x_datetime(date_breaks = "2 day",date_labels = "%d/%m/%y") +
  NULL

ggsave('hono_met23.svg',
       path = "~/Cape Verde/peroxy_campaign/output/plots/timeseries",
       width = 30,
       height = 12,
       units = 'cm')

# Previous years ----------------------------------------------------------

dat_prev = bind_rows(dat15,dat19,dat20) %>% 
  arrange(date) %>% 
  timeAverage("1 hour") %>%
  mutate(campaign = case_when (date > "2015-11-24 17:00" & date < "2015-12-03 19:00" ~ "November 2015",
                               date > "2019-08-15 12:29" & date < "2019-08-29 01:00" ~ "August 2019",
                               date > "2020-02-14 01:00" & date < "2020-02-26 02:56" ~ "February 2020",
                               TRUE ~ "no campaign"))

met_data_prev = left_join(dat_prev,met_data)

met_data_prev %>% 
  filter(campaign == "November 2015") %>% 
  mutate(sahara = na.approx(sahara,na.rm = F),
         rh = ifelse(is.na(hono),NA,rh)) %>%
  fill(sahara,.direction = "up") %>%
  ggplot(aes(date,hono,col = rh)) +
  geom_path(size = 0.8) +
  scale_color_viridis() +
  labs(x = NULL,
       y = "HONO (ppt)",
       color = "RH") +
  theme_bw() +
  # facet_wrap(~factor(campaign,levels = c("November 2015","August 2019","February 2020")),
  #            scales = "free",ncol = 1) +
  # theme(legend.position = "top") +
  scale_x_datetime(date_breaks = "1 day",date_labels = "%d/%m") +
  NULL


# All together ------------------------------------------------------------

all_data = bind_rows(met_data_prev,dat23_met) %>% 
  arrange(date) %>% 
  timeAverage("1 hour") %>%
  mutate(campaign = case_when (date > "2015-11-24 17:00" & date < "2015-12-03 19:00" ~ "November 2015",
                               date > "2019-08-15 12:29" & date < "2019-08-29 01:00" ~ "August 2019",
                               date > "2020-02-14 01:00" & date < "2020-02-26 02:56" ~ "February 2020",
                               date >= "2023-02-07 08:35" ~ "February 2023",
                               TRUE ~ "no campaign"))

write.csv(all_data,"~/Cape Verde/peroxy_campaign/output/data/hono_nox_air_met.csv",row.names = F)

all_data %>% 
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
