
#checking the spec rad data and trying to figure out if it's in UTC or not


# Reading data in ---------------------------------------------------------

spec_rad15 = read.csv("data/spec_rad/jrates_all_new_2015-2020.csv") %>% 
  clean_names() %>%
  mutate(date = dmy_hm(date)) %>% 
  filter(date > "2015-11-23" & date < "2015-12-04") %>%
  timeAverage("1 hour") %>% 
  select(date,jhono = jhono_calc,jhno3 = jhno3_calc,solar_radiation)

spec_rad23 = read.csv("data/spec_rad/Specrad_hour_23_with_calc.csv") %>% 
  mutate(date = dmy_hm(date),
         # date = date + 3600 #if data is in Cape Verde time, add an hour for it to be UTC
         ) %>% 
  clean_names() %>% 
  mutate(jhono = ifelse(is.na(j_hono),jhono_calc,j_hono),
         jhno3 = ifelse(is.na(j_hno3),jhno3_calc,j_hno3)) %>% 
  select(date,jhono,jhno3,solar_radiation)

spec_rad_historic = read.csv("data/spec_rad/2016_2020_Spec_rad_Hourly.csv") %>% 
  mutate(date = dmy_hm(date)) %>% 
  clean_names() %>% 
  mutate(jhono = ifelse(is.na(j_hono),jhono_calc,j_hono),
         jhno3 = ifelse(is.na(j_hno3),jhno3_calc,j_hno3)) %>% 
  select(date,jhono,jhno3,solar_radiation)

#fill NAs with averages from hours where spec rad data is missing when reading data in
spec_rad_to_fix = bind_rows(spec_rad15,spec_rad_historic,spec_rad23) %>% 
  mutate(hour = hour(date))

#find average jhono and jhno3 values for each hour
spec_rad_mean = spec_rad_to_fix %>% 
  group_by(hour) %>% 
  summarise(jhono_avg = mean(jhono,na.rm = T),
            jhno3_avg = mean(jhno3,na.rm = T),
            sr_avg = mean(solar_radiation,na.rm = T))

#replace NAs with average value for that hour
#add campaign flag to plot campaign diurnals
spec_rad = left_join(spec_rad_to_fix,spec_rad_mean,by = "hour") %>% 
  mutate(jhono = ifelse(is.na(jhono),jhono_avg,jhono),
         jhno3 = ifelse(is.na(jhno3),jhno3_avg,jhno3),
         solar_radiation = ifelse(is.na(solar_radiation),sr_avg,solar_radiation),
         year = year(date),
         campaign = case_when (date > "2015-11-24 17:00" & date < "2015-12-03 19:00" ~ "November 2015",
                               date > "2019-08-15 12:29" & date < "2019-08-29 01:00" ~ "August 2019",
                               date > "2020-02-14 01:00" & date < "2020-02-26 02:56" ~ "February 2020",
                               date >= "2023-02-07 08:35" ~ "February 2023",
                               TRUE ~ "no campaign")) %>% 
  select(-c(jhono_avg,jhno3_avg,sr_avg))


# Plotting diurnals -------------------------------------------------------

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

# Notes -------------------------------------------------------------------

#the data looks a lot lower if the 2015 to 2020 dataset is used
#so I don't think it's right
#I also just can't tell what timezone it's in
