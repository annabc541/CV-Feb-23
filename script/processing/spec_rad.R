library(tidyverse)
library(lubridate)
library(openair)
library(janitor)
library(zoo)
library(viridis)

Sys.setenv(TZ = 'UTC')

#script for creating a clean spec rad dataset for HONO analysis

# Reading in data ---------------------------------------------------------

#for 2015 data, use calculated data rather than measured data because of weird spec rad cal
spec_rad15 = read.csv("data/spec_rad/jrates_all_new_2015-2020.csv") %>% 
  clean_names() %>%
  mutate(date = dmy_hm(date)) %>% 
  # filter(date > "2015-11-23" & date < "2015-12-04") %>%
  timeAverage("1 hour") %>% 
  select(date,jhono = jhono_calc,jhno3 = jhno3_calc,jno2 = jno2_calc,jo1d = jo1d_calc)

#for 2019 and 2020 data (calc and measured) - 2020 measured data in lt rather than utc
spec_rad_calc = read.csv("data/spec_rad/2016_2020_Spec_rad_Hourly.csv") %>% 
  clean_names() %>% 
  mutate(date = dmy_hm(date),
         year = year(date),
         jhono = ifelse(is.na(j_hono),jhono_calc,j_hono),
         jhno3 = ifelse(is.na(j_hno3),jhno3_calc,j_hno3),
         jno2 = ifelse(is.na(j_no2),jno2_calc,j_no2),
         jo1d = ifelse(is.na(j_o1d),jo1d_calc,j_o1d)
  ) %>% 
  select(date:j_o1d,j_no2,j_hono,j_hno3,jhono:jo1d)

#for 2020 data with correct timezone - however doesn't have calculated data
spec_rad_timestamp_adj = read.csv("data/spec_rad/Spec_rad_Hourly_timestamp_adj.csv") %>%
  clean_names() %>%
  select(-c(date,x)) %>%
  mutate(date = dmy_hm(new_date),
         year = year(date)) %>%
  select(date,j_o1d,j_no2,j_hono,j_hno3) %>%
  rename_with(.fn = function(.x){paste0(.x,"_adj")},.cols = -date)

#for 2023 data
spec_rad23 = read.csv("data/spec_rad/Specrad_hour_23_with_calc_UPDATED.csv") %>% 
  mutate(date = dmy_hm(date),
         hour = hour(date),
         doy = yday(date)) %>% 
  filter(date >= "2023-02-07 08:35" & date < "2023-02-27") %>% 
  clean_names() %>% 
  mutate(jhono = ifelse(is.na(j_hono),jhono_calc,j_hono),
         jhno3 = ifelse(is.na(j_hno3),jhno3_calc,j_hno3),
         jo1d= ifelse(is.na(j_o1d),jo1d_calc,j_o1d)) %>% 
  select(date,hour,jhono,jhno3,jno2 = j_no2,jo1d)

# Sorting 2019 and 2020 ---------------------------------------------------

#using adjusted timezone data for 2020 (lines up with solar radiation)
spec_rad1920 = left_join(spec_rad_calc,spec_rad_timestamp_adj,by = "date") %>% 
  mutate(year = year(date),
         jhono = case_when(year == 2020 & is.na(j_hono_adj) == F ~ j_hono_adj,
                           year == 2020 & is.na(j_hono_adj) == T ~ jhono_calc,
                           TRUE ~ jhono),
         jhno3 = case_when(year == 2020 & is.na(j_hno3_adj) == F ~ j_hno3_adj,
                           year == 2020 & is.na(j_hno3_adj) == T ~ jhno3_calc,
                           TRUE ~ jhno3),
         jno2 = case_when(year == 2020 & is.na(j_no2_adj) == F ~ j_no2_adj,
                          year == 2020 & is.na(j_no2_adj) == T ~ jno2_calc,
                           TRUE ~ jno2),
         jo1d = case_when(year == 2020 & is.na(j_o1d_adj) == F ~ j_o1d_adj,
                          year == 2020 & is.na(j_o1d_adj) == T ~ jo1d_calc,
                           TRUE ~ jo1d)) %>% 
  select(date,jhono,jhno3,jno2,jo1d)

# spec_rad1920 %>%
#   mutate(month = month(date),
#          day = day(date),
#          year = year(date)) %>%
#   filter(month == 2,
#          year == 2018,
#          # is.na(j_hono) == F,
#          # date < "2020-07-07" & date > "2020-07-01"
#          ) %>%
#   mutate(solar_radiation = solar_radiation/10^6) %>%
#   pivot_longer(c(jhono,solar_radiation)) %>%
#   ggplot(aes(date,value,col = name)) +
#   geom_path() +
#   # facet_grid(rows = vars(name),scales = "free_y") +
#   # facet_wrap(vars(year),scales = "free_x",ncol = 1) +
#   # scale_x_datetime(breaks = "6 hours",date_labels = "%H:%M")
#   NULL

# Calculations for missing spec rad data values ---------------------------

#fill NAs with averages from hours where spec rad data is missing when reading data in
#should only be used for nighttime values, since calculated spec rad values are only for daytime
#missing daytime values should be replaced by calculated values
spec_rad_to_fix = bind_rows(spec_rad15,spec_rad1920,spec_rad23) %>% 
  mutate(hour = hour(date))

#find average j-values for each hour
spec_rad_mean = spec_rad_to_fix %>% 
  group_by(hour) %>% 
  summarise(jhono_avg = mean(jhono,na.rm = T),
            jhno3_avg = mean(jhno3,na.rm = T),
            jno2_avg = mean(jno2,na.rm = T),
            jo1d_avg = mean(jo1d,na.rm = T))

#replace NAs with average value for that hour
spec_rad = left_join(spec_rad_to_fix,spec_rad_mean,by = "hour") %>% 
  mutate(jhono = ifelse(is.na(jhono),jhono_avg,jhono),
         jhno3 = ifelse(is.na(jhno3),jhno3_avg,jhno3),
         jno2 = ifelse(is.na(jno2),jno2_avg,jno2),
         jo1d = ifelse(is.na(jo1d),jo1d_avg,jo1d)) %>% 
  select(-c(jhono_avg,jhno3_avg,jno2_avg,jo1d_avg,hour)) %>%
  arrange(date)

spec_rad %>% 
  filter(date > "2023-02-01") %>% 
  pivot_longer(c(jhono,jhno3,jno2,jo1d)) %>%
  ggplot(aes(date,value,col = name)) +
  geom_path() +
  facet_grid(rows = vars(name),scales = "free")

# write.csv(spec_rad,"data/spec_rad/spec_rad_processed.csv",row.names = F)

