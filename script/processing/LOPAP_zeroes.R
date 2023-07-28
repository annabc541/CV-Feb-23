library(tidyverse)
library(lubridate)

#to look at zeroes in the raw data and be able to find when they start and end

Sys.setenv(TZ = "UTC")

raw_dat = read.csv("data/raw_data/reagents0.5.csv") %>% 
  mutate(date = glue::glue("{Date} {Time}"),
       date = dmy_hms(date)) %>% 
  arrange(date) %>% 
  select(date,ch1,ch2)

cal = raw_dat %>% 
  filter(date > "2023-02-09 12:30" & date < "2023-02-09 12:55")

cal %>% 
  pivot_longer(c(ch1,ch2)) %>%
  ggplot(aes(date,value)) +
  geom_point() +
  facet_grid(rows = vars(name),scales = "free_y") +
  scale_x_datetime(date_breaks = "5 min",date_labels = "%H:%M") +
  NULL




zero = raw_dat %>% 
  filter(date > "2023-02-09 05:20" & date < "2023-02-09 06:10")

zero %>% 
  pivot_longer(c(ch1,ch2)) %>%
  ggplot(aes(date,value)) +
  geom_point() +
  facet_grid(rows = vars(name),scales = "free_y") +
  scale_x_datetime(date_breaks = "5 min",date_labels = "%H:%M") +
  NULL

initial_value = zero$ch1[1]
ten_percent = initial_value*10/100
min_value = min(zero$ch1)
final_value = zero$ch1[90]
ninty_percent = final_value*90/100
