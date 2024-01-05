library(tidyverse)
library(lubridate)
library(janitor)

Sys.setenv(TZ = 'UTC')

#looking at HONO data from the ARNA flight campaigns

arna_hono = read.csv("data/hono/arna_hono/Renoxification_data_for_Anna_v2.csv") %>% 
  clean_names() %>% 
  mutate(start_time = dmy_hm(start_time),
         end_time = dmy_hm(end_time))

arna_hono %>% 
  mutate(year = year(start_time),
         hour = hour(start_time),
         year = as.character(year)) %>% 
  filter(altitude_m < 1000) %>%
  # pivot_longer(c(no3_ppt,hono_ppt_v,no_ppt_v,no2_ppt_v)) %>% 
  # filter(year == 2020) %>% 
  ggplot(aes(hono_ppt_v,altitude_m,col = year)) +
  geom_point() +
  theme_bw() +
  # scale_colour_viridis_d() +
  # facet_grid(cols = vars(name), scales = "free") +
  theme(legend.position = "top") +
  labs(x = "HONO / ppt",
       y = "Altitude / m",
       col = NULL) +
  NULL

ggsave('hono_altitude_arna_mbl.svg',
       path = "output/plots/arna",
       width = 30,
       height = 12,
       # height = 12,
       # width = 30,
       units = 'cm')
