library(tidyverse)
library(lubridate)
library(openair)
library(janitor)
# library(zoo)


Sys.setenv(TZ = 'UTC')

hono_updated = read.csv("output/data/hono23.csv") %>% 
  mutate(date = ymd_hms(date)) %>% 
  # timeAverage("5 min") %>% 
  select(date,hono_updated = hono,flag)

hono_updated %>% 
  mutate(hono_updated = ifelse(flag == 0,hono_updated,NA_real_)) %>% 
  ggplot(aes(date,hono_updated)) +
  theme_bw() +
  geom_path(size = 0.8) +
  labs(x = "Datetime (UTC)",
       y = "HONO / ppt",
       col = NULL) +
  scale_x_datetime(date_breaks = "1 day",date_labels = "%d/%m") +
  scale_colour_viridis_d() +
  theme(legend.position = "top")


hono = read.csv("output/data/archive/processed_in_r3.csv") %>% 
  mutate(date = ymd_hms(date)) %>% 
  # timeAverage("5 min") %>%
  select(date,hono)

all_hono = left_join(hono_updated,hono,by = "date")

all_hono %>% 
  mutate(hono_updated = ifelse(flag == 0,hono_updated,NA_real_)) %>% 
  pivot_longer(c(hono_updated,hono)) %>% 
  ggplot(aes(date,value,col = name)) +
  theme_bw() +
  geom_path(size = 0.8) +
  labs(x = "Datetime (UTC)",
       y = "HONO / ppt",
       color = NULL) +
  # facet_grid(rows = vars(name),scales = "free_y")
  # scale_colour_viridis_d() +
  theme(legend.position = "top")

last_plot() + aes(group=rev(name))

ggsave('comparing_after_mfc_cal.svg',
       path = "output/plots_analysis",
       width = 30,
       height = 12,
       units = 'cm')
