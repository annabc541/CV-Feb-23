#for plotting hono data for tap report
#plotting data that was analysed in CV - not final analysis


library(tidyverse)
library(lubridate)
library(openair)

setwd("~/CV Feb 23/processed_data")
Sys.setenv(TZ = "UTC")

processed_dat = read.csv("initial_processing.csv",header=TRUE,na.strings= c('NA','missing')) %>% 
  tibble()m
  mutate(date=waclr::parse_excel_date(date))


processed_dat %>% 
  timeAverage(avg.time = "30 min") %>% 
  ggplot(aes(date,hono)) +
  geom_path(size = 0.7) +
  scale_x_datetime(date_breaks = "1 day",
                   date_labels = "%d %b") +
  labs(x = "Date",
       y = "HONO / ppt") +
  theme_bw() +
  NULL

timeVariation(processed_dat,pollutant = "hono")


ggsave('initial_hono_timeseries.svg',
       path = '~/CV Feb 23/plots',
       width = 31,
       height = 15,
       units = 'cm')
