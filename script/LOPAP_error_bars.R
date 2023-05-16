library(tidyverse)
library(lubridate)

#to look at error bars after having applied zeroes

setwd("~/CV Feb 23/processing/final_processing")
Sys.setenv(TZ = "UTC")

zeroed_dat = read.csv("cal1_zeroed.csv",header=TRUE,na.strings= c('NA','missing')) %>%
    tibble() %>% 
    mutate(Date = dmy_hms(Date)) %>% 
  rename(ch1 = channel.1,
         ch2 = channel.2,
         err1 = error,
         err2 = error.1,
         date = Date)


zeroed_dat %>%
  filter(date > "2023-02-11 15:10" & date < "2023-02-11 15:22") %>%
  pivot_longer(c(ch1,ch2)) %>%
  ggplot(aes(date,value)) +
  geom_path() +
  # geom_errorbar(aes(ymin=ch1-err1,ymax=ch1+err1),col = "red") +
  facet_grid(rows = vars(name), scales = "free") +
  scale_x_datetime(date_breaks = "1 min",date_labels = "%H:%M") +
  NULL
