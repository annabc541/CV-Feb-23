library(tidyverse)
library(lubridate)
library(oce)
library(openair)

#to flag data properly and remove instances when zeroing/calibrating/if there are issues

setwd("~/CV Feb 23/processed_data")
Sys.setenv(TZ = "UTC")

# Functions ---------------------------------------------------------------

#for despiking
tidy_rle = function(rleObj){
  
  require(dplyr)
  
  rleDf = tibble(lengths = rleObj$lengths,
                 values = rleObj$values) %>% 
    mutate(idxStart = cumsum(c(1,lengths))[1:(nrow(.))],
           idxEnd = cumsum(lengths))
  
  #
  rleDf
}


# Reading in data ---------------------------------------------------------

#read in data that has been fully processed in excel
#two sets of reagents have already been collated in excel
dat = read.csv("processing_the_third.csv",header=TRUE,na.strings= c('NA','missing')) %>% 
  tibble() %>% 
  mutate(date = dmy_hm(date))

#time averaging because it takes a measurement every 30 seconds, but when bringing the data in from excel, it has forgotten
#the seconds, so all measurements taken on the minute, leading to confusing graphs
dat1 = dat %>% timeAverage("1 min")

# Flagging data -----------------------------------------------------------

#plotting to see exactly where zeroes/problem periods beginning and end

dat1 %>%
  filter(date > "2023-02-19" & date < "2023-02-20") %>%
  ggplot(aes(date,hono)) +
  geom_path() +
  theme_bw() +
  scale_x_datetime(date_breaks = "1 hour",date_labels = "%H:%M") +
  NULL

#1 for water/abs closed
#2 for zero
#3 for cal
#4 for air in abs
#5 other

flagged_dat = dat %>% 
  mutate(flag = case_when(date < "2023-02-10 11:25" ~ 1,
                          between(date,as.POSIXct("2023-02-10 12:29"),as.POSIXct("2023-02-10 13:18")) ~ 2,
                          between(date,as.POSIXct("2023-02-10 18:23"),as.POSIXct("2023-02-10 19:04")) ~ 2,
                          between(date,as.POSIXct("2023-02-11 00:27"),as.POSIXct("2023-02-11 01:10")) ~ 2,
                          between(date,as.POSIXct("2023-02-11 01:34"),as.POSIXct("2023-02-11 08:45")) ~ 4,
                          between(date,as.POSIXct("2023-02-11 12:40"),as.POSIXct("2023-02-11 16:00")) ~ 3,
                          between(date,as.POSIXct("2023-02-11 17:18"),as.POSIXct("2023-02-11 18:14")) ~ 2,
                          between(date,as.POSIXct("2023-02-11 23:15"),as.POSIXct("2023-02-12 00:05")) ~ 2, #impossible to see zero, this is a bit of a guess
                          between(date,as.POSIXct("2023-02-12 05:25"),as.POSIXct("2023-02-12 06:10")) ~ 2,
                          between(date,as.POSIXct("2023-02-12 09:30"),as.POSIXct("2023-02-12 11:15")) ~ 1,
                          between(date,as.POSIXct("2023-02-12 11:47"),as.POSIXct("2023-02-12 12:36")) ~ 2,
                          between(date,as.POSIXct("2023-02-12 17:50"),as.POSIXct("2023-02-12 18:35")) ~ 2,
                          between(date,as.POSIXct("2023-02-13 00:03"),as.POSIXct("2023-02-13 00:33")) ~ 2, #as above with other nighttime zero
                          between(date,as.POSIXct("2023-02-13 05:50"),as.POSIXct("2023-02-13 06:43")) ~ 2,
                          between(date,as.POSIXct("2023-02-13 11:42"),as.POSIXct("2023-02-13 12:40")) ~ 2,
                          between(date,as.POSIXct("2023-02-13 16:04"),as.POSIXct("2023-02-13 16:47")) ~ 5, #new file not lining up
                          between(date,as.POSIXct("2023-02-13 18:03"),as.POSIXct("2023-02-13 18:54")) ~ 2,
                          between(date,as.POSIXct("2023-02-14 00:15"),as.POSIXct("2023-02-14 01:05")) ~ 2,
                          between(date,as.POSIXct("2023-02-14 06:45"),as.POSIXct("2023-02-14 07:27")) ~ 2,
                          between(date,as.POSIXct("2023-02-14 10:38"),as.POSIXct("2023-02-14 10:48")) ~ 5, #cleaned inlet
                          between(date,as.POSIXct("2023-02-14 12:50"),as.POSIXct("2023-02-14 13:32")) ~ 2,
                          between(date,as.POSIXct("2023-02-14 19:09"),as.POSIXct("2023-02-14 19:52")) ~ 2,
                          between(date,as.POSIXct("2023-02-15 01:25"),as.POSIXct("2023-02-15 02:10")) ~ 2,
                          between(date,as.POSIXct("2023-02-15 07:30"),as.POSIXct("2023-02-15 08:26")) ~ 2,
                          between(date,as.POSIXct("2023-02-15 13:42"),as.POSIXct("2023-02-15 14:30")) ~ 2,
                          between(date,as.POSIXct("2023-02-15 20:02"),as.POSIXct("2023-02-15 20:42")) ~ 2,
                          between(date,as.POSIXct("2023-02-16 02:15"),as.POSIXct("2023-02-16 03:10")) ~ 2,
                          between(date,as.POSIXct("2023-02-16 08:34"),as.POSIXct("2023-02-16 09:28")) ~ 2,
                          between(date,as.POSIXct("2023-02-16 14:52"),as.POSIXct("2023-02-16 15:25")) ~ 2,
                          between(date,as.POSIXct("2023-02-16 15:25"),as.POSIXct("2023-02-16 16:15")) ~ 1,
                          between(date,as.POSIXct("2023-02-16 21:08"),as.POSIXct("2023-02-16 21:54")) ~ 2,
                          between(date,as.POSIXct("2023-02-17 03:35"),as.POSIXct("2023-02-17 04:12")) ~ 2,
                          between(date,as.POSIXct("2023-02-17 09:03"),as.POSIXct("2023-02-17 15:55")) ~ 1, #changing reagents
                          between(date,as.POSIXct("2023-02-17 15:55"),as.POSIXct("2023-02-17 18:12")) ~ 5, #zeroes and air in abs
                          between(date,as.POSIXct("2023-02-17 23:45"),as.POSIXct("2023-02-18 01:09")) ~ 2,
                          between(date,as.POSIXct("2023-02-18 04:47"),as.POSIXct("2023-02-18 08:55")) ~ 4,
                          between(date,as.POSIXct("2023-02-18 09:14"),as.POSIXct("2023-02-18 10:27")) ~ 2,
                          between(date,as.POSIXct("2023-02-18 10:36"),as.POSIXct("2023-02-18 11:03")) ~ 4,
                          between(date,as.POSIXct("2023-02-18 15:53"),as.POSIXct("2023-02-18 16:38")) ~ 2,
                          between(date,as.POSIXct("2023-02-19 08:53"),as.POSIXct("2023-02-19 11:27")) ~ 5, #power cut, resetting everything after
                          TRUE ~ 0))


#plotting flagged data to see what it looks like
flagged_dat %>%
  # filter(date > "2023-02-18" & date < "2023-02-19") %>% 
  mutate(hono = case_when(flag != 0 ~ NA_real_,
                          hono > 30 ~ NA_real_,
                          hono < -3 ~ NA_real_,
                          TRUE ~ hono)) %>%
  ggplot(aes(date,hono)) +
  geom_path() +
  theme_bw() +
  NULL

#nb massive spike on 11/02, will probably be removed with despiking, but should look into what has caused it











#create flagged dataset with different columns for different reasons why the data has been flagged
flagged_dat = dat %>% 
  mutate(zero = case_when(between(date,as.POSIXct("2023-02-10 12:31"),as.POSIXct("2023-02-10 13:20")) ~ 1,
                          between(date,as.POSIXct("2023-02-10 18:30"),as.POSIXct("2023-02-10 19:15")) ~ 1,
                          between(date,as.POSIXct("2023-02-11 00:30"),as.POSIXct("2023-02-11 01:15")) ~ 1,
                          between(date,as.POSIXct("2023-02-11 17:24"),as.POSIXct("2023-02-11 18:18")) ~ 1,
                          between(date,as.POSIXct("2023-02-11 23:27"),as.POSIXct("2023-02-12 00:10")) ~ 1,
                          between(date,as.POSIXct("2023-02-12 05:26"),as.POSIXct("2023-02-12 06:07")) ~ 1,
                          between(date,as.POSIXct("2023-02-12 11:50"),as.POSIXct("2023-02-12 12:45")) ~ 1,
                          between(date,as.POSIXct("2023-02-12 17:35"),as.POSIXct("2023-02-12 18:43")) ~ 1,
                          between(date,as.POSIXct("2023-02-13 00:03"),as.POSIXct("2023-02-13 00:33")) ~ 1,
                          between(date,as.POSIXct("2023-02-13 05:55"),as.POSIXct("2023-02-13 06:44")) ~ 1,
                          between(date,as.POSIXct("2023-02-13 11:45"),as.POSIXct("2023-02-13 12:47")) ~ 1,
                          between(date,as.POSIXct("2023-02-13 18:05"),as.POSIXct("2023-02-13 18:55")) ~ 1,
                          between(date,as.POSIXct("2023-02-14 00:15"),as.POSIXct("2023-02-14 01:05")) ~ 1, 
                          between(date,as.POSIXct("2023-02-14 06:35"),as.POSIXct("2023-02-14 07:25")) ~ 1, #instrumental noise
                          between(date,as.POSIXct("2023-02-14 12:45"),as.POSIXct("2023-02-14 13:50")) ~ 1, #instrumental noise
                          between(date,as.POSIXct("2023-02-14 19:10"),as.POSIXct("2023-02-14 19:55")) ~ 1, #instrumental noise
                          between(date,as.POSIXct("2023-02-15 01:25"),as.POSIXct("2023-02-15 02:17")) ~ 1, #instrumental noise
                          between(date,as.POSIXct("2023-02-15 07:36"),as.POSIXct("2023-02-15 08:24")) ~ 1, #instrumental noise
                          between(date,as.POSIXct("2023-02-15 13:42"),as.POSIXct("2023-02-15 14:37")) ~ 1,
                          between(date,as.POSIXct("2023-02-15 20:05"),as.POSIXct("2023-02-15 20:50")) ~ 1, #instrumental noise before zero
                          between(date,as.POSIXct("2023-02-16 02:20"),as.POSIXct("2023-02-16 03:15")) ~ 1, #instrumental noise after zero
                          between(date,as.POSIXct("2023-02-16 08:27"),as.POSIXct("2023-02-16 09:45")) ~ 1, #instrumental noise after zero
                          between(date,as.POSIXct("2023-02-16 21:05"),as.POSIXct("2023-02-16 22:00")) ~ 1, #instrumental noise before and after zero
                          between(date,as.POSIXct("2023-02-17 03:22"),as.POSIXct("2023-02-17 04:15")) ~ 1, #instrumental noise
                          between(date,as.POSIXct("2023-02-17 17:30"),as.POSIXct("2023-02-17 18:10")) ~ 1,
                          between(date,as.POSIXct("2023-02-17 23:40"),as.POSIXct("2023-02-18 00:24")) ~ 1, #big spikes overnight, not instrumental noise? not sure how to deal with them, don't interrupt zero but are before and after it
                          between(date,as.POSIXct("2023-02-18 09:14"),as.POSIXct("2023-02-18 10:30")) ~ 1,
                          between(date,as.POSIXct("2023-02-18 15:55"),as.POSIXct("2023-02-18 16:40")) ~ 1,
                          between(date,as.POSIXct("2023-02-19 16:19"),as.POSIXct("2023-02-19 17:16")) ~ 1, #air in abs right as zero ends
                          between(date,as.POSIXct("2023-02-20 10:55"),as.POSIXct("2023-02-20 11:34")) ~ 1,
                          between(date,as.POSIXct("2023-02-20 17:12"),as.POSIXct("2023-02-20 17:47")) ~ 1,
                          between(date,as.POSIXct("2023-02-21 08:51"),as.POSIXct("2023-02-21 09:40")) ~ 1,
                          between(date,as.POSIXct("2023-02-21 16:55"),as.POSIXct("2023-02-21 17:28")) ~ 1,
                          between(date,as.POSIXct("2023-02-21 23:02"),as.POSIXct("2023-02-21 23:45")) ~ 1,
                          between(date,as.POSIXct("2023-02-22 05:15"),as.POSIXct("2023-02-22 06:15")) ~ 1,
                          between(date,as.POSIXct("2023-02-22 11:34"),as.POSIXct("2023-02-22 12:23")) ~ 1,
                          between(date,as.POSIXct("2023-02-22 17:51"),as.POSIXct("2023-02-22 18:34")) ~ 1,
                          between(date,as.POSIXct("2023-02-23 00:09"),as.POSIXct("2023-02-23 00:45")) ~ 1,
                          between(date,as.POSIXct("2023-02-23 06:23"),as.POSIXct("2023-02-23 07:03")) ~ 1,
                          between(date,as.POSIXct("2023-02-23 12:35"),as.POSIXct("2023-02-23 13:25")) ~ 1,
                          between(date,as.POSIXct("2023-02-23 18:55"),as.POSIXct("2023-02-23 19:55")) ~ 1,
                          between(date,as.POSIXct("2023-02-24 01:12"),as.POSIXct("2023-02-24 01:52")) ~ 1,
                          between(date,as.POSIXct("2023-02-24 07:30"),as.POSIXct("2023-02-24 08:08")) ~ 1,
                          between(date,as.POSIXct("2023-02-24 13:36"),as.POSIXct("2023-02-24 14:26")) ~ 1,
                          between(date,as.POSIXct("2023-02-24 20:00"),as.POSIXct("2023-02-24 20:38")) ~ 1,
                          between(date,as.POSIXct("2023-02-25 02:05"),as.POSIXct("2023-02-25 02:55")) ~ 1,
                          between(date,as.POSIXct("2023-02-25 08:30"),as.POSIXct("2023-02-25 09:13")) ~ 1,
                          between(date,as.POSIXct("2023-02-25 14:40"),as.POSIXct("2023-02-25 15:20")) ~ 1,
                          between(date,as.POSIXct("2023-02-26 09:24"),as.POSIXct("2023-02-26 10:19")) ~ 1, #pump changed in the middle of zero
                          between(date,as.POSIXct("2023-02-26 15:45"),as.POSIXct("2023-02-26 16:45")) ~ 1,
                          TRUE ~ 0),
         water = case_when(date < "2023-02-10 11:23" ~ 1,
                           between(date,as.POSIXct("2023-02-12 09:33"),as.POSIXct("2023-02-12 10:53")) ~ 1,
                           between(date,as.POSIXct("2023-02-17 08:49"),as.POSIXct("2023-02-17 15:00")) ~ 1,
                           between(date,as.POSIXct("2023-02-19 08:49"),as.POSIXct("2023-02-19 11:00")) ~ 1,
                           between(date,as.POSIXct("2023-02-23 08:30"),as.POSIXct("2023-02-23 10:50")) ~ 1,
                           TRUE ~ 0),
         cal = case_when(between(date,as.POSIXct("2023-02-11 13:07"),as.POSIXct("2023-02-11 16:08")) ~ 1,
                         between(date,as.POSIXct("2023-02-21 14:36"),as.POSIXct("2023-02-21 16:21")) ~ 1, #air in abs just as cal ended
                         TRUE ~ 0),
         air = case_when(between(date,as.POSIXct("2023-02-11 02:01"),as.POSIXct("2023-02-11 08:38")) ~ 1,
                         between(date,as.POSIXct("2023-02-14 10:35"),as.POSIXct("2023-02-14 10:50")) ~ 1, #cleaned inlet at 10:48
                         between(date,as.POSIXct("2023-02-16 14:50"),as.POSIXct("2023-02-16 16:15")) ~ 1, #air in abs right after zero
                         between(date,as.POSIXct("2023-02-17 15:00"),as.POSIXct("2023-02-17 17:30")) ~ 1, #air in abs during zero
                         between(date,as.POSIXct("2023-02-18 04:53"),as.POSIXct("2023-02-18 08:40")) ~ 1,
                         between(date,as.POSIXct("2023-02-18 10:35"),as.POSIXct("2023-02-18 10:55")) ~ 1,
                         between(date,as.POSIXct("2023-02-19 14:30"),as.POSIXct("2023-02-19 15:25")) ~ 1, #zero but tube was disconnected at inlet initially
                         between(date,as.POSIXct("2023-02-19 17:16"),as.POSIXct("2023-02-20 09:20")) ~ 1,
                         between(date,as.POSIXct("2023-02-20 22:27"),as.POSIXct("2023-02-21 07:47")) ~ 1,
                         between(date,as.POSIXct("2023-02-21 16:21"),as.POSIXct("2023-02-21 16:55")) ~ 1,
                         between(date,as.POSIXct("2023-02-24 08:42"),as.POSIXct("2023-02-24 10:36")) ~ 1,
                         between(date,as.POSIXct("2023-02-25 09:13"),as.POSIXct("2023-02-25 10:50")) ~ 1, #pump at 20
                         between(date,as.POSIXct("2023-02-25 15:20"),as.POSIXct("2023-02-26 08:40")) ~ 1,
                         TRUE ~ 0),
         flag = case_when(zero == 0 & water == 0 & cal == 0 & air == 0 ~ 0, #flag column to remove all flagged data easily
                          between(date,as.POSIXct("2023-02-25 10:50"),as.POSIXct("2023-02-26 09:57")) ~ 2,
                          TRUE ~ 1)) 

#plotting flagged data to see what it looks like
flagged_dat %>% 
  mutate(hono = ifelse(flag == 0, hono, NA_real_),
         doy = yday(date)) %>%
  # filter(doy == 58) %>%
  ggplot(aes(date,hono)) +
  geom_path() +
  # geom_errorbar(aes(ymin=hono-err,ymax=hono+err),col = "red") +
  theme_bw() +
  # scale_x_datetime(date_breaks = "2 min",
  # date_labels = "%H:%M"
  # ) +
  NULL

#not sure what's going on on the last day, data drops below zero significantly?
