library(tidyverse)
library(lubridate)
library(zoo)
library(oce)
library(openair)
library(tayloRswift)
library(viridis)

Sys.setenv(TZ = "UTC")

#processing in r with calibration values from excel spreadsheet

cal0.5_ch1 = 359.93
cal0.5_ch2 = 380.61
cal1_ch1 = 625.39
cal1_ch2 = 650.99
cal2_ch1 = 683.29
cal2_ch2 = 710.10

date_corr1 = 22.75 * 60
date_corr2 = 23.67 * 60
date_corr3 = 22.33 * 60

se = 1

# Functions ---------------------------------------------------------------

ppt <- function(ch1,ch2,se){
  
  x = (ch1/se) - (ch2-ch1*(1-se))
  return(x)
}

#for night zeroes and despiking
tidy_rle = function(rleObj){
  
  require(dplyr)
  
  rleDf = tibble(lengths = rleObj$lengths,
                 values = rleObj$values) %>% 
    mutate(idxStart = cumsum(c(1,lengths))[1:(nrow(.))],
           idxEnd = cumsum(lengths))
  
  #
  rleDf
}

# First set of reagents ---------------------------------------------------

#read in data
raw_dat1 = read.csv("data/raw_data/reagents1.csv") %>% 
  mutate(date = glue::glue("{Date} {Time}"),
         date = ymd_hms(date)) %>% 
  arrange(date) %>% 
  select(date,ch1 = X550.6,ch2 = X550)

#create flag for zeroing
#flag only when values are actually low

wip_dat1 = raw_dat1 %>% 
  mutate(zeroing = case_when(between(date,as.POSIXct("2023-02-10 13:11"),as.POSIXct("2023-02-10 13:21")) ~ 1,
                             between(date,as.POSIXct("2023-02-10 19:13:30"),as.POSIXct("2023-02-10 19:22")) ~ 1,
                             between(date,as.POSIXct("2023-02-11 01:09"),as.POSIXct("2023-02-11 01:23")) ~ 1,
                             between(date,as.POSIXct("2023-02-11 13:40"),as.POSIXct("2023-02-11 14:53")) ~ 1,
                             between(date,as.POSIXct("2023-02-11 18:03:30"),as.POSIXct("2023-02-11 18:17")) ~ 1,
                             between(date,as.POSIXct("2023-02-12 00:01"),as.POSIXct("2023-02-12 00:17")) ~ 1,
                             between(date,as.POSIXct("2023-02-12 06:01"),as.POSIXct("2023-02-12 06:17")) ~ 1,
                             between(date,as.POSIXct("2023-02-12 12:34:30"),as.POSIXct("2023-02-12 12:47")) ~ 1,
                             between(date,as.POSIXct("2023-02-12 18:34"),as.POSIXct("2023-02-12 18:47")) ~ 1,
                             between(date,as.POSIXct("2023-02-13 00:34"),as.POSIXct("2023-02-13 00:47")) ~ 1,
                             between(date,as.POSIXct("2023-02-13 06:30:30"),as.POSIXct("2023-02-13 06:49")) ~ 1,
                             between(date,as.POSIXct("2023-02-13 12:27"),as.POSIXct("2023-02-13 12:44")) ~ 1,
                             between(date,as.POSIXct("2023-02-13 18:40"),as.POSIXct("2023-02-13 19:00")) ~ 1,
                             between(date,as.POSIXct("2023-02-14 00:55"),as.POSIXct("2023-02-14 01:17")) ~ 1,
                             between(date,as.POSIXct("2023-02-14 07:18"),as.POSIXct("2023-02-14 07:32")) ~ 1,
                             between(date,as.POSIXct("2023-02-14 13:37"),as.POSIXct("2023-02-14 13:45")) ~ 1,
                             between(date,as.POSIXct("2023-02-14 19:49"),as.POSIXct("2023-02-14 19:53")) ~ 1,
                             between(date,as.POSIXct("2023-02-15 08:20"),as.POSIXct("2023-02-15 08:34")) ~ 1,
                             between(date,as.POSIXct("2023-02-15 14:31"),as.POSIXct("2023-02-15 14:43")) ~ 1,
                             between(date,as.POSIXct("2023-02-15 20:45"),as.POSIXct("2023-02-15 20:59")) ~ 1,
                             between(date,as.POSIXct("2023-02-16 02:57"),as.POSIXct("2023-02-16 03:11")) ~ 1,
                             between(date,as.POSIXct("2023-02-16 09:15"),as.POSIXct("2023-02-16 09:30")) ~ 1,
                             between(date,as.POSIXct("2023-02-16 15:27"),as.POSIXct("2023-02-16 15:45")) ~ 1,
                             between(date,as.POSIXct("2023-02-16 21:47:30"),as.POSIXct("2023-02-16 22:02")) ~ 1,
                             between(date,as.POSIXct("2023-02-17 04:10"),as.POSIXct("2023-02-17 04:18")) ~ 1,
                             TRUE ~ 0),
         #create columns with only zero values
         ch1_zeroes = ifelse(zeroing == 1, ch1,NA),
         ch2_zeroes = ifelse(zeroing == 1, ch2,NA_real_),
         #interpolate between zeroes
         ch1_zeroes = na.approx(ch1_zeroes,na.rm = F),
         ch2_zeroes = na.approx(ch2_zeroes,na.rm = F)) %>% 
  #fill the values before first zero has been measured
  fill(ch1_zeroes,ch2_zeroes,.direction = "up")

#apply zeroes and cal correction

#1 for water/abs closed
#2 for zero
#3 for cal
#4 for air in abs
#5 other
final_dat1 = wip_dat1 %>% 
  mutate(date = date - date_corr1,
         ch1_zeroed = ch1 - ch1_zeroes,
         ch2_zeroed = ch2 - ch2_zeroes,
         ch1_ppt = ch1_zeroed * cal1_ch1,
         ch2_ppt = ch2_zeroed * cal1_ch2,
         hono = ppt(ch1_ppt,ch2_ppt,se),
         reagents = 1,
         flag = case_when(date < "2023-02-10 11:25" ~ 1,
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
                          between(date,as.POSIXct("2023-02-17 09:03"),as.POSIXct("2023-02-17 15:55")) ~ 1,
                          TRUE ~ 0))

# final_dat1 %>%
#   # filter(flag == 0) %>%
#   mutate(hono = ifelse(flag == 0,hono,NA_real_)) %>% 
#   ggplot(aes(date,hono)) +
#   # facet_grid(rows = vars(name),scales = "free_y") +
#   geom_path() +
#   # scale_x_datetime(date_breaks = "1 day",date_labels = "%b %d") +
#   # scale_color_taylor(palette = "taylorRed") +
#   # labs(y = "Channel 2 abs",
#   #      x = NULL)
#   NULL
# 
# ggsave('r1_spikes.png',
#        path = "output/plots_analysis/zeroing",
#        width = 30,
#        height = 12,
#        units = 'cm') 


# Second set of reagents - ZA zeroes --------------------------------------------------

#read in data
raw_dat2 = read.csv("data/raw_data/reagents2.csv") %>% 
  mutate(date = glue::glue("{Date} {Time}"),
         date = ymd_hms(date)) %>% 
  arrange(date) %>% 
  select(date,ch1 = X550.6,ch2 = X550)

#use za up until power cut on evening of 18/02

wip_dat2 = raw_dat2 %>% 
  filter(date < "2023-02-21") %>% #data before power cut
  mutate(zeroing = case_when(between(date,as.POSIXct("2023-02-17 18:15"),as.POSIXct("2023-02-17 18:24")) ~ 1,
                             between(date,as.POSIXct("2023-02-18 00:29"),as.POSIXct("2023-02-18 00:39")) ~ 1,
                             between(date,as.POSIXct("2023-02-18 09:50"),as.POSIXct("2023-02-18 10:35")) ~ 1,
                             between(date,as.POSIXct("2023-02-18 16:41"),as.POSIXct("2023-02-18 16:52")) ~ 1,
                             between(date,as.POSIXct("2023-02-19 17:02"),as.POSIXct("2023-02-19 17:13")) ~ 1, #lower than most, after power cut
                             between(date,as.POSIXct("2023-02-20 11:39"),as.POSIXct("2023-02-20 11:47")) ~ 1, #after power cut
                             between(date,as.POSIXct("2023-02-20 17:55"),as.POSIXct("2023-02-20 18:04")) ~ 1, #after power cut
                             TRUE ~ 0),
         #create columns with only zero values
         ch1_zeroes = ifelse(zeroing != 0, ch1,NA_real_),
         ch2_zeroes = ifelse(zeroing != 0, ch2,NA_real_),
         #interpolate between zeroes
         ch1_zeroes = na.approx(ch1_zeroes,na.rm = F),
         ch2_zeroes = na.approx(ch2_zeroes,na.rm = F)) %>% 
  #fill the values before first zero has been measured
  fill(ch1_zeroes,ch2_zeroes,.direction = "updown")

final_dat2 = wip_dat2 %>% 
  mutate(date = date - date_corr2,
         ch1_zeroed = ch1 - ch1_zeroes,
         ch2_zeroed = ch2 - ch2_zeroes,
         ch1_ppt = ch1_zeroed * cal2_ch1,
         ch2_ppt = ch2_zeroed * cal2_ch2,
         hono = ppt(ch1_ppt,ch2_ppt,se),
         reagents = 2,
         flag = (case_when(between(date,as.POSIXct("2023-02-17 09:03"),as.POSIXct("2023-02-17 15:55")) ~ 1, #changing reagents
                           between(date,as.POSIXct("2023-02-17 15:55"),as.POSIXct("2023-02-17 18:12")) ~ 5, #zeroes and air in abs
                           between(date,as.POSIXct("2023-02-17 23:45"),as.POSIXct("2023-02-18 01:09")) ~ 2,
                           between(date,as.POSIXct("2023-02-18 04:47"),as.POSIXct("2023-02-18 08:55")) ~ 4,
                           between(date,as.POSIXct("2023-02-18 09:14"),as.POSIXct("2023-02-18 10:27")) ~ 2,
                           between(date,as.POSIXct("2023-02-18 10:36"),as.POSIXct("2023-02-18 11:03")) ~ 4,
                           between(date,as.POSIXct("2023-02-18 15:53"),as.POSIXct("2023-02-18 16:38")) ~ 2,
                           between(date,as.POSIXct("2023-02-19 08:53"),as.POSIXct("2023-02-19 11:27")) ~ 5, #power cut, resetting everything after
                           between(date,as.POSIXct("2023-02-19 14:20"),as.POSIXct("2023-02-19 15:25")) ~ 5, #zero tube not connected at inlet, not proper zero
                           between(date,as.POSIXct("2023-02-19 16:25"),as.POSIXct("2023-02-19 17:15")) ~ 2,
                           between(date,as.POSIXct("2023-02-19 17:17"),as.POSIXct("2023-02-20 09:02")) ~ 4,
                           between(date,as.POSIXct("2023-02-20 10:54"),as.POSIXct("2023-02-20 11:50")) ~ 2,
                           between(date,as.POSIXct("2023-02-20 17:12"),as.POSIXct("2023-02-20 17:54")) ~ 2,
                           between(date,as.POSIXct("2023-02-20 22:27"),as.POSIXct("2023-02-21 07:54")) ~ 4,
                           TRUE ~ 0)))


# Second set of reagents - night zeroes-----------------------------------------------------------

night_zeroing = raw_dat2 %>%
  filter(date > "2023-02-21") %>% #data after power cut
  mutate(time = hour(date),
         zeroing = case_when(between(date,as.POSIXct("2023-02-21 09:31"),as.POSIXct("2023-02-21 09:44")) ~ 1,
                             between(date,as.POSIXct("2023-02-21 23:47:30"),as.POSIXct("2023-02-21 23:59")) ~ 1,
                             between(date,as.POSIXct("2023-02-22 06:02"),as.POSIXct("2023-02-22 06:15")) ~ 1,
                             between(date,as.POSIXct("2023-02-22 12:22"),as.POSIXct("2023-02-22 12:30:30")) ~ 1,
                             between(date,as.POSIXct("2023-02-22 18:39"),as.POSIXct("2023-02-22 18:47")) ~ 1,
                             between(date,as.POSIXct("2023-02-23 00:51"),as.POSIXct("2023-02-23 01:02")) ~ 1,
                             between(date,as.POSIXct("2023-02-23 07:07"),as.POSIXct("2023-02-23 07:18")) ~ 1,
                             between(date,as.POSIXct("2023-02-23 13:25"),as.POSIXct("2023-02-23 13:35")) ~ 1,
                             between(date,as.POSIXct("2023-02-23 19:42"),as.POSIXct("2023-02-23 19:52")) ~ 1,
                             between(date,as.POSIXct("2023-02-24 01:57"),as.POSIXct("2023-02-24 02:05")) ~ 1,
                             between(date,as.POSIXct("2023-02-24 08:11"),as.POSIXct("2023-02-24 08:22")) ~ 1,
                             between(date,as.POSIXct("2023-02-24 14:32"),as.POSIXct("2023-02-24 14:38")) ~ 1,
                             between(date,as.POSIXct("2023-02-24 20:40"),as.POSIXct("2023-02-24 20:54")) ~ 1,
                             between(date,as.POSIXct("2023-02-25 02:57"),as.POSIXct("2023-02-25 03:06")) ~ 1,
                             between(date,as.POSIXct("2023-02-25 09:10"),as.POSIXct("2023-02-25 09:16")) ~ 1,
                             between(date,as.POSIXct("2023-02-26 16:32"),as.POSIXct("2023-02-26 16:45")) ~ 1,
                             TRUE ~ 0),
         date = date - date_corr2, #so that flagging is applied properly
         flag = (case_when(between(date,as.POSIXct("2023-02-20 22:27"),as.POSIXct("2023-02-21 07:54")) ~ 4,
                           between(date,as.POSIXct("2023-02-21 08:50"),as.POSIXct("2023-02-21 09:56")) ~ 2,
                           between(date,as.POSIXct("2023-02-21 14:50"),as.POSIXct("2023-02-21 16:15")) ~ 3, #not very clear in terms of start and finish of zeroes, also followed by air in abs
                           between(date,as.POSIXct("2023-02-21 16:15"),as.POSIXct("2023-02-21 16:27")) ~ 4,
                           between(date,as.POSIXct("2023-02-21 16:27"),as.POSIXct("2023-02-21 17:39")) ~ 2,
                           between(date,as.POSIXct("2023-02-21 23:03"),as.POSIXct("2023-02-21 23:51")) ~ 2,
                           between(date,as.POSIXct("2023-02-22 05:15"),as.POSIXct("2023-02-22 06:16")) ~ 2,
                           between(date,as.POSIXct("2023-02-22 11:30"),as.POSIXct("2023-02-22 12:30")) ~ 2,
                           between(date,as.POSIXct("2023-02-22 17:52"),as.POSIXct("2023-02-22 18:37")) ~ 2,
                           between(date,as.POSIXct("2023-02-23 00:09"),as.POSIXct("2023-02-23 00:44")) ~ 2,
                           between(date,as.POSIXct("2023-02-23 06:24"),as.POSIXct("2023-02-23 07:03")) ~ 2,
                           between(date,as.POSIXct("2023-02-23 08:44"),as.POSIXct("2023-02-23 10:39")) ~ 1,
                           between(date,as.POSIXct("2023-02-23 12:35"),as.POSIXct("2023-02-23 13:31")) ~ 2,
                           between(date,as.POSIXct("2023-02-23 18:51"),as.POSIXct("2023-02-23 19:32")) ~ 2,
                           between(date,as.POSIXct("2023-02-24 01:07"),as.POSIXct("2023-02-24 01:49")) ~ 2,
                           between(date,as.POSIXct("2023-02-24 07:30"),as.POSIXct("2023-02-24 08:20")) ~ 2,
                           between(date,as.POSIXct("2023-02-24 08:40"),as.POSIXct("2023-02-24 11:04")) ~ 5, #cleaned inlet and syringing
                           between(date,as.POSIXct("2023-02-24 13:36"),as.POSIXct("2023-02-24 14:40")) ~ 2,
                           between(date,as.POSIXct("2023-02-24 19:59"),as.POSIXct("2023-02-24 20:38")) ~ 2,
                           between(date,as.POSIXct("2023-02-25 02:05"),as.POSIXct("2023-02-25 02:54")) ~ 2,
                           between(date,as.POSIXct("2023-02-25 08:30"),as.POSIXct("2023-02-25 09:09")) ~ 2,
                           between(date,as.POSIXct("2023-02-25 09:09"),as.POSIXct("2023-02-26 10:15")) ~ 5, #liquid pump at 20
                           between(date,as.POSIXct("2023-02-26 15:52"),as.POSIXct("2023-02-26 16:32")) ~ 2,
                           TRUE ~ 0)),
         night_flag = ifelse(time > 21 | time < 4,1,0)) #flag when it's dark

nights = rle(night_zeroing$night_flag) %>%
  tidy_rle() %>% 
  filter(values == 1) %>% 
  mutate(id = 1:nrow(.)) %>% 
  as.list() %>% 
  purrr::pmap_df(~data.frame(idx = ..3:..4,id = ..5)) %>% #df with row numbers and corresponding groups
  tibble() 

night_flagged = night_zeroing %>% 
  mutate(idx = 1:nrow(.)) %>% 
  left_join(nights, "idx") %>% #joins two dfs by their row number
  mutate(id = ifelse(is.na(id), 0, id)) #makes id (group) = 0 during the day

night_avg = night_flagged %>% 
  filter(flag == 0, #don't want any flagged moments included in nighttime average
         id != 0) %>%
  group_by(id) %>% 
  summarise(ch1_night = mean(ch1),
            ch2_night = mean(ch2),
            idx = mean(idx)) %>% 
  ungroup() %>% 
  mutate(idx = round(idx),
         ch1_inter = ifelse(id < 6,ch1_night,NA_real_),
         ch2_inter = ifelse(id < 6,ch2_night,NA_real_))
#id column = 5.5, not adding values to ch1/ch2_night cols because these weren't actually measured
#adding idx, row number from last measurement on 24/02 before abs were cleaned and measurements went a bit weird
#adding extrapolated values for that idx, using only previously three nighttime averages because of best
#line fit
night_avg[nrow(night_avg) + 1,] = list(5.5,NA_real_,NA_real_,14299,0.05299002,0.04949119)
night_avg = night_avg %>% arrange(idx)

final_dat_night = night_zeroing %>% 
  mutate(idx = 1:nrow(.)) %>% 
  left_join(night_avg) %>%
  mutate(ch1_zeroes = na.approx(ch1_inter,na.rm = F),
         ch2_zeroes = na.approx(ch2_inter,na.rm = F)) %>% 
  fill(ch1_zeroes,ch2_zeroes,.direction = "up") %>%
  mutate(ch1_zeroed = ifelse(date < "2023-02-24 08:41",ch1 - ch1_zeroes,ch1 - 0.04973971),
         ch2_zeroed = ifelse(date < "2023-02-24 08:41",ch2 - ch2_zeroes,ch2 - 0.04323023),
         ch1_ppt = ch1_zeroed * cal2_ch1,
         ch2_ppt = ch2_zeroed * cal2_ch2,
         reagents = 2.1,
         hono = ppt(ch1_ppt,ch2_ppt,se)) %>% 
  select(-c(id,idx))


# final_dat_night %>% 
#   mutate(ch1 = ifelse(flag == 0, ch1,NA_real_),
#          ch1_zeroed = ifelse(flag == 0, ch1_zeroed,NA_real_),
#          ch2 = ifelse(flag == 0, ch2,NA_real_),
#          ch2_zeroed = ifelse(flag == 0, ch2_zeroed,NA_real_),
#          hono = ifelse(flag == 0, hono,NA_real_)) %>%
#   pivot_longer(c(ch1,ch1_night,ch1_zeroes,ch1_zeroed)) %>%
#   # pivot_longer(c(ch2,ch2_zeroes,ch2_zeroed)) %>%
#   ggplot() +
#   geom_path(aes(date,hono)) +
#   # geom_path(aes(date,value,col = name)) +
#   # geom_point(aes(date,ch1_inter)) +
#   # scale_color_taylor("speakNowLive")
#   NULL


# Data without a proper calibration ---------------------------------------

raw_dat3 = read.csv("data/raw_data/reagents0.5_raw.csv") %>% 
  mutate(date = dmy_hms(date))

#create flag for zeroing
#flag only when values are actually low

wip_dat3 = raw_dat3 %>% 
  mutate(zeroing = case_when(between(date,as.POSIXct("2023-02-07 09:15"),as.POSIXct("2023-02-07 09:45")) ~ 1,
                             between(date,as.POSIXct("2023-02-07 15:57"),as.POSIXct("2023-02-07 16:13")) ~ 1,
                             between(date,as.POSIXct("2023-02-07 21:58"),as.POSIXct("2023-02-07 22:15")) ~ 1,
                             between(date,as.POSIXct("2023-02-08 03:55"),as.POSIXct("2023-02-08 04:15")) ~ 1,
                             between(date,as.POSIXct("2023-02-08 15:14"),as.POSIXct("2023-02-08 15:31")) ~ 1,
                             between(date,as.POSIXct("2023-02-08 23:38"),as.POSIXct("2023-02-08 23:50")) ~ 1,
                             between(date,as.POSIXct("2023-02-09 05:40"),as.POSIXct("2023-02-09 05:55")) ~ 1,
                             TRUE ~ 0),
         #create columns with only zero values
         ch1_zeroes = ifelse(zeroing == 1, ch1,NA_real_),
         ch2_zeroes = ifelse(zeroing == 1, ch2,NA_real_),
         #interpolate between zeroes
         ch1_zeroes = na.approx(ch1_zeroes,na.rm = F),
         ch2_zeroes = na.approx(ch2_zeroes,na.rm = F)) %>% 
  #fill the values before first zero has been measured
  fill(ch1_zeroes,ch2_zeroes,.direction = "updown")

#apply zeroes and cal correction

#1 for water/abs closed
#2 for zero
#3 for cal
#4 for air in abs
#5 other

final_dat3 = wip_dat3 %>% 
  mutate(date = date - date_corr3,
         ch1_zeroed = ch1 - ch1_zeroes,
         ch2_zeroed = ch2 - ch2_zeroes,
         ch1_ppt = ch1_zeroed * cal0.5_ch1,
         ch2_ppt = ch2_zeroed * cal0.5_ch2,
         hono = ppt(ch1_ppt,ch2_ppt,se),
         reagents = 0.5,
         flag = case_when(date < "2023-02-07 10:00" ~ 5,#not sure why, just looks weird,zero in there somewhere
                          between(date,as.POSIXct("2023-02-07 15:20"),as.POSIXct("2023-02-07 16:05")) ~ 2,
                          between(date,as.POSIXct("2023-02-07 21:20"),as.POSIXct("2023-02-07 22:25")) ~ 2,
                          between(date,as.POSIXct("2023-02-08 03:00"),as.POSIXct("2023-02-08 04:45")) ~ 5,#zero somewhere in here, also insturmental noise spikes
                          between(date,as.POSIXct("2023-02-08 09:00"),as.POSIXct("2023-02-08 10:15")) ~ 5,#instrumental noise spike weridness, as well as zero
                          between(date,as.POSIXct("2023-02-08 12:30"),as.POSIXct("2023-02-08 14:00")) ~ 4,
                          between(date,as.POSIXct("2023-02-08 14:30"),as.POSIXct("2023-02-08 15:45")) ~ 2,#changed zero value
                          between(date,as.POSIXct("2023-02-08 16:45"),as.POSIXct("2023-02-08 17:45")) ~ 2,
                          between(date,as.POSIXct("2023-02-08 23:00"),as.POSIXct("2023-02-09 00:30")) ~ 2,#zeros and instrumental noise spikes
                          between(date,as.POSIXct("2023-02-09 04:45"),as.POSIXct("2023-02-09 05:45")) ~ 2,
                          date > "2023-02-09 09:30" ~ 5,
                          TRUE ~ 0))

# Despiking data -------------------------------------

hono_dat = bind_rows(final_dat3,final_dat1,final_dat2,final_dat_night) %>% 
  arrange(date)

#using despike function (import oce library) to automatically find and remove instrumental noise spikes
despiking = hono_dat %>% 
  mutate(flagged_hono = ifelse(flag == 0, hono,NA_real_), #removing flagged data
         despiked_hono = despike(flagged_hono, reference = "median",n = 3, k = 19, replace = "NA"), #despiking
         instrumental_noise_flag = ifelse(is.na(flagged_hono-despiked_hono) & flag == 0,1,0)) #flag that is one only with instrumental noise spikes, ignores NAs from previous flag

#despiking only removes peak of spike, not rise, so we need to remove rows before and after spike has been removed
#creating df of rows where there's no instrumental noise flag because these are the rows we want to remove data from
no_noise = rle(despiking$instrumental_noise_flag) %>%
  tidy_rle() %>% 
  filter(values == 0) %>% 
  mutate(id = 1:nrow(.)) %>% 
  as.list() %>% 
  purrr::pmap_df(~data.frame(idx = ..3:..4,id = ..5)) %>% #df with row numbers and corresponding groups
  tibble()

despiked_dat = despiking %>% 
  mutate(idx = 1:nrow(.)) %>% 
  left_join(no_noise, "idx") %>% #joins two dfs by their row number
  select(-idx) %>% 
  mutate(id = ifelse(is.na(id), 0, id),
         idx_2 = 1:nrow(.)) %>% #makes id (group) = 0 when there are instrumental noise spikes
  group_by(id) %>% 
  mutate(idx = 1:n(), #numbers each row based on group
         instrumental_noise_flag = ifelse(idx < 5, NA_real_, instrumental_noise_flag), #removes first x values of each group
         idx = n():1,
         instrumental_noise_flag = ifelse(idx < 7, NA_real_,instrumental_noise_flag)) #removes last x values of group 
  

despiked_dat %>% 
  # filter(date < "2023-02-07 18:00" & date > "2023-02-07 12:00") %>% 
  mutate(flagged_hono = ifelse(instrumental_noise_flag == 0,flagged_hono,NA_real_)) %>%
  timeAverage("15 min") %>% 
  ggplot(aes(date,flagged_hono)) +
  scale_x_datetime(date_breaks = "1 day",date_labels = "%d/%m") +
  geom_path()


# Error analysis ----------------------------------------------------------

dat1 = despiked_dat %>% 
  ungroup() %>% 
  mutate(hono = case_when(flag != 0 ~ NA_real_,
                          instrumental_noise_flag != 0 ~ NA_real_,
                          TRUE ~ hono),
         flag = case_when(instrumental_noise_flag == 1 ~ 6, TRUE ~ flag)) %>%
  select(date,ch1,ch2,zeroing,hono,reagents,flag)

zero = rle(dat1$zeroing) %>%
  tidy_rle() %>% 
  filter(values == 1) %>% 
  mutate(id = 1:nrow(.)) %>% 
  as.list() %>% 
  purrr::pmap_df(~data.frame(idx = ..3:..4,id = ..5)) %>% #df with row numbers and corresponding groups
  tibble() 

errors = dat1 %>% 
  mutate(idx = 1:nrow(.)) %>% 
  left_join(zero, "idx") %>% #joins two dfs by their row number
  mutate(id = ifelse(is.na(id), 0, id)) %>%  #makes id (group) = 0 when not zeroing (24 zeroes)
  filter(id != 0) %>% 
  group_by(id) %>% 
  summarise(sd1 = sd(ch1),
            sd2 = sd(ch2),
            idx = median(idx),
            idx = round(idx))

dat_errors = dat1 %>% 
  mutate(idx = 1:nrow(.)) %>% 
  left_join(errors, "idx") %>% 
  mutate(sd1 = case_when(reagents == 0.5 ~ 2* sd1 * cal0.5_ch1,
                         reagents == 1 ~ 2* sd1 * cal1_ch1,
                         reagents >= 2 ~ 2* sd1 * cal2_ch1),
         sd2 = case_when(reagents == 0.5 ~ 2* sd2 * cal0.5_ch1,
                         reagents == 1 ~ 2* sd2 * cal1_ch1,
                         reagents >= 2 ~ 2* sd2 * cal2_ch1),
         lod = (sd1^2 + sd2^2)^0.5,
         lod = na.approx(lod,na.rm = F),
         error = hono*0.1+lod) %>% 
  fill(lod,.direction = "updown")

dat_errors %>% 
  # filter(reagents == 1) %>%
  # pivot_longer(c(lod,error,hono)) %>% 
  # timeAverage("1 hour") %>% 
  # mutate(reagents = as.character(reagents)) %>%
  filter(reagents == 0.5) %>% 
  ggplot(aes(date,hono)) +
  # facet_grid(rows = vars(name),scales = "free_y") +
  geom_path() +
  geom_ribbon(aes(ymin = hono - error,ymax = hono + error),alpha = 0.1)
  scale_color_viridis(discrete = "TRUE")

# Saving fully processed data ---------------------------------------------

#flag = 6 when there's a spike due to instrumental noise
#flag = 7 to indicate data is low quality - due to no proper zeroes available
processed_dat = dat_errors %>% 
  ungroup() %>% 
  mutate(low_quality_flag = case_when(between(date,as.POSIXct("2023-02-19"),as.POSIXct("2023-02-21")) ~ 1, #not properly zeroed
                                      date > "2023-02-24 08:41" ~ 1, #not properly zeroed
                                      date < "2023-02-09 12:00" ~ 1, #not properly calibrated
                                      TRUE ~ 0),
         date = date + 3600) %>% #data in UTC
  # timeAverage("5 min") %>%
  select(date,hono,error,low_quality_flag) %>% 
  mutate(low_quality_flag = ifelse(low_quality_flag == 0, low_quality_flag,1))

processed_dat %>%
  timeAverage("5 min") %>% 
  pivot_longer(c(hono,error)) %>% 
  ggplot(aes(date,value,col = low_quality_flag)) +
  geom_path() +
  facet_grid(rows = vars(name))

ggsave('processing_all_cal2.png',
       path = "output/plots",
       width = 30,
       height = 12,
       units = 'cm')

write.csv(processed_dat,"output/data/processed_in_r4.csv",row.names = FALSE)

# What cal values to use for the first few days of data? ------------------

#deragned plotting of data processed using the calibration values from the weird calibrations
#and the values for cal 1 

processed_dat1 = processed_dat %>% 
  select(date,hono_og = hono)

processed_dat2 = processed_dat %>% 
  rename(hono_all_cal = hono)


processed_dat = left_join(processed_dat2,processed_dat1)

processed_dat %>% 
  # filter(date < "2023-02-09 12:00") %>% 
  rename(hono_cal2 = hono_og,
         hono_cal1 = hono_all_cal) %>% 
  pivot_longer(c(hono_cal2,hono_cal1)) %>% 
  ggplot(aes(date,value,col = low_quality_flag)) +
  geom_path() +
  facet_grid(rows = vars(name))
