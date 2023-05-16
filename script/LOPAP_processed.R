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
#NB separate spreadsheet has been made just for the data to be read in, not getting it from processing excel doc
dat1 = read.csv("dat_cal1.csv",header=TRUE,na.strings= c('NA','missing')) %>%
  tibble() %>% 
  mutate(date = waclr::parse_excel_date(date))

dat2 = read.csv("dat_cal2.csv",header=TRUE,na.strings= c('NA','missing')) %>%
  tibble() %>% 
  mutate(date = waclr::parse_excel_date(date)) %>% 
  filter(date > "2023-02-17 08:50")

dat = rbind(dat1,dat2)


# Flagging data -----------------------------------------------------------

#plotting to see exactly where zeroes/problem periods beginning and end

dat %>% 
  mutate(doy = yday(date)) %>%
  filter(
    doy == 57,
         # date > "2023-02-26 15:45" &
           # date < "2023-02-26 16:45"
         ) %>%
  ggplot(aes(date,hono)) +
  geom_path() +
  # geom_errorbar(aes(ymin=hono-err,ymax=hono+err),col = "red") +
  theme_bw() +
  # scale_x_datetime(date_breaks = "5 min",date_labels = "%H:%M") +
  NULL

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

# Removing instrumental noise ---------------------------------------------

#using despike function (import oce library) to automatically find and remove instrumental noise spikes
despiking = flagged_dat %>% 
  mutate(hono = ifelse(flag == 0, hono,NA_real_), #removing flagged data
         despiked_hono = despike(hono, reference = "median",n = 3, k = 19, replace = "NA"), #despiking
         instrumental_noise_flag = ifelse(is.na(hono-despiked_hono) & flag == 0,1,0)) #flag that is one only with instrumental noise spikes, ignores NAs from previous flag

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
         despiked_hono2 = ifelse(idx < 5, NA_real_, despiked_hono), #removes first x values of each group
         idx = n():1,
         despiked_hono3 = ifelse(idx < 7, NA_real_,despiked_hono2)) #removes last x values of group 

processed_dat = despiked_dat %>% 
  ungroup() %>% 
  select(date,zero,water,cal,air,flag,HONO = despiked_hono3)


processed_dat %>% 
  # timeAverage("1 hour") %>% 
  mutate(doy = yday(date)) %>% 
  # filter(
    # doy != 57,
  #   date > "2023-02-17 03:22" &
  #     date < "2023-02-17 04:15"
  # ) %>%
  ggplot(aes(date,HONO)) +
  geom_path() +
  # geom_errorbar(aes(ymin=hono-err,ymax=hono+err),col = "red") +
  theme_bw() +
  labs(x = "Date",
       y = "HONO / ppt") +
  # scale_x_datetime(date_breaks = "1 days",
  #                  date_labels = "%d/%m/%y"
  # ) +
  NULL


# Testing code - don't wanna remove because I might need it ---------------


# #creating test df, substituting all flagged values with NAs (not just dropping flagged values - win length wouldn't work well)
# test = flagged_dat %>% 
#   mutate(ch1 = ifelse(flag == 0, ch1,NA_real_),
#          ch2 = ifelse(flag == 0, ch2,NA_real_),
#          hono = ifelse(flag == 0, hono,NA_real_)) %>% 
#   select(date,ch1,ch2,hono,flag)
# 
# #using test df and different window lengths (k) to see which has the best filtering
# test1 = test %>% 
#   mutate(despiked25 = despike(hono, reference = "median",n = 3, k = 25, replace = "NA"),
#          despiked21 = despike(hono, reference = "median",n = 3, k = 21, replace = "NA"),
#          despiked23 = despike(hono, reference = "median",n = 3, k = 23, replace = "NA"),
#          despiked27 = despike(hono, reference = "median",n = 3, k = 27, replace = "NA"),
#          despiked29 = despike(hono, reference = "median",n = 3, k = 29, replace = "NA"),
#          despiked19 = despike(hono, reference = "median",n = 3, k = 19, replace = "NA"), #best filtering
#          despiked31 = despike(hono, reference = "median",n = 3, k = 31, replace = "NA"))
# 
# #plotting up despiked df and normal df to compare
# test1 %>% 
#   mutate(doy = yday(date)) %>% 
#   filter(doy == 48) %>%
#   timeAverage(avg.time = "5 min") %>% 
#   pivot_longer(c(hono,despiked19)) %>%
#   ggplot(aes(date,value)) +
#   # ggplot(aes(date,despiked19)) +
#   facet_grid(rows = vars(name),scales = "free_y")+
#   geom_path()
# 
# #changes what colours are in front and what colours are behind in your plot!
# last_plot() + aes(group=rev(name))
# 
# #saving plots of the process for later presentations/reference
# ggsave('comparing_spikes.png',
#        path = '~/CV Feb 23/plots',
#        width = 31,
#        height = 15,
#        units = 'cm')
# 
# # #checking outcome of despiking exploits
# despiked_dat %>%
#   mutate(doy = yday(date)) %>%
#   filter(doy == 44) %>%
#   pivot_longer(c(hono,despiked_hono,despiked_hono2,despiked_hono3)) %>%
#   ggplot() +
#   # geom_point(aes(date,instrumental_noise_flag)) +
#   geom_path(aes(date,value)) +
#   facet_grid(rows = vars(name))
# 
# 
# #checking that my instrumental noise flag did what I wanted it to do
# despiking %>%
#   mutate(doy = yday(date)) %>% 
#   filter(doy == 47) %>%
#   # pivot_longer(c(instrumental_noise_flag,ch1)) %>% 
#   ggplot() +
#   geom_point(aes(date,instrumental_noise_flag)) +
#   geom_path(aes(date,hono))
