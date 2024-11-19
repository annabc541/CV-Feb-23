library(tidyverse)
library(lubridate)
library(zoo)
library(oce)
library(openair)

Sys.setenv(TZ = "UTC")

#creating plots to show issues with zero measurements

# Functions ---------------------------------------------------------------

tidy_rle = function(rleObj){
  
  require(dplyr)
  
  rleDf = tibble(lengths = rleObj$lengths,
                 values = rleObj$values) %>% 
    mutate(idxStart = cumsum(c(1,lengths))[1:(nrow(.))],
           idxEnd = cumsum(lengths))
  
  #
  rleDf
}

# Reading in raw data -----------------------------------------------------

raw_dat1 = read.csv("data/raw_data/reagents1.csv") %>% 
  tibble() %>% 
  mutate(date = glue::glue("{Date} {Time}"),
         date = ymd_hms(date)) %>% 
  arrange(date) %>% 
  select(date,ch1 = X550.6,ch2 = X550)

raw_dat2 = read.csv("data/raw_data/reagents2.csv") %>% 
  mutate(date = glue::glue("{Date} {Time}"),
         date = ymd_hms(date)) %>% 
  arrange(date) %>% 
  select(date,ch1 = X550.6,ch2 = X550)

#data without a proper cal
raw_dat3 = read.csv("data/raw_data/reagents0.5_raw.csv") %>% 
  mutate(date = dmy_hms(date))

# Getting zero air zeroes 1 -------------------------------------------------

#create flag for zeroing
#flag only when values are actually low

zero_flag = raw_dat1 %>% 
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
                             TRUE ~ 0))

#creates a group for each zero and maintains row no. of main df so can easily left_join
zeroing = rle(zero_flag$zeroing) %>%
  tidy_rle() %>% 
  filter(values == 1) %>% 
  mutate(id = 1:nrow(.)) %>% 
  as.list() %>% 
  purrr::pmap_df(~data.frame(idx = ..3:..4,id = ..5)) %>% 
  tibble() 

#join dfs with groups for each zero
zeroes_grouped = zero_flag %>% 
  mutate(idx = 1:nrow(.)) %>% 
  left_join(zeroing, "idx") %>% #joins two dfs by their row number
  mutate(id = ifelse(is.na(id), 0, id)) #makes id (group) = 0 when not zeroing

#average zero value for each group
zero_avg = zeroes_grouped %>% 
  filter(id != 0) %>%
  group_by(id) %>% 
  summarise(ch1_zeroes = mean(ch1),
            ch2_zeroes = mean(ch2),
            idx = mean(idx)) %>% 
  ungroup() %>% 
  mutate(idx = round(idx))

#interpolate between zeroes and subtract zeroes from measurements
zeroed1 = zero_flag %>% 
  mutate(idx = 1:nrow(.)) %>% 
  left_join(zero_avg)
  # mutate(ch1_zeroes = na.approx(ch1_zeroes,na.rm = F),
  #        ch2_zeroes = na.approx(ch2_zeroes,na.rm = F)) %>% 
  # fill(ch1_zeroes,ch2_zeroes,.direction = "up") %>%
  # mutate(ch1_zeroed = ch1 - ch1_zeroes,
  #        ch2_zeroed = ch2 - ch2_zeroes)


# Getting zero air zeroes 2 -----------------------------------------------

zero_flag = raw_dat2 %>% 
  mutate(zeroing = case_when(between(date,as.POSIXct("2023-02-17 18:15"),as.POSIXct("2023-02-17 18:24")) ~ 1,
                             between(date,as.POSIXct("2023-02-18 00:29"),as.POSIXct("2023-02-18 00:39")) ~ 1,
                             between(date,as.POSIXct("2023-02-18 09:50"),as.POSIXct("2023-02-18 10:35")) ~ 1,
                             between(date,as.POSIXct("2023-02-18 16:41"),as.POSIXct("2023-02-18 16:52")) ~ 1,
                             between(date,as.POSIXct("2023-02-19 17:02"),as.POSIXct("2023-02-19 17:13")) ~ 1, #lower than most, after power cut
                             between(date,as.POSIXct("2023-02-20 11:39"),as.POSIXct("2023-02-20 11:47")) ~ 1, #after power cut
                             between(date,as.POSIXct("2023-02-20 17:55"),as.POSIXct("2023-02-20 18:04")) ~ 1, #after power cut
                             between(date,as.POSIXct("2023-02-21 09:31"),as.POSIXct("2023-02-21 09:44")) ~ 1,
                             between(date,as.POSIXct("2023-02-21 17:15"),as.POSIXct("2023-02-21 17:40")) ~ 1,#postcal
                             between(date,as.POSIXct("2023-02-21 23:47:30"),as.POSIXct("2023-02-21 23:59")) ~ 1,
                             between(date,as.POSIXct("2023-02-22 06:02"),as.POSIXct("2023-02-22 06:15")) ~ 1,
                             between(date,as.POSIXct("2023-02-22 12:22"),as.POSIXct("2023-02-22 12:30:30")) ~ 1,
                             between(date,as.POSIXct("2023-02-22 18:39"),as.POSIXct("2023-02-22 18:47")) ~ 1,
                             between(date,as.POSIXct("2023-02-23 00:51"),as.POSIXct("2023-02-23 01:02")) ~ 1,
                             between(date,as.POSIXct("2023-02-23 07:07"),as.POSIXct("2023-02-23 07:18")) ~ 1,
                             between(date,as.POSIXct("2023-02-23 13:25"),as.POSIXct("2023-02-23 13:35")) ~ 1,
                             between(date,as.POSIXct("2023-02-23 19:42"),as.POSIXct("2023-02-23 19:52")) ~ 1,
                             between(date,as.POSIXct("2023-02-24 01:57"),as.POSIXct("2023-02-24 02:05")) ~ 1,
                             between(date,as.POSIXct("2023-02-24 08:11"),as.POSIXct("2023-02-24 08:22")) ~ 1, #cleaned inlet and messed around with liquid pump after this zero
                             between(date,as.POSIXct("2023-02-24 14:32"),as.POSIXct("2023-02-24 14:38")) ~ 1,
                             between(date,as.POSIXct("2023-02-24 20:40"),as.POSIXct("2023-02-24 20:54")) ~ 1,
                             between(date,as.POSIXct("2023-02-25 02:57"),as.POSIXct("2023-02-25 03:06")) ~ 1,
                             between(date,as.POSIXct("2023-02-25 09:10"),as.POSIXct("2023-02-25 09:16")) ~ 1,
                             between(date,as.POSIXct("2023-02-26 16:32"),as.POSIXct("2023-02-26 16:45")) ~ 1,
                             TRUE ~ 0))

#creates a group for each zero and maintains row no. of main df so can easily left_join
zeroing = rle(zero_flag$zeroing) %>%
  tidy_rle() %>% 
  filter(values == 1) %>% 
  mutate(id = 1:nrow(.)) %>% 
  as.list() %>% 
  purrr::pmap_df(~data.frame(idx = ..3:..4,id = ..5)) %>% 
  tibble() 

#join dfs with groups for each zero
zeroes_grouped = zero_flag %>% 
  mutate(idx = 1:nrow(.)) %>% 
  left_join(zeroing, "idx") %>% #joins two dfs by their row number
  mutate(id = ifelse(is.na(id), 0, id)) #makes id (group) = 0 when not zeroing

#average zero value for each group
zero_avg = zeroes_grouped %>% 
  filter(id != 0) %>%
  group_by(id) %>% 
  summarise(ch1_zeroes = mean(ch1),
            ch2_zeroes = mean(ch2),
            idx = mean(idx)) %>% 
  ungroup() %>% 
  mutate(idx = round(idx))

#interpolate between zeroes and subtract zeroes from measurements
zeroed2 = zero_flag %>% 
  mutate(idx = 1:nrow(.)) %>% 
  left_join(zero_avg) %>% 
  mutate(ch1_zeroes = na.approx(ch1_zeroes,na.rm = F),
         ch2_zeroes = na.approx(ch2_zeroes,na.rm = F)) %>%
  fill(ch1_zeroes,ch2_zeroes,.direction = "up") %>%
  mutate(ch1_zeroed = ch1 - ch1_zeroes,
         ch2_zeroed = ch2 - ch2_zeroes,
         date = date - time_corr)

# Nighttime zeroes --------------------------------------------------------

time_corr = 23.67 * 60

night_zeroing = raw_dat2 %>%
  filter(date > "2023-02-21") %>% #data after power cut
  mutate(time = hour(date),
         date = date - time_corr, #so that flagging is applied properly
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
         date = date + time_corr,
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
night_avg[nrow(night_avg) + 1,] = list(5.5,.05299002,0.04949119,14299,0.05299002,0.04949119)
night_avg = night_avg %>% arrange(idx)

night_zeroed = night_zeroing %>% 
  mutate(idx = 1:nrow(.)) %>% 
  left_join(night_avg) %>% 
  mutate(ch1_zeroes = na.approx(ch1_inter,na.rm = F),
         ch2_zeroes = na.approx(ch2_inter,na.rm = F)) %>%
  fill(ch1_zeroes,ch2_zeroes,.direction = "up") %>% 
  mutate(ch1_zeroed_night = ifelse(date < "2023-02-24 08:41",ch1 - ch1_zeroes,ch1 - 0.04973971),
         ch2_zeroed_night = ifelse(date < "2023-02-24 08:41",ch2 - ch2_zeroes,ch2 - 0.04323023),
         measuring_conditions = "Reagents 2, nighttime zeroes")

zeroed = night_zeroed %>% 
  select(date,ch1_zeroed_night,ch2_zeroed_night) %>% 
  left_join(zeroed2,by = "date") %>% 
  select(date:ch2_zeroed_night,ch1_zeroed,ch2_zeroed)

# Plots -------------------------------------------------------------------

night_zeroed_plots = night_zeroed %>% 
  select(date,ch1_night,ch2_night)

zero_plots_thesis = zeroed1 %>% 
  bind_rows(zeroed2) %>% 
  arrange(date) %>% 
  mutate(zeroes_used = ifelse(date < "2023-02-19","ZA values","Nighttime values"),
         reagent_batch = ifelse(date < "2023-02-17 09:00","First batch of reagents","Second batch of reagents")) %>% 
  select(date:zeroing,ch1_zeroes,ch2_zeroes,reagent_batch,zeroes_used) %>% 
  left_join(night_zeroed_plots)

zero_plots_thesis %>% 
  # filter(zeroing == 1) %>%
  rename(`Channel 1` = ch1_zeroes,
         `Channel 2` = ch2_zeroes) %>% 
  pivot_longer(c(`Channel 1`,`Channel 2`)) %>% 
  ggplot(aes(date,value,col = zeroes_used)) +
  theme_bw() +
  labs(x = NULL,
       y = "Zeroes absorbance",
       col = "Zeroing with") +
  scale_colour_manual(values = c("steelblue1","darkorange")) +
  theme(legend.position = "top",
        text = element_text(size = 16)) +
  geom_vline(xintercept = as.POSIXct("2023-02-19"),col = "red",linewidth = 1) +
  geom_point() +
  facet_grid(rows = vars(name),cols = vars(reagent_batch),scales = "free")

# ggsave('lopap_za_zeroes_feb23.png',
#        path = "~/Writing/Thesis/Chapter 4 (HONO in CVAO)/Images",
#        width = 29,
#        height = 12,
#        units = 'cm')

night_zeroed_plots %>% 
  pivot_longer(c(ch1_night,ch2_night)) %>% 
  ggplot(aes(date,value,col = name)) +
  geom_point()
