library(tidyverse)
library(lubridate)
library(zoo)
library(oce)
library(openair)

#just checking how much the HONO values would change if the internal mass flow controller was off by
#a lot (like 2 or 0.5 as opposed to 1.05)
#cal seems to be the main place where there are differences, the shap remains the same, it's just the size
#of the diurnal peak that could change, but even if the mass flow calibration shows that it's much higher
#than the display value (double) it will still be around 5ppt (and if it's much lower than the display
#value it will be around 20/30ppt which is not really likely, so feeling a bit less stressed about this)

Sys.setenv(TZ = "UTC")

cal_ch1 = 566.46
cal_ch2 = 589.64
se1 = 0.999

#values for upper and lower from the reagents1_pom excel file, changed flowcontroller calib. to be 2 and 30
#for upper and 0.5 and 7.5 for lower
cal_upper_ch1 = 298.71
cal_upper_ch2 = 310.94
se_upper = 0.981

cal_lower_ch1 = 1194.84
cal_lower_ch2 = 1243.74
se_lower = 1

date_corr1 = 22.75 * 60

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


# Normal ------------------------------------------------------------------

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
         ch1_ppt = ch1_zeroed * cal_ch1,
         ch2_ppt = ch2_zeroed * cal_ch2,
         hono = ppt(ch1_ppt,ch2_ppt,se1),
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

final_dat1 %>%
  # filter(flag == 0) %>%
  mutate(hono = ifelse(flag == 0,hono,NA_real_)) %>%
  ggplot(aes(date,hono)) +
  # facet_grid(rows = vars(name),scales = "free_y") +
  geom_path() +
  # scale_x_datetime(date_breaks = "1 day",date_labels = "%b %d") +
  # scale_color_taylor(palette = "taylorRed") +
  # labs(y = "Channel 2 abs",
  #      x = NULL)
  NULL

despiking = final_dat1 %>% 
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

# Upper -------------------------------------------------------------------

#data when the flow controller calibration has been set at 2 rather than 1.05

final_dat_upper = wip_dat1 %>% 
  mutate(date = date - date_corr1,
         ch1_zeroed = ch1 - ch1_zeroes,
         ch2_zeroed = ch2 - ch2_zeroes,
         ch1_ppt = ch1_zeroed * cal_upper_ch1,
         ch2_ppt = ch2_zeroed * cal_upper_ch2,
         hono = ppt(ch1_ppt,ch2_ppt,se_upper),
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

final_dat_upper %>%
  # filter(flag == 0) %>%
  mutate(hono = ifelse(flag == 0,hono,NA_real_)) %>%
  ggplot(aes(date,hono)) +
  # facet_grid(rows = vars(name),scales = "free_y") +
  geom_path() +
  # scale_x_datetime(date_breaks = "1 day",date_labels = "%b %d") +
  # scale_color_taylor(palette = "taylorRed") +
  # labs(y = "Channel 2 abs",
  #      x = NULL)
  NULL

despiking_upper = final_dat_upper %>% 
  mutate(flagged_hono = ifelse(flag == 0, hono,NA_real_), #removing flagged data
         despiked_hono = despike(flagged_hono, reference = "median",n = 3, k = 19, replace = "NA"), #despiking
         instrumental_noise_flag = ifelse(is.na(flagged_hono-despiked_hono) & flag == 0,1,0)) #flag that is one only with instrumental noise spikes, ignores NAs from previous flag

#despiking only removes peak of spike, not rise, so we need to remove rows before and after spike has been removed
#creating df of rows where there's no instrumental noise flag because these are the rows we want to remove data from
no_noise_upper = rle(despiking$instrumental_noise_flag) %>%
  tidy_rle() %>% 
  filter(values == 0) %>% 
  mutate(id = 1:nrow(.)) %>% 
  as.list() %>% 
  purrr::pmap_df(~data.frame(idx = ..3:..4,id = ..5)) %>% #df with row numbers and corresponding groups
  tibble()

despiked_dat_upper = despiking_upper %>% 
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


despiked_dat_upper %>% 
  # filter(date < "2023-02-07 18:00" & date > "2023-02-07 12:00") %>% 
  mutate(flagged_hono = ifelse(instrumental_noise_flag == 0,flagged_hono,NA_real_)) %>%
  timeAverage("15 min") %>% 
  ggplot(aes(date,flagged_hono)) +
  scale_x_datetime(date_breaks = "1 day",date_labels = "%d/%m") +
  geom_path()

# Lower -------------------------------------------------------------------

#data when the flow controller calibration has been set at 0.5 rather than 1.05

final_dat_lower = wip_dat1 %>% 
  mutate(date = date - date_corr1,
         ch1_zeroed = ch1 - ch1_zeroes,
         ch2_zeroed = ch2 - ch2_zeroes,
         ch1_ppt = ch1_zeroed * cal_lower_ch1,
         ch2_ppt = ch2_zeroed * cal_lower_ch2,
         hono = ppt(ch1_ppt,ch2_ppt,se_lower),
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

final_dat_lower %>%
  # filter(flag == 0) %>%
  mutate(hono = ifelse(flag == 0,hono,NA_real_)) %>%
  ggplot(aes(date,hono)) +
  # facet_grid(rows = vars(name),scales = "free_y") +
  geom_path() +
  # scale_x_datetime(date_breaks = "1 day",date_labels = "%b %d") +
  # scale_color_taylor(palette = "taylorRed") +
  # labs(y = "Channel 2 abs",
  #      x = NULL)
  NULL

despiking_lower = final_dat_lower %>% 
  mutate(flagged_hono = ifelse(flag == 0, hono,NA_real_), #removing flagged data
         despiked_hono = despike(flagged_hono, reference = "median",n = 3, k = 19, replace = "NA"), #despiking
         instrumental_noise_flag = ifelse(is.na(flagged_hono-despiked_hono) & flag == 0,1,0)) #flag that is one only with instrumental noise spikes, ignores NAs from previous flag

#despiking only removes peak of spike, not rise, so we need to remove rows before and after spike has been removed
#creating df of rows where there's no instrumental noise flag because these are the rows we want to remove data from
no_noise_lower = rle(despiking$instrumental_noise_flag) %>%
  tidy_rle() %>% 
  filter(values == 0) %>% 
  mutate(id = 1:nrow(.)) %>% 
  as.list() %>% 
  purrr::pmap_df(~data.frame(idx = ..3:..4,id = ..5)) %>% #df with row numbers and corresponding groups
  tibble()

despiked_dat_lower = despiking_lower %>% 
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


despiked_dat_lower %>% 
  # filter(date < "2023-02-07 18:00" & date > "2023-02-07 12:00") %>% 
  mutate(flagged_hono = ifelse(instrumental_noise_flag == 0,flagged_hono,NA_real_)) %>%
  timeAverage("15 min") %>% 
  ggplot(aes(date,flagged_hono)) +
  scale_x_datetime(date_breaks = "1 day",date_labels = "%d/%m") +
  geom_path()


# Joining and plotting together -------------------------------------------

normal = despiked_dat %>% ungroup() %>% 
  select(date,normal_hono = flagged_hono,instrumental_noise_flag)

upper = despiked_dat_upper %>% ungroup() %>% 
  select(date,upper_hono = flagged_hono)

lower = despiked_dat_lower %>% ungroup() %>% 
  select(date,lower_hono = flagged_hono)

df_list = list(normal,upper,lower)
comparison = df_list %>% reduce(full_join,by = "date")

comparison %>% 
  # filter(date < "2023-02-07 18:00" & date > "2023-02-07 12:00") %>% 
  mutate(normal_hono = ifelse(instrumental_noise_flag == 0,normal_hono,NA_real_),
         lower_hono = ifelse(instrumental_noise_flag == 0,lower_hono,NA_real_),
         upper_hono = ifelse(instrumental_noise_flag == 0,upper_hono,NA_real_)) %>%
  timeAverage("15 min") %>% 
  pivot_longer(c(normal_hono,upper_hono,lower_hono)) %>% 
  ggplot(aes(date,value,col = name)) +
  scale_x_datetime(date_breaks = "1 day",date_labels = "%d/%m") +
  geom_path() +
  scale_color_viridis_d()
