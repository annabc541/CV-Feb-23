library(tidyverse)
library(lubridate)
library(zoo)
library(oce)
library(openair)

Sys.setenv(TZ = "UTC")

#compare final data zeroed with za and nighttime values for second batch of reagents

# Functions ----------------------------------------------------------------

tidy_rle = function(rleObj){
  
  require(dplyr)
  
  rleDf = tibble(lengths = rleObj$lengths,
                 values = rleObj$values) %>% 
    mutate(idxStart = cumsum(c(1,lengths))[1:(nrow(.))],
           idxEnd = cumsum(lengths))
  
  #
  rleDf
}

ppt <- function(ch1,ch2,se){
  
  x = (ch1/se) - (ch2-ch1*(1-se))
  return(x)
}

lod <- function(ch1_zero_3sd,ch2_zero_3sd,slope_cal1,slope_cal2){
  
  x = ((ch1_zero_3sd*slope_cal1)^2+(ch2_zero_3sd*slope_cal2)^2)^0.5
  return(x)
}

# Gas flow ---------------------------------------------------------

#calibrate internal lopap mfc with external flow meter
#use values to correct gas flow
mfc_cal = read.csv("data/raw_data/mfc.csv")
model_mfc_cal = lm(measured ~ set,mfc_cal)
intercept_mfc = summary(model_mfc_cal)$coefficients[1,1]
slope_mfc = summary(model_mfc_cal)$coefficients[2,1]
gas_flow_set = 1000
actual_gas_flow = gas_flow_set*slope_mfc + intercept_mfc
#values from sampling efficiency controlled by pure hono source
#can change this when testing with pure hono source in june
sampling_efficiency = 1 - exp(-7768.943*1/actual_gas_flow-0.116560784)

remove(mfc_cal,model_mfc_cal,intercept_mfc,slope_mfc,gas_flow_set)

# Reading in data ---------------------------------------------------------

raw_dat2 = read.csv("data/raw_data/reagents2.csv") %>% 
  mutate(date = glue::glue("{Date} {Time}"),
         date = ymd_hms(date)) %>% 
  arrange(date) %>% 
  select(date,ch1 = X550.6,ch2 = X550)


# Zeroes r2 (za) ------------------------------------------------------------------

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
            ch1_zero_3sd = 3 * sd(ch1),
            ch2_zero_3sd = 3 * sd(ch2),
            idx = mean(idx)) %>% 
  ungroup() %>% 
  mutate(idx = round(idx))

#interpolate between zeroes and subtract zeroes from measurements
za_zeroed = zero_flag %>% 
  mutate(idx = 1:nrow(.)) %>% 
  left_join(zero_avg) %>% 
  mutate(ch1_zeroes = na.approx(ch1_zeroes,na.rm = F),
         ch2_zeroes = na.approx(ch2_zeroes,na.rm = F)) %>% 
  fill(ch1_zeroes,ch2_zeroes,.direction = "up") %>% 
  fill(ch1_zero_3sd,ch2_zero_3sd,.direction = "downup") %>% 
  mutate(ch1_zeroed = ch1 - ch1_zeroes,
         ch2_zeroed = ch2 - ch2_zeroes,
         measuring_conditions = "Reagents 2, ZA zeroes") 

remove(zeroing,zeroes_grouped,zero_flag,zero_avg)

# Calibration r2 (za) -------------------------------------------------------------

#cal parameters
time_corr = 23.67 * 60
liquid_flow1 = 5/27.10 
liquid_flow2 = 5/27.58
conc_cal = 1000/100000 #standard conc / dilution factor
hono_cal_conc_ch1 = conc_cal/1000 * liquid_flow1/1000 /46*6.022*10^23/(2.46*10^19* actual_gas_flow) * 10^12
hono_cal_conc_ch2 = conc_cal/1000 * liquid_flow2/1000 /46*6.022*10^23/(2.46*10^19* actual_gas_flow) * 10^12

# zeroed = bind_rows(za_zeroed,night_zeroed_sd)

cal_za = za_zeroed %>% 
  # select(date,ch1_zeroed,ch2_zeroed)  %>% 
  filter(date > "2023-02-21 15:35" & date < "2023-02-21 18:00") %>%
  mutate(cal_zero_ch1 = ifelse(between(date,as.POSIXct("2023-02-21 17:15"),as.POSIXct("2023-02-21 17:40")),ch1_zeroed,NA),
         cal_zero_ch2 = ifelse(between(date,as.POSIXct("2023-02-21 17:15"),as.POSIXct("2023-02-21 17:40")),ch2_zeroed,NA),
         cal_ch1 = ifelse(between(date,as.POSIXct("2023-02-21 16:20"),as.POSIXct("2023-02-21 16:34")),ch1_zeroed,NA),
         cal_ch2 = ifelse(between(date,as.POSIXct("2023-02-21 16:24"),as.POSIXct("2023-02-21 16:34")),ch2_zeroed,NA)) %>% 
  summarise(cal_zero_ch1 = mean(cal_zero_ch1,na.rm = T),
            cal_zero_ch2 = mean(cal_zero_ch2,na.rm = T),
            cal_ch1 = mean(cal_ch1,na.rm = T),
            cal_ch2 = mean(cal_ch2,na.rm = T))

cal_df1 = data.frame(y = c(0,hono_cal_conc_ch1),
                     x = c(cal_za$cal_zero_ch1,cal_za$cal_ch1))

model_cal1 = lm(y ~ x,cal_df1)
# intercept_mfc = summary(model_mfc_cal)$coefficients[1,1]
slope_cal1 = summary(model_cal1)$coefficients[2,1]

cal_df2 = data.frame(y = c(0,hono_cal_conc_ch2),
                     x = c(cal_za$cal_zero_ch2,cal_za$cal_ch2))

model_cal2 = lm(y ~ x,cal_df2)
# intercept_mfc = summary(model_mfc_cal)$coefficients[1,1]
slope_cal2 = summary(model_cal2)$coefficients[2,1]

dat2_calibrated_za = za_zeroed %>% 
  # select(-c(zeroing,id,idx,time,flag,night_flag)) %>% 
  mutate(ch1_ppt = ch1_zeroed * slope_cal1,
         ch2_ppt = ch2_zeroed * slope_cal2,
         hono = ppt(ch1_ppt,ch2_ppt,sampling_efficiency),
         # hono_lod = lod(ch1_zero_3sd,ch2_zero_3sd,slope_cal1,slope_cal2),
         # hono_err = abs(hono * rel_error/100 + hono_lod),
         date = date - time_corr,
         flag = (case_when(between(date,as.POSIXct("2023-02-17 08:30"),as.POSIXct("2023-02-17 18:12")) ~ 1, #changing reagents
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
                           between(date,as.POSIXct("2023-02-20 22:27"),as.POSIXct("2023-02-21 07:54")) ~ 4,
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
                           TRUE ~ 0)))

dat2_calibrated_za %>% 
  mutate(hono = ifelse(flag == 0,hono,NA_real_)) %>% 
  ggplot(aes(date,hono)) +
  geom_path()

remove(cal_za,cal_df1,cal_df2,model_cal1,model_cal2)

# Zeroes r2 (night) -------------------------------------------------------

time_corr = 23.67 * 60

night_zeroing = raw_dat2 %>%
  filter(date > "2023-02-19") %>% #data after power cut
  mutate(time = hour(date),
         date = date - time_corr, #so that flagging is applied properly
         flag = (case_when(between(date,as.POSIXct("2023-02-19 08:53"),as.POSIXct("2023-02-19 11:27")) ~ 5, #power cut, resetting everything after
                           between(date,as.POSIXct("2023-02-19 14:20"),as.POSIXct("2023-02-19 15:25")) ~ 5, #zero tube not connected at inlet, not proper zero
                           between(date,as.POSIXct("2023-02-19 16:25"),as.POSIXct("2023-02-19 17:15")) ~ 2,
                           between(date,as.POSIXct("2023-02-19 17:17"),as.POSIXct("2023-02-20 09:02")) ~ 4,
                           between(date,as.POSIXct("2023-02-20 10:54"),as.POSIXct("2023-02-20 11:50")) ~ 2,
                           between(date,as.POSIXct("2023-02-20 17:12"),as.POSIXct("2023-02-20 17:54")) ~ 2,
                           between(date,as.POSIXct("2023-02-20 22:27"),as.POSIXct("2023-02-21 07:54")) ~ 4,
                           between(date,as.POSIXct("2023-02-20 22:27"),as.POSIXct("2023-02-21 07:54")) ~ 4,
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
night_avg[nrow(night_avg) + 1,] = list(5.5,NA_real_,NA_real_,14299,0.05299002,0.04949119)
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

remove(nights,night_zeroing,night_flagged,night_avg)

# Calibration r2 (night) --------------------------------------------------

cal = night_zeroed %>% 
  # select(date,ch1_zeroed,ch2_zeroed)  %>% 
  filter(date > "2023-02-21 15:35" & date < "2023-02-21 18:00") %>%
  mutate(cal_zero_ch1 = ifelse(between(date,as.POSIXct("2023-02-21 17:15"),as.POSIXct("2023-02-21 17:40")),ch1_zeroed_night,NA),
         cal_zero_ch2 = ifelse(between(date,as.POSIXct("2023-02-21 17:15"),as.POSIXct("2023-02-21 17:40")),ch2_zeroed_night,NA),
         cal_ch1 = ifelse(between(date,as.POSIXct("2023-02-21 16:20"),as.POSIXct("2023-02-21 16:34")),ch1_zeroed_night,NA),
         cal_ch2 = ifelse(between(date,as.POSIXct("2023-02-21 16:24"),as.POSIXct("2023-02-21 16:34")),ch2_zeroed_night,NA)) %>% 
  summarise(cal_zero_ch1 = mean(cal_zero_ch1,na.rm = T),
            cal_zero_ch2 = mean(cal_zero_ch2,na.rm = T),
            cal_ch1 = mean(cal_ch1,na.rm = T),
            cal_ch2 = mean(cal_ch2,na.rm = T))

cal_df1 = data.frame(y = c(0,hono_cal_conc_ch1),
                     x = c(cal$cal_zero_ch1,cal$cal_ch1))

model_cal1 = lm(y ~ x,cal_df1)
# intercept_mfc = summary(model_mfc_cal)$coefficients[1,1]
slope_cal1 = summary(model_cal1)$coefficients[2,1]

cal_df2 = data.frame(y = c(0,hono_cal_conc_ch2),
                     x = c(cal$cal_zero_ch2,cal$cal_ch2))

model_cal2 = lm(y ~ x,cal_df2)
# intercept_mfc = summary(model_mfc_cal)$coefficients[1,1]
slope_cal2 = summary(model_cal2)$coefficients[2,1]

dat2_calibrated_night = night_zeroed %>% 
  # select(-c(zeroing,id,idx,time,flag,night_flag)) %>% 
  mutate(ch1_ppt_night = ch1_zeroed_night * slope_cal1,
         ch2_ppt_night = ch2_zeroed_night * slope_cal2,
         hono_night = ppt(ch1_ppt_night,ch2_ppt_night,sampling_efficiency),
         # hono_lod = lod(ch1_zero_3sd,ch2_zero_3sd,slope_cal1,slope_cal2),
         # hono_err = abs(hono * rel_error/100 + hono_lod),
         date = date - time_corr,
         flag = (case_when(between(date,as.POSIXct("2023-02-17 08:30"),as.POSIXct("2023-02-17 18:12")) ~ 1, #changing reagents
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
                           between(date,as.POSIXct("2023-02-20 22:27"),as.POSIXct("2023-02-21 07:54")) ~ 4,
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
                           TRUE ~ 0)))

dat2_calibrated_night %>% 
  mutate(hono_night = ifelse(flag == 0,hono_night,NA_real_)) %>% 
  ggplot(aes(date,hono_night)) +
  geom_path()

remove(cal)

# Comparing the two -------------------------------------------------------

za = dat2_calibrated_za %>% 
  mutate(hono = ifelse(flag == 0,hono,NA_real_)) %>% 
  select(date,hono)

night = dat2_calibrated_night %>% 
  mutate(hono_night = ifelse(flag == 0,hono_night,NA_real_)) %>% 
  select(date,hono_night)

dat = za %>% left_join(night,by = "date") %>% 
  bind_rows(processed_dat1,processed_dat3) %>% 
  arrange(date)

rects = data.frame(xstart = c("2023-02-07 07:38:05","2023-02-17 08:46:55"),
                   xend = c("2023-02-17 08:46:55","2023-02-26 17:35"),
                   cols = c("First batch of reagents","Second batch of reagents"))

dat %>% 
  timeAverage("5 min") %>%
  mutate(hono = ifelse(flag == 0 | is.na(flag) == T, hono,NA_real_)) %>% 
  rename(`ZA zeroes` = hono,
         `Night zeroes` = hono_night) %>% 
  pivot_longer(c(`ZA zeroes`,`Night zeroes`)) %>% 
  ggplot() +
  theme_bw() +
  labs(x = NULL,
       y = "HONO mixing ratio (ppt)",
       col = NULL,
       fill = NULL)+
  geom_path(aes(date,value,col = name),linewidth = 0.8) +
  geom_vline(xintercept = as.POSIXct("2023-02-19"),col = "red",linewidth = 1) +
  geom_rect(data = rects, aes(xmin = as.POSIXct(xstart), xmax = as.POSIXct(xend), ymin = -Inf, ymax = Inf, fill = cols), alpha = 0.1) +
  theme(legend.position = "top",
        text = element_text(size = 20)) +
  scale_x_datetime(date_breaks = "2 day",date_labels = "%b %d") +
  scale_colour_manual(values = c("steelblue1","darkorange")) +
  scale_fill_manual(values = c("springgreen4","firebrick4"))

ggsave('hono_nihgt_vs_za_feb23.png',
       path = '~/Writing/Thesis/Chapter 4 (HONO in CVAO)/Images',
       width = 29,
       height = 12,
       units = 'cm')
