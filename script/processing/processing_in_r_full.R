library(tidyverse)
library(lubridate)
library(zoo)
library(oce)
library(openair)
library(plotly)

Sys.setenv(TZ = "UTC")

#the only thing that is not processed in r is the time correction
#updated to zero all data with second batch of reagents using nighttime values

# rel_error = 10 #set at 10%, conservative value

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

lod <- function(ch1_zero_sd,ch2_zero_sd,slope_cal1,slope_cal2){
  
  x = ((ch1_zero_sd*slope_cal1)^2+(ch2_zero_sd*slope_cal2)^2)^0.5
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

# Zeroes r1 ------------------------------------------------------------------

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
            ch1_zero_sd = 2 * sd(ch1),
            ch2_zero_sd = 2 * sd(ch2),
            idx = mean(idx)) %>% 
  ungroup() %>% 
  mutate(idx = round(idx))

#interpolate between zeroes and subtract zeroes from measurements
zeroed = zero_flag %>% 
  mutate(idx = 1:nrow(.)) %>% 
  left_join(zero_avg) %>% 
  mutate(ch1_zeroes = na.approx(ch1_zeroes,na.rm = F),
         ch2_zeroes = na.approx(ch2_zeroes,na.rm = F),
         ch1_zero_sd = na.approx(ch1_zero_sd,na.rm = F),
         ch2_zero_sd = na.approx(ch2_zero_sd,na.rm = F)) %>% 
  fill(ch1_zeroes,ch2_zeroes,ch1_zero_sd,ch2_zero_sd,.direction = "downup") %>% 
  # fill(ch1_zero_sd,ch2_zero_sd,.direction = "downup") %>% 
  mutate(ch1_zeroed = ch1 - ch1_zeroes,
         ch2_zeroed = ch2 - ch2_zeroes)

# Calibration r1 -------------------------------------------------------------

#cal parameters
time_corr = 22.75 * 60
liquid_flow1 = 5/27.92 #took 27.92min to fill up 5ml volumetric flask
liquid_flow2 = 5/28.67
conc_cal = 1000/100000 #standard conc / dilution factor
hono_cal_conc_ch1 = conc_cal/1000 * liquid_flow1/1000 /46*6.022*10^23/(2.46*10^19* actual_gas_flow) * 10^12
hono_cal_conc_ch2 = conc_cal/1000 * liquid_flow2/1000 /46*6.022*10^23/(2.46*10^19* actual_gas_flow) * 10^12

cal = zeroed %>% 
  select(date,ch1_zeroed,ch2_zeroed) %>% 
  filter(date > "2023-02-11 13:40" & date < "2023-02-11 15:56") %>%
  mutate(cal_zero_ch1 = ifelse(between(date,as.POSIXct("2023-02-11 13:40:30"),as.POSIXct("2023-02-11 14:53")),ch1_zeroed,NA),
         cal_zero_ch2 = ifelse(between(date,as.POSIXct("2023-02-11 13:40:30"),as.POSIXct("2023-02-11 14:53")),ch2_zeroed,NA),
         cal_ch1 = ifelse(between(date,as.POSIXct("2023-02-11 15:11"),as.POSIXct("2023-02-11 15:25")),ch1_zeroed,NA),
         cal_ch2 = ifelse(between(date,as.POSIXct("2023-02-11 15:11"),as.POSIXct("2023-02-11 15:25")),ch2_zeroed,NA)) %>% 
  summarise(across(c(cal_zero_ch1:cal_ch2),list(mean = ~mean(.,na.rm = T),
                                          sd = ~2*sd(.,na.rm = T),
                                          count = ~sum(!is.na(.))))) %>% 
  mutate(ch1_cal_zero_uncertainty = (cal_zero_ch1_sd/(cal_zero_ch1_count)^0.5),
         ch1_cal_uncertainty = (cal_ch1_sd/cal_ch1_count^0.5),
         ch1_error = ((ch1_cal_zero_uncertainty^2 + ch1_cal_uncertainty^2)^0.5)/(cal_ch1_mean - cal_zero_ch1_mean),
         #1.27% error due to nitrite standard concentration and dilution
         cal_error_ch1 = ((0.0544)^2+(ch1_error)^2)^0.5,
         ch2_cal_zero_uncertainty = (cal_zero_ch1_sd/cal_zero_ch1_count^0.5),
         ch2_cal_uncertainty = (cal_ch2_sd/cal_ch2_count^0.5),
         ch2_error = (ch2_cal_zero_uncertainty^2 + ch2_cal_uncertainty^2)^0.5/(cal_ch2_mean - cal_zero_ch2_mean),
         cal_error_ch2 = ((0.0544)^2+(ch2_error)^2)^0.5)
  # summarise(cal_zero_ch1 = mean(cal_zero_ch1,na.rm = T),
  #           cal_zero_ch2 = mean(cal_zero_ch2,na.rm = T),
  #           cal_ch1 = mean(cal_ch1,na.rm = T),
  #           cal_ch2 = mean(cal_ch2,na.rm = T))

cal_df1 = data.frame(y = c(0,hono_cal_conc_ch1),
                    x = c(cal$cal_zero_ch1_mean,cal$cal_ch1_mean))

model_cal1 = lm(y ~ x,cal_df1)
# intercept_mfc = summary(model_mfc_cal)$coefficients[1,1]
slope_cal1 = summary(model_cal1)$coefficients[2,1]

cal_df2 = data.frame(y = c(0,hono_cal_conc_ch2),
                     x = c(cal$cal_zero_ch2_mean,cal$cal_ch2_mean))

model_cal2 = lm(y ~ x,cal_df2)
# intercept_mfc = summary(model_mfc_cal)$coefficients[1,1]
slope_cal2 = summary(model_cal2)$coefficients[2,1]

# Applying cal r1 ------------------------------------------------------------

dat1_calibrated = zeroed %>% 
  select(-c(zeroing,id,idx)) %>% 
  mutate(ch1_ppt = ch1_zeroed * slope_cal1,
         ch2_ppt = ch2_zeroed * slope_cal2,
         hono = ppt(ch1_ppt,ch2_ppt,sampling_efficiency),
         # hono_err = abs(hono * rel_error/100 + hono_lod),
         date = date - time_corr,
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
                          between(date,as.POSIXct("2023-02-17 09:03"),as.POSIXct("2023-02-17 18:12")) ~ 1,
                          TRUE ~ 0))

#flags:
#0 good data
#1 for water/abs closed
#2 for zero
#3 for cal
#4 for air in abs
#5 other

# dat1_calibrated %>% 
#   filter(flag == 0) %>% 
#   ggplot(aes(date,hono)) +
#   geom_point()

# Despiking r1 ---------------------------------------------------------------

#using despike function (import oce library) to automatically find and remove instrumental noise spikes
despiking = dat1_calibrated %>% 
  mutate(flagged_hono = ifelse(flag == 0, hono,NA_real_), #removing flagged data
         despiked_hono = despike(flagged_hono, reference = "median",n = 3, k = 19, replace = "NA"), #despiking
         #flag that is one only with instrumental noise spikes, ignores NAs from previous flag
         instrumental_noise_flag = ifelse(is.na(flagged_hono-despiked_hono) & flag == 0,1,0)) 

#despiking only removes peak of spike, not rise, so we need to remove rows before and after spike
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
         instrumental_noise_flag = ifelse(idx < 5, 1, instrumental_noise_flag), #removes first x values of each group
         idx = n():1,
         instrumental_noise_flag = ifelse(idx < 7, 1,instrumental_noise_flag)) #removes last x values of group 

# despiked_dat %>% 
#   mutate(flagged_hono = ifelse(instrumental_noise_flag == 1, NA_real_,flagged_hono)) %>%
#   ggplot(aes(date,flagged_hono)) +
#   scale_x_datetime(date_breaks = "1 day",date_labels = "%d/%m") +
#   geom_path()

processed_dat1 = despiked_dat %>% 
  ungroup() %>% 
  mutate(hono = ifelse(instrumental_noise_flag == 1, NA_real_,hono),
         ch1_ppt = ifelse(instrumental_noise_flag == 1, NA_real_,ch1_ppt),
         ch2_ppt = ifelse(instrumental_noise_flag == 1, NA_real_,ch2_ppt),
         ch1 = ifelse(instrumental_noise_flag == 1, NA_real_,ch1),
         ch2 = ifelse(instrumental_noise_flag == 1, NA_real_,ch2),
         hono_lod = lod(ch1_zero_sd,ch2_zero_sd,slope_cal1,slope_cal2),
         hono_err =((hono_lod)^2 + (cal$cal_error_ch1*ch1_ppt)^2 + (cal$cal_error_ch2*ch2_ppt)^2)^0.5,
         # hono_err_percent = hono_err/hono * 100,
         hono_err_percent = ifelse(hono>0 & hono_err>0 & hono>hono_err,hono_err/hono *100,NA_real_),
         # hono_err_manual = hono * 0.1 + hono_lod,
         # hono_err_manual_prop = ((hono*0.1)^2+(hono_lod)^2)^0.5,
         measuring_conditions = "ZA zeroes") %>%
  select(date,hono,hono_lod,hono_err,hono_err_percent,flag,measuring_conditions)

# Zeroes r2 (za) ------------------------------------------------------------------
# 
# zero_flag = raw_dat2 %>% 
#   mutate(zeroing = case_when(between(date,as.POSIXct("2023-02-17 18:15"),as.POSIXct("2023-02-17 18:24")) ~ 1,
#                              between(date,as.POSIXct("2023-02-18 00:29"),as.POSIXct("2023-02-18 00:39")) ~ 1,
#                              between(date,as.POSIXct("2023-02-18 09:50"),as.POSIXct("2023-02-18 10:35")) ~ 1,
#                              between(date,as.POSIXct("2023-02-18 16:41"),as.POSIXct("2023-02-18 16:52")) ~ 1,
#                              between(date,as.POSIXct("2023-02-19 17:02"),as.POSIXct("2023-02-19 17:13")) ~ 1, #lower than most, after power cut
#                              between(date,as.POSIXct("2023-02-20 11:39"),as.POSIXct("2023-02-20 11:47")) ~ 1, #after power cut
#                              between(date,as.POSIXct("2023-02-20 17:55"),as.POSIXct("2023-02-20 18:04")) ~ 1, #after power cut
#                              TRUE ~ 0)) %>% 
#   filter(date < "2023-02-21")
# 
# #creates a group for each zero and maintains row no. of main df so can easily left_join
# zeroing = rle(zero_flag$zeroing) %>%
#   tidy_rle() %>% 
#   filter(values == 1) %>% 
#   mutate(id = 1:nrow(.)) %>% 
#   as.list() %>% 
#   purrr::pmap_df(~data.frame(idx = ..3:..4,id = ..5)) %>% 
#   tibble() 
# 
# #join dfs with groups for each zero
# zeroes_grouped = zero_flag %>% 
#   mutate(idx = 1:nrow(.)) %>% 
#   left_join(zeroing, "idx") %>% #joins two dfs by their row number
#   mutate(id = ifelse(is.na(id), 0, id)) #makes id (group) = 0 when not zeroing
# 
# #average zero value for each group
# zero_avg = zeroes_grouped %>% 
#   filter(id != 0) %>%
#   group_by(id) %>% 
#   summarise(ch1_zeroes = mean(ch1),
#             ch2_zeroes = mean(ch2),
#             ch1_zero_3sd = 3 * sd(ch1),
#             ch2_zero_3sd = 3 * sd(ch2),
#             idx = mean(idx)) %>% 
#   ungroup() %>% 
#   mutate(idx = round(idx))
# 
# #interpolate between zeroes and subtract zeroes from measurements
# za_zeroed = zero_flag %>% 
#   mutate(idx = 1:nrow(.)) %>% 
#   left_join(zero_avg) %>% 
#   mutate(ch1_zeroes = na.approx(ch1_zeroes,na.rm = F),
#          ch2_zeroes = na.approx(ch2_zeroes,na.rm = F)) %>% 
#   fill(ch1_zeroes,ch2_zeroes,.direction = "up") %>% 
#   fill(ch1_zero_3sd,ch2_zero_3sd,.direction = "downup") %>% 
#   mutate(ch1_zeroed = ch1 - ch1_zeroes,
#          ch2_zeroed = ch2 - ch2_zeroes,
#          measuring_conditions = "Reagents 2, ZA zeroes") 
# 

# Zeroes r2 (night) -------------------------------------------------------

time_corr = 23.67 * 60

night_zeroing = raw_dat2 %>%
  # filter(date > "2023-02-21") %>% #data after power cut
  mutate(time = hour(date),
         date = date - time_corr, #so that flagging is applied properly
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
  mutate(id = ifelse(is.na(id), 0, id), #makes id (group) = 0 during the day
         ch1_sd = ifelse(time == 22,ch1,NA_real_),
         ch2_sd = ifelse(time == 22,ch2,NA_real_)) #for calculating the sd by looking at the variation over an hour at night (which is the same length as the averaging time)

night_avg = night_flagged %>% 
  filter(flag == 0, #don't want any flagged moments included in nighttime average
         id != 0) %>%
  group_by(id) %>% 
  summarise(ch1_night = median(ch1),
            ch2_night = median(ch2),
            ch1_zero_sd = 2 * sd(ch1_sd,na.rm = T),
            ch2_zero_sd = 2 * sd(ch2_sd,na.rm = T),
            idx = mean(idx)) %>% 
  ungroup() %>% 
  mutate(idx = round(idx),
         ch1_inter = ifelse(id <= 6,ch1_night,NA_real_),
         ch2_inter = ifelse(id <= 6,ch2_night,NA_real_))
#id column = 5.5, not adding values to ch1/ch2_night cols because these weren't actually measured
#adding idx, row number from last measurement on 24/02 before abs were cleaned and measurements went a bit weird
#adding extrapolated values for that idx, using only previously three nighttime averages because of best
#line fit
night_avg[nrow(night_avg) + 1,] = list(6.5,NA_real_,NA_real_,NA_real_,NA_real_,19945,0.05299002,0.04949119)
night_avg = night_avg %>% arrange(idx)

night_zeroed = night_zeroing %>% 
  mutate(idx = 1:nrow(.)) %>% 
  left_join(night_avg) %>% 
  mutate(ch1_zeroes = na.approx(ch1_inter,na.rm = F),
         ch2_zeroes = na.approx(ch2_inter,na.rm = F),
         ch1_zero_sd = na.approx(ch1_zero_sd,na.rm = F),
         ch2_zero_sd = na.approx(ch2_zero_sd,na.rm = F)) %>% 
  fill(ch1_zeroes,ch2_zeroes,ch1_zero_sd,ch2_zero_sd,.direction = "downup") %>% 
  mutate(ch1_zeroed = ifelse(date < "2023-02-24 08:41",ch1 - ch1_zeroes,ch1 - 0.04973971),
         ch2_zeroed = ifelse(date < "2023-02-24 08:41",ch2 - ch2_zeroes,ch2 - 0.04323023),
         measuring_conditions = "Reagents 2, nighttime zeroes")

# Calibration r2 -------------------------------------------------------------

#cal parameters
time_corr = 23.67 * 60
liquid_flow1 = 5/27.10 
liquid_flow2 = 5/27.58
conc_cal = 1000/100000 #standard conc / dilution factor
hono_cal_conc_ch1 = conc_cal/1000 * liquid_flow1/1000 /46*6.022*10^23/(2.46*10^19* actual_gas_flow) * 10^12
hono_cal_conc_ch2 = conc_cal/1000 * liquid_flow2/1000 /46*6.022*10^23/(2.46*10^19* actual_gas_flow) * 10^12

cal = night_zeroed %>% 
  select(date,ch1_zeroed,ch2_zeroed)  %>% 
  filter(date > "2023-02-21 15:35" & date < "2023-02-21 18:00") %>%
  mutate(cal_zero_ch1 = ifelse(between(date,as.POSIXct("2023-02-21 17:15"),as.POSIXct("2023-02-21 17:40")),ch1_zeroed,NA),
         cal_zero_ch2 = ifelse(between(date,as.POSIXct("2023-02-21 17:15"),as.POSIXct("2023-02-21 17:40")),ch2_zeroed,NA),
         cal_ch1 = ifelse(between(date,as.POSIXct("2023-02-21 16:20"),as.POSIXct("2023-02-21 16:34")),ch1_zeroed,NA),
         cal_ch2 = ifelse(between(date,as.POSIXct("2023-02-21 16:24"),as.POSIXct("2023-02-21 16:34")),ch2_zeroed,NA)) %>% 
  summarise(across(c(cal_zero_ch1:cal_ch2),list(mean = ~mean(.,na.rm = T),
                                                sd = ~2*sd(.,na.rm = T),
                                                count = ~sum(!is.na(.))))) %>% 
  mutate(ch1_cal_zero_uncertainty = (cal_zero_ch1_sd/(cal_zero_ch1_count)^0.5),
         ch1_cal_uncertainty = (cal_ch1_sd/cal_ch1_count^0.5),
         ch1_error = ((ch1_cal_zero_uncertainty^2 + ch1_cal_uncertainty^2)^0.5)/(cal_ch1_mean - cal_zero_ch1_mean),
         cal_error_ch1 = ((0.0544)^2+(ch1_error)^2)^0.5,
         ch2_cal_zero_uncertainty = (cal_zero_ch1_sd/cal_zero_ch1_count^0.5),
         ch2_cal_uncertainty = (cal_ch2_sd/cal_ch2_count^0.5),
         ch2_error = (ch2_cal_zero_uncertainty^2 + ch2_cal_uncertainty^2)^0.5/(cal_ch2_mean - cal_zero_ch2_mean),
         cal_error_ch2 = ((0.0544)^2+(ch2_error)^2)^0.5)

cal_df1 = data.frame(y = c(0,hono_cal_conc_ch1),
                     x = c(cal$cal_zero_ch1_mean,cal$cal_ch1_mean))

model_cal1 = lm(y ~ x,cal_df1)
# intercept_mfc = summary(model_mfc_cal)$coefficients[1,1]
slope_cal1 = summary(model_cal1)$coefficients[2,1]

cal_df2 = data.frame(y = c(0,hono_cal_conc_ch2),
                     x = c(cal$cal_zero_ch2_mean,cal$cal_ch2_mean))

model_cal2 = lm(y ~ x,cal_df2)
# intercept_mfc = summary(model_mfc_cal)$coefficients[1,1]
slope_cal2 = summary(model_cal2)$coefficients[2,1]


# Applying cal r2 ------------------------------------------------------------

dat2_calibrated = night_zeroed %>% 
  select(-c(time,flag,night_flag)) %>%
  mutate(ch1_ppt = ch1_zeroed * slope_cal1,
         ch2_ppt = ch2_zeroed * slope_cal2,
         hono = ppt(ch1_ppt,ch2_ppt,sampling_efficiency),
         # hono_lod = lod(ch1_zero_sd,ch2_zero_sd,slope_cal1,slope_cal2),
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

processed_dat2 = dat2_calibrated %>%
  mutate(measuring_conditions = "Nighttime zeroes") %>% 
  # filter(flag == 0) %>%
  mutate(hono_lod = lod(ch1_zero_sd,ch2_zero_sd,slope_cal1,slope_cal2),
         hono_err =((hono_lod)^2 + (cal$cal_error_ch1*ch1_ppt)^2 + (cal$cal_error_ch2*ch2_ppt)^2)^0.5,
         hono_err_percent = ifelse(hono>0 & hono_err>0 & hono>hono_err,hono_err/hono *100,NA_real_)) %>%
  select(date,hono,hono_lod,hono_err,hono_err_percent,flag,measuring_conditions)

# Zeroes r0.5 -------------------------------------------------------------

zero_flag = raw_dat3 %>% 
  mutate(zeroing = case_when(between(date,as.POSIXct("2023-02-07 09:15"),as.POSIXct("2023-02-07 09:45")) ~ 1,
                             between(date,as.POSIXct("2023-02-07 15:57"),as.POSIXct("2023-02-07 16:13")) ~ 1,
                             between(date,as.POSIXct("2023-02-07 21:58"),as.POSIXct("2023-02-07 22:15")) ~ 1,
                             between(date,as.POSIXct("2023-02-08 03:55"),as.POSIXct("2023-02-08 04:15")) ~ 1,
                             between(date,as.POSIXct("2023-02-08 15:14"),as.POSIXct("2023-02-08 15:31")) ~ 1,
                             between(date,as.POSIXct("2023-02-08 23:38"),as.POSIXct("2023-02-08 23:50")) ~ 1,
                             between(date,as.POSIXct("2023-02-09 05:40"),as.POSIXct("2023-02-09 05:55")) ~ 1,
                             between(date,as.POSIXct("2023-02-09 11:40"),as.POSIXct("2023-02-09 12:07")) ~1, #precal zero
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
            ch1_zero_sd = 2 * sd(ch1),
            ch2_zero_sd = 2 * sd(ch2),
            idx = mean(idx)) %>% 
  ungroup() %>% 
  mutate(idx = round(idx))

#interpolate between zeroes and subtract zeroes from measurements
zeroed = zero_flag %>% 
  mutate(idx = 1:nrow(.)) %>% 
  left_join(zero_avg) %>% 
  mutate(ch1_zeroes = na.approx(ch1_zeroes,na.rm = F),
         ch2_zeroes = na.approx(ch2_zeroes,na.rm = F),
         ch1_zero_sd = na.approx(ch1_zero_sd,na.rm = F),
         ch2_zero_sd = na.approx(ch2_zero_sd,na.rm = F)) %>% 
  fill(ch1_zeroes,ch2_zeroes,ch1_zero_sd,ch2_zero_sd,.direction = "downup") %>% 
  fill(ch1_zeroes,ch2_zeroes,ch1_zero_sd,ch2_zero_sd,.direction = "downup") %>%
  mutate(ch1_zeroed = ch1 - ch1_zeroes,
         ch2_zeroed = ch2 - ch2_zeroes)

# Calibration r0.5 --------------------------------------------------------

#cal parameters
time_corr = 22.33 * 60
liquid_flow1 = 5/27.92
liquid_flow2 = 5/28.67
conc_cal = 1000/100000 #standard conc / dilution factor
hono_cal_conc_ch1 = conc_cal/1000 * liquid_flow1/1000 /46*6.022*10^23/(2.46*10^19* actual_gas_flow) * 10^12
hono_cal_conc_ch2 = conc_cal/1000 * liquid_flow2/1000 /46*6.022*10^23/(2.46*10^19* actual_gas_flow) * 10^12

cal = zeroed %>% 
  select(date,ch1_zeroed,ch2_zeroed) %>% 
  filter(date > "2023-02-09 11:40" & date < "2023-02-09 13:56") %>%
  mutate(cal_zero_ch1 = ifelse(between(date,as.POSIXct("2023-02-09 11:40"),as.POSIXct("2023-02-09 12:07")),ch1_zeroed,NA),
         cal_zero_ch2 = ifelse(between(date,as.POSIXct("2023-02-09 11:40"),as.POSIXct("2023-02-09 12:07")),ch2_zeroed,NA),
         cal_ch1 = ifelse(between(date,as.POSIXct("2023-02-09 12:29"),as.POSIXct("2023-02-09 12:59")),ch1_zeroed,NA),
         cal_ch2 = ifelse(between(date,as.POSIXct("2023-02-09 12:29"),as.POSIXct("2023-02-09 12:59")),ch2_zeroed,NA)) %>% 
  summarise(across(c(cal_zero_ch1:cal_ch2),list(mean = ~mean(.,na.rm = T),
                                                sd = ~2*sd(.,na.rm = T),
                                                count = ~sum(!is.na(.))))) %>% 
  mutate(ch1_cal_zero_uncertainty = (cal_zero_ch1_sd/(cal_zero_ch1_count)^0.5),
         ch1_cal_uncertainty = (cal_ch1_sd/cal_ch1_count^0.5),
         ch1_error = ((ch1_cal_zero_uncertainty^2 + ch1_cal_uncertainty^2)^0.5)/(cal_ch1_mean - cal_zero_ch1_mean),
         cal_error_ch1 = ((0.0544)^2+(ch1_error)^2)^0.5,
         ch2_cal_zero_uncertainty = (cal_zero_ch1_sd/cal_zero_ch1_count^0.5),
         ch2_cal_uncertainty = (cal_ch2_sd/cal_ch2_count^0.5),
         ch2_error = (ch2_cal_zero_uncertainty^2 + ch2_cal_uncertainty^2)^0.5/(cal_ch2_mean - cal_zero_ch2_mean),
         cal_error_ch2 = ((0.0544)^2+(ch2_error)^2)^0.5)

cal_df1 = data.frame(y = c(0,hono_cal_conc_ch1),
                     x = c(cal$cal_zero_ch1_mean,cal$cal_ch1_mean))

model_cal1 = lm(y ~ x,cal_df1)
# intercept_mfc = summary(model_mfc_cal)$coefficients[1,1]
slope_cal1 = summary(model_cal1)$coefficients[2,1]

cal_df2 = data.frame(y = c(0,hono_cal_conc_ch2),
                     x = c(cal$cal_zero_ch2_mean,cal$cal_ch2_mean))

model_cal2 = lm(y ~ x,cal_df2)
# intercept_mfc = summary(model_mfc_cal)$coefficients[1,1]
slope_cal2 = summary(model_cal2)$coefficients[2,1]


# Applying cal r0.5 -------------------------------------------------------

dat3_calibrated = zeroed %>% 
  select(-c(zeroing,id,idx)) %>% 
  mutate(ch1_ppt = ch1_zeroed * slope_cal1,
         ch2_ppt = ch2_zeroed * slope_cal2,
         hono = ppt(ch1_ppt,ch2_ppt,sampling_efficiency),
         # hono_lod = lod(ch1_zero_sd,ch2_zero_sd,slope_cal1,slope_cal2),
         # hono_err = abs(hono * rel_error/100 + hono_lod),
         date = date - time_corr,
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

# dat3_calibrated %>% 
#   filter(flag == 0) %>% 
#   ggplot(aes(date,hono)) +
#   geom_point()


# Despiking r0.5 ----------------------------------------------------------

#using despike function (import oce library) to automatically find and remove instrumental noise spikes
despiking = dat3_calibrated %>% 
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
         instrumental_noise_flag = ifelse(idx < 5, 1, instrumental_noise_flag), #removes first x values of each group
         idx = n():1,
         instrumental_noise_flag = ifelse(idx < 7, 1,instrumental_noise_flag)) #removes last x values of group 

# despiked_dat %>% 
#   mutate(flagged_hono = ifelse(instrumental_noise_flag == 1, NA_real_,flagged_hono)) %>%
#   # timeAverage("1 hour") %>% 
#   ggplot(aes(date,flagged_hono)) +
#   scale_x_datetime(date_breaks = "1 day",date_labels = "%d/%m") +
#   geom_path()

processed_dat3 = despiked_dat %>% 
  ungroup() %>% 
  mutate(hono = ifelse(instrumental_noise_flag == 1, NA_real_,hono),
         ch1_ppt = ifelse(instrumental_noise_flag == 1, NA_real_,ch1_ppt),
         ch2_ppt = ifelse(instrumental_noise_flag == 1, NA_real_,ch2_ppt),
         hono_lod = lod(ch1_zero_sd,ch2_zero_sd,slope_cal1,slope_cal2),
         hono_err =((hono_lod)^2 + (cal$cal_error_ch1*ch1_ppt)^2 + (cal$cal_error_ch2*ch2_ppt)^2)^0.5,
         hono_err_percent = ifelse(hono>0 & hono_err>0 & hono>hono_err,hono_err/hono *100,NA_real_),
         measuring_conditions = "ZA zeroes") %>% 
  select(date,hono,hono_lod,hono_err,hono_err_percent,flag,measuring_conditions)

# Final data --------------------------------------------------------------

#flags:
#0 good data
#1 for water/abs closed
#2 for zero
#3 for cal
#4 for air in abs
#5 other

#binding all the data together, removing data from periods when there were issues and calculating LOD for the campaign
#looking at how it changes if nighttime zeroes or za zeroes are used
final_dat = bind_rows(processed_dat3,processed_dat1,processed_dat2) %>% 
  arrange(date) %>% 
  mutate(hono = ifelse(flag == 0,hono,NA_real_),
         hono_err = ifelse(flag == 0,hono_err,NA_real_),
         hono_lod = ifelse(flag == 0,hono_lod,NA_real_))

final_dat_lod_err = final_dat %>% 
  group_by(measuring_conditions) %>% 
  summarise(across(c(hono_err,hono_lod),
                   list(mean = ~mean(.,na.rm = T),
                        two_sd = ~2 * sd(.,na.rm = T))))

# hono_lod = mean(final_dat$hono_lod,na.rm = T)
# hono_lod_sd = 2 * sd(final_dat$hono_lod,na.rm = T)
# hono_err = mean(final_dat$hono_err,na.rm = T)
# hono_err_sd = 2 * sd(final_dat$hono_err,na.rm = T)

hourly_dat = final_dat %>% 
  select(-flag) %>% 
  timeAverage("1 hour") %>% 
  mutate(hono_err_percent = ifelse(hono>0 & hono_err>0 & hono>hono_err,hono_err/hono * 100,NA_real_),
         # date = format(date, "%Y-%m-%d %H:%M:%S")
         )

hourly_dat %>% 
  pivot_longer(c(hono,hono_err,hono_err_percent)) %>% 
  ggplot(aes(date,value)) +
  geom_path() +
  facet_grid(rows = vars(name),scales = "free")

hono23 %>%
  mutate(hono_max_err = hono + hono_err,
         hono_min_err = hono - hono_err) %>% 
  ggplot() +
  theme_bw() +
  geom_path(aes(date,hono)) +
  geom_ribbon(aes(date,ymax = hono_max_err,ymin = hono_min_err),fill = "steelblue1",alpha = 0.5) +
  labs(x = NULL,
       y = "HONO (ppt)",
       col = NULL)

#write.csv(hourly_dat,"output/data/hono23_hourly_utc.csv",row.names = FALSE)

# Data to send to us epa --------------------------------------------------
# 
# dat = final_dat %>%
#   timeAverage("1 hour") %>%
#   rename(date_utc = date,
#          hono_ppt = hono,
#          hono_err_ppt = hono_err,
#          hono_lod_ppt = hono_lod)
# 
# dat %>%
#   ggplot(aes(date_utc,hono_ppt)) +
#   theme_bw() +
#   geom_path() +
#   geom_ribbon(aes(ymin=hono_ppt-hono_err_ppt, ymax=hono_ppt+hono_err_ppt),
#               alpha = 0.2) +
#   labs(x = "Datetime (UTC)",
#        y = "HONO / ppt",
#        col = NULL)
# 
# diurnal_dat = final_dat_all %>%
#   mutate(hour = hour(date)) %>%
#   group_by(hour) %>%
#   summarise(diurnal_hono = mean(hono,na.rm = T),
#             diurnal_hono_3sd = 2*sd(hono,na.rm = T))
# 
# diurnal_dat %>%
#   ggplot(aes(hour,diurnal_hono)) +
#   theme_bw() +
#   geom_path() +
#   geom_ribbon(aes(ymin=diurnal_hono-diurnal_hono_err, ymax=diurnal_hono+diurnal_hono_err),
#               alpha = 0.2) +
#   labs(x = "Datetime (UTC)",
#        y = "HONO / ppt",
#        col = NULL)
# 
# 
# # write.csv(diurnal_dat,"output/shared_data/diurnal_hono_feb23.csv",row.names = FALSE)
