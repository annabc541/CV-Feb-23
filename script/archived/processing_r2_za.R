

#code used for processing all the data measured with the second set of reagents using ZA

# Zeroes ------------------------------------------------------------------

#testing 3 different eras for zeroes (before power cut,between power cut and changing pump,after changing
#pump)

dat2_zero_flag = raw_dat2 %>% 
  mutate(zeroing = case_when(between(date,as.POSIXct("2023-02-17 18:15"),as.POSIXct("2023-02-17 18:24")) ~ 1,
                             between(date,as.POSIXct("2023-02-18 00:29"),as.POSIXct("2023-02-18 00:39")) ~ 1,
                             between(date,as.POSIXct("2023-02-18 09:50"),as.POSIXct("2023-02-18 10:35")) ~ 1,
                             between(date,as.POSIXct("2023-02-18 16:41"),as.POSIXct("2023-02-18 16:52")) ~ 1,#power cut after this zero
                             # between(date,as.POSIXct("2023-02-19 17:02"),as.POSIXct("2023-02-19 17:13")) ~ 2,
                             between(date,as.POSIXct("2023-02-20 11:39"),as.POSIXct("2023-02-20 11:47")) ~ 2,
                             between(date,as.POSIXct("2023-02-20 17:55"),as.POSIXct("2023-02-20 18:04")) ~ 2,
                             between(date,as.POSIXct("2023-02-21 09:31"),as.POSIXct("2023-02-21 09:44")) ~ 2,
                             between(date,as.POSIXct("2023-02-21 17:15"),as.POSIXct("2023-02-21 17:40")) ~ 2,#postcal
                             between(date,as.POSIXct("2023-02-21 23:47:30"),as.POSIXct("2023-02-21 23:59")) ~ 2,
                             between(date,as.POSIXct("2023-02-22 06:02"),as.POSIXct("2023-02-22 06:15")) ~ 2,
                             between(date,as.POSIXct("2023-02-22 12:22"),as.POSIXct("2023-02-22 12:30:30")) ~ 2,
                             between(date,as.POSIXct("2023-02-22 18:39"),as.POSIXct("2023-02-22 18:47")) ~ 2,
                             between(date,as.POSIXct("2023-02-23 00:51"),as.POSIXct("2023-02-23 01:02")) ~ 2,
                             between(date,as.POSIXct("2023-02-23 07:07"),as.POSIXct("2023-02-23 07:18")) ~ 2,
                             between(date,as.POSIXct("2023-02-23 13:25"),as.POSIXct("2023-02-23 13:35")) ~ 2,
                             between(date,as.POSIXct("2023-02-23 19:42"),as.POSIXct("2023-02-23 19:52")) ~ 2,
                             between(date,as.POSIXct("2023-02-24 01:57"),as.POSIXct("2023-02-24 02:05")) ~ 2,
                             between(date,as.POSIXct("2023-02-24 08:11"),as.POSIXct("2023-02-24 08:22")) ~ 2, #cleaned inlet and messed around with liquid pump after this zero
                             between(date,as.POSIXct("2023-02-24 14:32"),as.POSIXct("2023-02-24 14:38")) ~ 3,
                             between(date,as.POSIXct("2023-02-24 20:40"),as.POSIXct("2023-02-24 20:54")) ~ 3,
                             between(date,as.POSIXct("2023-02-25 02:57"),as.POSIXct("2023-02-25 03:06")) ~ 3,
                             between(date,as.POSIXct("2023-02-25 09:10"),as.POSIXct("2023-02-25 09:16")) ~ 3,
                             # between(date,as.POSIXct("2023-02-26 16:32"),as.POSIXct("2023-02-26 16:45")) ~ 3, #remove 26th?
                             TRUE ~ 0))

dat2_zero_flag %>% 
  filter(zeroing != 0) %>% 
  pivot_longer(c(ch1,ch2)) %>% 
  ggplot(aes(date,value,col = zeroing)) +
  geom_point() +
  facet_grid(rows = vars(name),scales = "free_y")

#creates a group for each zero and maintains row no. of main df so can easily left_join
zeroing = rle(dat2_zero_flag$zeroing) %>%
  tidy_rle() %>% 
  filter(values == 2) %>% 
  mutate(id = 1:nrow(.)) %>% 
  as.list() %>% 
  purrr::pmap_df(~data.frame(idx = ..3:..4,id = ..5)) %>% 
  tibble() 

#join dfs with groups for each zero
dat2_zeroes_grouped = dat2_zero_flag %>% 
  mutate(idx = 1:nrow(.)) %>% 
  left_join(zeroing, "idx") %>% #joins two dfs by their row number
  mutate(id = ifelse(is.na(id), 0, id)) #makes id (group) = 0 when not zeroing

#average zero value for each group
zero_avg = dat2_zeroes_grouped %>% 
  filter(id != 0) %>%
  group_by(id) %>% 
  summarise(ch1_zeroes = mean(ch1),
            ch2_zeroes = mean(ch2),
            idx = mean(idx)) %>% 
  ungroup() %>% 
  mutate(idx = round(idx))

#interpolate between zeroes and subtract zeroes from measurements
dat2_zeroed = dat2_zero_flag %>% 
  mutate(idx = 1:nrow(.)) %>%
  # filter(date < "2023-02-18 16:52") %>% 
  left_join(zero_avg) %>% 
  mutate(ch1_zeroes = na.approx(ch1_zeroes,na.rm = F),
         ch2_zeroes = na.approx(ch2_zeroes,na.rm = F)) %>% 
  fill(ch1_zeroes,ch2_zeroes,.direction = "downup") %>% 
  mutate(ch1_zeroed = ch1 - ch1_zeroes,
         ch2_zeroed = ch2 - ch2_zeroes)


dat2_zeroed_pt2 = dat2_zero_flag %>% 
  mutate(idx = 1:nrow(.)) %>% 
  filter(date > "2023-02-20" & date < "2023-02-24 08:22") %>% 
  left_join(zero_avg) %>% 
  mutate(ch1_zeroes = na.approx(ch1_zeroes,na.rm = F),
         ch2_zeroes = na.approx(ch2_zeroes,na.rm = F)) %>% 
  fill(ch1_zeroes,ch2_zeroes,.direction = "up") %>% 
  mutate(ch1_zeroed = ch1 - ch1_zeroes,
         ch2_zeroed = ch2 - ch2_zeroes)

dat2_zeroed_pt3 = dat2_zero_flag %>% 
  mutate(idx = 1:nrow(.)) %>% 
  filter(date > "2023-02-24 08:22" & date < "2023-02-25 09:16") %>% 
  left_join(zero_avg) %>% 
  mutate(ch1_zeroes = na.approx(ch1_zeroes,na.rm = F),
         ch2_zeroes = na.approx(ch2_zeroes,na.rm = F)) %>% 
  fill(ch1_zeroes,ch2_zeroes,.direction = "up") %>% 
  mutate(ch1_zeroed = ch1 - ch1_zeroes,
         ch2_zeroed = ch2 - ch2_zeroes)

dat2_zeroed = bind_rows(dat2_zeroed_pt1,dat2_zeroed_pt2,dat2_zeroed_pt3)

dat2_zeroed %>% 
  # filter(zeroing != 0) %>% 
  pivot_longer(c(ch2_zeroes,ch1_zeroes)) %>% 
  ggplot(aes(date,value,col = name)) +
  geom_point() +
  facet_grid(rows = vars(name),scales = "free_y") +
  NULL


# Calibration -------------------------------------------------------------

#using zero after calibration

#cal parameters
time_corr2 = 23.67 * 60
liquid_flow1 = 5/27.10 #took 27.92min to fill up 5ml volumetric flask
liquid_flow2 = 5/27.58
conc_cal = 1000/100000 #standard conc / dilution factor
hono_cal_conc_ch1 = conc_cal/1000 * liquid_flow1/1000 /46*6.022*10^23/(2.46*10^19* actual_gas_flow) * 10^12
hono_cal_conc_ch2 = conc_cal/1000 * liquid_flow2/1000 /46*6.022*10^23/(2.46*10^19* actual_gas_flow) * 10^12


dat2_cal = dat2_zeroed %>% 
  select(date,ch1_zeroed,ch2_zeroed) %>% 
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
                     x = c(dat2_cal$cal_zero_ch1,dat2_cal$cal_ch1))

model_cal1 = lm(y ~ x,cal_df1)
# intercept_mfc = summary(model_mfc_cal)$coefficients[1,1]
slope_cal1 = summary(model_cal1)$coefficients[2,1]

cal_df2 = data.frame(y = c(0,hono_cal_conc_ch2),
                     x = c(dat2_cal$cal_zero_ch2,dat2_cal$cal_ch2))

model_cal2 = lm(y ~ x,cal_df2)
# intercept_mfc = summary(model_mfc_cal)$coefficients[1,1]
slope_cal2 = summary(model_cal2)$coefficients[2,1]


# Applying cal r2 ------------------------------------------------------------

dat2_calibrated = dat2_zeroed %>% 
  select(-c(zeroing,id,idx)) %>% 
  mutate(ch1_ppt = ch1_zeroed * slope_cal1,
         ch2_ppt = ch2_zeroed * slope_cal2,
         hono = ppt(ch1_ppt,ch2_ppt,sampling_efficiency),
         date = date - time_corr2,
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

dat2_calibrated %>% 
  filter(flag == 0) %>% 
  ggplot(aes(date,hono)) +
  geom_point()
