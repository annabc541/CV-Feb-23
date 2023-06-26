
#using despiked data from processing in r script

dat = despiked_dat %>% 
  ungroup() %>% 
  select(date,ch1,ch2,ch1_ppt,ch2_ppt,hono,zeroing,night_flag) %>% 
  mutate(low_quality_flag = case_when(between(date,as.POSIXct("2023-02-19"),as.POSIXct("2023-02-21")) ~ 1, #not properly zeroed
                                      date > "2023-02-24 08:41" ~ 1, #not properly zeroed
                                      date < "2023-02-09 12:00" ~ 1, #not properly calibrated
                                      TRUE ~ 0),
         zeroing = ifelse(zeroing == 2,0,zeroing))

#want to get the detection limit
#in excel it is calculated by:
#1. getting 2*SD for both channels during a "typical zero" (before calibration is applied)
#2. error propagation: LOD = ((SD_1 *cal)^2 + (SD_2 *cal)^2) ^0.5

dat1 = dat %>% filter(date > "2023-02-10 12:00" & date < "2023-02-17") %>% 
  select(-c(low_quality_flag,night_flag))

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

dat1_1 = dat1 %>% 
  mutate(idx = 1:nrow(.)) %>% 
  left_join(errors, "idx") %>% 
  mutate(sd1 = na.approx(sd1,na.rm = F),
         sd2 = na.approx(sd2,na.rm = F),
         sd1 = 2* sd1 * cal1_ch1,
         sd2 = 2* sd2 * cal1_ch2,
         lod = (sd1^2 + sd2^2)^0.5)

dat1_1 %>% 
  pivot_longer(c(sd1,sd2,lod)) %>% 
  ggplot(aes(date,value,col = name)) +
  facet_grid(rows = vars(name)) +
  geom_path()

lod = median(dat1_1$lod,na.rm = TRUE)

#I don't like just using an arbitrary zero as representative of all zeroes, so instead I have grouped all
#zero measurements by cycle and calculated the standard deviation, then interpolated between zero cycles
#then multiplied by 2 and by the cal values to get 2 sigma in ppt
#then got the lod by error propagation across the two channels
#next step would be to get the total error by adding the relative error

