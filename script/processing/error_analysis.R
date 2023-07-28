remove(despiking,final_dat_night,final_dat1,final_dat2,final_dat3,hono_dat,night_avg,night_flagged,night_zeroing,nights,no_noise,raw_dat1,raw_dat2,raw_dat3,wip_dat1,wip_dat2,wip_dat3,cal0.5_ch1,cal0.5_ch2,cal2_ch1,cal2_ch2,date_corr2,date_corr3,se)

#using despiked data from processing in r script

dat = despiked_dat %>% 
  ungroup() %>% 
  select(date,ch1,ch2,ch1_ppt,ch2_ppt,hono,zeroing,night_flag,reagents,flag,instrumental_noise_flag) %>% 
  mutate(low_quality_flag = case_when(between(date,as.POSIXct("2023-02-19"),as.POSIXct("2023-02-21")) ~ 1, #not properly zeroed
                                      date > "2023-02-24 08:41" ~ 1, #not properly zeroed
                                      date < "2023-02-09 12:00" ~ 1, #not properly calibrated
                                      TRUE ~ 0))

#want to get the detection limit
#in excel it is calculated by:
#1. getting 2*SD for both channels during a "typical zero" (before calibration is applied)
#2. error propagation: LOD = ((SD_1 *cal)^2 + (SD_2 *cal)^2) ^0.5
#3. interpolating LOD to use for final error calculations with final values
# measurements * 0.1 + LOD
#0.1 is the relative error the LOPAP manufacturers recommend using (instead of calculating errors in flow
#rates and calibrations)
#but that calculation is definitely not how you do error propagation? Ugh I don't know and won't be able to
#justify it really

dat1 = dat %>%  
  select(-c(ch1_ppt,ch2_ppt))

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
  filter(flag == 0,
         instrumental_noise_flag == 0) %>% 
  pivot_longer(c(lod,error,hono)) %>% 
  mutate(reagents = as.character(reagents)) %>% 
  ggplot(aes(date,value,col = reagents)) +
  facet_grid(rows = vars(name),scales = "free_y") +
  geom_point() +
  scale_color_viridis(discrete = "TRUE")

lod = median(dat1_1$lod,na.rm = TRUE)


#I don't like just using an arbitrary zero as representative of all zeroes, so instead I have grouped all
#zero measurements by cycle and calculated the standard deviation, then interpolated between zero cycles
#then multiplied by 2 and by the cal values to get 2 sigma in ppt
#then got the lod by error propagation across the two channels
#next step would be to get the total error by adding the relative error

