library(tidyverse)
library(zoo)
library(openair)

Sys.setenv(TZ = "UTC")
#data is in BST (UTC +1)

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
#need to check this again
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

raw_dat = read.table("data/hono_gas_source_tests/test5.dat",skip = 4,header = TRUE,sep = ";") %>% 
  mutate(Date = gsub("\\.","-",Date),
         date = paste(Date,Time),
         date = dmy_hms(date)) %>% 
  select(date,ch1 = X550.6,ch2 = X550.0)

# Zeroing -----------------------------------------------------------------

zero_flag = raw_dat %>% 
  mutate(zeroing = case_when(
                             # date > "2024-04-24 16:15" & date < "2024-04-24 17:06" ~ 1, #not stabilised
                             date > "2024-04-25 08:55" & date < "2024-04-25 09:02" ~ 1, #stable
                             date > "2024-04-25 14:25" & date < "2024-04-25 15:13" ~ 1, #pre-cal
                             # date > "2024-04-25 15:45" & date < "2024-04-25 16:02" ~ 1, #post-cal
                             date > "2024-04-26 08:34" & date < "2024-04-26 08:49" ~ 1,
                             date > "2024-04-26 15:12" & date < "2024-04-26 15:37" ~ 1, #bump in zero
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
zeroed = zero_flag %>% 
  mutate(idx = 1:nrow(.)) %>% 
  left_join(zero_avg) %>% 
  mutate(ch1_zeroes = na.approx(ch1_zeroes,na.rm = F),
         ch2_zeroes = na.approx(ch2_zeroes,na.rm = F)) %>% 
  fill(ch1_zeroes,ch2_zeroes,.direction = "up") %>% 
  fill(ch1_zero_3sd,ch2_zero_3sd,.direction = "downup") %>% 
  mutate(ch1_zeroed = ch1 - ch1_zeroes,
         ch2_zeroed = ch2 - ch2_zeroes)

# Calibration -------------------------------------------------------------

#cal parameters
time_corr = 16.25 * 60
#still need to measure this and it will be different since the pump is now set to 20 rather than 10
liquid_flow1 = 5/14.77 #took 14:46 min to fill up 5ml volumetric flask
liquid_flow2 = 5/21.50 #took 21:30 min to fill up 5ml volumetric flask
conc_cal = 1000/100000 #standard conc / dilution factor
hono_cal_conc_ch1 = conc_cal/1000 * liquid_flow1/1000 /46*6.022*10^23/(2.46*10^19* actual_gas_flow) * 10^12
hono_cal_conc_ch2 = conc_cal/1000 * liquid_flow2/1000 /46*6.022*10^23/(2.46*10^19* actual_gas_flow) * 10^12

cal = zeroed %>% 
  select(date,ch1_zeroed,ch2_zeroed) %>% 
  filter(date > "2024-04-25 15:00" & date < "2024-04-25 16:00") %>%
  mutate(cal_zero_ch1 = ifelse(between(date,as.POSIXct("2024-04-25 15:00"),as.POSIXct("2024-04-25 15:10")),ch1_zeroed,NA),
         cal_zero_ch2 = ifelse(between(date,as.POSIXct("2024-04-25 15:00"),as.POSIXct("2024-04-25 15:10")),ch2_zeroed,NA),
         cal_ch1 = ifelse(between(date,as.POSIXct("2024-04-25 15:21"),as.POSIXct("2024-04-25 15:32")),ch1_zeroed,NA),
         cal_ch2 = ifelse(between(date,as.POSIXct("2024-04-25 15:21"),as.POSIXct("2024-04-25 15:32")),ch2_zeroed,NA)) %>% 
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


# Applying cal 1 ----------------------------------------------------------

#good data, measuring ambient 0
#good data, measuring HONO gas source 1
#cals,zeroing,water,other issues 2

dat = zeroed %>% 
  select(-c(zeroing,id,idx)) %>% 
  mutate(ch1_ppt = ch1_zeroed * slope_cal1,
         ch2_ppt = ch2_zeroed * slope_cal2,
         hono = ppt(ch1_ppt,ch2_ppt,sampling_efficiency),
         hono_lod = lod(ch1_zero_3sd,ch2_zero_3sd,slope_cal1,slope_cal2),
         # hono_err = abs(hono * rel_error/100 + hono_lod),
         date = date - time_corr,
         flag = case_when(date < "2024-04-24 17:00" ~ 2, #measurements with correct reagents and spectra started at 17:00, remove data before then
                          date > "2024-04-24 17:00" & date < "2024-04-25 08:15" ~ 0,
                          date > "2024-04-25 08:15" & date < "2024-04-25 10:36" ~ 2,
                          date > "2024-04-25 10:36" & date < "2024-04-25 13:50" ~ 1,
                          date > "2024-04-25 13:50" & date < "2024-04-25 16:00" ~ 2,#cal
                          date > "2024-04-25 16:00" & date < "2024-04-26 07:50" ~ 1,
                          date > "2024-04-26 07:50" & date < "2024-04-26 08:40" ~ 2,
                          date > "2024-04-26 08:40" & date < "2024-04-26 14:40" ~ 1,
                          date > "2024-04-26 14:40" ~ 2))

dat %>% 
  mutate(hono = ifelse(flag == 1,hono,NA_real_)) %>%
  filter(date > "2024-04-25 17:00" & date < "2024-04-26 15:00") %>% 
  # timeAverage("5 min") %>%
  # pivot_longer(c(hono,ch1_zeroed,ch2_zeroed)) %>% 
  ggplot(aes(date,hono)) +
  theme_bw() +
  geom_path() +
  labs(x = NULL,
       y = "HONO / ppt") +
  scale_x_datetime(breaks = "2 hours",date_labels = "%b %d %H:%M")

ggsave('lopap_hono_gas_source.svg',
       path = "output/plots/hono_gas_source_tests",
       width = 30,
       height = 12.7,
       units = 'cm')
