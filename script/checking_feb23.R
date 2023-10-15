library(tidyverse)
library(lubridate)
library(openair)
library(janitor)
library(zoo)
library(viridis)

Sys.setenv(TZ = 'UTC')

# spec rad ----------------------------------------------------------------

spec_rad23 = read.csv("data/spec_rad/Specrad_hour_23_with_calc.csv") %>% 
  mutate(date = dmy_hm(date),
         hour = hour(date),
         doy = yday(date)) %>% 
  filter(date >= "2023-02-07 08:35" & date < "2023-02-27") %>% 
  clean_names() %>% 
  mutate(jhono = case_when(is.na(j_hono) ~ jhono_calc,
                           doy == 43 ~ NA_real_,
                           doy == 51 ~ NA_real_,
                           TRUE ~ j_hono),
         jhno3 = case_when(is.na(j_hno3) ~ jhno3_calc,
                           doy == 43 ~ NA_real_,
                           doy == 51 ~ NA_real_,
                           TRUE ~ j_hno3)) %>% 
  select(date,hour,jhono,jhno3)

#fill NAs with averages from hours where spec rad data is missing when reading data in
#find average jhono and jhno3 values for each hour
spec_rad_mean = spec_rad23 %>% 
  group_by(hour) %>% 
  summarise(jhono_avg = mean(jhono,na.rm = T),
            jhno3_avg = mean(jhno3,na.rm = T))

#replace NAs with average value for that hour
spec_rad23_corr = left_join(spec_rad23,spec_rad_mean,by = "hour") %>% 
  mutate(jhono = ifelse(is.na(jhono),jhono_avg,jhono),
         jhno3 = ifelse(is.na(jhno3),jhno3_avg,jhno3),
         date = date + 3600, #changing data to utc
  ) %>% 
  select(-c(jhono_avg,jhno3_avg))

# other data --------------------------------------------------------------

oh_dat = read.csv("data/OH_provisional.csv") %>% 
  clean_names() %>% 
  mutate(date = dmy_hm(time),
         oh = na.approx(oh_molecules_cm_3,na.rm = F)) %>% #interpolate missing values
  select(date,oh) %>% 
  timeAverage("1 hour")

nox23 = read.csv("data/nox_data/nox23.csv") %>% 
  mutate(date = ymd_hms(X)) %>% 
  filter(date > "2023-02-07" & date < "2023-02-27") %>% 
  timeAverage("5 min") %>%
  select(date,no = NO_Conc_art_corrected,no2 = NO2_Conc_diode)

hono23 = read.csv("output/data/hono23_unchanged_tz.csv") %>% 
  mutate(date = ymd_hms(date),
         date = date + 3600) %>% 
  select(date,hono)

df_list = list(hono23,nox23,oh_dat,spec_rad23_corr)

dat = df_list %>% reduce(full_join,by = "date") %>% 
  arrange(date)


# pss ---------------------------------------------------------------------

f_calc_all = dat %>%   
  filter(date >= "2023-02-07 08:35" & date < "2023-02-27",
         hour >= 11 & hour <= 15
  ) %>%
  mutate(lifetime = 1/jhono,
         h = lifetime * dv,
         kdep1 = 0.01/h,
         kdep3 = 0.03/h,
         no_molecules = no * 2.46 * 10^19 * 10^-12,
         production_without_nitrate = kp*oh*no_molecules,
         loss1 = jhono + (kl*oh) + kdep1,
         loss_hono1 = loss1 * hono * 2.46 * 10^19 * 10^-12,
         loss3 = jhono + (kl*oh) + kdep3,
         loss_hono3 = loss3 * hono * 2.46 * 10^19 * 10^-12,
         missing_production1 = loss_hono1 - production_without_nitrate,
         missing_production3 = loss_hono3 - production_without_nitrate)

#finding f - daytime median of missing production and jhno3 used
#specifically between 10 and 15 local time - 11 and 16 UTC

missing_production1 = mean(f_calc_all$missing_production1,na.rm = TRUE)
missing_production3 = mean(f_calc_all$missing_production3,na.rm = TRUE)
jhno3 = mean(f_calc_all$jhno3,na.rm = TRUE)
f1 = missing_production1/(nitrate*jhno3)
f3 = missing_production3/(nitrate*jhno3)

pss_calc = dat %>% 
  filter(date >= "2023-02-07 08:35" & date < "2023-02-27") %>% 
  mutate(hour = hour(date),
         lifetime = ifelse(hour >= 11 & hour <= 15,1/jhono,NA_real_),
         lifetime = na.approx(lifetime,na.rm = FALSE),
         nitrate = na.approx(nitrate,na.rm = FALSE),
         h = lifetime * dv,
         kdep1 = 0.01/h,
         # kdep3 = 0.03/h,
         no_molecules = no * 2.46 * 10^19 * 10^-12,
         pss = ((kp*oh*no_molecules + (jhno3 * nitrate * f1)) / (jhono + (kl*oh) + kdep1))
                         / (2.46 * 10^19 * 10^-12))

pss_calc %>% 
  rename('PSS' = pss,
         'Observed' = hono) %>%
  pivot_longer(c('PSS','Observed',jhono,jhno3)) %>%
  ggplot(aes(date,value,col = name)) +
  theme_bw() +
  labs(x = "Datetime (UTC)",
       y = "HONO (ppt)",
       color = NULL) +
  geom_path(size = 0.8) +
  theme(legend.position = "top") +
  scale_x_datetime(breaks = "1 day",date_labels = "%d/%m") +
  scale_color_viridis_d() +
  facet_grid(rows = vars(name),scales = "free") +
  # facet_wrap(vars(campaign),scales = "free", ncol =1) +
  NULL


diurnal = pss_calc %>% 
  filter(is.na(hono) == FALSE) %>% 
  mutate(hono_pre_cut = ifelse(date < "2023-02-18",hono,NA_real_),
         hono_post_cut = ifelse(date > "2023-02-19",hono,NA_real_)) %>% 
  rename(HONO = hono,PSS = pss) %>% 
  timeVariation(pollutant = c("hono_pre_cut","hono_post_cut","HONO","PSS","jhono","jhno3"))

diurnal_dat = diurnal$data$hour

diurnal_dat %>% 
  ggplot(aes(hour,Mean,col = variable)) +
  geom_line(size = 1) +
  scale_color_viridis_d() +
  theme_bw() +
  labs(x = "Hour of day (UTC)",
       y = "HONO (ppt)",
       color = NULL) +
  scale_x_continuous(breaks = c(0:23)) +
  # facet_grid(rows = vars(variable),scales = "free") +
  # ylim(-1.5,13) + #in order to have same sized axes for diurnals for all three campaigns
  theme(legend.position = "top")
