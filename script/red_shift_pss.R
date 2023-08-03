library(lubridate)
library(tidyverse)
library(openair)
library(janitor)
library(viridis)
library(zoo)

#for using the normalised jno2 photolysis rate constant to calculate HONO PSS
#now checking what HONO PSS looks like with normalised jo1d (blue shifted)

# Functions ---------------------------------------------------------------

min_max_norm <- function(x) {
  (x - min(x,na.rm = TRUE)) / (max(x,na.rm = TRUE) - min(x,na.rm = TRUE))
}

# Sorting spec rad --------------------------------------------------------

#using calculated j values for period when spec rad wasn't working
spec_rad = read.csv("data/Specrad_hour_23_with_calc.csv") %>% 
  mutate(date = dmy_hm(date),
         hour = hour(date)) %>% 
  clean_names() %>% 
  mutate(jhono = ifelse(is.na(j_hono),jhono_calc,j_hono),
         jhno3 = ifelse(is.na(j_hno3),jhno3_calc,j_hno3),
         jo1d = ifelse(is.na(j_o1d),jo1d_calc,j_o1d)) %>% 
  select(date,hour,jhono,jhno3,jo1d,jno2 = j_no2)

#calculating average j values for each hour
spec_rad_mean = spec_rad %>% 
  group_by(hour) %>% 
  summarise(jhono_avg = mean(jhono,na.rm = T),
            jhno3_avg = mean(jhno3,na.rm = T),
            jno2_avg = mean(jno2,na.rm = T),
            jo1d_avg = mean(jo1d,na.rm = T))

#replacing missing j values with hourly average calculated above
#normalising jno2 and jo1d to jhno3
#not sure why I didn't using interpolation? Will have to check which one is better?
spec_rad_full = left_join(spec_rad,spec_rad_mean,by = "hour") %>% 
  mutate(jhono = ifelse(is.na(jhono),jhono_avg,jhono),
         jhno3 = ifelse(is.na(jhno3),jhno3_avg,jhno3),
         jno2 = ifelse(is.na(jno2),jno2_avg,jno2),
         jo1d = ifelse(is.na(jo1d),jo1d_avg,jo1d),
         jno2_norm = min_max_norm(jno2),
         jno2_norm = jno2_norm * max(jhno3),
         jo1d_norm = min_max_norm(jo1d),
         jo1d_norm = jo1d_norm * max(jhno3)) %>% 
  select(-c(jhono_avg,jhno3_avg,jno2_avg,jo1d_avg))

spec_rad_full %>% 
  filter(date < "2023-03-01") %>%
  pivot_longer(c(jhno3,jo1d_norm,jno2_norm)) %>% 
  ggplot(aes(date,value,col = name)) +
  geom_path() +
  scale_color_viridis_d()
  # facet_grid(rows = vars(name),scales = "free_y") +
  NULL

# PSS ------------------------------------------------------------

kp = 3.3 * 10^-11 #rate constant for oh + no -> hono (from Atkinson et al. 2004)
kl = 6 * 10^-12 #rate constant for oh + hono -> h2o + no2 (from Atkinson et al. 2004)
dv = 0.3 #deardfroff velocity, value used by Simone

dat = read.csv("output/data/all_data.csv") %>% 
  mutate(date = ymd_hms(date)) %>% 
  select(-c(jhono,jhno3))

#hono = k[OH][NO]+ jHNO3 * f * pNO3 / jHONO + k[OH] + kdep

#units for calculation are molecule per cm3

#can change f and kdep and other parameters as needed and compare them

pss_calc = left_join(dat,spec_rad_full,by = "date") %>% 
  filter(campaign == "February 2023") %>% 
  mutate(hour = hour(date),
         lifetime = ifelse(hour >= 11 & hour <= 15,1/jhono,NA_real_),
         lifetime = na.approx(lifetime,na.rm = FALSE),
         nitrate = na.approx(nitrate,na.rm = FALSE),
         h = lifetime * dv,
         kdep1 = 0.01/h,
         no_molecules = no * 2.46 * 10^19 * 10^-12,
         pss = (kp*oh*no_molecules + (jhno3 * nitrate * 58)) / (jhono + (kl*oh) + kdep1) / (2.46 * 10^19 * 10^-12),
         pss_red = (kp*oh*no_molecules + (jno2_norm * nitrate * 58)) / (jhono + (kl*oh) + kdep1) / (2.46 * 10^19 * 10^-12),
         pss_blue = (kp*oh*no_molecules + (jo1d_norm * nitrate * 58)) / (jhono + (kl*oh) + kdep1) / (2.46 * 10^19 * 10^-12))


pss_calc %>% 
  rename(HONO = hono,PSS = pss,PSS_jno2 = pss_red,PSS_jo1d = pss_blue) %>% 
  pivot_longer(c(HONO,PSS,PSS_jno2,PSS_jo1d)) %>% 
  ggplot(aes(date,value,col = name)) +
  geom_path(size = 0.8) +
  scale_color_viridis_d() +
  # facet_grid(rows = vars(name),scale = "free_y") +
  theme_bw() +
  labs(x = "Datetime (UTC)",
       y = "HONO (ppt)",
       color = NULL) +
  theme(legend.position = "top") +
  NULL

ggsave('pss_jno2_timeseries.svg',
       path = "output/plots/red_shift",
       width = 30,
       height = 12,
       units = 'cm')

# Checking diurnals -------------------------------------------------------

diurnal = pss_calc %>% 
  filter(is.na(hono) == FALSE) %>% 
  rename(HONO = hono,PSS = pss,PSS_jno2 = pss_red,PSS_jo1d = pss_blue) %>% 
  timeVariation(pollutant = c("HONO","PSS","PSS_jno2","PSS_jo1d"))

diurnal_dat = diurnal$data$hour

diurnal_dat %>% 
  ggplot(aes(hour,Mean,col = variable)) +
  geom_path(size = 0.8) +
  scale_color_viridis_d() +
  theme_bw() +
  labs(x = "Hour of day (UTC)",
       y = "HONO (ppt)",
       color = NULL) +
  scale_x_continuous(breaks = c(0,4,8,12,16,20)) +
  # facet_grid(rows = vars(variable),scales = "free_y") +
  theme(legend.position = "top")

ggsave('diurnal_red_blue_shifted_pss.svg',
       path = "output/plots/red_shift",
       width = 30,
       height = 12,
       units = 'cm')


# Checking diurnal shape of individual elements ---------------------------

#very preliminary, just want to see what the shape is so normalising everything
#this will just be qualitative, not quantitative

normalised = pss_calc %>% 
  mutate(jhno3_norm = min_max_norm(jhno3) * max(hono,na.rm = TRUE),
         jhono_norm = min_max_norm(jhono) * max(hono,na.rm = TRUE),
         no_norm = min_max_norm(no) * max(hono,na.rm = TRUE),
         kdep1_norm = min_max_norm(kdep1) * max(hono,na.rm = TRUE),
         oh_norm = min_max_norm(oh) * max(hono,na.rm = TRUE))

normalised_diurnal = normalised %>% 
  filter(is.na(hono) == FALSE) %>% 
  timeVariation(pollutant = c("hono","pss","oh_norm"))

normalised_diurnal_dat = normalised_diurnal$data$hour

normalised_diurnal_dat %>% 
  ggplot(aes(hour,Mean,col = variable)) +
  geom_line(size = 1) +
  scale_color_viridis_d() +
  theme_bw() +
  labs(x = "Hour of day (UTC)",
       y = "HONO (ppt)",
       color = NULL) +
  scale_x_continuous(breaks = c(0,4,8,12,16,20)) +
  # facet_grid(rows = vars(variable),scales = "free_y") +
  theme(legend.position = "top")


#looking at production and loss mechanisms

prod_loss = pss_calc %>% 
  mutate(oh = oh / (2.46 * 10^19 * 10^-12), #convert to ppt
         nitrate = nitrate / (2.46 * 10^19 * 10^-12), #convert to ppt
         hono_photolysis = jhono * hono,
         hono_oh = kl * hono * oh,
         hono_deposition = kdep1 * hono,
         pno3_photolysis = jhno3 * 20 * nitrate,
         no_oh = kp * no * oh,
         loss = hono_photolysis + hono_deposition + hono_oh,
         production = pno3_photolysis + no_oh)


normalised_pl = prod_loss %>% 
  mutate(hono_photolysis = min_max_norm(hono_photolysis) * max(hono,na.rm = TRUE),
         hono_oh = min_max_norm(hono_oh) * max(hono,na.rm = TRUE),
         hono_deposition = min_max_norm(hono_deposition) * max(hono,na.rm = TRUE),
         pno3_photolysis = min_max_norm(pno3_photolysis) * max(hono,na.rm = TRUE),
         no_oh = min_max_norm(no_oh) * max(hono,na.rm = TRUE))

normalised_diurnal_pl = normalised_pl %>% 
  filter(is.na(hono) == FALSE) %>% 
  timeVariation(pollutant = c("hono","pss","no_oh"))

normalised_diurnal_dat = normalised_diurnal_pl$data$hour

normalised_diurnal_dat %>% 
  ggplot(aes(hour,Mean,col = variable)) +
  geom_line(size = 1) +
  scale_color_viridis_d() +
  theme_bw() +
  labs(x = "Hour of day (UTC)",
       y = "HONO (ppt)",
       color = NULL) +
  scale_x_continuous(breaks = c(0,4,8,12,16,20)) +
  # facet_grid(rows = vars(variable),scales = "free_y") +
  theme(legend.position = "top")
