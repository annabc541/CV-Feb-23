library(tidyverse)
library(lubridate)
library(openair)
library(janitor)
library(zoo)
library(viridis)

kp = 3.3 * 10^-11 #rate constant for oh + no -> hono (from Atkinson et al. 2004)
kl = 6 * 10^-12 #rate constant for oh + hono -> h2o + no2 (from Atkinson et al. 2004)
dep_velocity = 0.03 #hono deposition velocity, Simone used 3 cms-1, converting to meters for units
dv = 0.3 #deardfroff velocity, value used by Simone
# kdep = dep_velocity/440
f = 70 #average enhancement factor found in Simone's paper

# Importing data ----------------------------------------------------------

spec_rad = read.csv("data/Specrad_hour_23_with_calc.csv") %>% 
  mutate(date = dmy_hm(date)) %>% 
  clean_names() %>% 
  mutate(jhono = ifelse(is.na(j_hono),jhono_calc,j_hono),
         jhno3 = ifelse(is.na(j_hno3),jhno3_calc,j_hno3)) %>% 
  select(date,jhono,jhno3)%>% 
  timeAverage("1 hour")

oh_data = read.csv("data/OH_provisional.csv") %>% 
  clean_names() %>% 
  mutate(date = dmy_hm(time)) %>% 
  select(date,oh = oh_molecules_cm_3) %>% 
  timeAverage("1 hour")

nox_dat = read.csv("data/nox_data/nox23.csv") %>% 
  mutate(date = ymd_hms(X)) %>% 
  filter(date > "2023-02-01" & date < "2023-03-01") %>% 
  select(date,no = NO_Conc_art_corrected) %>% 
  timeAverage("1 hour")

hono_dat = read.csv("output/data/hono_2023.csv") %>% 
  mutate(date = ymd_hms(date)) %>% 
  select(date,hono) %>% 
  timeAverage("1 hour")


# Looking at kdep ---------------------------------------------------------

spec_rad %>% 
  filter(date < "2023-03-01") %>% 
  mutate(hour = hour(date),
         lifetime1 = ifelse(hour > 10 & hour < 12,1/jhono,NA_real_),
         lifetime = na.approx(lifetime1,na.rm = F),
         h = lifetime * 0.3,
         kdep = 3*10^-2/h,
         flag = ifelse(is.na(lifetime1),"yes","no")) %>% 
  pivot_longer(c(lifetime,h,kdep)) %>% 
  ggplot(aes(date,value,col = flag)) +
  # geom_path(aes(date,value)) +
  geom_point() +
  labs(x = NULL,
       y = NULL,
       col = NULL) +
  theme_bw() +
  theme(legend.position = "None") +
  facet_grid(rows = vars(name),scales = "free_y") +
  scale_color_manual(values = c('red',"black"))


ggsave('kdep.png',
       path = "output/plots/pss",
       width = 30,
       height = 12,
       units = 'cm')

# Photostationary state calculations --------------------------------------

#hono = k[OH][NO]+ jHNO3 * f / jHONO + k[OH] + kdep

df_list = list(spec_rad,nox_dat,oh_data,hono_dat)

pss = df_list %>% reduce(full_join,by = "date")

pss_calc = pss %>% 
  mutate(hour = hour(date),
         lifetime = ifelse(hour > 11 & hour < 13,1/jhono,NA_real_),
         lifetime2 = na.approx(lifetime,na.rm = F),
         h = lifetime2 * dv,
         kdep = dep_velocity/h,
         # kdep = dep_velocity/h,
         no_molecules = no * 2.46 * 10^19 * 10^-12,
         hono_pss_m = ((kp*oh*no_molecules) + (jhno3 * f)) / (jhono + (kl*oh) + kdep),
         hono_pss_m1 = ((kp*oh*no_molecules)) / (jhono + (kl*oh) + kdep),
         hono_pss_nitrate_m = ((kp*oh*no_molecules) + (jhno3 * f * 1.02 * 6.022*10^23*10^-9*10^-5)) / (jhono + (kl*oh) + kdep),
         hono_pss1 = hono_pss_nitrate_m / (2.46 * 10^19 * 10^-12))

pss_calc %>% 
  filter(date > "2023-02-06" & date < "2023-02-27") %>%
  pivot_longer(c(hono,hono_pss1)) %>%
  ggplot(aes(date,value)) +
  facet_grid(rows = vars(name),scales = "free_y") +
  labs(x = "Date (UTC)",
       y = "HONO / ppt") +
  theme_bw() +
  geom_path()

ggsave('pss_without_nitrate.png',
       path = "output/plots/pss",
       width = 30,
       height = 12,
       units = 'cm')


# Diurnals ----------------------------------------------------------------

diurnal = pss_calc %>% 
  filter(is.na(hono) == FALSE) %>% 
  timeVariation(pollutant = c("hono","hono_pss1"))

diurnal_dat = diurnal$data$hour

diurnal_dat %>% 
  ggplot(aes(hour,Mean,col = variable)) +
  geom_line(size = 1) +
  # facet_grid(cols = vars(variable),scales = "free_y") +
  scale_color_manual(values = viridis(2)) +
  theme_bw() +
  labs(x = "Hour of day (UTC)",
       y = "HONO / ppt",
       color = NULL) +
  scale_x_continuous(breaks = c(0,4,8,12,16,20)) +
  # facet_grid(rows = vars(variable),scales = "free_y") +
  theme(legend.position = "top")

ggsave('hono_honopss2.png',
       path = "output/plots/diurnals",
       width = 11,
       height = 13,
       units = 'cm')

# Not in use atm ----------------------------------------------------------



checking = pss_calc %>% 
  mutate(hono_lifetime_m = hono_lifetime / 60,
         hour = hour(date)) %>% 
  filter(hour > 9 & hour < 14,
         hour != 0) %>%
  select(date,hour,hono_lifetime_m,h)


checking %>%
  filter(date > "2023-02-06" & date < "2023-02-15") %>% 
  # filter(hono_lifetime_m < 10^7) %>% 
  ggplot(aes(date,h)) + geom_point() +
  scale_x_datetime(date_breaks = "1 day",date_labels = "%d/%m")


# other_calcs = pss %>% 
#   mutate(l_photolysis = jhono,
#          l_hono_oh = kl * oh,
#          l_deposition = kdep,
#          hour = hour(date)) %>% 
#   filter(hour != 0,
#          hour > 9 & hour < 17)
# 
# mean_photolysis = mean(other_calcs$l_photolysis,na.rm = TRUE)
# mean_hono_oh = mean(other_calcs$l_hono_oh,na.rm = TRUE)
# mean_deposition = mean(other_calcs$l_deposition,na.rm = TRUE)
# 
# other_calcs %>% 
#   pivot_longer(c(l_photolysis,l_hono_oh,l_deposition)) %>% 
#   ggplot(aes(date,value,col = name)) +
#   facet_grid(rows = vars(name),scales = "free_y") +
#   geom_point()