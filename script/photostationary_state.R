library(tidyverse)
library(lubridate)
library(openair)
library(janitor)
library(zoo)
library(viridis)

#defining constant values used for PSS calculations

kp = 3.3 * 10^-11 #rate constant for oh + no -> hono (from Atkinson et al. 2004)
kl = 6 * 10^-12 #rate constant for oh + hono -> h2o + no2 (from Atkinson et al. 2004)
dv = 0.3 #deardfroff velocity, value used by Simone

#uncertainy quantities

#dep_velocity = 0.03 #hono deposition velocity, Simone used 3 cms-1, converting to meters for units
#nb Tudor Hill paper uses dep velocity = 1 cms-1 and that's a "less controversial" value to use
#f = 70 #average enhancement factor found in Simone's paper
#calculated daytime enhancement factor (if I calculated it correctly) is either 60 (dep velocity = 0.03)
#or 57 (dep velocity = 0.01)

# Historical nitrate values in February -----------------------------------

nitrate_dat = read.csv("data/nitrate_ammonium_CVAO_12-19.csv") %>% 
  clean_names() %>%
  mutate(date = mdy_hm(start_local_time),
         month = month(date),
         year = year(date)) %>% 
  filter(month == 2) %>% 
  group_by(year) %>% 
  summarise(nitrate = mean(nitrate_mg_m))

nitrate = mean(nitrate_dat$nitrate) #in micrograms per meter cubed
#multiplied by 10^-12 to convert to g and to convert to cm-3 (10^-6 for both)
#divided by nitrate molar mass 62.004 g mol-1
#multiplied by Avogadro's number -> 6.022 * 10^23 molecule mol-1
nitrate = (nitrate * 10^-12 *6.022 * 10^23)/62.004 #molecule cm-3

remove(nitrate_dat)

# Importing data ----------------------------------------------------------

dat = read.csv("output/data/data_for_pss.csv") %>% 
  mutate(date = dmy_hm(date))

# Photostationary state calculations --------------------------------------

#hono = k[OH][NO]+ jHNO3 * f * pNO3 / jHONO + k[OH] + kdep

#units for calculation are molecule per cm3

#can change f and kdep and other parameters as needed and compare them

pss_calc = dat %>% 
  rename(measured = hono) %>%
  mutate(lifetime = ifelse(hour >= 11 & hour <= 15,1/jhono,NA_real_),
         lifetime = na.approx(lifetime,na.rm = FALSE),
         h = lifetime * dv,
         kdep1 = 0.01/h,
         kdep3 = 0.03/h,
         no_molecules = no * 2.46 * 10^19 * 10^-12,
         pss57 = ((kp*oh*no_molecules + (jhno3 * nitrate * 57)) / (jhono + (kl*oh) + kdep1)) 
         / (2.46 * 10^19 * 10^-12),
         pss60 = ((kp*oh*no_molecules + (jhno3 * nitrate * 60)) / (jhono + (kl*oh) + kdep3))
         / (2.46 * 10^19 * 10^-12),
         pss70_1 = ((kp*oh*no_molecules + (jhno3 * nitrate * 70)) / (jhono + (kl*oh) + kdep1))
         / (2.46 * 10^19 * 10^-12),
         pss70_3 = ((kp*oh*no_molecules + (jhno3 * nitrate * 70)) / (jhono + (kl*oh) + kdep3))
         / (2.46 * 10^19 * 10^-12),
         # without_nitrate = ( kp*oh*no_molecules) / (jhono + (kl*oh) + kdep1)
         # / (2.46 * 10^19 * 10^-12),
         # without_enhancement = (kp*oh*no_molecules + (jhno3 * nitrate)) / (jhono + (kl*oh) + kdep1)
         # / (2.46 * 10^19 * 10^-12),
         )

pss_calc %>% 
  filter(date > "2023-02-06" & date < "2023-02-27") %>%
  rename("Observed" = measured,"Dep velocity = 1" = pss57, "Dep velocity = 3" = pss60) %>%
  pivot_longer(c("Observed","Dep velocity = 3","Dep velocity = 1")) %>%
  # pivot_longer(c(measured,pss57,pss60,pss70_1,pss70_3)) %>% 
  ggplot(aes(date,value,col = name)) +
  # facet_grid(rows = vars(name)) +
  # facet_grid(rows = vars(factor(name,levels=c("missing_production","pss","f"))),
  #            scales = "free_y") +
  labs(x = "Date (UTC)",
       y = "HONO (ppt)",
       col = NULL) +
  scale_color_manual(values = viridis(3)) +
  theme_bw() +
  theme(legend.position = "top") +
  scale_x_datetime(date_breaks = "1 day",date_labels = "%d/%m") +
  geom_path(size = 0.8)

last_plot() + aes(group=rev(name)) #changes what colour is in front in the plot

ggsave('f_calc.svg',
       path = "output/plots/leeds_meeting",
       width = 30,
       height = 12,
       units = 'cm')


# Diurnals ----------------------------------------------------------------

diurnal = pss_calc %>% 
  filter(is.na(measured) == FALSE) %>% 
  rename("Observed" = measured,"f = 57" = pss57,"f = 60" = pss60,"Dep velocity = 1" = pss70_1,"Dep velocity = 3" = pss70_3) %>%
  timeVariation(pollutant = c("Observed","f = 57","f = 60","Dep velocity = 1","Dep velocity = 3"))

diurnal_dat = diurnal$data$hour

diurnal_dat %>% 
  ggplot(aes(hour,Mean,col = variable)) +
  geom_line(size = 1) +
  # facet_grid(cols = vars(variable),scales = "free_y") +
  scale_color_manual(values = viridis(5)) +
  theme_bw() +
  labs(x = "Hour of day (UTC)",
       y = "HONO (ppt)",
       color = NULL) +
  scale_x_continuous(breaks = c(0,4,8,12,16,20)) +
  # facet_grid(rows = vars(factor(variable,levels=c("measured","without_nitrate","without_enhancement"))),
  #            scales = "free_y") +
  theme(legend.position = "top")

ggsave('diurnals.svg',
       path = "output/plots/leeds_meeting",
       width = 30,
       height = 12,
       units = 'cm')


# Calculating enhancement factor ------------------------------------------

#can change anything in code below to find f for different parameters

#for kdep = 0.01 f = 57
#for kdep = 0.03 f = 61

f_calc = dat %>% 
  filter(date > "2023-02-06" & date < "2023-02-27",
         hour >= 11 & hour <= 15) %>%
  mutate(lifetime = 1/jhono,
         h = lifetime * dv,
         kdep = 0.01/h, #change as needed
         no_molecules = no * 2.46 * 10^19 * 10^-12,
         production_without_nitrate = kp*oh*no_molecules,
         loss = jhono + (kl*oh) + kdep,
         loss_hono = loss * hono * 2.46 * 10^19 * 10^-12,
         missing_production = loss_hono - production_without_nitrate)

#finding f - daytime median of missing production and jhno3 used
#specifically between 10 and 15 local time - 11 and 16 UTC
missing_production = mean(f_calc$missing_production,na.rm = TRUE)
jhno3 = mean(f_calc$jhno3,na.rm = TRUE)
f = missing_production/(nitrate*jhno3)

# Calculating median loss and production through various pathways ---------

loss_production = dat %>% 
  mutate(hour = hour(date)) %>% 
  filter(date > "2023-02-06" & date < "2023-02-27",
         hour > 9 & hour < 16) %>%
  mutate(lifetime = ifelse(hour > 10 & hour < 15,1/jhono,NA_real_),
         lifetime = na.approx(lifetime,na.rm = FALSE),
         h = lifetime * dv,
         kdep1 = 0.01/h,
         kdep3 = 0.03/h,
         no_molecules = no * 2.46 * 10^19 * 10^-12,
         production_without_nitrate = (kp*oh*no_molecules)/(2.46 * 10^19 * 10^-12),
         loss_photolysis = jhono * hono,
         loss_reaction = (kl*oh*hono* 2.46 * 10^19 * 10^-12)/(2.46 * 10^19 * 10^-12),
         loss_deposition3 = kdep3 * hono,
         loss_deposition1 = kdep1 * hono)

loss_photolysis = median(loss_production$loss_photolysis,na.rm =TRUE)
loss_reaction = median(loss_production$loss_reaction,na.rm =TRUE)
loss_deposition1 = median(loss_production$loss_deposition1,na.rm =TRUE)
loss_deposition3 = median(loss_production$loss_deposition3,na.rm =TRUE)
production = median(loss_production$production_without_nitrate,na.rm =TRUE)

# Not in use atm ----------------------------------------------------------

#importing all the data individually and then creating one dataframe

spec_rad = read.csv("data/Specrad_hour_23_with_calc.csv") %>% 
  mutate(date = dmy_hm(date),
         hour = hour(date)) %>% 
  clean_names() %>% 
  mutate(jhono = ifelse(is.na(j_hono),jhono_calc,j_hono),
         jhno3 = ifelse(is.na(j_hno3),jhno3_calc,j_hno3)) %>% 
  select(date,hour,jhono,jhno3)

#to remove NAs in jhono and jhno3 values

#find average jhono and jhno3 values for each hour
spec_rad_mean = spec_rad %>% 
  group_by(hour) %>% 
  summarise(jhono_avg = mean(jhono,na.rm = T),
            jhno3_avg = mean(jhno3,na.rm = T))

#replace NAs with average value for that hour
spec_rad = left_join(spec_rad,spec_rad_mean,by = "hour") %>% 
  mutate(jhono = ifelse(is.na(jhono),jhono_avg,jhono),
         jhno3 = ifelse(is.na(jhno3),jhno3_avg,jhno3)) %>% 
  select(-c(jhono_avg,jhno3_avg))

oh_dat = read.csv("data/OH_provisional.csv") %>% 
  clean_names() %>% 
  mutate(date = dmy_hm(time),
         oh = na.approx(oh_molecules_cm_3,na.rm = F)) %>% #interpolate missing values
  select(date,oh) %>% 
  timeAverage("1 hour")

nox_dat = read.csv("data/nox_data/nox23.csv") %>% 
  mutate(date = ymd_hms(X)) %>% 
  filter(date > "2023-02-01" & date < "2023-03-01") %>% 
  select(date,no = NO_Conc_art_corrected) %>%  
  timeAverage("1 hour")

hono_dat = read.csv("output/data/processed_in_r2.csv") %>% 
  mutate(date = ymd_hms(date)) %>% 
  select(date,hono) %>% 
  timeAverage("1 hour")

df_list = list(nox_dat,oh_dat,hono_dat,spec_rad)
dat = df_list %>% reduce(full_join,by = "date")
write.csv(dat,"output/data/data_for_pss.csv",row.names = FALSE) #saving as .csv

#looking at the deposition rate and figuring out which hours should be used to calculate it
#currently using 10 to 15 (which in UTC time is 11 to 16)

#plotting the spread of h, HONO lifetime and kdep for those hours
spec_rad %>% 
  mutate(hour = hour(date),
         lifetime = ifelse(hour >= 11 & hour <= 15,1/jhono,NA_real_),
         lifetime = na.approx(lifetime,na.rm = F),
         h = lifetime * 0.3,
         kdep = 3*10^-2/h) %>% 
  filter(date < "2023-03-01",
         hour >= 11 & hour <= 15) %>% 
  mutate(hour = as.character(hour)) %>% 
  pivot_longer(c(lifetime,h,kdep)) %>% 
  ggplot(aes(date,value,col = hour)) +
  # geom_path(aes(date,value)) +
  geom_point() +
  labs(x = NULL,
       y = NULL) +
  theme_bw() +
  facet_grid(rows = vars(name),scales = "free_y")

ggsave('kdep.png',
       path = "output/plots/pss",
       width = 30,
       height = 12,
       units = 'cm')

#looking at how big an effect the choice of hours for kdep has
#seeing what happens if I set kdep to zero outside afternoon hours -> don't do this
pss_calc = pss %>% 
  rename(measured = hono) %>%
  mutate(hour = hour(date),
         lifetime = ifelse(hour > 10 & hour < 15,1/jhono,NA_real_),
         lifetime = na.approx(lifetime,na.rm = FALSE),
         h = lifetime * dv,
         kdep1 = ifelse(hour >= 11 & hour <= 16,3*10^-2/h,0),
         kdep3 = dep_velocity/h,
         no_molecules = no * 2.46 * 10^19 * 10^-12,
         # without_nitrate = ( kp*oh*no_molecules) / (jhono + (kl*oh) + kdep),
         # without_enhancement = (kp*oh*no_molecules + (jhno3 * nitrate)) / (jhono + (kl*oh) + kdep),
         pss1 = (kp*oh*no_molecules + (jhno3 * nitrate * f)) / (jhono + (kl*oh) + kdep1),
         pss2 = (kp*oh*no_molecules + (jhno3 * nitrate * f)) / (jhono + (kl*oh) + kdep3),
         # without_nitrate = without_nitrate / (2.46 * 10^19 * 10^-12),
         # without_enhancement = without_enhancement / (2.46 * 10^19 * 10^-12),
         pss1 = pss1/ (2.46 * 10^19 * 10^-12),
         pss2 = pss2/ (2.46 * 10^19 * 10^-12))

pss_calc %>% pivot_longer(c(pss1,pss2)) %>% 
  filter(date < "2023-02-25" & date > "2023-02-07") %>% 
  ggplot(aes(date,value,col = name)) +
  geom_path() +
  # facet_grid(rows = vars(name),scales = "free_y")
  NULL


#finding r squared for plot of missing production source vs jhno3 (units are ppt h-1)
model <- lm(finding_f$missing_production_ppt_h~finding_f$jhno3_h)
lm(formula = finding_f$missing_production_ppt_h~finding_f$jhno3_h)
summary(model)