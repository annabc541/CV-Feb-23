library(lubridate)
library(tidyverse)
library(openair)
library(janitor)
library(viridis)
library(zoo)

#for comparing the different photolysis rate constants to each other

# Functions ---------------------------------------------------------------

min_max_norm <- function(x) {
  (x - min(x,na.rm = TRUE)) / (max(x,na.rm = TRUE) - min(x,na.rm = TRUE))
}

# Sorting spec rad --------------------------------------------------------

#read in spec rad data
spec_rad = read.csv("data/Specrad_hour_23_with_calc.csv") %>% 
  mutate(date = dmy_hm(date),
         hour = hour(date)) %>% 
  clean_names() %>% 
  mutate(jhono = ifelse(is.na(j_hono),jhono_calc,j_hono),
         jhno3 = ifelse(is.na(j_hno3),jhno3_calc,j_hno3),
         jo1d = ifelse(is.na(j_o1d),jo1d_calc,j_o1d)) %>% 
  select(date,hour,jhono,jhno3,jo1d,jno2 = j_no2,jo3p = j_o3p,jh2o2 = j_h2o2)

#fill missing spec rad data with average hourly values
spec_rad_mean = spec_rad %>% 
  group_by(hour) %>% 
  summarise(jhono_avg = mean(jhono,na.rm = T),
            jhno3_avg = mean(jhno3,na.rm = T),
            jno2_avg = mean(jno2,na.rm = T),
            jo1d_avg = mean(jo1d,na.rm = T),
            jo3p_avg = mean(jo3p,na.rm = T),
            jh2o2_avg = mean(jh2o2,na.rm = T))

spec_rad_full = left_join(spec_rad,spec_rad_mean,by = "hour") %>%
  ungroup() %>% 
  mutate(jhono = ifelse(is.na(jhono),jhono_avg,jhono),
         jhno3 = ifelse(is.na(jhno3),jhno3_avg,jhno3),
         jno2 = ifelse(is.na(jno2),jno2_avg,jno2),
         jo1d = ifelse(is.na(jo1d),jo1d_avg,jo1d),
         jo3p = ifelse(is.na(jo3p),jo3p_avg,jo3p),
         jh2o2 = ifelse(is.na(jh2o2),jh2o2_avg,jh2o2)) %>% 
  select(-c(jhono_avg,jhno3_avg,jno2_avg,jo1d_avg,jo3p_avg,jh2o2_avg))



# Photolysis rates --------------------------------------------------------

#normalise desired photolysis rates, so they can all be compared on one plot
date = spec_rad_full %>% select(date)%>% 
  mutate(index = 1:nrow(.))

spec_rad_norm = as.data.frame(lapply(spec_rad_full[3:8], min_max_norm)) %>% 
  mutate(index = 1:nrow(.))

test = left_join(date,spec_rad_norm) %>% 
  select(-index)
  
test %>% 
  pivot_longer(c(jhno3,jo1d,jhono,jo1d,jo3p,jh2o2,jno2)) %>% 
  ggplot(aes(date,value,col = name)) +
  geom_path() +
  scale_color_viridis_d()
# facet_grid(rows = vars(name),scales = "free_y") +
NULL

diurnal = test %>% 
  timeVariation(pollutant = c("jhno3","jo1d","jno2"))

diurnal_dat = diurnal$data$hour

diurnal_dat %>% 
  ggplot(aes(hour,Mean,col = variable)) +
  geom_line(size = 1) +
  scale_color_viridis_d() +
  theme_bw() +
  labs(x = "Hour of day (UTC)",
       y = "Normalised j",
       color = NULL) +
  scale_x_continuous(breaks = c(0,4,8,12,16,20)) +
  # facet_grid(rows = vars(variable),scales = "free_y") +
  theme(legend.position = "top")

ggsave('normalised_photolysis_rates_limited.svg',
       path = "output/plots/red_shift",
       width = 30,
       height = 12,
       units = 'cm')