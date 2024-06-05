library(tidyverse)
library(lubridate)

#measured data
aerosols23 = read.csv("data/aerosol_data/cvao_aerosols23.csv") %>% 
  mutate(date = dmy_hm(Start)) %>%
  clean_names() %>% 
  select(date,measured_nitrate = nitrate)

#nitrate data for Feb 2023 from machine learning
nitrate_dat_ml = read.csv("data/aerosol_data/CVAO_Nitrate_Prediction_Feb2023.csv") %>%
  mutate(date = ymd(date)) %>%
  select(date,machine_learning_nitrate = nitrate_ug_m3)

aerosols23 %>% 
  left_join(nitrate_dat_ml,by = "date") %>% 
  ggplot(aes(measured_nitrate,machine_learning_nitrate)) +
  geom_point() +
  geom_abline(aes(slope = 1,intercept = 0),col = "red") +
  theme_bw() +
  xlim(0.25,2.25) +
  ylim(0.25,2.25) +
  labs(x = "Measured nitrate (ug/m3)",
       y = "Nitrate from machine learning (ug/m3)")

ggsave('nitrate_comparison.svg',
       path = "output/plots/comparing_measured_ml",
       width = 12.7,
       height = 12.7,
       units = 'cm')
