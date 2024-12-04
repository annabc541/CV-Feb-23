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
  geom_point(col = "navy",size = 2) +
  geom_abline(aes(slope = 1,intercept = 0),col = "darkorange", size = 1) +
  theme_bw() +
  xlim(0,2.25) +
  ylim(0,2.25) +
  labs(x = "Measured nitrate (ug/m3)",
       y = "ML nitrate (ug/m3)") +
  theme(text = element_text(size = 20))

ggsave('nitrate_comparison_feb23.png',
       path = "~/Writing/Thesis/Chapter 4 (HONO in CVAO)/Images",
       width = 13,
       height = 13,
       units = 'cm')
