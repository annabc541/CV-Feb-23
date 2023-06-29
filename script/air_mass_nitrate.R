air_mass = read.csv("data/new_CVAO_sector_%_boxes_1.csv") %>% 
  rename(date = X) %>% 
  mutate(date = ymd_hm(date)) %>% 
  clean_names()

nitrate_dat = read.csv("data/nitrate_ammonium_CVAO_12-19.csv") %>% 
  clean_names() %>%
  mutate(date = mdy_hm(start_local_time),
         month = month(date),
         date = round_date(date, "6 hour")) %>% 
  select(sample_no,date,month,nitrate = nitrate_mg_m,ammonium = ammonium_mg_m)

nitrate_dat %>% 
  group_by(month) %>% 
  summarise(nitrate = mean(nitrate,na.rm = TRUE),
            ammonium = mean(ammonium,na.rm = TRUE),
            sahara = mean(sahara,na.rm = TRUE)) %>% 
  ungroup() %>% 
  ggplot(aes(month,ammonium,fill = sahara)) +
  scale_fill_viridis() +
  geom_col(stat = "identity")

dat = left_join(air_mass,nitrate_dat)

dat %>% 
  group_by(month) %>% 
  summarise(nitrate = median(nitrate,na.rm = TRUE),
            ammonium = median(ammonium,na.rm = TRUE),
            sahara = median(sahara,na.rm = TRUE)) %>% 
  ungroup() %>% 
  ggplot(aes(month,ammonium,fill = sahara)) +
  scale_fill_viridis() +
  geom_col(stat = "identity")


