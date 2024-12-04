library(tidyverse)
library(openair)
library(janitor)
library(zoo)

Sys.setenv(TZ = 'UTC')

#plots for thesis

# Reading in data ---------------------------------------------------------

#reading in nox data with uncertainties
#from ebas data wrangling R script

no15 = read.csv("data/nox_data_thesis/no15.csv") %>% 
  mutate(date = ymd_hms(date)) %>% 
  select(date,no_ppt = no_ppt_ebas,no_u_ppt)

nox19 = read.csv("data/nox_data_thesis/nox19.csv") %>% 
  mutate(date = ymd_hms(date))

nox20 = read.csv("data/nox_data_thesis/nox20.csv") %>% 
  mutate(date = ymd_hms(date))

nox23 = read.csv("~/Cape Verde/nox/processing/ozone_correction/processed_data/nox2023.csv") %>% 
  mutate(date = ymd_hms(date))

nox = bind_rows(no15,nox19,nox20,nox23) %>% 
  select(-c(no_flag,no2_flag,no_lod_ppt,no2_lod_ppt))

#read in full data (data joined in creating_master_df)

dat_full = read.csv("output/data/all_data_utc.csv") %>% 
  mutate(date = ymd_hms(date),
         campaign = case_when(date > "2015-11-24" & date < "2015-12-04" ~ "November 2015",
                              date > "2019-08-15" & date < "2019-08-29" ~ "August 2019",
                              date > "2020-02-14" & date < "2020-02-27" ~ "February 2020",
                              date > "2023-02-07" & date < "2023-02-27" ~ "February 2023"),
         hono_err = case_when(campaign == "November 2015" ~ hono * 0.1 + 0.2,
                              campaign == "February 2020" ~ hono * 0.1 + 0.2,
                              TRUE ~ hono_err)) %>% 
  select(-c(no,no2)) %>% 
  left_join(nox,by = "date")

# write.csv(dat_full,"~/Cape Verde/peroxy_campaign/output/data/all_data_utc_updated_nox.csv",row.names = F)

# Timeseries for feb 2023 -------------------------------------------------

#timeseries 2023 with shading for errors/uncertainty

test = dat_full%>% 
  select(date,hono,hono_err,no_ppt,no_u_ppt,no2_ppt,no2_u_ppt,campaign) %>% 
  mutate(hono_err_plot_max = hono + 2 * hono_err,
         hono_err_plot_min = hono - 2 * hono_err,
         no_err_plot_max = no_ppt + no_u_ppt,
         no_err_plot_min = no_ppt - no_u_ppt,
         no2_err_plot_max = no2_ppt + no2_u_ppt,
         no2_err_plot_min = no2_ppt - no2_u_ppt) %>% 
  rename(HONO = hono,
         NO = no_ppt,
         `NO[2]` = no2_ppt) %>% 
  pivot_longer(c(HONO,NO,`NO[2]`)) %>% 
  pivot_longer(cols = c(hono_err_plot_max,no_err_plot_max,no2_err_plot_max),
               values_to = "max_err_v",names_to = "max_err_n") %>% 
  pivot_longer(cols = c(hono_err_plot_min,no_err_plot_min,no2_err_plot_min),
               values_to = "min_err_v",names_to = "min_err_n") %>% 
  mutate(flag = case_when(name == "HONO" & min_err_n == "hono_err_plot_min" & max_err_n == "hono_err_plot_max" ~ "hono",
                          name == "NO" & min_err_n == "no_err_plot_min" & max_err_n == "no_err_plot_max" ~ "no",
                          name == "NO[2]" & min_err_n == "no2_err_plot_min" & max_err_n == "no2_err_plot_max" ~ "no2")) %>% 
  filter(is.na(flag) == F)

test %>%
  filter(campaign == "February 2023") %>% 
  mutate(hour = hour(date)) %>% 
  ggplot() +
  theme_bw() +
  geom_path(aes(date,value,col = name),size = 0.75) +
  geom_ribbon(aes(date,ymin = min_err_v,ymax = max_err_v,fill = name),alpha = 0.4) +
  facet_grid(rows = vars(name),scales = "free",labeller = label_parsed) +
  scale_colour_manual(values = c("darkorange","steelblue1","navy")) +
  scale_fill_manual(values = c("darkorange","steelblue1","navy")) +
  scale_x_datetime(date_breaks = "2 days",date_labels = "%d %b") +
  theme(legend.position = "None",
        text = element_text(size =  20)) +
  labs(x = NULL,
       y = "Mixing ratio (ppt)")

# ggsave('hono_nox_timeseries23.png',
#        path = "~/Writing/Thesis/Chapter 4 (HONO in CVAO)/Images",
#        width = 29,
#        height = 12,
#        units = 'cm')

# Air masses for feb 2023 -------------------------------------------------

#reading in air mass data
air_masses = read.csv("~/Cape Verde/new_CVAO_sector_%_boxes_1.csv") %>% 
  rename(date = X) %>% 
  mutate(date = ymd_hms(date)) %>% 
  rename_with(~ gsub("\\.", " ", .))

#plotting
air_masses %>% 
  filter(date > "2023-02-07" & date < "2023-02-27") %>% 
  # timeAverage("1 day") %>% 
  mutate(year = year(date),
         doy = yday(date)) %>%
  pivot_longer(cols = -c(date,year,doy)) %>% 
  ggplot(aes(date,value,fill = name)) +
  theme_bw() +
  geom_area() +
  theme(legend.position = "top",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.margin = unit(c(0,0.6,0.1, 0.1), "cm"),
        text = element_text(size = 16)
  ) +
  labs(x = NULL,
       y = "Air mass composition (%)",
       col = NULL,
       fill = NULL) +
  scale_x_datetime(date_breaks = "1 days",date_labels = "%d/%m",expand = expansion(mult = c(0,0))) +
  # scale_x_continuous(expand = expansion(mult = c(0,0))) +
  scale_y_continuous(expand = expansion(mult = c(0,0))) +
  scale_fill_manual(values = c("North Atlantic" = "navy",
                               "South Atlantic" = "steelblue1",
                               "Sahara" = "goldenrod1",
                               "Sahel" = "darkorange3",
                               "West Africa" = "firebrick4",
                               "Central Africa" = "khaki4",
                               "South America" = "darkseagreen1",
                               "North America" = "springgreen4",
                               "Europe" = "darkolivegreen3",
                               "Upwelling" = "deepskyblue3"))

# ggsave('CVAO_airmasses_feb23.png',
#        path = "~/Writing/Thesis/Chapter 4 (HONO in CVAO)/Images",
#        width = 29,
#        height = 12,
#        units = 'cm')



# Boxplot showing variations in mixing ratios with saharan air ------------

dat_boxplot = dat_full %>% 
  filter(campaign == "February 2023") %>% 
  mutate(hour = hour(date),
         daytime_no = ifelse(hour >= 11 & hour <= 15,no_ppt,NA_real_),
         daytime_no2 = ifelse(hour >= 11 & hour <= 15,no2_ppt,NA_real_),
         daytime_hono = ifelse(hour >= 11 & hour <= 15,hono,NA_real_)) %>% 
  timeAverage("1 day") %>%
  mutate(sahara_categories = case_when(sahara < 1 ~ "0%",
                                       sahara > 1 & sahara < 20 ~ "< 20%",
                                       sahara > 20 ~ "> 20%"))

dat_boxplot %>% 
  rename(NO = daytime_no,
         HONO = daytime_hono,
         `NO[2]` = daytime_no2) %>% 
  pivot_longer(c(HONO,NO,`NO[2]`)) %>%
  ggplot(aes(factor(sahara_categories,levels = c("0%","< 20%","> 20%")),value,fill = sahara_categories)) +
  theme_bw() +
  geom_boxplot() +
  facet_wrap(vars(name),labeller = label_parsed,scales = "free") +
  scale_fill_manual(values = c("darkorange","steelblue1","navy")) +
  labs(x = "Saharan air mass",
       y = "Mixing ratio (ppt)",
       fill = NULL) +
  theme(legend.position = "None",
        text = element_text(size =  20))

# ggsave('hono_nox_sahara_boxplot.png',
#        path = "~/Writing/Thesis/Chapter 4 (HONO in CVAO)/Images",
#        width = 29,
#        height = 12,
#        units = 'cm')

# Diurnal with SE ---------------------------------------------------------

diurnal = dat_full %>%
  filter(campaign == "February 2023") %>% 
  filter(is.na(hono) == FALSE) %>%
  mutate(hour = hour(date)) %>% 
  group_by(hour) %>%
  select(hono,no = no_ppt,no2 = no2_ppt) %>% 
  summarise(across(where(is.numeric),list(mean = ~mean(.,na.rm = T),sd = ~sd(.,na.rm = T),se = ~sd(., na.rm = TRUE) / sqrt(length(.))))) %>% 
  ungroup()

diurnal %>% 
  mutate(hono_err_plot_max = hono_mean + hono_se,
         hono_err_plot_min = hono_mean - hono_se,
         no_err_plot_max = no_mean + no_se,
         no_err_plot_min = no_mean - no_se,
         no2_err_plot_max = no2_mean + no2_se,
         no2_err_plot_min = no2_mean - no2_se) %>% 
  rename(HONO = hono_mean,NO = no_mean,'NO[2]' = no2_mean) %>%
  pivot_longer(c(HONO,NO,`NO[2]`)) %>% 
  pivot_longer(cols = c(hono_err_plot_max,no_err_plot_max,no2_err_plot_max),
               values_to = "max_err_v",names_to = "max_err_n") %>% 
  pivot_longer(cols = c(hono_err_plot_min,no_err_plot_min,no2_err_plot_min),
               values_to = "min_err_v",names_to = "min_err_n") %>% 
  mutate(flag = case_when(name == "HONO" & min_err_n == "hono_err_plot_min" & max_err_n == "hono_err_plot_max" ~ "hono",
                          name == "NO" & min_err_n == "no_err_plot_min" & max_err_n == "no_err_plot_max" ~ "no",
                          name == "NO[2]" & min_err_n == "no2_err_plot_min" & max_err_n == "no2_err_plot_max" ~ "no2")) %>% 
  filter(is.na(flag) == F) %>% 
  ggplot() +
  theme_bw() +
  geom_path(aes(hour,value,col = name),size = 0.75) +
  geom_ribbon(aes(hour,ymin = min_err_v,ymax = max_err_v,fill = name),alpha = 0.25) +
  facet_grid(rows = vars(name),scales = "free",labeller = label_parsed) +
  scale_colour_manual(values = c("darkorange","steelblue1","navy")) +
  scale_fill_manual(values = c("darkorange","steelblue1","navy")) +
  # scale_x_datetime(date_breaks = "2 days",date_labels = "%d %b") +
  theme(legend.position = "None",
        text = element_text(size =  20)) +
  scale_x_continuous(breaks = c(0,4,8,12,16,20)) +
  labs(x = "Hour of day (UTC)",
       y = "Mixing ratio (ppt)")

# ggsave('hono_diurnal23.png',
#        path = "~/Writing/Thesis/Chapter 4 (HONO in CVAO)/Images",
#        width = 29,
#        height = 14,
#        units = 'cm')

# Other campaigns ---------------------------------------------------------

test %>% 
  filter(campaign != "no campaign",
         campaign != "February 2023") %>% 
  mutate(campaign2 = case_when(campaign == "November 2015" ~"November~2015",
                               campaign == "August 2019" ~"August~2019",
                               campaign == "February 2020" ~"February~2020")) %>%
  ggplot() +
  theme_bw() +
  geom_path(aes(date,value,col = name),size = 0.75,group = 1) +
  geom_ribbon(aes(date,ymin = min_err_v,ymax = max_err_v,fill = name),alpha = 0.4) +
  labs(x = NULL,
       y = "Mixing ratio (ppt)",
       col = NULL) +
  scale_colour_manual(values = c("darkorange","steelblue1","navy")) +
  scale_fill_manual(values = c("darkorange","steelblue1","navy")) +
  facet_grid(cols = vars(factor(campaign2,levels = c("November~2015","August~2019","February~2020"))),
             scales = "free",rows = vars(name),labeller = label_parsed) +
  # theme(legend.position = "top") +
  scale_x_datetime(date_breaks = "4 day",date_labels = "%d/%m") +
  theme(legend.position = "None",
        text = element_text(size =  20)) +
  NULL

ggsave('hono_nox_timeseries_other_campaigns.png',
       path = "~/Writing/Thesis/Chapter 4 (HONO in CVAO)/Images",
       width = 32,
       height = 12,
       units = 'cm')


# HONO diurnals from all campaigns ----------------------------------------

diurnal_campaigns = dat_full %>%
  timeVariation(pollutant = "hono_err",group = "campaign")

diurnal_campaigns_dat_hono = diurnal_campaigns$data$hour %>% 
  rename(hono = Mean)
diurnal_campaigns_dat_hono_err = diurnal_campaigns$data$hour %>% 
  rename(hono_err = Mean) %>% 
  select(hour,hono_err,variable) %>% 
  ungroup()

diurnal_campaigns_dat_hono %>% 
  left_join(diurnal_campaigns_dat_hono_err,by = c("hour","variable")) %>% 
  ggplot() +
  geom_path(aes(hour,hono,col = variable),size = 1) +
  # geom_path(aes(hour,hono_err,col = variable),size = 1) +
  geom_ribbon(aes(x = hour,ymin = hono - hono_err,ymax = hono + hono_err,fill = variable),alpha = 0.4) +
  theme_bw() +
  labs(x = "Hour of day (UTC)",
       y = "HONO (ppt)",
       color = NULL,
       fill = NULL) +
  scale_colour_manual(values = c("darkorange","steelblue1","navy","firebrick"),
                      breaks = c("November 2015","August 2019","February 2020","February 2023")) +
  scale_fill_manual(values = c("steelblue1","navy","firebrick","darkorange")) +
  scale_x_continuous(breaks = c(0,4,8,12,16,20)) +
  theme(legend.position = "None",
        text = element_text(size =  20))

ggsave('hono_diurnal_campaigns.png',
       path = "~/Writing/Thesis/Chapter 4 (HONO in CVAO)/Images",
       width = 29,
       height = 12,
       units = 'cm')


# NO diurnals from all campaigns ----------------------------------------

diurnal_campaigns = dat_full %>%
  timeVariation(pollutant = "no_u_ppt",group = "campaign")

diurnal_campaigns_dat_no = diurnal_campaigns$data$hour %>% 
  rename(no = Mean)
diurnal_campaigns_dat_no_err = diurnal_campaigns$data$hour %>% 
  rename(no_err = Mean) %>% 
  select(hour,no_err,variable) %>% 
  ungroup()

diurnal_campaigns_dat_no %>% 
  left_join(diurnal_campaigns_dat_no_err,by = c("hour","variable")) %>% 
  left_join(diurnal_campaigns_dat_hono,by = c("hour","variable")) %>% 
  left_join(diurnal_campaigns_dat_hono_err,by = c("hour","variable")) %>% 
  rename(HONO = hono,NO = no) %>% 
  pivot_longer(c(HONO,NO)) %>% 
  ggplot() +
  geom_path(aes(hour,value,col = variable),size = 1) +
  # geom_path(aes(hour,hono_err,col = variable),size = 1) +
  # geom_ribbon(aes(x = hour,ymin = no - no_err,ymax = no + no_err,fill = variable),alpha = 0.4) +
  theme_bw() +
  labs(x = "Hour of day (UTC)",
       y = "Mixing ratio (ppt)",
       color = NULL,
       fill = NULL) +
  facet_grid(cols = vars(name)) +
  scale_colour_manual(values = c("darkorange","steelblue1","navy","firebrick"),
                      breaks = c("November 2015","August 2019","February 2020","February 2023")) +
  scale_fill_manual(values = c("steelblue1","navy","firebrick","darkorange")) +
  scale_x_continuous(breaks = c(0,4,8,12,16,20)) +
  theme(legend.position = "top",
        text = element_text(size =  20))

ggsave('hono_diurnal_campaigns.png',
       path = "~/Writing/Thesis/Chapter 4 (HONO in CVAO)/Images",
       width = 29,
       height = 12,
       units = 'cm')


# ARNA flight data --------------------------------------------------------

arna_dat = read.csv("data/hono/arna_hono/Renoxification_data_for_Anna_v2.csv") %>% 
  clean_names() %>% 
  mutate(start_time = dmy_hm(start_time),
         end_time = dmy_hm(end_time))

rects = data.frame(xstart = c(0),
                   xend = c(500),
                   cols = c("MBL"))

feb_dat = dat_full %>% 
  filter(campaign == "February 2023") %>% 
  select(date,hono,hono_err,no_ppt,no_u_ppt,no2_ppt,no2_u_ppt,campaign) %>% 
  mutate(hono_max_err = hono + 2 * hono_err,
         hono_min_err = hono - 2 * hono_err,
         no_max_err = no_ppt + no_u_ppt,
         no_min_err = no_ppt - no_u_ppt,
         no2_max_err = no2_ppt + no2_u_ppt,
         no2_min_err = no2_ppt - no2_u_ppt,
         hour = hour(date),
         altitude_m = 7.5) %>% 
  rename(HONO = hono,
         NO = no_ppt,
         `NO[2]` = no2_ppt) %>% 
  filter(hour >= 11 & hour <= 15) %>% 
  timeAverage("1 day") %>%
  mutate(campaign = "February 2023") %>%
  select(date,HONO,NO,`NO[2]`,hono_max_err:no2_min_err,altitude_m,campaign)

arna_plot_mean = arna_dat %>% 
  mutate(campaign = ifelse(start_time < "2020-01-01","ARNA 2019","ARNA 2020"),
         hono_max_err = hono_ppt_v + hono_uncertainty_ppt,
         hono_min_err = hono_ppt_v - hono_uncertainty_ppt,
         no_max_err = no_ppt_v + no_uncertainty_ppt,
         no_min_err = no_ppt_v - no_uncertainty_ppt,
         no2_max_err = no2_ppt_v + no2_uncertainty_ppt,
         no2_min_err = no2_ppt_v - no2_uncertainty_ppt) %>% 
  rename(HONO = hono_ppt_v,
         NO = no_ppt_v,
         `NO[2]` = no2_ppt_v,
         date = start_time) %>%
  select(date,HONO,NO,`NO[2]`,hono_max_err:no2_min_err,altitude_m,campaign) %>% 
  bind_rows(feb_dat) %>% 
  mutate(categories = case_when(altitude_m == 7.5 ~ "ground",
                                altitude_m > 7.5 & altitude_m < 500 ~ "mbl",
                                altitude_m > 500 ~"ft")) %>% 
  group_by(categories) %>% 
  select(HONO,NO,`NO[2]`) %>% 
  summarise(across(where(is.numeric),list(mean = ~mean(.,na.rm = T),sd = ~sd(.,na.rm = T),se = ~sd(., na.rm = TRUE) / sqrt(length(.)))))

arna_plot = arna_dat %>% 
  mutate(campaign = ifelse(start_time < "2020-01-01","ARNA 2019","ARNA 2020"),
         hono_max_err = hono_ppt_v + hono_uncertainty_ppt,
         hono_min_err = hono_ppt_v - hono_uncertainty_ppt,
         no_max_err = no_ppt_v + no_uncertainty_ppt,
         no_min_err = no_ppt_v - no_uncertainty_ppt,
         no2_max_err = no2_ppt_v + no2_uncertainty_ppt,
         no2_min_err = no2_ppt_v - no2_uncertainty_ppt) %>% 
  rename(HONO = hono_ppt_v,
         NO = no_ppt_v,
         `NO[2]` = no2_ppt_v,
         date = start_time) %>%
  select(date,HONO,NO,`NO[2]`,hono_max_err:no2_min_err,altitude_m,campaign) %>% 
  bind_rows(feb_dat) %>% 
  pivot_longer(c(HONO,NO,`NO[2]`)) %>% 
  pivot_longer(cols = c(hono_max_err,no_max_err,no2_max_err),
               values_to = "max_err_v",names_to = "max_err_n") %>% 
  pivot_longer(cols = c(hono_min_err,no_min_err,no2_min_err),
               values_to = "min_err_v",names_to = "min_err_n") %>% 
  mutate(flag = case_when(name == "HONO" & min_err_n == "hono_min_err" & max_err_n == "hono_max_err" ~ "hono",
                          name == "NO" & min_err_n == "no_min_err" & max_err_n == "no_max_err" ~ "no",
                          name == "NO[2]" & min_err_n == "no2_min_err" & max_err_n == "no2_max_err" ~ "no2")) %>% 
  filter(is.na(flag) == F)

arna_plot %>% 
  ggplot() +
  theme_bw() +
  geom_point(aes(value,altitude_m,col = campaign)) +
  geom_pointrange(aes(y = altitude_m, col = campaign, x = value,
                      xmin = min_err_v,xmax = max_err_v)) +
  geom_rect(data = rects, aes(ymin =xstart, ymax = xend, xmin = -Inf, xmax = Inf, fill = cols), alpha = 0.1) +
  scale_colour_manual(values = c("darkorange","navy","springgreen4")) +
  scale_fill_manual(values = c("steelblue1")) +
  facet_grid(cols = vars(name),scales = "free",labeller = label_parsed) +
  labs(x = "Mixing ratio (ppt)",
       y = "Altitude (m)",
       col = NULL,
       fill = NULL) +
  theme(legend.position = "top",
        text = element_text(size =  20))

# ggsave('hono_nox_arna_feb23.png',
#        path = "~/Writing/Thesis/Chapter 4 (HONO in CVAO)/Images",
#        width = 29,
#        height = 15,
#        units = 'cm')
