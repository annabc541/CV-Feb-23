library(lubridate)
library(tidyverse)
library(openair)
library(janitor)
library(viridis)
library(zoo)

# Reading in data ---------------------------------------------------------

#NOx data
files = list.files("data/nox_data",full.names=TRUE)
datList = list()

for(index in 1:length(files)) {
  datList[[index]] = read.csv(files[index],header=TRUE,na.strings= c('NA','missing'))%>%
    tibble()
  
}

nox_dat = bind_rows(datList) %>% 
  mutate(date = ymd_hms(X)) %>% 
  select(date,no = NO_Conc_art_corrected,no2 = NO2_Conc_diode) %>% 
  timeAverage("5 min")

#HONO data
files = list.files("data/roberto_data",full.names=TRUE)
datList = list()

for(index in 1:length(files)) {
  datList[[index]] = read.csv(files[index],header=TRUE,na.strings= c('NA','missing'))%>%
    tibble()
  
}

hono = bind_rows(datList) %>% 
  mutate(date = dmy_hms(start.gmt)) %>% 
  select(date,hono = hono.ppt)

#check that this data has seconds in it and is read in properly
hono23 = read.csv("output/data/processed_in_r3.csv") %>% 
  mutate(date = ymd_hms(date))

hono_dat = bind_rows(hono,hono23)

#Air masses
air_mass = read.csv("data/new_CVAO_sector_%_boxes_1.csv") %>% 
  rename(date = X) %>% 
  mutate(date = ymd_hm(date)) %>% 
  clean_names()

#putting HONO first means we only get the other data for campaign periods - change as needed
df_list = list(hono_dat,nox_dat,air_mass)

dat = df_list %>% reduce(left_join,by = "date") %>% 
  arrange(date) %>% 
  mutate(campaign = case_when (date <= "2019-08-29 00:55" ~ "August 2019",
                               between(date,as.POSIXct("2020-02-14 01:00"),as.POSIXct("2020-02-27 00:55")) ~ "February 2020",
                               date >= "2023-02-07 08:35" ~ "February 2023",
                               TRUE ~ "no campaign"))



# HONO 2023 colour-coded by quality of data ------------------------------------

hono23 %>% 
  mutate(flag = case_when(low_quality_flag == 1 ~ "Low quality",
                          date > "2023-02-21" ~ "Night zeroes",
                          TRUE ~ "Good data")) %>% 
  ggplot(aes(date,hono,col = flag)) +
  geom_path(size = 0.8) +
  scale_color_viridis(discrete = TRUE) +
  labs(x = NULL,
       y = "HONO (ppt)",
       color = NULL) +
  theme_bw() +
  theme(legend.position = "bottom") +
  scale_x_datetime(date_breaks = "1 day",date_labels = "%d/%m") +
  NULL

ggsave('hono_qc.svg',
       path = "output/plots/leeds_meeting",
       width = 30,
       height = 12,
       units = 'cm')


# Air masses for the month ------------------------------------------------

air_mass %>% 
  filter(date >= "2019-08-15" & date <= "2019-08-29") %>% 
  timeAverage("1 day") %>% 
  remove_constant() %>% 
  pivot_longer(-date) %>%
  mutate(date = as.character(date)) %>% 
  ggplot(aes(date,value,fill = name)) +
  geom_bar(stat = "identity") +
  theme_bw() +
  labs(x = NULL,
       y = NULL,
       fill = NULL) +
  theme(legend.position = "top",
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_viridis(discrete = TRUE) +
  NULL

ggsave('air_masses_aug19.png',
       path = "output/plots/air_masses",
       width = 30,
       height = 12,
       units = 'cm')


# HONO colour coded by Saharan air mass -----------------------------------

dat %>% 
  filter(campaign == "February 2023") %>% 
  timeAverage("1 hour") %>% 
  mutate(sahara = na.approx(sahara,na.rm = F)) %>% 
  fill(sahara,.direction = "up") %>% 
  ggplot(aes(date,hono,col = sahara)) +
  geom_path(size = 0.8) +
  scale_color_viridis() +
  labs(x = NULL,
       y = "HONO (ppt)",
       color = "Sahara %") +
  theme_bw() +
  # theme(legend.position = "top") +
  scale_x_datetime(date_breaks = "1 day",date_labels = "%d/%m") +
  NULL

ggsave('hono_j_sahara_feb23.svg',
       path = "output/plots/leeds_meeting",
       width = 30,
       height = 12,
       units = 'cm')


# HONO, NO and NOx colour coded by Saharan air mass -----------------------

dat %>% 
  filter(date > "2023-02-03" & date < "2023-02-28") %>%
  timeAverage("1 hour") %>% 
  mutate(sahara = na.approx(sahara,na.rm = F)) %>% 
  fill(sahara,.direction = "up") %>% 
  rename('NO[2]' = no2,
         NO = no,
         HONO = hono) %>% 
  pivot_longer(c(NO,'NO[2]')) %>% 
  ggplot(aes(date,value,col = sahara)) +
  geom_path(size = 0.8) +
  scale_color_viridis() +
  facet_grid(rows = vars(name),scales = 'free_y',labeller = label_parsed) +
  labs(x = NULL,
       y = expression(NO[x]~(ppt)),
       color = "Sahara %") +
  theme_bw() +
  # theme(legend.position = "top") +
  scale_x_datetime(date_breaks = "1 day",date_labels = "%d/%m") +
  NULL

ggsave('nox_feb23.svg',
       path = "output/plots/leeds_meeting",
       width = 30,
       height = 12,
       units = 'cm')

# Comparing years ---------------------------------------------------------

dat %>% 
  timeAverage("1 hour") %>%
  mutate(campaign = case_when (date <= "2019-08-29 00:55" ~ "August 2019",
                               between(date,as.POSIXct("2020-02-14 01:00"),as.POSIXct("2020-02-27 00:55")) ~ "February 2020",
                               date >= "2023-02-07 08:35" ~ "February 2023",
                               TRUE ~ "no campaign")) %>%
  filter(campaign != "no campaign") %>% 
  mutate(sahara = na.approx(sahara,na.rm = F)) %>%
  fill(sahara,.direction = "up") %>%
  ggplot(aes(date,hono,col = sahara)) +
  geom_path(size = 0.8) +
  scale_color_viridis() +
  labs(x = NULL,
       y = "HONO (ppt)",
       color = "Sahara %") +
  theme_bw() +
  facet_wrap(vars(campaign),scales = "free_x",ncol = 1) +
  # theme(legend.position = "top") +
  scale_x_datetime(date_breaks = "1 day",date_labels = "%d/%m") +
  NULL

ggsave('hono_across_the_years.svg',
       path = "output/plots/leeds_meeting",
       width = 30,
       height = 12,
       units = 'cm')


# Diurnals ----------------------------------------------------------------

#all hono data in one plot, colour coded by campaign
diurnals = dat %>% 
  filter(campaign != "no campaign") %>% 
  mutate(no = case_when(is.na(hono) ~ NA_real_,
                        campaign == "August 2019" & date > "2019-08-24" ~ NA_real_,
                        TRUE ~ no),
         no2 = case_when(is.na(hono) ~ NA_real_,
                         campaign == "August 2019" & date > "2019-08-24" ~ NA_real_,
                         TRUE ~ no2),
         hono = ifelse(campaign == "August 2019" & date > "2019-08-24",NA_real_,hono)) %>% 
  pivot_wider(names_from = campaign,values_from = hono)

diurnal = diurnals %>% 
  timeVariation(pollutant = c("August 2019","February 2020","February 2023"))

diurnal_dat = diurnal$data$hour

diurnal_dat %>% 
  ggplot(aes(hour,Mean,col = variable)) +
  geom_path(size = 0.8) +
  # facet_grid(cols = vars(variable),scales = "free_y") +
  scale_color_manual(values = viridis(3)) +
  theme_bw() +
  labs(x = "Hour of day (UTC)",
       y = expression(NO[2]~(ppt)),
       color = NULL) +
  scale_x_continuous(breaks = c(0,4,8,12,16,20)) +
  # ylim(-1,12) +
  theme(legend.position = "top")

ggsave('no2_three_campaigns.svg',
       path = "output/plots/leeds_meeting",
       width = 11,
       height = 13,
       units = 'cm')

#hono,no and no2 in the same plot per campaign
diurnal = dat %>% 
  mutate(no = case_when(is.na(hono) ~ NA_real_,
                        campaign == "August 2019" & date > "2019-08-24" ~ NA_real_,
                        TRUE ~ no),
         no2 = case_when(is.na(hono) ~ NA_real_,
                         campaign == "August 2019" & date > "2019-08-24" ~ NA_real_,
                        TRUE ~ no2),
         hono = ifelse(campaign == "August 2019" & date > "2019-08-24",NA_real_,hono),
         hono_sahara = case_when(campaign == "February 2023" & date >= "2023-02-19"~ hono,
                                 campaign == "February 2023" & date <= "2023-02-09"~ hono,
                                 date > "2023-02-26" ~ NA_real_,
                                 TRUE ~ NA_real_),
         hono_no_sahara = ifelse(campaign == "February 2023" & is.na(hono_sahara),hono,NA_real_),
         nox = no + no2) %>%
  rename(HONO = hono,NO = no,NOx = nox) %>% 
  filter(campaign == "February 2023",
         no2 < 50) %>% 
  timeVariation(pollutant = c("HONO","NOx"))

diurnal_dat = diurnal$data$hour

diurnal_dat %>% 
  ggplot(aes(hour,Mean,col = variable)) +
  geom_line(size = 1) +
  # facet_grid(cols = vars(variable),scales = "free_y") +
  scale_color_manual(values = viridis(2)) +
  theme_bw() +
  labs(x = "Hour of day (UTC)",
       y = "Mixing ratio (ppt)",
       color = NULL) +
  scale_x_continuous(breaks = c(0,4,8,12,16,20)) +
  ylim(-1,12) +
  theme(legend.position = "top")

ggsave('hono_no_aug19.svg',
       path = "output/plots/leeds_meeting",
       width = 11,
       height = 13,
       units = 'cm')

# Looking at diurnals -----------------------------------------------------

spec_rad = read.csv("data/spec_rad.csv") %>% 
  mutate(date = dmy_hm(date)) %>% 
  clean_names()

spec_rad %>% 
  mutate(jhono = ifelse(is.na(j_hono),calculated_jhono,j_hono)) %>% 
  pivot_longer(c(jhono,j_hno3)) %>% 
  ggplot(aes(date,value,col = name)) +
  geom_path() +
  facet_grid(rows = vars(name),scales = "free")

diurnals = left_join(dat,spec_rad) %>% 
  filter(campaign == "February 2023") %>% 
  mutate(jhono = ifelse(is.na(j_hono),calculated_jhono,j_hono),
         sahara = na.approx(sahara,na.rm = F)) %>%
  fill(sahara,.direction = "up") %>%
  timeAverage("1 hour") %>%
  mutate(nox = no + no2) %>% 
  select(date,hono,no,no2,jhono,sahara,nox,j_hno3)

diurnals %>% 
  # mutate(j_hno3 = j_hno3/10^-3) %>% 
  pivot_longer(c(hono,jhono,j_hno3)) %>% 
  ggplot(aes(date,value,col = sahara)) +
  geom_path(size = 0.8) +
  scale_color_viridis() +
  labs(x = NULL,
       y = NULL,
       color = "Sahara %") +
  theme_bw() +
  # theme(legend.position = "top") +
  scale_x_datetime(date_breaks = "1 day",date_labels = "%d/%m") +
  facet_grid(rows = vars(name),scales = "free")

myplot = diurnals %>% 
  # filter(date > "2023-02-19") %>% 
  timeVariation(pollutant = c("hono","jhono","j_hno3"))


diurnal_dat = myplot$data$hour

diurnal_dat %>% 
  ggplot(aes(hour,Mean,col = variable)) +
  geom_line(size = 1) +
  facet_grid(rows = vars(variable),scales = "free_y") +
  scale_color_manual(values = viridis(3)) +
  theme_bw() +
  theme(legend.position = "None")
NULL