library(tidyverse)
library(lubridate)
library(openair)
library(janitor)
library(zoo)
library(viridis)

#defining constant values used for PSS calculations

kp = 3.3 * 10^-11 #rate constant for oh + no -> hono (from Atkinson et al. 2004)
kl = 6 * 10^-12 #rate constant for oh + hono -> h2o + no2 (from Atkinson et al. 2004)
dv = 0.3 #deardroff velocity, value used by Simone
nitrate = 1.20 * 10^10 #constant value until more recent measurements are received from TROPOS
#for February 2023 f1 = 63 f3 = 70
#for February 2020 f1 = 10 f3 = 11
#for August 2019 f1 = 21 f3 = 22
#for November 2015 f1 = 7 f3 = 8
#function for displaying linear equation and r squared on plot
lm_eqn <- function(df,x,y){
  m <- lm(y ~ x, df);
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                   list(a = format(unname(coef(m)[1]), digits = 2),
                        b = format(unname(coef(m)[2]), digits = 2),
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));
}

#read in full data (data joined in creating_master_df)

dat = read.csv("output/data/all_data_utc.csv") %>% 
  mutate(date = ymd_hms(date)) %>% 
  fill(c(nitrate_ug_m3,nitrate_molecules_cm3))


# HONO and jhno3 ----------------------------------------------------------

dat %>% 
  filter(campaign == "November 2015") %>% 
  rename(HONO = hono,
         "j[HNO3]" = jhno3) %>% 
  pivot_longer(c(HONO,"j[HNO3]")) %>% 
  ggplot(aes(date,value)) +
  geom_path() +
  facet_grid(rows = vars(name),scales = "free_y",labeller = label_parsed) +
  theme_bw() +
  labs(x = "Datetime (UTC)",
       y = NULL)

# ggsave('hono_jhno3_nov15.svg',
#        path = "output/plots",
#        width = 30,
#        height = 12,
#        units = 'cm')

# Missing HONO and jhno3 --------------------------------------------------

missing_hono = dat %>%   
  rename(oh_m_cm3 = oh,no_ppt = no,no2_ppt = no2,hono_ppt = hono) %>% 
  filter(hour >= 11 & hour <= 16) %>%
  mutate(lifetime = 1/jhono,
         h = lifetime * dv,
         kdep = 0.01/h,
         nitrate_m_cm3 = (nitrate_ug_m3 * 10^-12 *6.022 * 10^23)/62.004,
         production_without_nitrate = kp*oh_m_cm3*no_ppt * 2.46 * 10^19 * 10^-12,
         loss = (jhono + (kl*oh_m_cm3) + kdep) * hono_ppt * 2.46 * 10^19 * 10^-12,
         missing_production = (loss - production_without_nitrate),
         missing_production = missing_production * 3600 /(2.46 * 10^19 * 10^-12), #in ppt per hour
         jhno3 = jhno3 * 3600, #per hour
         nitrate_ppt = nitrate_ug_m3 * 10^6/62.004 * 8.314*293.15/101325, #ppt
         nitrate_jhno3 = jhno3 * nitrate_ppt)

missing_hono %>% 
  # filter(campaign!="February 2023" & campaign!="February 2020") %>%
  timeAverage("1 day") %>%
  filter(is.na(hono_ppt) == F) %>% 
  mutate(
    # across(c(upwelling:south_atlantic), ~ na.approx(.x,na.rm =F)),
         polluted_air = north_america+europe,
         african_air = west_africa+sahara+upwelling+sahel+central_africa,
         clean_air = north_atlantic+south_atlantic+upwelling,
         jhno3 = jhno3 *10^3,
         year = year(date)) %>%
  ggplot(aes(jhno3,missing_production,col = clean_air)) +
  geom_point(aes(shape = as.character(year))) +
  # geom_abline(slope = 1) +
  # geom_smooth(method = "lm",se=F) +
  # geom_text(aes(x = 1.75, y = 50, label = lm_eqn(missing_hono,jhno3,missing_production)), parse = TRUE) +
  theme_bw() +
  theme(legend.position = "top") +
  labs(y = "Missing HONO source (ppt/hour)",
       x = expression(j[HNO[3]]~(10^{-3}~per~hour)),
       shape = NULL) +
  scale_colour_viridis_c() +
  NULL

ggsave('missing_hono_jhno3.svg',
       path = "output/plots/pss/missing_hono_jhno3",
       width = 30,
       height = 12,
       units = 'cm')

dat = missing_hono %>% 
  filter(campaign == "February 2020") %>% 
  timeAverage("1 day")

model = lm(missing_production ~ jhno3,feb23)

lm(missing_production ~ jhno3,feb23)

summary(model)



# HONO/NO2 ratio ----------------------------------------------------------

dat %>% 
  filter(campaign != "February 2020",
         campaign != "no campaign") %>%
  mutate(hono_no2 = hono/no2) %>% 
  # pivot_longer(c(hono,no2,hono_no2)) %>% 
  ggplot(aes(date,hono_no2,col = hono)) +
  geom_point() +
  scale_colour_viridis_c() +
  facet_wrap(~factor(campaign,levels = c("November 2015","August 2019","February 2023")),
             scales = "free",ncol = 1) +
  # theme(legend.position = "top") +
  scale_x_datetime(date_breaks = "1 day",date_labels = "%d/%m")


hono_no2_ratio = dat %>% 
  filter(campaign != "February 2020",
         campaign != "no campaign") %>%
  mutate(hono_no2 = hono/no2) %>% 
  pivot_wider(names_from = campaign,values_from = hono_no2)

diurnal = hono_no2_ratio %>% 
  rename("Nov 2015"="November 2015","Aug 2019"="August 2019","Feb 2023"="February 2023") %>% 
  timeVariation(pollutant = c("Nov 2015","Aug 2019","Feb 2023"))

diurnal_dat = diurnal$data$hour

diurnal_dat %>% 
  ggplot(aes(hour,Mean,col = variable)) +
  theme_bw() +
  geom_path(size = 0.8) +
  # facet_grid(cols = vars(variable),scales = "free_y") +
  scale_colour_viridis_d() +
  labs(x = "Hour of day (UTC)",
       y = " HONO/NO2",
       color = NULL) +
  scale_x_continuous(breaks = c(0,4,8,12,16,20)) +
  # ylim(-0.5,12) +
  theme(legend.position = "top")

ggsave('hono_no2_ratio_diurnal.svg',
       path = "output/plots",
       width = 30,
       height = 12,
       units = 'cm')
