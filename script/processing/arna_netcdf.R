library(ncdf4)
library(tidyverse)
library(ggmap)
library(lubridate)


# Importing netcdf --------------------------------------------------------

nc = nc_open("data/arna_hono/core-nitrates_faam_20190819_v002_r1_c206.nc")
print(nc) #info on variables
var = names(nc$var) #get variables
# var2 = var[-c(12:34)] #removing variables about calibration etc as they are different lengths and don't work

for (i in 1:length(var)) {
  vname <- var[i]
  raw <- as.vector(ncvar_get(nc,vname,collapse_degen=FALSE))
  if(i==1){
    dat <- data.frame(raw)
    names(dat) <- vname
  } 
  else {
    dat <- cbind(dat,raw)
    names(dat)[ncol(dat)] <- vname
  }
}

dat = dat %>% 
  rename(hono_mr = `non-core/hono_mr`,
         hono_lod = `non-core/hono_lod`,
         hono_u = `non-core/hono_u`,
         hono_flag = `non-core/hono_flag`)


# Checking HONO -----------------------------------------------------------

dat %>% 
  mutate(index = 1:nrow(.)) %>% 
  filter(index > 100000,
         hono_mr < 250,
         hono_mr > 0,
         hono_flag == 0
         ) %>%
  # pivot_longer(c(hono_mr,hono_lod)) %>% 
  ggplot(aes(altitude,hono_mr,col = hono_flag)) +
  geom_point() +
  # facet_grid(rows = vars(name),scales = "free_y") +
  scale_colour_viridis_c()

# Flight track ------------------------------------------------------------

dat_map = dat %>% 
  mutate(hono_mr = ifelse(hono_flag == 0,hono_mr,NA))

bb = c(min(dat$longitude,na.rm = TRUE),min(dat$latitude,na.rm = TRUE),max(dat$longitude,na.rm = TRUE),max(dat$latitude,na.rm = TRUE))

map = get_stamenmap(bb,zoom = 10)

ggmap(map)+
  geom_point(dat = dat,
             aes(longitude,latitude,col = hono_mr)) +
  theme_bw() +
  scale_colour_viridis_c()
  NULL



# Merge data --------------------------------------------------------------

nox_merge = read.csv("data/arna_hono/c206_arna_core_nox_merge_10Hz.csv") %>% 
    rename(hono_mr = non_core_group.hono_mr,
           hono_lod = non_core_group.hono_lod,
           hono_u = non_core_group.hono_u,
           hono_flag = non_core_group.hono_flag)

nox_merge %>% 
  filter(hono_flag == 0) %>% 
  mutate(index = 1:nrow(.)) %>%
  pivot_longer(c(hono_mr,hono_lod)) %>% 
  ggplot(aes(index,value,col = altitude)) +
  facet_grid(rows = vars(name),scales = "free_y") +
  geom_point() +
  scale_colour_viridis_c()

merge_dat = read.csv("data/arna_hono/c209_merge.csv") %>% 
  mutate(date = ymd_hms(date)) %>% 
  rename(hono_mr = non.core.hono_mr,
         hono_lod = non.core.hono_lod,
         hono_u = non.core.hono_u,
         hono_flag = non.core.hono_flag)

merge_dat %>% 
  filter(hono_flag == 0,
         # hono_mr < 1000,
         # hono_mr > hono_lod
         ) %>%
  pivot_longer(c(hono_mr,hono_lod)) %>%
  ggplot(aes(date,value,col = altitude)) +
  facet_grid(rows = vars(name),scales = "free_y") +
  geom_point() +
  scale_colour_viridis_c()
