
setwd("~/Cape Verde/peroxy_campaign/data/arna_hono/from_simone")

files = list.files(full.names=TRUE)
datList = list()
for(index in 1:length(files)) {
  
  datList[[index]] = read.table(files[index],header=TRUE,sep = ",", na.strings= c('NA','missing'))%>%
    clean_names() %>%
    # mutate(date = ymd_hms(date)) %>% 
    tibble()
  
}

flight_dat = bind_rows(datList) %>% 
  mutate(date = ymd_hms(date))

c206 = read.csv("C206.csv") %>% 
  clean_names() %>%
  mutate(date = ymd_hms(date))

flight_dat %>% 
  mutate(doy = yday(date),
         year = year(date)) %>%
  filter(year == 2020,
         hono_ppt_v > -50,
         hono_ppt_v < 150) %>% 
  ggplot(aes(date,hono_ppt_v,col = altitude)) +
  geom_point() +
  facet_wrap(vars(doy),scales = "free_x") +
  scale_colour_viridis_c() +
  NULL

ggsave('hono_flights20.svg',
       path = "~/Cape Verde/peroxy_campaign/output/plots/timeseries",
       width = 30,
       height = 16,
       units = 'cm')
