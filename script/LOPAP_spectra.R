#to look at spectra recorded while measuring hono
#particularly used to see if anything happened to the spectra during the periods of instrumental noise

library(tidyverse)
library(lubridate)

setwd("~/CV Feb 23/raw_data")


# Importing data ----------------------------------------------------------

files = list.files(pattern = ".spec",full.names=TRUE)
datList = list()

for(index in 1:length(files)) {
  
  datList[[index]] = read.table(files[index],sep = ";",na.strings= c('NA','missing'))
  
}

spec_dat = bind_rows(datList) %>% 
  tibble()


# Separating two channels -------------------------------------------------

raw_dat1 = spec_dat %>% 
  rename(channel = V1) %>% 
  filter(channel == "[Ch0]") %>% 
  select(-c(V3,channel)) 

raw_dat1[c('time', 'date')] <- str_split_fixed(raw_dat1$V2, ' ',2)

spec_dat_ch0 = raw_dat1 %>% 
  mutate(date = glue::glue("{date} {time}"),
         date = dmy_hms(date)) %>% 
  select(date,everything())

raw_dat2 = spec_dat %>% 
  rename(channel = V1) %>% 
  filter(channel == "[Ch1]") %>% 
  select(-c(V3,channel))

raw_dat2[c('time', 'date')] <- str_split_fixed(raw_dat2$V2, ' ',2)

spec_dat_ch1 = raw_dat2 %>% 
  mutate(date = glue::glue("{date} {time}"),
         date = dmy_hms(date)) %>% 
  select(date,everything()) %>%
  select(-V2)


# Transposing spec data ---------------------------------------------------

ch1_spec = spec_dat_ch1 %>% 
  filter(date > "2023-02-13 07:57" & date < "2023-02-13 08:04")

transposed_dat <- t(ch1_spec) %>% as_data_frame()
names(transposed_dat) <- transposed_dat[1,]
transposed_dat <- transposed_dat[-1,]

# Plotting spectra when there's a random spike ----------------------------

plot(transposed_dat$`2023-02-13 07:59:36`)
