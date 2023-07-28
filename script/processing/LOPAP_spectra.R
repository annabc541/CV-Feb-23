library(tidyverse)
library(lubridate)
library(janitor)
library(ggplot2)

# Importing data ----------------------------------------------------------

files = list.files("data/raw_data/super_raw",pattern = "w.spec",full.names=TRUE)
datList = list()

for(index in 1:length(files)) {
  
  datList[[index]] = read.table(files[index],sep = ";",na.strings= c('NA','missing'))
  
}

spec_dat = bind_rows(datList) %>% 
  tibble()
  # remove_constant() %>% 
  # remove_empty()

# Separating two channels -------------------------------------------------

raw_dat1 = spec_dat %>% 
  rename(channel = V1) %>% 
  filter(channel == "[Ch0]") %>% 
  select(-channel) 

raw_dat1[c('time', 'date')] <- str_split_fixed(raw_dat1$V2, ' ',2)

spec_dat_ch0 = raw_dat1 %>% 
  mutate(date = glue::glue("{date} {time}"),
         date = dmy_hms(date)) %>% 
  select(date,everything(),-c(V2,V3))

raw_dat2 = spec_dat %>% 
  rename(channel = V1) %>% 
  filter(channel == "[Ch1]") %>% 
  select(-channel)

raw_dat2[c('time', 'date')] <- str_split_fixed(raw_dat2$V2, ' ',2)

spec_dat_ch1 = raw_dat2 %>% 
  mutate(date = glue::glue("{date} {time}"),
         date = dmy_hms(date)) %>% 
  select(date,everything(),-V2)

good_spectra0 = spec_dat_ch0 %>% filter(date == "2023-02-06 12:30:25")
good_spectra1 = spec_dat_ch1 %>% filter(date == "2023-02-06 12:30:25")

# Transposing spec data ---------------------------------------------------
# 
# ch1_spec = spec_dat_ch1 %>% 
#   filter(date > "2023-02-06 14:57" & date < "2023-02-06 15:04")

transposed_dat <- t(good_spectra1) %>% as_tibble()
# names(transposed_dat) <- transposed_dat[1,]
transposed_dat <- transposed_dat[-1,]


# Looking at spectra when integration times were good ---------------------

ch0_good_spectra = transposed_dat %>% rename(ch0 = V1) %>% mutate(id = 1:nrow(.))
ch1_good_spectra = transposed_dat %>% rename(ch1 = V1) %>% mutate(id = 1:nrow(.))

good_spectra = left_join(ch0_good_spectra,ch1_good_spectra) %>% 
  filter(id < 2049) %>% 
  mutate(ch0 = as.numeric(ch0),
         ch1 = as.numeric(ch1))

good_spectra %>% 
  pivot_longer(c(ch0,ch1)) %>% 
  ggplot(aes(id,value,col = name)) +
  geom_path()

# Plotting spectra when there's a random spike ----------------------------

plot(good_spectra$id)
