early_dat = read.csv("data/processed_data/processed_problem_period.csv") %>% 
  mutate(date = dmy_hms(date))


early_dat %>% 
  filter(date > "2023-02-08 12:00" & date < "2023-02-08 13:00") %>% 
  ggplot(aes(date,hono)) +
  geom_point() +
  scale_x_datetime(date_breaks = "15 min",date_labels = "%H:%M") 

#1 for water/abs closed
#2 for zero
#3 for cal
#4 for air in abs
#5 other

flagged_data = early_dat %>% 
  mutate(flag = case_when(date < "2023-02-07 10:00" ~ 5,#not sure why, just looks weird,zero in there somewhere
                          between(date,as.POSIXct("2023-02-07 15:20"),as.POSIXct("2023-02-07 16:05")) ~ 2,
                          between(date,as.POSIXct("2023-02-07 21:20"),as.POSIXct("2023-02-07 22:25")) ~ 2,
                          between(date,as.POSIXct("2023-02-08 03:00"),as.POSIXct("2023-02-08 04:45")) ~ 5,#zero somewhere in here, also insturmental noise spikes
                          between(date,as.POSIXct("2023-02-08 09:00"),as.POSIXct("2023-02-08 10:15")) ~ 5,#instrumental noise spike weridness, as well as zero
                          between(date,as.POSIXct("2023-02-08 09:00"),as.POSIXct("2023-02-08 10:15")) ~ 5,
                          TRUE ~ 0))