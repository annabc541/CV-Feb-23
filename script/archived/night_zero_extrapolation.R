#finding values to add in order to extrapolate nighttime zero values properly for 24th February

#creating small df with only data that will be used for extrapolation
night_avg_ex = night_avg %>% 
  filter(id != 6,id != 2) %>% 
  select(x = idx,y = ch1_night)

model = lm(y ~ x,data = night_avg_ex)
my_coef = coef(model)
predict(model,newdata = data.frame(x = 14299)) #14299 last row number from 24th February, y = 0.05299002

night_avg_ex[nrow(night_avg_ex) + 1,] = list(14299,0.05299002)

night_avg_ex %>% 
  ggplot(aes(x,y)) +
  geom_point()

#repeat process for ch2
night_avg_ex = night_avg %>% 
  filter(id != 6, id != 2) %>%
  select(x = idx,y = ch2_night)

model = lm(y ~ x,data = night_avg_ex)
my_coef = coef(model)
predict(model,newdata = data.frame(x = 14299)) #y = 0.04949119

night_avg_ex[nrow(night_avg_ex) + 1,] = list(14299,0.04949119)

night_avg_ex %>% 
  ggplot(aes(x,y)) +
  geom_point()

