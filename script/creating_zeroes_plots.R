final_dat_night %>%
  pivot_longer(c(ch2,ch2_zeroed,ch2_zeroes)) %>% 
  mutate(value = ifelse(flag == 0,value,NA_real_)) %>% 
  ggplot(aes(date,value)) +
  geom_path() +
  facet_grid(rows = vars(name),scales = "free_y")

test_night = final_dat_night %>% 
  select(date,ch1_zeroes_night = ch1_zeroes,ch1_zeroed_night = ch1_zeroed,ch1_ppt_night = ch1_ppt,
         ch2_zeroes_night = ch2_zeroes,ch2_zeroed_night = ch2_zeroed,ch2_ppt_night = ch2_ppt,hono_night = hono,
         ch1_night,ch2_night)

comparison = final_dat2 %>% left_join(test_night) %>% 
  select(-zeroing)

comparison %>%
  pivot_longer(c(hono,hono_night)) %>%
  # pivot_longer(c(ch2,ch2_zeroed,ch2_zeroed_night)) %>%
  # pivot_longer(c(ch1_zeroes_night,hono_night,ch1)) %>%
  mutate(value = ifelse(flag == 0,value,NA_real_)) %>% 
  ggplot() +
  geom_path(aes(date,value,col = name)) +
  # geom_point(aes(date,ch1_night),size = 3) +
  # geom_point(aes(date,ch2_night),size = 3) +
  scale_color_taylor() +
  labs(x = "Date",
       y = "Abs",
       col = NULL) +
  scale_x_datetime(date_breaks = "1 day",date_labels = "%b %d") +
  theme(legend.position = "top") +
  # facet_grid(rows = vars(name),scales = "free_y") +
  NULL
  
  ggsave('night_vs_za_r2.png',
         path = "output/plots_analysis/zeroing",
         width = 30,
         height = 12,
         units = 'cm')
