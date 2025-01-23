
arna_ground %>% 
  filter(campaign != "February 2020", campaign != "November 2015",campaign != "August 2019",
         altitude < 500 | is.na(altitude) == T) %>% 
  mutate(f_sub_matt = f_calc - f_para_matt,
         f_sub_simone = f_calc - f_para_simone) %>% 
  # mutate(campaign = case_when(campaign == "ARNA 2019" & altitude >))
  rename(`f[Rowlinson]` = f_ratio_matt,
         `f[Andersen]` = f_ratio_simone,
         `f[obs]` = f_calc) %>%
  pivot_longer(c(`f[Andersen]`,`f[Rowlinson]`)) %>%
  ggplot(aes(rh,value,shape = campaign,col = nitrate_ug_m3)) +
  geom_point(size = 2) +
  theme_bw() +
  facet_wrap(~name,scales = "free",
             labeller = label_parsed) +
  scale_colour_viridis_c() +
  # scale_colour_manual(values = c("steelblue1","navy","darkolivegreen3","springgreen4","darkorange"),
  #                     breaks = c("ARNA 2019","ARNA 2020","November 2015","August 2019","February 2023")) +
  labs(x = "RH (%)",
       y = expression(f[obs]/f[parametrised]),
       col = expression(Nitrate~(ug~m^-3)),
       shape = "Campaign") +
  # geom_hline(yintercept = 1,linetype = "dashed") +
  # theme(legend.position = "top") +
  # scale_colour_viridis_d() +
  NULL

dat_for_r_squared = arna_ground %>% 
  filter(campaign != "February 2020", campaign != "November 2015",campaign != "August 2019",
         altitude < 500 | is.na(altitude) == T)

model <- lm(f_ratio_simone ~ rh, data = dat_for_r_squared)
r_squared <- summary(model)$r.squared

ggsave('f_ratio_vs_rh_coloured_nitrate.png',
       path = "output/plots/rh_f_relationship",
       width = 30,
       height = 14,
       units = 'cm')
