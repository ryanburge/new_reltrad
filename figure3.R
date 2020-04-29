
gg <- gss %>% 
  filter(year == 2018) %>% 
  mutate(trad = frcode(denom == 14 ~ "Southern Baptist",
                       denom == 70 ~ "Non-Denom", 
                       denom == 22 ~ "United Meth.")) %>% 
  group_by(trad) %>% 
  ct(attend, wt = wtss) %>% 
  mutate(att = frcode(attend == 0 ~ "Never",
                      attend == 1 ~ "Less than\nOnce a Year",
                      attend == 2 ~ "Once a Year",
                      attend == 3 ~ "Several Times\na Year",
                      attend == 4 ~ "Once a\nMonth",
                      attend == 5 ~ "2-3x\na Month",
                      attend == 6 ~ "Nearly\nEvery Week",
                      attend == 7 ~ "Every Week",
                      attend == 8 ~ "More than\nOnce a Week")) %>% 
  na.omit() 

gg %>% 
  ggplot(., aes(x = att, y = pct, fill = trad)) +
  geom_col(color = "black", position = "dodge") +
  lab_bar(top = TRUE, type = pct, pos = .01, sz = 4) +
  theme_gg("Abel", legend = TRUE) + 
  y_pct() + 
  geom_rect(aes(xmin = 0.45, xmax = 4.55, ymin = 0, ymax = .35), fill = "gray",  alpha = .025) +
  scale_fill_d3() +
  add_text(x = 2.5, y = .28, word = "These Categories Are Unclassified\nFor Non-Denominational Protestants", sz = 5) +
  labs(x = "", y = "", title = "Attendance Distribution of Protestant Traditions", caption = "@ryanburge\nData: GSS 2018") +
  ggsave("D://new_reltrad/images/fig3.png", type = "cairo-png", width = 10)



gg %>% 
  ggplot(., aes(x = att, y = pct, fill = trad)) +
  geom_col(color = "black", position = "dodge") +
  lab_bar(top = TRUE, type = pct, pos = .01, sz = 4) +
  theme_gg("Abel", legend = TRUE) + 
  y_pct() + 
  geom_rect(aes(xmin = 0.45, xmax = 4.55, ymin = 0, ymax = .35), fill = "gray",  alpha = .025) +
  scale_fill_d3() +
  add_text(x = 2.5, y = .28, word = "These Categories Are Unclassified\nFor Non-Denominational Protestants", sz = 5) +
  labs(x = "", y = "", title = "", caption = "") +
  ggsave("D://new_reltrad/images/stripped_fig3.png", type = "cairo-png", width = 10)