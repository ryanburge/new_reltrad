

gss <- read.fst("D://gss18b.fst")


graph <- gss %>% 
  gss_reltrad(reltrad) %>% 
  filter(reltrad == "Unclassified") %>% 
  group_by(year) %>% 
  ct(denom, wt = wtss) %>% 
  mutate(denom = frcode(denom == 60 ~ "Other Prot.",
                        denom == 70 ~ "Non-Denom.", TRUE ~ "Something\nElse")) 

graph %>% 
  ggplot(., aes(x = year, y = pct, color = denom, group = denom)) +
  geom_point(size=3, color="white") +
  geom_point(size=2, shape=1) +
  geom_point(size=1, shape=19) +
  geom_line() +
  scale_color_lancet() + 
  y_pct() +
  theme_gg("Abel") +
  add_text(x = 2006, y = .80, word = "Non-Denoms.", sz = 4) +
  add_text(x = 1993, y = .38, word = "Something\nElse", sz = 4) +
  add_text(x = 2005, y = .04, word = "Other Prot.", sz = 4) +
  scale_x_continuous(breaks = c(1978, 1988, 1998, 2008, 2018)) +
  labs(x = "", y = "Share of the Unclassified", title = "Most of the Unclassifieds Are Non-Denominational", caption = "Data: GSS 1972-2018") +
  ggsave("D://new_reltrad/images/fig2.png", type = "cairo-png", width = 8)
