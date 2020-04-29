
aaa1 <- gss %>% 
  filter(year >= 2010) %>% 
  mutate(trad = frcode(denom == 14 & race != 2 ~ "Southern Baptist",
                       denom == 70 ~ "Non-Denom",
                       denom == 22 ~ "United Meth.")) %>% 
  mutate(att2 = frcode(attend <= 3 ~ "Low Attenders",
                       attend >= 4 ~ "High Attenders")) %>% 
  mutate(literal = case_when(bible == 1 ~ 1, bible == 2 | bible == 3 ~ 0)) %>% 
  group_by(trad, att2) %>% 
  mean_ci(literal, wt = wtss, ci = .84) %>% 
  na.omit() %>% 
  mutate(type = "Biblical Literalist")

aaa2 <- gss %>% 
  filter(year >= 2010) %>% 
  mutate(trad = frcode(denom == 14 & race != 2 ~ "Southern Baptist",
                       denom == 70 ~ "Non-Denom",
                       denom == 22 ~ "United Meth.")) %>% 
  mutate(att2 = frcode(attend <= 3 ~ "Low Attenders",
                       attend >= 4 ~ "High Attenders")) %>% 
  mutate(ba = case_when(reborn == 1 ~ 1, reborn == 2 ~ 0)) %>%  
  group_by(trad, att2) %>% 
  mean_ci(ba, wt = wtss, ci = .84) %>% 
  na.omit() %>% 
  mutate(type = "Born-Again")

aaa3 <- gss %>% 
  filter(year >= 2010) %>% 
  mutate(trad = frcode(denom == 14 & race != 2 ~ "Southern Baptist",
                       denom == 70 ~ "Non-Denom",
                       denom == 22 ~ "United Meth.")) %>% 
  mutate(att2 = frcode(attend <= 3 ~ "Low Attenders",
                       attend >= 4 ~ "High Attenders")) %>% 
  mutate(rep = case_when(partyid >= 4 & partyid <= 6 ~ 1, partyid <= 3 ~ 0)) %>%  
  group_by(trad, att2) %>% 
  mean_ci(rep, wt = wtss, ci = .84) %>% 
  na.omit() %>% 
  mutate(type = "Republican ID")


aaa4 <- gss %>% 
  filter(year >= 2010) %>% 
  mutate(trad = frcode(denom == 14 & race != 2 ~ "Southern Baptist",
                       denom == 70 ~ "Non-Denom",
                       denom == 22 ~ "United Meth.")) %>% 
  mutate(att2 = frcode(attend <= 3 ~ "Low Attenders",
                       attend >= 4 ~ "High Attenders")) %>% 
  mutate(rep = case_when(god == 6 ~ 1, god <= 5 ~ 0)) %>%  
  group_by(trad, att2) %>% 
  mean_ci(rep, wt = wtss, ci = .84) %>% 
  na.omit() %>% 
  mutate(type = "Know God Exists")

aaa5 <- gss %>% 
  filter(year >= 2010) %>% 
  mutate(trad = frcode(denom == 14 & race != 2 ~ "Southern Baptist",
                       denom == 70 ~ "Non-Denom",
                       denom == 22 ~ "United Meth.")) %>% 
  mutate(att2 = frcode(attend <= 3 ~ "Low Attenders",
                       attend >= 4 ~ "High Attenders")) %>% 
  mutate(rep = case_when(polviews == 5 | polviews == 6 | polviews == 7 ~ 1, polviews <= 4 ~ 0)) %>%  
  group_by(trad, att2) %>% 
  mean_ci(rep, wt = wtss, ci = .84) %>% 
  na.omit() %>% 
  mutate(type = "Political Conservative")

graph <- bind_df("aaa")

graph$factor <- factor(graph$type, levels = c("Know God Exists", "Born-Again", "Biblical Literalist", "Republican ID", "Political Conservative"))


left <- graph %>% 
  filter(att2 == "High Attenders") %>% 
  ggplot(., aes(y=mean, x= fct_rev(factor), color = trad)) +
  geom_point(aes(shape = trad), position=position_dodge(width=0.5), size =4) +
  geom_errorbar(aes(ymin = lower, ymax=upper), position=position_dodge(0.5), size = 1, show.legend = FALSE, width = 0) +
  coord_flip() +
  theme_gg("Abel", legend = TRUE) +
  labs(title = "", x = "Identifier", y = "Share Identifying", caption = "", subtitle = "High Attenders") +
  scale_y_continuous(labels = percent) +
  scale_color_d3() +
  ggsave("D://new_reltrad/images/fig4_left.png", type = "cairo-png", width = 8)


right <- graph %>% 
  filter(att2 == "Low Attenders") %>% 
  ggplot(., aes(y=mean, x= fct_rev(factor), color = trad)) +
  geom_point(aes(shape = trad), position=position_dodge(width=0.5), size =4) +
  geom_errorbar(aes(ymin = lower, ymax=upper), position=position_dodge(0.5), size = 1, show.legend = FALSE, width = 0) +
  coord_flip() +
  theme_gg("Abel", legend = TRUE) +
  labs(title = "", x = "Identifier", y = "Share Identifying", caption = "", subtitle = "Low Attenders") +
  scale_y_continuous(labels = percent) +
  scale_color_d3() +
  ggsave("D://new_reltrad/images/fig4_right.png", type = "cairo-png", width = 8)


graph %>% 
  ggplot(., aes(y=mean, x= fct_rev(factor), color = trad)) +
  geom_point(aes(shape = trad), position=position_dodge(width=0.5), size =4) +
  geom_errorbar(aes(ymin = lower, ymax=upper), position=position_dodge(0.5), size = 1, show.legend = FALSE, width = 0) +
  coord_flip() +
  facet_wrap(~ att2) +
  theme_gg("Abel", legend = TRUE) +
  labs(title = "", x = "Identifier", y = "Share Identifying", caption = "") +
  scale_y_continuous(labels = percent) +
  scale_color_d3() +
  ggsave("D://new_reltrad/images/stripped_fig4_both.png", type = "cairo-png", width = 10)
