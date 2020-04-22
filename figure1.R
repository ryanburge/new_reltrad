## Unclassified Time Series ####

gss <- read.fst("D://gss18b.fst")

over <- gss %>% 
  filter(year == 1982 | year == 1987) %>% 
  group_by(year) %>% 
  ct(reltrad, wt = oversamp)

## This is the weight for the rest of the sample 
wtss <- gss %>% 
  group_by(year) %>% 
  ct(reltrad, wt = wtssall)

###Removing the two years that used the overweight 
wtss <- wtss %>% 
  filter(year != 1982) %>% 
  filter(year != 1987)

## Bind them both together 
graph <- bind_rows(over, wtss) %>% 
  gss_reltrad(reltrad)


graph %>% 
  filter(reltrad == "Unclassified") %>% 
  ggplot(., aes(x = year, y = pct)) +
  geom_point(size=3, color="white") +
  geom_point(size=2, shape=1, color = "steelblue") +
  geom_point(size=1, shape=19, color = "steelblue") +  
  geom_line(color = "steelblue") + 
  scale_x_continuous(breaks = c(1978, 1988, 1998, 2008, 2018)) +
  scale_y_continuous(labels = percent) + 
  theme_gg("Abel") +
  labs(x = "", y = "", title = "Share Who Are Unclassifed in RELTRAD", caption= "Data: GSS 1972-2018") +
  ggsave("D://new_reltrad/images/fig1.png", type = "cairo-png", width = 8)
