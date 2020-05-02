## Run old_reltrad.R 

old <- gss

regg <- old %>%
  mutate(black = case_when(race == 2 ~ 1, TRUE ~ 0)) %>% 
  mutate(female = case_when(sex == 2 ~ 1, TRUE ~ 0)) %>% 
  mutate(south = case_when(region == 5 | region == 6 | region == 7 ~ 1, TRUE ~ 0)) %>% 
  mutate(literal = case_when(bible == 1 ~ 1, TRUE ~ 0)) %>% 
  gss_reltrad(reltrad) %>% 
  filter(reltrad != "Unclassified") %>% 
  mutate(reltrad = fct_relevel(reltrad, "No Religion")) %>% 
  select(black, age, educ, female, year, income, south, attend, literal, reltrad) 


reg1 <- lm(attend ~ black + age + educ + female + year + income + south + reltrad, data = regg)

## Run figure8.R

new <- gss

regg2 <- new %>%
  mutate(black = case_when(race == 2 ~ 1, TRUE ~ 0)) %>% 
  mutate(female = case_when(sex == 2 ~ 1, TRUE ~ 0)) %>% 
  mutate(south = case_when(region == 5 | region == 6 | region == 7 ~ 1, TRUE ~ 0)) %>% 
  mutate(literal = case_when(bible == 1 ~ 1, TRUE ~ 0)) %>% 
  filter(reltrad != "Unclassified") %>% 
  mutate(reltrad = fct_relevel(reltrad, "No Religion")) %>% 
  select(black, age, educ, female, year, income, south, attend, literal, reltrad) 

reg2 <- lm(attend ~ black + age + educ + female + year + income + south + reltrad, data = regg2)

coef_names <- c("Black" = "black",
                "Age" = "age",
                "Education" = "educ",
                "Female" = "female",
                "Year" = "year",
                "Income" = "income",
                "South" = "south",
                "Evangelical" = "reltradEvangelical",
                "Mainline" = "reltradMainline",
                "Black Prot." = "reltradBlack Prot.",
                "Catholic" = "reltradCatholic",
                "Jewish" = "reltradJewish",
                "Other Faith" = "reltradOther Faith",
                "Unclassified" = "reltradUnclassified", 
                "Non-Denom." = "reltradNon-Denom.")

gg <- plot_summs(reg1, reg2, scale = TRUE, robust = "HC3", coefs = coef_names, model.names = c("Prior Version", "New Version"), colors = "Qual2")

gg + 
  theme_gg("Abel", legend = TRUE) + 
  labs(y = "", x = "Predicting Church Attendance", subtitle = "") +
  ggsave("D://new_reltrad/images/figure9.png", type = "cairo-png", width = 7)
