

library(socsci)
library(car)
library(haven)
library(fst)
## This syntax was translated from Stata by Blake Eggleston ###

## Read the GSS Data in as an object named gss ####
gss <- read.fst("D://clean_gss.fst")

gss <- gss %>% as_tibble()

#####> naffil xaffil####
gss <- gss %>% 
  mutate(naffil = recode(zap_labels(relig), "1=1;2=4;3=5;4=9;5:10=6;11=1;12=6;13=1;else=0")) %>% 
  mutate(xaffil = factor(naffil,levels = c(0,1,4,5,6,9), labels = c(NA,"prot","cath","jew","other","nonaf")))


#####> Black Protestants ####
gss <- gss %>% 
  mutate(xbp = zap_labels(other)) %>% 
  mutate(xbp = recode(xbp,"7=1;14=1;15=1;21=1;37=1;38=1;56=1;78=1;79=1;85=1;86=1;87=1;88=1;98=1;103=1;104=1;128=1;133=1;else=0")) %>% 
  mutate(denom = zap_labels(denom)) %>% 
  mutate(
    xbp = case_when(
      denom == 12 ~ 1, denom == 13 ~ 1, denom == 20 ~ 1, denom == 21 ~ 1, TRUE ~ xbp
    )
  ) %>% 
  mutate(race = zap_labels(race)) %>% 
  mutate(bl = race) %>% 
  mutate(bl = recode(bl, "2=1;else=0")) %>% 
  mutate(bldenom = denom*bl) %>% 
  mutate(
    xbp = case_when(
      bldenom == 23 ~ 1, bldenom == 28 ~ 1, bldenom == 18 ~ 1, bldenom == 15 ~ 1, bldenom == 10 ~ 1, bldenom == 11 ~ 1, bldenom == 14 ~ 1, TRUE ~ xbp
    )
  ) %>% 
  mutate(other = zap_labels(other)) %>% 
  mutate(blother = other*bl) %>% 
  mutate(
    xbp = case_when(
      blother == 93 ~ 1,
      TRUE ~ xbp
    )
  )
#####> xev ####
gss <- gss %>% 
  mutate(xev = recode(other, "2=1;3=1;5=1;6=1;9=1;10=1;12=1;13=1;16=1;18=1;
                      20=1;22=1;24=1;26=1;27=1;28=1;31=1;32=1;34=1;35=1;36=1;
                      39=1;41=1;42=1;43=1;45=1;47=1;51=1;52=1;53=1;55=1;57=1;
                      63=1;65=1;66=1;67=1;68=1;69=1;76=1;77=1;83=1;84=1;90=1;
                      91=1;92=1;94=1;97=1;100=1;101=1;102=1;106=1;107=1;108=1;
                      109=1;110=1;111=1;112=1;115=1;116=1;117=1;118=1;120=1;
                      121=1;122=1;124=1;125=1;127=1;129=1;131=1;132=1;134=1;
                      135=1;138=1;139=1;140=1;146=1;else=0")) %>% 
  mutate(
    xev = case_when(
      denom == 32 ~ 1,
      denom == 33 ~ 1,
      denom == 34 ~ 1,
      denom == 42 ~ 1,
      TRUE ~ xev
    )
  ) %>% 
  mutate(wh = recode(race, "1=1;2=0;3=1")) %>% 
  mutate(whdenom = denom*wh) %>% 
  mutate(
    xev = case_when(
      whdenom == 23 ~ 1,
      whdenom == 18 ~ 1,
      whdenom == 15 ~ 1,
      whdenom == 10 ~ 1,
      whdenom == 14 ~ 1,
      TRUE ~ xev
    )
  ) %>% 
  mutate(whother = other*wh) %>% 
  mutate(
    xev = case_when(
      whother == 93 ~ 1,
      TRUE ~ xev
    )
  ) %>% 
  mutate(
    xev = case_when(
      xbp == 1 ~ 0,
      TRUE ~ xev
    )
  )
#####> xml ####
gss <- gss %>%
  mutate(xml = recode(other, "1=1;8=1;19=1;23=1;25=1;40=1;44=1;46=1;48=1;49=1;50=1;54=1;70=1;71=1;72=1;73=1;81=1;89=1;96=1;99=1;105=1;119=1;148=1;else=0")) %>% 
  mutate(
    xml = case_when(
      denom == 22 ~ 1, denom == 30 ~ 1, denom == 31 ~ 1, denom == 35 ~ 1, denom == 38 ~ 1, denom == 40 ~ 1, denom == 41 ~ 1, denom == 43 ~ 1, denom == 48 ~ 1, denom == 50 ~ 1, whdenom == 11 ~ 1, whdenom == 28 ~ 1, TRUE ~ xml
    )
  )
#####> xcath xjew xother####
gss <- gss %>% 
  mutate(xcath = recode(other, "123=1;else=0")) %>% 
  mutate(
    xcath = case_when(
      naffil == 4 ~ 1, TRUE ~ xcath
    )
  ) %>% 
  mutate(
    xjew = case_when(
      naffil == 5 ~ 1, TRUE ~ 0
    )
  ) %>% 
  mutate(xother = recode(other, "11=1;17=1;29=1;30=1;33=1;58=1;59=1;60=1;61=1;62=1;64=1;74=1;75=1;80=1;82=1;95=1;113=1;114=1;130=1;136=1;141=1;145=1;else=0")) %>% 
  mutate(noxev = 1-xev) %>% 
  mutate(noxevxaf = noxev*naffil) %>% 
  mutate(
    xother = case_when(
      noxevxaf == 6 ~ 1,
      TRUE ~ xother
    )
  )
#####> xnonaff xprotdk ####
gss <- gss %>% 
  mutate(xnonaff = recode(naffil,"9=1;else=0")) %>% 
  mutate(attend = zap_labels(attend)) %>% 
  mutate(xprotdk = recode(denom, "70=1;else=0")) %>% 
  mutate(
    xev = case_when(
      xprotdk == 1 ~ 1,
      TRUE ~ xev
    )
  )
#####> xtn inter ####
gss <- gss %>% 
  mutate(xtn = recode(zap_labels(relig), "11=1;else=0")) %>%
  mutate(denom2 = recode(denom, "70=1;10:60=0")) %>%
  mutate(
    xtn = case_when(
      denom2 == 1 ~ 2,
      TRUE ~ xtn
    )
  ) %>% 
  mutate(xtn = recode(xtn, "1=1;else=0")) %>% 
  mutate(
    xev = case_when(
      xtn == 1 ~ 1,
      TRUE ~ xev
    )
  ) %>% 
  mutate(inter = recode(zap_labels(relig),"13=1;else=0")) %>% 
  mutate(
    xev = case_when(
      inter == 1 ~ 1,TRUE ~ xev
    )
  )
#####> reltrad ####
gss <- gss %>% 
  mutate(
    reltrad = case_when(
      xnonaff == 1 ~ 7, xother == 1 ~ 6, xjew == 1 ~ 5, xcath == 1 ~ 4, xbp == 1 ~ 3, xml == 1 ~ 2, xev == 1 ~ 1, TRUE ~ 8
    )
  ) 


gss <- gss %>% 
  mutate(hiatt = case_when(attend == 4 | attend == 5 | attend == 6 | attend == 7 | attend == 8 ~ 1, TRUE ~ 0)) %>% 
  mutate(loatt = case_when(attend == 0 | attend == 1 | attend == 2 | attend == 3 | attend == 9 ~ 1, TRUE ~ 0)) 
                      

gss <- gss %>% 
  mutate(hi_evangelical = case_when(xev == 1 & hiatt == 1 ~ 1)) %>% 
  mutate(hi_mainline = case_when(xml == 1 & hiatt == 1 ~ 1)) %>% 
  mutate(hi_bprot = case_when(xbp == 1 & hiatt == 1 ~ 1)) %>% 
  mutate(hi_catholic = case_when(xcath ==1 & hiatt == 1 ~ 1)) %>%  
  mutate(hi_jewish = case_when(xjew ==1 & hiatt == 1 ~ 1)) %>% 
  mutate(hi_other = case_when(xother==1 & hiatt ==1 ~ 1)) %>%
  mutate(hi_nofaith = case_when(xnonaff == 1 ~ 1)) %>% 
  mutate(hi_reltrad = case_when(hi_evangelical == 1 ~ 1,
                             hi_mainline == 1 ~ 2, 
                             hi_bprot == 1 ~ 3, 
                             hi_catholic == 1 ~ 4, 
                             hi_jewish == 1 ~ 5, 
                             hi_other == 1 ~ 6, 
                             xnonaff == 1 ~ 7))

gss <- gss %>% 
  mutate(lo_evangelical = case_when(xev == 1 & loatt == 1 ~ 1)) %>% 
  mutate(lo_mainline = case_when(xml == 1 & loatt == 1 ~ 1)) %>% 
  mutate(lo_bprot = case_when(xbp == 1 & loatt == 1 ~ 1)) %>% 
  mutate(lo_catholic = case_when(xcath ==1 & loatt == 1 ~ 1)) %>%  
  mutate(lo_jewish = case_when(xjew ==1 & loatt == 1 ~ 1)) %>% 
  mutate(lo_other = case_when(xother==1 & loatt ==1 ~ 1)) %>%
  mutate(lo_nofaith = case_when(xnonaff == 1 ~ 1)) %>% 
  mutate(lo_reltrad = case_when(lo_evangelical == 1 ~ 1,
                                lo_mainline == 1 ~ 2, 
                                lo_bprot == 1 ~ 3, 
                                lo_catholic == 1 ~ 4, 
                                lo_jewish == 1 ~ 5, 
                                lo_other == 1 ~ 6, 
                                xnonaff == 1 ~ 7))


over <- gss %>% 
  filter(year == 1982 | year == 1987) %>% 
  group_by(year) %>% 
  ct(hi_reltrad, wt = oversamp)

## This is the weight for the rest of the sample ####
wtss <- gss %>% 
  group_by(year) %>% 
  ct(hi_reltrad, wt = wtssall)

###Removing the two years that used the overweight ####
wtss <- wtss %>% 
  filter(year != 1982) %>% 
  filter(year != 1987)

## Bind them both together ####
graph <- bind_rows(over, wtss) 

graph <- graph %>% 
  gss_reltrad(hi_reltrad) %>% 
  mutate(type = "High Attend")




over <- gss %>% 
  filter(year == 1982 | year == 1987) %>% 
  group_by(year) %>% 
  ct(lo_reltrad, wt = oversamp)

## This is the weight for the rest of the sample ####
wtss <- gss %>% 
  group_by(year) %>% 
  ct(lo_reltrad, wt = wtssall)

###Removing the two years that used the overweight ####
wtss <- wtss %>% 
  filter(year != 1982) %>% 
  filter(year != 1987)

## Bind them both together ####
graph1 <- bind_rows(over, wtss) 

graph1 <- graph1 %>% 
  gss_reltrad(lo_reltrad) %>% 
  mutate(type = "Low Attend")

gg <- bind_rows(graph, graph1) %>% 
  select(year, reltrad, n, pct, type)



gg %>% 
  filter(reltrad != "Unclassified") %>%
  filter(reltrad != "No Religion") %>% 
  ggplot(., aes(x = year, y = pct, fill = type)) +
  geom_col(color = "black") +
  facet_wrap(~ reltrad) +
  theme_gg("Abel", legend = TRUE) +
  y_pct() + 
  fill5_4() +
  scale_x_continuous(breaks = c(1978, 1988, 1998, 2008, 2018)) +
  labs(x = "", y = "", title = "", caption = "") +
  ggsave("D://new_reltrad/images/stripped_fig5.png", width = 7)





