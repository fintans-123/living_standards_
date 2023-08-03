
pal <-  c( "#003CAB", "#C20800","#FFBA22")

library(pacman)
p_load(dplyr, stringr, sjlabelled,afex,emmeans,ggpubr,ggplot2,extrafont)
setwd('C:/Users/fintan.smith/Documents/Solo projects/CoL')
df<- read_spss("MERGE.sav")
useful<- df%>% dplyr::select(identity, contains("FS1a"), profile_gender, age, profile_gross_household, profile_gross_personal, pastvote_ge_2019, politics_scale_profile_update, W8)
useful<- useful%>%
  as_numeric()%>%
  mutate(equal= rowSums(select(., contains("FS1a_G_")),na.rm = T))%>%
  mutate(equal= equal/10)





for_vote_anova <- useful%>%
  filter(pastvote_ge_2019 == 1 | pastvote_ge_2019==2| pastvote_ge_2019==3)%>%
  sjlabelled::as_label(pastvote_ge_2019)

anova_vote <-afex::aov_ez(id="identity", dv="equal", between = "pastvote_ge_2019",for_vote_anova)

nice(anova_vote)

emm_vote<- as_tibble(emmeans(anova_vote, specs= "pastvote_ge_2019"))




ggplot() + geom_point(data = emm_vote, aes(x = pastvote_ge_2019, y = emmean, colour = pastvote_ge_2019),
                      position = position_dodge(width = 0.9), size = 3) + ylim(2.8,3.3)+
  geom_errorbar(data = emm_vote, aes(x = pastvote_ge_2019, ymin = lower.CL, ymax = upper.CL), width = 0.2,
                position = position_dodge(0.9), stat="identity")+
  geom_line(data = emm_vote, aes(x = pastvote_ge_2019, y = emmean, colour = pastvote_ge_2019, group = pastvote_ge_2019), position = position_dodge(width = 0.9),
            size = 0.5) +
  scale_colour_manual("Pastvote",values = pal) + 
  scale_fill_manual("pastvote_ge_2019", values = pal)+
  theme_pubclean()+theme(text=element_text(family="Arial", face="bold", size=15))+labs(title = ggtitle(' Overall, Labour voters are most likely to believe each\n expense should be affordable for those on benefits'),
                                                                                       x='Past Vote in 2019 General Election',
                                                                                       colour="pastvote_ge_2019",
                                                                                       y = "Mean rating across expenses (1| Only wealthiest - 4| Even those on benefits)",
                                                                                       caption= "24 January - 10 February 2023")+
  theme(plot.caption = element_text(color="#CCD1DB"))



