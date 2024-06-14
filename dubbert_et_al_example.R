
wtc_df=wtc_df%>% 
  filter(dtrans >-25 & dtrans<25) %>% 
  mutate(Camp = str_replace_all(Camp, pattern = "3", replacement = "Campaign 2")) %>% 
  mutate(Camp = str_replace_all(Camp, pattern = "1", replacement = "Campaign 1"))

wtc_df$DELTA_e= (wtc_df$evapObs-wtc_df$source)/(1+wtc_df$source)

wtc_df$DELTA_e_ss= (wtc_df$evapSS-wtc_df$source)/(1+wtc_df$source)

wtc_df$epkepplus=wtc_df$alphak*wtc_df$betaplus

wtc_df$oneminush=1-(wtc_df$RH_al_mean/100)

wtc_df$DELTA_E=(wtc_df$DELTA_e-wtc_df$DELTA_e_ss)/(wtc_df$epkepplus*wtc_df$oneminush)


wtc_df$De_minus_D_e_ss=wtc_df$DELTA_e-wtc_df$DELTA_e_ss
wtc_df$De_minus_D_e_ss_div_rh=(wtc_df$DELTA_e-wtc_df$DELTA_e_ss)/(1-(wtc_df$RH_al_mean/100))

my_theme3    <- theme(axis.text.x = element_text( color="black", size=25),
                      panel.border = element_rect(fill = "NA", colour = "black", size = 2),
                      panel.background = element_blank(),
                      plot.title = element_text(size=37),
                      axis.text.y = element_text( color="black", size=25),
                      axis.title.y = element_text(color="black", size=25),
                      axis.title.x = element_text(color="black", size=25))

#labs(y=(bquote(delta[v])), x = "",element_text(size = 6))+


wtc_df %>% 
  #filter(Camp== 1) %>% filter(trt == "ambient") %>% 
  ggplot(aes(x=De_minus_D_e_ss, y=DELTA_E))+
  geom_point(aes(colour=as.factor(trt), shape=as.factor(Camp)), size=2)+ 
  #labs(x=(bquote("Assimilation weighted"~delta[e-ss])), y=(bquote("Assimilation weighted"~delta[e])),element_text(size = 6))+
  scale_color_manual(name="Legend:",labels = c("Ambient Chamber", "Elevated Chamber"),values = c("black", "red"))+
  scale_shape_manual(name="Legend:",labels = c("Campaign 1 ", "Campaign 2" ),values = c(15,17))+
  scale_y_continuous ( breaks=scales::pretty_breaks(n=5),limits = c(-3, 3))+
  scale_x_continuous ( breaks=scales::pretty_breaks(n=5),limits = c(-3, 3))+
  geom_abline (slope=1,  color="grey", size=2)+
  my_theme3+
  labs(y=(bquote(Delta[E])), x=(bquote(Delta[e]~"-"~Delta[C])),element_text(size = 6))+
  facet_wrap(~trt+Camp)

wtc_df %>% 
  #filter(Camp== 1) %>% filter(trt == "ambient") %>% 
  ggplot(aes(x=De_minus_D_e_ss_div_rh, y=DELTA_E))+
  geom_point(aes(colour=as.factor(trt), shape=as.factor(Camp)), size=2)+ 
  #labs(x=(bquote("Assimilation weighted"~delta[e-ss])), y=(bquote("Assimilation weighted"~delta[e])),element_text(size = 6))+
  scale_color_manual(name="Legend:",labels = c("Ambient Chamber", "Elevated Chamber"),values = c("black", "red"))+
  scale_shape_manual(name="Legend:",labels = c("Campaign 1 ", "Campaign 2" ),values = c(15,17))+
  scale_y_continuous ( breaks=scales::pretty_breaks(n=5),limits = c(-3, 3))+
  scale_x_continuous ( breaks=scales::pretty_breaks(n=5),limits = c(-3, 3))+
  geom_abline (slope=1,  color="grey", size=0.5)+
  my_theme3+
  labs(y=(bquote(Delta[E])), x=(bquote(Delta[e]~"-"~Delta[C]~"/ (1-h)")),element_text(size = 6))+
  facet_wrap(~trt+Camp)


wtc_df %>% 
  #filter(Camp== 1) %>% filter(trt == "ambient") %>% 
  ggplot(aes(x=dtrans, y=RH_al_mean))+
  geom_point(aes(colour=as.factor(trt), shape=as.factor(Camp)), size=2)



