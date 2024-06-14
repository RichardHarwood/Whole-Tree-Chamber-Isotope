###Compare dv from ss and nss
my_theme2    <- theme(axis.text.x = element_text( color="black", size=25),
                      panel.border = element_rect(fill = "NA", colour = "black", size = 2),
                      panel.background = element_blank(),
                      plot.title = element_text(size=37),
                      axis.text.y = element_text( color="black", size=25),
                      axis.title.y = element_text(color="black", size=40),
                      axis.title.x = element_text(color="black", size=40),
                      legend.position = "none")

my_theme3    <- theme(axis.text.x = element_text( color="black", size=25),
                      panel.border = element_rect(fill = "NA", colour = "black", size = 2),
                      panel.background = element_blank(),
                      plot.title = element_text(size=37),
                      axis.text.y = element_text( color="black", size=25),
                      axis.title.y = element_text(color="black", size=25),
                      axis.title.x = element_text(color="black", size=25))

wtc_df$cond_steady_state<-wtc_df$d18O.corrected_WV+wtc_df$Eplus

wtc_df$d18O.corrected_Cond

wtc_df$Eplus

wtc_df %>% 
  #filter(Camp== 1) %>% filter(trt == "ambient") %>% 
  ggplot(aes(x=cond_steady_state, y=d18O.corrected_Cond))+
  geom_point(aes(colour=as.factor(trt), shape=as.factor(Camp)), size=2)+ 
  #labs(x=(bquote("Assimilation weighted"~delta[e-ss])), y=(bquote("Assimilation weighted"~delta[e])),element_text(size = 6))+
  scale_color_manual(name="Legend:",labels = c("Ambient Chamber", "Elevated Chamber"),values = c("black", "red"))+
  scale_shape_manual(name="Legend:",labels = c("Campaign 1 ", "Campaign 2" ),values = c(15,17))+
  scale_y_continuous ( breaks=scales::pretty_breaks(n=5),limits = c(-5, 5))+
  scale_x_continuous ( breaks=scales::pretty_breaks(n=5),limits = c(-5, 5))+
  geom_abline (slope=1,  color="grey", size=2)+
  my_theme3+
  labs(y=(bquote(delta[C])), x=(bquote(delta[C-ss])),element_text(size = 6))


dv_df<-wtc_df %>% 
  filter(sun == "day") %>% 
  filter(rangewtc == "within") %>% 
  filter(dtrans >-25 & dtrans<25) %>% 
  mutate(dv_dtrans_calc=(E*dtrans+F*d18O.corrected_WV.AMB.-C*d18O.corrected_Cond-Scalculated*deltas)/V) %>% 
  mutate(dv_at_zero=(E*0+F*d18O.corrected_WV.AMB.-C*d18O.corrected_Cond-Scalculated*deltas)/V) %>% 
  #mutate(dv_at_source=(E*source+F*d18O.corrected_WV.AMB.-C*cond_steady_state)/V) %>% #old
  mutate(dv_at_source=(E*source+F*d18O.corrected_WV.AMB.-C*Eplus)/(V+C)) %>% #LUCAS INTEGRATED
  rename(dv_lgr=d18O.corrected_WV) %>% 
  select(source,dtrans,E,Scalculated,deltas,F,d18O.corrected_WV.AMB.,C,d18O.corrected_Cond,cond_steady_state,V,dv_lgr,
         dv_dtrans_calc,dv_at_zero,dv_lgr,dv_at_source, trt,rangewtc,Camp,chamber)
  
head(dv_df)

write.csv(dv_df, "dv_df.csv")


dv_ss_gg_c1_amb<-dv_df %>% 
  filter(Camp== 1) %>% filter(trt == "ambient") %>% 
  ggplot(aes(x=dv_lgr, y=dv_at_source))+
  geom_point(aes(),colour="black", size=2)+ 
  labs(x=(bquote(delta[v])), y = (bquote(delta[v-ss])),element_text(size = 6))+
  scale_y_continuous ( breaks=scales::pretty_breaks(n=5),limits = c(-15, -8))+
  scale_x_continuous ( breaks=scales::pretty_breaks(n=5),limits = c(-15, -8))+
  geom_abline (slope=1,  color="black", size=2)+
  ggtitle(("(a) Campaign 1 (Ambient)"))+
  my_theme2;

dv_ss_gg_c1_hot<-dv_df %>% 
  filter(Camp== 1) %>% filter(trt == "elevated ") %>% 
  ggplot(aes(x=dv_lgr, y=dv_at_source))+
  geom_point(aes(),colour="red", size=2)+ 
  labs(x=(bquote(delta[v])), y = (bquote(delta[v-ss])),element_text(size = 6))+
  scale_y_continuous ( breaks=scales::pretty_breaks(n=5),limits = c(-15, -8))+
  scale_x_continuous ( breaks=scales::pretty_breaks(n=5),limits = c(-15, -8))+
  geom_abline (slope=1,  color="black", size=2)+
  ggtitle(("(b) Campaign 1 (Elevated)"))+
  my_theme2;

dv_ss_gg_c2_amb<-dv_df %>% 
  filter(Camp== 3) %>% filter(trt == "ambient") %>% 
  ggplot(aes(x=dv_lgr, y=dv_at_source))+
  geom_point(aes(),colour="black", size=2)+ 
  labs(x=(bquote(delta[v])), y = (bquote(delta[v-ss])),element_text(size = 6))+
  scale_y_continuous ( breaks=scales::pretty_breaks(n=5),limits = c(-15, -8))+
  scale_x_continuous ( breaks=scales::pretty_breaks(n=5),limits = c(-15, -8))+
  geom_abline (slope=1,  color="black", size=2)+
  ggtitle(("(c) Campaign 2 (Ambeint)"))+
  my_theme2;

dv_ss_gg_c2_hot<-dv_df %>% 
  filter(Camp== 3) %>% filter(trt == "elevated ") %>% 
  ggplot(aes(x=dv_lgr, y=dv_at_source))+
  geom_point(aes(),colour="red", size=2)+ 
  labs(x=(bquote(delta[v])), y = (bquote(delta[v-ss])),element_text(size = 6))+
  scale_y_continuous ( breaks=scales::pretty_breaks(n=5),limits = c(-15, -8))+
  scale_x_continuous ( breaks=scales::pretty_breaks(n=5),limits = c(-15, -8))+
  geom_abline (slope=1,  color="black", size=2)+
  ggtitle(("(d) Campaign 2 (Elevated)"))+
  my_theme2;


dv_graph=plot_grid(dv_ss_gg_c1_amb,
                   dv_ss_gg_c1_hot,
                   dv_ss_gg_c2_amb,
                   dv_ss_gg_c2_hot,
                   ncol = 2, align = 'hv')

ggsave("fig5_new.png",plot=dv_graph, width = 40, height = 40, units = "cm", dpi=100)

#explore
wtc_df %>% 
  #filter(Camp== 1) %>% filter(trt == "ambient") %>% 
  ggplot(aes(x=d18O.corrected_WV.AMB., y=d18O.corrected_WV))+
  geom_point(aes(colour=as.factor(trt), shape=as.factor(Camp)), size=2)+ 
  #labs(x=(bquote("Assimilation weighted"~delta[e-ss])), y=(bquote("Assimilation weighted"~delta[e])),element_text(size = 6))+
  scale_color_manual(name="Legend:",labels = c("Ambient Chamber", "Elevated Chamber"),values = c("black", "red"))+
  scale_shape_manual(name="Legend:",labels = c("Campaign 1 ", "Campaign 2" ),values = c(15,17))+
  #scale_y_continuous ( breaks=scales::pretty_breaks(n=5),limits = c(-5, 5))+
  #scale_x_continuous ( breaks=scales::pretty_breaks(n=5),limits = c(-5, 5))+
  #geom_abline (slope=1,  color="grey", size=2)+
  my_theme3+
  facet_wrap(~trt+Camp)
  #labs(y=(bquote(delta[C])), x=(bquote(delta[C-ss])),element_text(size = 6))


wtc_df['Time']<-(dmy_hm(wtc_df$Tdh, quiet=TRUE, tz="UTC"))
wtc_df %>% 
  filter(Camp== 1) %>% filter(trt == "ambient") %>% 
  ggplot(aes(x=Time, y=d18O.corrected_WV.AMB.))+
  geom_point(aes(colour=as.factor(trt), shape=as.factor(Camp)), size=2)
  #labs(x=(bquote("Assimilation weighted"~delta[e-ss])), y=(bquote("Assimilation weighted"~delta[e])),element_text(size = 6))+
  #scale_y_continuous ( breaks=scales::pretty_breaks(n=5),limits = c(-5, 5))+
  #scale_x_date ( breaks=scales::pretty_breaks(n=5))+
  #geom_abline (slope=1,  color="grey", size=2)+
  #my_theme3
#labs(y=(bquote(delta[C])), x=(bquote(delta[C-ss])),element_text(size = 6))

wtc_df %>% 
  filter(Camp== 3) %>% filter(trt == "ambient") %>% 
  ggplot(aes(x=Time, y=d18O.corrected_WV.AMB.))+
  geom_point(aes(colour=as.factor(trt), shape=as.factor(Camp)), size=2)
  #labs(x=(bquote("Assimilation weighted"~delta[e-ss])), y=(bquote("Assimilation weighted"~delta[e])),element_text(size = 6))+

  #scale_y_continuous ( breaks=scales::pretty_breaks(n=5),limits = c(-5, 5))+
  #scale_x_date ( breaks=scales::pretty_breaks(n=5))+
  #geom_abline (slope=1,  color="grey", size=2)+
  #my_theme3

dv_df$differnce=abs(dv_df$dv_lgr) -abs(dv_df$dv_at_source)

max(dv_df$differnce)
mean(dv_df$differnce)
