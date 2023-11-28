###Compare dv from ss and nss
my_theme2    <- theme(axis.text.x = element_text( color="black", size=25),
                      panel.border = element_rect(fill = "NA", colour = "black", size = 2),
                      panel.background = element_blank(),
                      plot.title = element_text(size=37),
                      axis.text.y = element_text( color="black", size=25),
                      axis.title.y = element_text(color="black", size=40),
                      axis.title.x = element_text(color="black", size=40),
                      legend.position = "none")




dv_df<-wtc_df %>% 
  filter(sun == "day") %>% 
  filter(rangewtc == "within") %>% 
  filter(dtrans >-25 & dtrans<25) %>% 
  mutate(dv_dtrans_calc=(E*dtrans+F*d18O.corrected_WV.AMB.-C*d18O.corrected_Cond-Scalculated*deltas)/V) %>% 
  mutate(dv_at_zero=(E*0+F*d18O.corrected_WV.AMB.-C*d18O.corrected_Cond-Scalculated*deltas)/V) %>% 
  mutate(dv_at_source=(E*(-3.28)+F*d18O.corrected_WV.AMB.-C*d18O.corrected_Cond-Scalculated*deltas)/V) %>%
  rename(dv_lgr=d18O.corrected_WV) %>% 
  select(dv_dtrans_calc,dv_at_zero,dv_lgr,dv_at_source, trt,rangewtc,Camp,rangewtc,chamber,dtrans) 

dv_df$dif<-abs(dv_df$dv_at_source)-abs(dv_df$dv_lgr )
min(dv_df$dif, na.rm=TRUE)
max(dv_df$dif, na.rm=TRUE)

dv_df%>%
  group_by(trt, Camp)%>% 
  summarise(dv_lgr_mean  = mean(dv_lgr, na.rm=TRUE),
            dv_lgr_SD  = sd(dv_lgr, na.rm=TRUE),
            dv_at_source_mean  = mean(dv_at_source, na.rm=TRUE),
            dv_at_source_SD  = sd(dv_at_source, na.rm=TRUE))

  

dv_ss_gg_c1_amb<-dv_df %>% 
  filter(Camp== 1) %>% filter(trt == "ambient") %>% 
  ggplot(aes(x=dv_lgr, y=dv_at_source))+
  geom_point(aes(),colour="black", size=3)+ 
  labs(x=(bquote(delta[v])), y = (bquote(delta[v-ss])),element_text(size = 6))+
  scale_y_continuous ( breaks=scales::pretty_breaks(n=5),limits = c(-20, 20))+
  scale_x_continuous ( breaks=scales::pretty_breaks(n=5),limits = c(-20, 20))+
  geom_abline (slope=1,  color="black", size=2)+
  ggtitle(("(a) Campaign 1 (Ambient)"))+
  my_theme2;

dv_ss_gg_c1_hot<-dv_df %>% 
  filter(Camp== 1) %>% filter(trt == "elevated ") %>% 
  ggplot(aes(x=dv_lgr, y=dv_at_source))+
  geom_point(aes(),colour="red", size=3)+ 
  labs(x=(bquote(delta[v])), y = (bquote(delta[v-ss])),element_text(size = 6))+
  scale_y_continuous ( breaks=scales::pretty_breaks(n=5),limits = c(-20, 20))+
  scale_x_continuous ( breaks=scales::pretty_breaks(n=5),limits = c(-20, 20))+
  geom_abline (slope=1,  color="black", size=2)+
  ggtitle(("(b) Campaign 1 (Elevated)"))+
  my_theme2;

dv_ss_gg_c2_amb<-dv_df %>% 
  filter(Camp== 3) %>% filter(trt == "ambient") %>% 
  ggplot(aes(x=dv_lgr, y=dv_at_source))+
  geom_point(aes(),colour="black", size=3)+ 
  labs(x=(bquote(delta[v])), y = (bquote(delta[v-ss])),element_text(size = 6))+
  scale_y_continuous ( breaks=scales::pretty_breaks(n=5),limits = c(-20, 20))+
  scale_x_continuous ( breaks=scales::pretty_breaks(n=5),limits = c(-20, 20))+
  geom_abline (slope=1,  color="black", size=2)+
  ggtitle(("(c) Campaign 2 (Ambient)"))+
  my_theme2;

dv_ss_gg_c2_hot<-dv_df %>% 
  filter(Camp== 3) %>% filter(trt == "elevated ") %>% 
  ggplot(aes(x=dv_lgr, y=dv_at_source))+
  geom_point(aes(),colour="red", size=3)+ 
  labs(x=(bquote(delta[v])), y = (bquote(delta[v-ss])),element_text(size = 6))+
  scale_y_continuous ( breaks=scales::pretty_breaks(n=5),limits = c(-20, 20))+
  scale_x_continuous ( breaks=scales::pretty_breaks(n=5),limits = c(-20, 20))+
  geom_abline (slope=1,  color="black", size=2)+
  ggtitle(("(d) Campaign 2 (Elevated)"))+
  my_theme2;


dv_graph=plot_grid(dv_ss_gg_c1_amb,
                   dv_ss_gg_c1_hot,
                   dv_ss_gg_c2_amb,
                   dv_ss_gg_c2_hot,
                   ncol = 2, align = 'hv')

dv_df %>% 
  #filter(Tdh == "25/10/2016 10:00") %>% 
  group_by(trt, Camp)%>% 
  filter(dv_at_source >-18 & dv_at_source<5) %>% 
  summarise(dv_lgr_mean  = mean(dv_lgr, na.rm=TRUE),
            dv_lgr_SD  = sd(dv_lgr, na.rm=TRUE),
            dv_at_source_mean  = mean(dv_at_source, na.rm=TRUE),
            dv_at_source_SD  = sd(dv_at_source, na.rm=TRUE))

