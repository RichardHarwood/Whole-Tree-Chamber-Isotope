setwd("D:/PROJECTS/WTC_4_18O_E/R_results/")
assim_weighted_df<-wtc_df %>% 
  group_by(chamber, trt, Camp) %>% 
  filter(FluxCO2_mean > 0) %>% 
  filter(dtrans >-25 & dtrans<25) %>% 
  filter(rangewtc == "within") %>% 
  summarise(de_photo_weighted_nss=weighted.mean(evapObs,FluxCO2_mean,na.rm = T),
            de_photo_weighted_ss=weighted.mean(evapSS,FluxCO2_mean,na.rm = T),
            FluxCO2_mean_mean  = mean(FluxCO2_mean, na.rm=TRUE),
            evapObs_mean  = mean(evapSS, na.rm=TRUE),
            evapSS_mean  = mean(evapSS, na.rm=TRUE),
            Eplus_mean  = mean(Eplus, na.rm=TRUE),
            dtrans_mean  = mean(dtrans, na.rm=TRUE),
            eai_mean  = mean(eai, na.rm=TRUE),
            HWTC_mean_mean  = mean(HWTC_mean, na.rm=TRUE),
            leaftemp_mean  = mean(leaftemp, na.rm=TRUE),
            d18O.corrected_WV_mean  = mean(d18O.corrected_WV, na.rm=TRUE))

my_theme3    <- theme(axis.text.x = element_text( color="black", size=25),
                      panel.border = element_rect(fill = "NA", colour = "black", size = 2),
                      panel.background = element_blank(),
                      plot.title = element_text(size=37),
                      axis.text.y = element_text( color="black", size=25),
                      axis.title.y = element_text(color="black", size=40),
                      axis.title.x = element_text(color="black", size=40))




de_aw_gg<-assim_weighted_df %>% 
  ggplot(aes(x=de_photo_weighted_ss, y=de_photo_weighted_nss))+
  geom_point(aes(colour=as.factor(trt), shape=as.factor(Camp)), size=4)+ 
  labs(x=(bquote("Assimilation weighted"~delta[e-ss])), y=(bquote("Assimilation weighted"~delta[e])),element_text(size = 6))+
  scale_color_manual(name="Legend:",labels = c("Ambient Chamber", "Elevated Chamber"),values = c("black", "red"))+
  scale_shape_manual(name="Legend:",labels = c("Campaign 1 ", "Campaign 2" ),values = c(15,17))+
  scale_y_continuous ( breaks=scales::pretty_breaks(n=5),limits = c(9, 18))+
  scale_x_continuous ( breaks=scales::pretty_breaks(n=5),limits = c(9, 18))+
  geom_abline (slope=1,  color="black", size=2, alpha=0.5)+
  my_theme3+theme(legend.position = "none")

#+
  #theme(legend.position = c(0.15, 0.85),
        #legend.background = element_blank(),
        #legend.box.background = element_rect(fill = "white", color = "black"), 
        #legend.spacing.y = unit(0, "cm"));de_aw_gg

#ggsave("Assimilation_weighted_graph.png",plot=de_aw_gg, width = 35, height = 35, units = "cm", dpi=300)
#
#mutate(evapObs=((1+Eplus/1000)*((1+ek_de/1000)*(1+dtrans/1000)*(1-eai)+eai*(1+d18O.corrected_WV/1000))-1)*1000) %>% 
#mutate(evapSS=((1+Eplus/1000)*((1+ek_de/1000)*(1+-3.28/1000)*(1-eai)+eai*(1+d18O.corrected_WV/1000))-1)*1000)
#

#why is c1 more enriched ?
assim_weighted_df_test<-wtc_df %>% 
  filter(FluxCO2_mean > 0) %>% 
  filter(dtrans >-25 & dtrans<25) %>% 
  filter(rangewtc == "within") %>% 
  select(evapObs,FluxCO2_mean,evapSS,Eplus,eai,ek_de,d18O.corrected_WV,dtrans,chamber, trt, Camp)

mean_test<-wtc_df %>% 
  group_by(Camp, trt) %>% 
  filter(FluxCO2_mean > 0) %>% 
  filter(dtrans >-25 & dtrans<25) %>% 
  filter(rangewtc == "within") %>% 
  summarise(de_photo_weighted_nss=weighted.mean(evapObs,FluxCO2_mean,na.rm = T),
            de_photo_weighted_ss=weighted.mean(evapSS,FluxCO2_mean,na.rm = T),
            FluxCO2_mean_mean  = mean(FluxCO2_mean, na.rm=TRUE),
            evapObs_mean  = mean(evapSS, na.rm=TRUE),
            evapSS_mean  = mean(evapSS, na.rm=TRUE),
            Eplus_mean  = mean(Eplus, na.rm=TRUE),
            dtrans_mean  = mean(dtrans, na.rm=TRUE),
            eai_mean  = mean(eai, na.rm=TRUE),
            HWTC_mean_mean  = mean(HWTC_mean, na.rm=TRUE),
            leaftemp_mean  = mean(leaftemp, na.rm=TRUE),
            d18O.corrected_WV_mean  = mean(d18O.corrected_WV, na.rm=TRUE))

assim_weighted_df %>% 
  ggplot(aes(x=leaftemp_mean, y=de_photo_weighted_nss))+
  geom_point(aes(colour=as.factor(trt), shape=as.factor(Camp)), size=4)

#Within
assim_weighted_df$dif<-assim_weighted_df$de_photo_weighted_nss-assim_weighted_df$de_photo_weighted_ss
assim_weighted_df$dif
