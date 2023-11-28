#There are 4 different environments (2 treatments x 2 times), and I think there should be one panel for each environment. 
#Of course dF will have only two panels but all others will have four.
my_theme    <- theme(axis.text.x = element_text( color="black", size=22, angle=90, vjust = 0.5, hjust=1),
                     panel.border = element_rect(fill = "NA", colour = "black", size = 2),
                     panel.background = element_blank(),
                     plot.title = element_text(size=37),
                     axis.text.y = element_text( color="black", size=25),
                     axis.title.y = element_text(color="black", size=40),
                     legend.position = "none")

start = as.character('2016-10-24 19:00:00') 
end = as.character('2016-10-25 05:00:00', tz="UTC")
start2 = as.character('2016-11-20 19:00:00') 
end2 = as.character('2016-11-21 05:00:00', tz="UTC")

annotation_c1<-annotate("rect", fill = "black", alpha = 0.15, 
                        xmin = end, xmax = start,
                        ymin = -Inf, ymax = Inf)
annotation_c2<-annotate("rect", fill = "black", alpha = 0.15, 
                        xmin = end2, xmax = start2,
                        ymin = -Inf, ymax = Inf)

dfa<-wtc_df
dfa["Td"]<-(dmy_hm(dfa$Td, quiet=TRUE, tz="UTC"));str(dfa$Td)
dfa$Td<-as.character(dfa$Td);str(dfa$Td)
########
breaks_1_df =dfa %>% 
  filter(Camp== 1) %>% filter(trt == "ambient") %>% filter(chamber=="C01") %>% 
  select(Td)
breaks_1<-breaks_1_df$Td

labels_1<-str_sub(breaks_1_df$Td, -8)
labels_1<-str_sub(labels_1, end=-4)
labels_1 <- as.data.frame(labels_1) %>% mutate(labels_1=ifelse(row_number()%%2==0,"",labels_1))
labels_1<-labels_1$labels_1
#breaks_1=as.character(paste0( breaks_1$Td, collapse=","))
########################
scale_c_1=scale_x_discrete(
  breaks = breaks_1,
  labels = labels_1)

######
########
breaks_2_df =dfa %>% 
  filter(Camp== 3) %>% filter(trt == "ambient") %>% filter(chamber=="C01") %>% 
  select(Td)
breaks_2<-breaks_2_df$Td

labels_2<-str_sub(breaks_2_df$Td, -8)
labels_2<-str_sub(labels_2, end=-4)
labels_2 <- as.data.frame(labels_2) %>% mutate(labels_2=ifelse(row_number()%%2==0,"",labels_2))
labels_2<-labels_2$labels_2
#breaks_1=as.character(paste0( breaks_1$Td, collapse=","))
########################
scale_c_2=scale_x_discrete(
  breaks = breaks_2,
  labels = labels_2)

#Isotopic leaf water turnovertime 
gg_amb_c1_isoh<-dfa %>% 
  filter(Camp== 1) %>% filter(trt == "ambient") %>% 
  ggplot(aes(x=Td, y=thiso))+
  annotation_c1+
  ggtitle(("(a)"))+
  geom_boxplot(aes(x=as.factor(Td), y=thiso),outlier.shape = NA)+
  geom_jitter(aes(),width = 0.2)+
  labs(y=(bquote(tau[i])), x = "",element_text(size = 6))+
  scale_y_continuous ( breaks=scales::pretty_breaks(n=5),
                       limits = c(0, 20))+scale_c_1+
  my_theme;gg_amb_c1_isoh


gg_hot_c1_isoh<-dfa %>% 
  filter(Camp== 1) %>% filter(trt == "elevated ") %>% 
  ggplot(aes(x=Td, y=thiso))+
  annotation_c1+
  ggtitle(("(b)"))+
  geom_boxplot(aes(x=as.factor(Td), y=thiso),colour="red",,outlier.shape = NA)+
  geom_jitter(aes(),width = 0.2,colour="red")+
  labs(y=(bquote(tau[i])), x = "",element_text(size = 6))+
  scale_y_continuous ( breaks=scales::pretty_breaks(n=5),
                       limits = c(0, 20))+scale_c_1+
  my_theme;gg_hot_c1_isoh

gg_amb_c2_isoh<-dfa %>% 
  filter(Camp== 3) %>% filter(trt == "ambient") %>% 
  ggplot(aes(x=Td, y=thiso))+
  annotation_c2+
  ggtitle(("(c)"))+
  geom_boxplot(aes(x=as.factor(Td), y=thiso),outlier.shape = NA)+
  geom_jitter(aes(),width = 0.2)+
  labs(y=(bquote(tau[i])), x = "",element_text(size = 6))+
  scale_y_continuous ( breaks=scales::pretty_breaks(n=5),
                       limits = c(0, 20))+scale_c_2+my_theme;gg_amb_c2_isoh
gg_hot_c2_isoh<-dfa %>% 
  filter(Camp== 3) %>% filter(trt == "elevated ") %>% 
  ggplot(aes(x=Td, y=thiso))+
  annotation_c2+
  ggtitle(("(d)"))+
  geom_boxplot(aes(x=as.factor(Td), y=thiso),colour="red",,outlier.shape = NA)+
  geom_jitter(aes(),width = 0.2,colour="red")+
  labs(y=(bquote(tau[i])), x = "",element_text(size = 6))+
  scale_y_continuous ( breaks=scales::pretty_breaks(n=5),
                       limits = c(0, 20))+scale_c_2+my_theme;gg_hot_c2_isoh




##############delta V the d180 of water vapour leaving the chamber####################
gg_amb_c1_dv<-dfa %>% 
  filter(Camp== 1) %>% filter(trt == "ambient") %>% 
  ggplot(aes(x=Td, y=d18O.corrected_WV))+
  annotation_c1+
  ggtitle(("(e)"))+
  geom_boxplot(aes(x=as.factor(Td), y=d18O.corrected_WV),outlier.shape = NA)+
  geom_jitter(aes(),width = 0.2)+
  labs(y=(bquote(delta[v])), x = "",element_text(size = 6))+
  scale_y_continuous ( breaks=scales::pretty_breaks(n=5),
                       limits = c(-20, -5))+scale_c_1+my_theme;gg_amb_c1_dv
gg_hot_c1_dv<-dfa %>% 
  filter(Camp== 1) %>% filter(trt == "elevated ") %>% 
  ggplot(aes(x=Td, y=d18O.corrected_WV))+
  annotation_c1+
  ggtitle(("(f)"))+
  geom_boxplot(aes(x=as.factor(Td), y=d18O.corrected_WV),colour="red",,outlier.shape = NA)+
  geom_jitter(aes(),width = 0.2,colour="red")+
  labs(y=(bquote(delta[v])), x = "",element_text(size = 6))+
  scale_y_continuous ( breaks=scales::pretty_breaks(n=5),
                       limits = c(-20, -5))+scale_c_1+my_theme;gg_hot_c1_dv
gg_amb_c2_dv<-dfa %>% 
  filter(Camp== 3) %>% filter(trt == "ambient") %>% 
  ggplot(aes(x=Td, y=d18O.corrected_WV))+
  annotation_c2+
  ggtitle(("(g)"))+
  geom_boxplot(aes(x=as.factor(Td), y=d18O.corrected_WV),outlier.shape = NA)+
  geom_jitter(aes(),width = 0.2)+
  labs(y=(bquote(delta[v])), x = "",element_text(size = 6))+
  scale_y_continuous ( breaks=scales::pretty_breaks(n=5),
                       limits = c(-20, -5))+scale_c_2+my_theme;gg_amb_c2_dv
gg_hot_c2_dv<-dfa %>% 
  filter(Camp== 3) %>% filter(trt == "elevated ") %>% 
  ggplot(aes(x=Td, y=d18O.corrected_WV))+
  annotation_c2+
  ggtitle(("(h)"))+
  geom_boxplot(aes(x=as.factor(Td), y=d18O.corrected_WV),colour="red",outlier.shape = NA)+
  geom_jitter(aes(),width = 0.2,color="red")+
  labs(y=(bquote(delta[v])), x = "",element_text(size = 6))+
  scale_y_continuous ( breaks=scales::pretty_breaks(n=5),
                       limits = c(-20, -5))+scale_c_2+my_theme;gg_hot_c2_dv
############################################################################################

##############delta V the d180 of condensation####################
gg_amb_c1_dc<-dfa %>% 
  filter(Camp== 1) %>% filter(trt == "ambient") %>% 
  ggplot(aes(x=Td, y=d18O.corrected_Cond))+
  annotation_c1+
  ggtitle(("(i)"))+
  geom_boxplot(aes(x=as.factor(Td), y=d18O.corrected_Cond),outlier.shape = NA)+
  geom_jitter(aes(),width = 0.2)+
  labs(y=(bquote(delta[C])), x = "",element_text(size = 6))+
  scale_y_continuous ( breaks=scales::pretty_breaks(n=5),
                       limits = c(-8, 2))+scale_c_1+my_theme;gg_amb_c1_dc

gg_hot_c1_dc<-dfa %>% 
  filter(Camp== 1) %>% filter(trt == "elevated ") %>% 
  ggplot(aes(x=Td, y=d18O.corrected_Cond))+
  annotation_c1+
  ggtitle(("(j)"))+
  geom_boxplot(aes(x=as.factor(Td), y=d18O.corrected_Cond),colour="red",outlier.shape = NA)+
  geom_jitter(aes(),width = 0.2,color="red")+
  labs(y=(bquote(delta[C])), x = "",element_text(size = 6))+
  scale_y_continuous ( breaks=scales::pretty_breaks(n=5),
                       limits = c(-8, 2))+scale_c_1+my_theme;gg_amb_c1_dc

gg_amb_c2_dc<-dfa %>% 
  filter(Camp== 3) %>% filter(trt == "ambient") %>% 
  ggplot(aes(x=Td, y=d18O.corrected_Cond))+
  annotation_c2+
  ggtitle(("(k)"))+
  geom_boxplot(aes(x=as.factor(Td), y=d18O.corrected_Cond),outlier.shape = NA)+
  geom_jitter(aes(),width = 0.2)+
  labs(y=(bquote(delta[C])), x = "",element_text(size = 6))+
  scale_y_continuous ( breaks=scales::pretty_breaks(n=5),
                       limits = c(-8, 2))+scale_c_2+my_theme;gg_amb_c2_dc

gg_hot_c2_dc<-dfa %>% 
  filter(Camp== 3) %>% filter(trt == "elevated ") %>% 
  ggplot(aes(x=Td, y=d18O.corrected_Cond))+
  annotation_c2+
  ggtitle(("(l)"))+
  geom_boxplot(aes(x=as.factor(Td), y=d18O.corrected_Cond),colour="red",outlier.shape = NA)+
  geom_jitter(aes(),width = 0.2,color="red")+
  labs(y=(bquote(delta[C])), x = "",element_text(size = 6))+
  scale_y_continuous ( breaks=scales::pretty_breaks(n=5),
                       limits = c(-8, 2))+scale_c_2+my_theme;gg_amb_c2_dc

##############delta V the d180 of atomsphere####################
gg_c1_datm<-dfa %>% 
  filter(Camp== 1) %>% filter(trt == "ambient") %>% 
  filter(chamber=="C01") %>%  # doesnt really matter which chamber
  ggplot(aes(x=Td, y=d18O.corrected_WV.AMB.))+
  annotation_c1+
  ggtitle(("(m)"))+
  #geom_boxplot(aes(x=as.factor(Td), y=d18O.corrected_WV.AMB.),outlier.shape = NA)+
  geom_point(aes(),size = 3)+
  labs(y=(bquote(delta[F])), x = "",element_text(size = 6))+
  scale_y_continuous ( breaks=scales::pretty_breaks(n=5),
                       limits = c(-16, -8))+scale_c_1+my_theme;gg_c1_datm

gg_c2_datm<-dfa %>% 
  filter(Camp== 3) %>% filter(trt == "ambient") %>% 
  filter(chamber=="C01") %>%  # doesnt really matter which chamber
  ggplot(aes(x=Td, y=d18O.corrected_WV.AMB.))+
  annotation_c2+
  ggtitle(("(n)"))+
  geom_point(aes(),size = 3)+
  labs(y=(bquote(delta[F])), x = "",element_text(size = 6))+
  scale_y_continuous ( breaks=scales::pretty_breaks(n=5),
                       limits = c(-16, -8))+scale_c_2+my_theme;gg_c2_datm

title <- ggdraw() + 
  draw_label(
    "Miles per gallon decline with displacement and horsepower",
    fontface = 'bold',
    x = 0,
    hjust = 0
  ) +
  theme(
    # add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 7)
  )

Measured_Isotopes=plot_grid(gg_amb_c1_isoh,
                            gg_hot_c1_isoh,
                            gg_amb_c2_isoh,
                            gg_hot_c2_isoh,
                            gg_amb_c1_dv,
                            gg_hot_c1_dv,
                            gg_amb_c2_dv,
                            gg_hot_c2_dv,
                            gg_amb_c1_dc,
                            gg_hot_c1_dc,
                            gg_amb_c2_dc,
                            gg_hot_c2_dc,
                            #gg_c1_datm,
                            #NULL,
                            #gg_c2_datm,
                            #NULL,
                            ncol = 4, align = 'hv')

Measured_Isotopes

plot_grid(
  title, Measured_Isotopes,
  ncol = 1,
  # rel_heights values control vertical title margins
  rel_heights = c(0.1, 1)
)

atm_stack<-plot_grid(gg_c1_datm,
                     gg_c2_datm,
                     ncol = 2, align = 'hv')


Measured_Isotopes_FIG<-plot_grid(Measured_Isotopes,atm_stack, ncol=1,rel_heights=c(1,0.3))