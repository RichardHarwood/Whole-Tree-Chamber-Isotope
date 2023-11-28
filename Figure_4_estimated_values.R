#####################MODELLED ISOTOPE FIGURE
####SS at leaf water)
dfa<-wtc_df

dfa_sub<-dfa %>% 
  select(Camp,trt,Tdh,chamber,evapSS, evapObs)

my_theme    <- theme(axis.text.x = element_text( color="black", size=22, angle=90, vjust = 0.5, hjust=1),
                     panel.border = element_rect(fill = "NA", colour = "black", size = 2),
                     panel.background = element_blank(),
                     plot.title = element_text(size=37),
                     axis.text.y = element_text( color="black", size=25),
                     axis.title.y = element_text(color="black", size=60),
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
dfa$Td<-as.character(dfa$Td)
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

gg_amb_c1_evapSS<-dfa %>% 
  filter(Camp== 1) %>% filter(trt == "ambient") %>% 
  ggplot(aes(x=Td, y=evapObs))+
  annotation_c1+
  ggtitle(("(a)"))+
  geom_boxplot(aes(x=as.factor(Td), y=evapSS),outlier.shape = NA)+
  geom_jitter(aes(),width = 0.2)+
  labs(y=(bquote(delta[e-ss])), x = "",element_text(size = 6))+
  scale_y_continuous ( breaks=scales::pretty_breaks(n=5),
                       limits = c(0, 20))+scale_c_1+my_theme;gg_amb_c1_evapSS

gg_hot_c1_evapSS<-dfa %>% 
  filter(Camp== 1) %>% filter(trt == "ambient") %>% 
  ggplot(aes(x=Td, y=evapObs))+
  annotation_c1+
  ggtitle(("(b)"))+
  geom_boxplot(aes(x=as.factor(Td), y=evapSS),colour="red",outlier.shape = NA)+
  geom_jitter(aes(),width = 0.2,color="red")+
  labs(y=(bquote(delta[e-ss])), x = "",element_text(size = 6))+
  scale_y_continuous ( breaks=scales::pretty_breaks(n=5),
                       limits = c(0, 20))+scale_c_1+my_theme;gg_hot_c1_evapSS

gg_amb_c2_evapSS<-dfa %>% 
  filter(Camp== 3) %>% filter(trt == "ambient") %>% 
  ggplot(aes(x=Td, y=evapObs))+
  annotation_c2+
  ggtitle(("(c)"))+
  geom_boxplot(aes(x=as.factor(Td), y=evapSS),outlier.shape = NA)+
  geom_jitter(aes(),width = 0.2)+
  labs(y=(bquote(delta[e-ss])), x = "",element_text(size = 6))+
  scale_y_continuous ( breaks=scales::pretty_breaks(n=5),
                       limits = c(0, 20))+scale_c_2+my_theme;gg_amb_c2_evapSS

gg_hot_c2_evapSS<-dfa %>% 
  filter(Camp== 3) %>% filter(trt == "ambient") %>% 
  ggplot(aes(x=Td, y=evapObs))+
  annotation_c2+
  ggtitle(("(d)"))+
  geom_boxplot(aes(x=as.factor(Td), y=evapSS),colour="red",outlier.shape = NA)+
  geom_jitter(aes(),width = 0.2,color="red")+
  labs(y=(bquote(delta[e-ss])), x = "",element_text(size = 6))+
  scale_y_continuous ( breaks=scales::pretty_breaks(n=5),
                       limits = c(0, 20))+scale_c_2+my_theme;gg_hot_c2_evapSS
#######################de using dE
gg_amb_c1_evapObs<-dfa %>% 
  filter(Camp== 1) %>% filter(trt == "ambient") %>% 
  ggplot(aes(x=Td, y=evapObs))+
  annotation_c1+
  ggtitle(("(e)"))+
  geom_boxplot(aes(x=as.factor(Td), y=evapObs),outlier.shape = NA)+
  geom_jitter(aes(),width = 0.2)+
  labs(y=(bquote(delta[e])), x = "",element_text(size = 6))+
  scale_y_continuous ( breaks=scales::pretty_breaks(n=5),
                       limits = c(0, 20))+scale_c_1+my_theme;gg_amb_c1_evapObs

gg_hot_c1_evapObs<-dfa %>% 
  filter(Camp== 1) %>% filter(trt == "elevated ") %>% 
  ggplot(aes(x=Td, y=evapObs))+
  annotation_c1+
  ggtitle(("(f)"))+
  geom_boxplot(aes(x=as.factor(Td), y=evapObs),colour="red",outlier.shape = NA)+
  geom_jitter(aes(),width = 0.2,color="red")+
  labs(y=(bquote(delta[e])), x = "",element_text(size = 6))+
  scale_y_continuous ( breaks=scales::pretty_breaks(n=5),
                       limits = c(0, 20))+scale_c_1+my_theme;gg_hot_c1_evapObs

gg_amb_c2_evapObs<-dfa %>% 
  filter(Camp== 3) %>% filter(trt == "ambient") %>% 
  ggplot(aes(x=Td, y=evapObs))+
  annotation_c2+
  ggtitle(("(g)"))+
  geom_boxplot(aes(x=as.factor(Td), y=evapObs),outlier.shape = NA)+
  geom_jitter(aes(),width = 0.2)+
  labs(y=(bquote(delta[e])), x = "",element_text(size = 6))+
  scale_y_continuous ( breaks=scales::pretty_breaks(n=5),
                       limits = c(0, 20))+scale_c_2+my_theme;gg_amb_c2_evapObs

gg_hot_c2_evapObs<-dfa %>% 
  filter(Camp== 3) %>% filter(trt == "elevated ") %>% 
  ggplot(aes(x=Td, y=evapObs))+
  annotation_c2+
  ggtitle(("(h)"))+
  geom_boxplot(aes(x=as.factor(Td), y=evapObs),colour="red",outlier.shape = NA)+
  geom_jitter(aes(),width = 0.2,color="red")+
  labs(y=(bquote(delta[e])), x = "",element_text(size = 6))+
  scale_y_continuous ( breaks=scales::pretty_breaks(n=5),
                       limits = c(0, 20))+scale_c_2+my_theme;gg_hot_c2_evapObs
##############dtrans
gg_amb_c1_dtrans<-dfa %>% 
  filter(Camp== 1) %>% filter(trt == "ambient") %>% 
  ggplot(aes(x=Td, y=dtrans))+
  annotation_c1+
  ggtitle(("(i)"))+
  geom_boxplot(aes(x=as.factor(Td), y=dtrans),outlier.shape = NA)+
  geom_jitter(aes(),width = 0.2)+
  labs(y=(bquote(delta[E])), x = "",element_text(size = 6))+
  scale_y_continuous ( breaks=scales::pretty_breaks(n=5),
                       limits = c(-10, 5))+scale_c_1+my_theme+
  geom_hline(yintercept=-3.28, colour ="red", size=1.5) ;gg_amb_c1_dtrans

gg_hot_c1_dtrans<-dfa %>% 
  filter(Camp== 1) %>% filter(trt == "elevated ") %>% 
  ggplot(aes(x=Td, y=dtrans))+
  annotation_c1+
  ggtitle(("(j)"))+
  geom_boxplot(aes(x=as.factor(Td), y=dtrans),colour="red",outlier.shape = NA)+
  geom_jitter(aes(),width = 0.2,color="red")+
  labs(y=(bquote(delta[E])), x = "",element_text(size = 6))+
  scale_y_continuous ( breaks=scales::pretty_breaks(n=5),
                       limits = c(-10, 5))+scale_c_1+my_theme+
  geom_hline(yintercept=-3.28, colour ="red", size=1.5) ;gg_hot_c1_dtrans

gg_amb_c2_dtrans<-dfa %>% 
  filter(Camp== 3) %>% filter(trt == "ambient") %>% 
  ggplot(aes(x=Td, y=dtrans))+
  annotation_c2+
  ggtitle(("(k)"))+
  geom_boxplot(aes(x=as.factor(Td), y=dtrans),outlier.shape = NA)+
  geom_jitter(aes(),width = 0.2)+
  #scale_shape_manual(name="Legend:",labels = c("in", "out" ),values = c(8,16))+
  labs(y=(bquote(delta[E])), x = "",element_text(size = 6))+
  scale_y_continuous ( breaks=scales::pretty_breaks(n=5),
                       limits = c(-10, 5))+scale_c_2+
  geom_hline(yintercept=-3.28, colour ="red", size=1.5)+ my_theme;

gg_hot_c2_dtrans<-dfa %>% 
  filter(Camp== 3) %>% filter(trt == "elevated ") %>% 
  ggplot(aes(x=Td, y=dtrans))+
  annotation_c2+
  ggtitle(("(l)"))+
  geom_boxplot(aes(x=as.factor(Td), y=dtrans),colour="red",outlier.shape = NA)+
  geom_jitter(aes(),width = 0.2,color="red")+
  labs(y=(bquote(delta[E])), x = "",element_text(size = 6))+
  scale_y_continuous ( breaks=scales::pretty_breaks(n=5),
                       limits = c(-10, 5))+scale_c_2+my_theme+
  geom_hline(yintercept=-3.28, colour ="red", size=1.5) ;


Modelled_Isotopes=plot_grid(gg_amb_c1_evapSS,
                            gg_hot_c1_evapSS,
                            gg_amb_c2_evapSS,
                            gg_hot_c2_evapSS,
                            gg_amb_c1_evapObs,
                            gg_hot_c1_evapObs,
                            gg_amb_c2_evapObs,
                            gg_hot_c2_evapObs,
                            gg_amb_c1_dtrans,
                            gg_hot_c1_dtrans,
                            gg_amb_c2_dtrans,
                            gg_hot_c2_dtrans,
                            ncol = 4, align = 'hv')


#ggsave("Modelled_Isotopes.png",plot=Modelled_Isotopes, width = 70, height = 50, units = "cm", dpi=300)