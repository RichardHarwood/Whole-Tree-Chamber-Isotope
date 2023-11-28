
library(tidyverse);library(plantecophys);library(gridExtra);library(cowplot);library(doBy)
dfa<-wtc_df 

#Now that the mass balance is done and we have dfa (our campaign data -data frame) we can graph:
#To make the graphs we calcuate the mean and std error for each treatment and each campaign.
#again it is somewhat repetetive making each panel, just changing variables adn when neccasry highlighting which variables
#have dew point and air temp to close
#Here is temperture 1= first campaign , 3 = 3rd campaign. there was a 2nd campaign but the LGR was down (blacked out area fig2)

wtc<-dfa
Cw<-wtc
Cw<-subset(Cw, Camp== 1)
Cw <- Cw[,c("rangewtc","Tdh","chamber","trt","Tair_al_mean")]
Trtmean<- summaryBy(.~Tdh+trt+rangewtc,FUN=c(mean),keep.names=T,data=Cw)
se <- function(x) sqrt(var(x)/length(x))
Trtsd<- summaryBy(.~Tdh+trt+rangewtc,FUN=c(se),keep.names=T,data=Cw)
data<-cbind(Trtmean,Trtsd);data<-as.data.frame(data)
data['Time']<-(dmy_hm(data$Tdh, quiet=TRUE, tz="UTC"));str(data)
#######################
Cwx<-wtc
Cwx<-subset(Cwx, Camp== 3)
Cwx <- Cwx[,c("rangewtc","Tdh","chamber","trt","Tair_al_mean")]
Trtmeanx<- summaryBy(.~Tdh+trt+rangewtc,FUN=c(mean),keep.names=T,data=Cwx)
#Trtmeanx<-filter(Trtmeanx, !grepl('<NA>', trt ))
se <- function(x) sqrt(var(x)/length(x))
Trtsdx<- summaryBy(.~Tdh+trt+rangewtc,FUN=c(se),keep.names=T,data=Cwx)
data2<-cbind(Trtmeanx,Trtsdx);data2<-as.data.frame(data2)
data2['Time']<-(dmy_hm(data2$Tdh, quiet=TRUE, tz="UTC"));str(data)
####################################################################
#change to bom first and last light
start = as.POSIXct('2016-10-24 19:00:00"', tz="UTC") 
end = as.POSIXct('2016-10-25 05:30:00', tz="UTC")
start2 = as.POSIXct('2016-11-20 19:00:00"', tz="UTC") 
end2 = as.POSIXct('2016-11-21 05:30:00', tz="UTC")

##########################################################
data<-na.omit(data)
t1<-ggplot(data=data, aes(x=Time, y=Tair_al_mean))+
  geom_point(aes(colour=trt, shape=rangewtc), size=5,)+ 
  annotate("rect", fill = "black", alpha = 0.15, 
           xmin = end, xmax = start,
           ymin = -Inf, ymax = Inf)+
  scale_color_manual(name="Legend:",labels = c("Transpired Ambient Chamber", "Transpired Elevated Chamber"),values = c("black", "red"))+
  scale_shape_manual(name="Legend:",labels = c("in", "out" ),values = c(8,16))+
  geom_errorbar(aes(ymin=Tair_al_mean-Tair_al_mean.1, ymax=Tair_al_mean+Tair_al_mean.1), width=.2,
                position=position_dodge(.9))+
  labs(y="Temperature (°C)", x = "",element_text(size = 6))+
  theme_classic()+theme(panel.border = element_rect(fill = "NA", colour = "black", size = 2))+
  ggtitle("(a)")+ theme(legend.position = "none")+
  scale_y_continuous ( breaks=scales::pretty_breaks(n=5),
                       limits = c(0, 40))+
  theme(axis.text.y = element_text( color="black", size=30))+
  theme(axis.text.x = element_text( color="black", size=20, angle=45, hjust=1 , vjust=1))+
  theme(axis.title.y = element_text(color="black", size=45))+ 
  theme(plot.title = element_text(size = 40, face = "bold"))

#ggsave("Fig3A.png",plot=t1, width = 30, height = 25, units = "cm", dpi=75)
t3<-ggplot(data=data2, aes(x=Time, y=Tair_al_mean))+
  geom_point(aes(colour=trt, shape=rangewtc), size=5,)+ 
  annotate("rect", fill = "black", alpha = 0.15, 
           xmin = end2, xmax = start2,
           ymin = -Inf, ymax = Inf)+
  scale_color_manual(name="Legend:",labels = c("Transpired Ambient Chamber", "Transpired Elevated Chamber"),values = c("black", "red"))+
  scale_shape_manual(name="Legend:",labels = c("in", "out" ),values = c(8,16))+
  geom_errorbar(aes(ymin=Tair_al_mean-Tair_al_mean.1, ymax=Tair_al_mean+Tair_al_mean.1), width=.2,
                position=position_dodge(.9))+
  labs(y="", x = "",element_text(size = 6))+
  theme_classic()+theme(panel.border = element_rect(fill = "NA", colour = "black", size = 2))+
  ggtitle("(b)")+ theme(legend.position = "none")+
  scale_y_continuous ( breaks=scales::pretty_breaks(n=5),
                       limits = c(0, 40))+
  theme(axis.text.y = element_text( color="black", size=30))+
  theme(axis.text.x = element_text( color="black", size=20, angle=45, hjust=1 , vjust=1))+
  theme(axis.title.y = element_text(color="black", size=45))+ 
  theme(plot.title = element_text(size = 40, face = "bold"))

#now VPD

wtc<-dfa
Cw<-wtc
Cw<-subset(Cw, Camp== 1)
Cw <- Cw[,c("rangewtc","Tdh","chamber","trt","vpd")]
Trtmean<- summaryBy(.~Tdh+trt+rangewtc,FUN=c(mean),keep.names=T,data=Cw)
#Trtmean<-filter(Trtmean, !grepl('<NA>', trt ));str(Trtmean)
se <- function(x) sqrt(var(x)/length(x))
Trtsd<- summaryBy(.~Tdh+trt+rangewtc,FUN=c(se),keep.names=T,data=Cw)
data<-cbind(Trtmean,Trtsd);data<-as.data.frame(data)
data['Time']<-(dmy_hm(data$Tdh, quiet=TRUE, tz="UTC"));str(data)
#######################
Cwx<-wtc
Cwx<-subset(Cwx, Camp== 3)
Cwx <- Cwx[,c("rangewtc","Tdh","chamber","trt","vpd")]
Trtmeanx<- summaryBy(.~Tdh+trt+rangewtc,FUN=c(mean),keep.names=T,data=Cwx)
#Trtmeanx<-filter(Trtmeanx, !grepl('<NA>', trt ))
se <- function(x) sqrt(var(x)/length(x))
Trtsdx<- summaryBy(.~Tdh+trt+rangewtc,FUN=c(se),keep.names=T,data=Cwx)
data2<-cbind(Trtmeanx,Trtsdx);data2<-as.data.frame(data2)
data2['Time']<-(dmy_hm(data2$Tdh, quiet=TRUE, tz="UTC"));str(data)
start = as.POSIXct('2016-10-24 19:00:00"', tz="UTC") 
end = as.POSIXct('2016-10-25 05:30:00', tz="UTC")
start2 = as.POSIXct('2016-11-20 19:00:00"', tz="UTC") 
end2 = as.POSIXct('2016-11-21 05:30:00', tz="UTC")
vp1<-ggplot(data=data, aes(x=Time, y=vpd))+
  geom_point(aes(colour=trt, shape=rangewtc), size=5,)+ 
  annotate("rect", fill = "black", alpha = 0.15, 
           xmin = end, xmax = start,
           ymin = -Inf, ymax = Inf)+
  scale_color_manual(name="Legend:",labels = c("Transpired Ambient Chamber", "Transpired Elevated Chamber"),values = c("black", "red"))+
  scale_shape_manual(name="Legend:",labels = c("in", "out" ),values = c(8,16))+
  geom_errorbar(aes(ymin=vpd-vpd.1, ymax=vpd+vpd.1), width=.2,
                position=position_dodge(.9))+
  labs(y="VPD (kPa)", x = "",element_text(size = 6))+
  theme_classic()+theme(panel.border = element_rect(fill = "NA", colour = "black", size = 2))+
  ggtitle("(c)")+ theme(legend.position = "none")+
  scale_y_continuous ( breaks=scales::pretty_breaks(n=5),
                       limits = c(0, 3.5))+
  theme(axis.text.y = element_text( color="black", size=30))+
  theme(axis.text.x = element_text( color="black", size=20, angle=45, hjust=1 , vjust=1))+
  theme(axis.title.y = element_text(color="black", size=45))+ 
  theme(plot.title = element_text(size = 40, face = "bold"))



vp3<-ggplot(data=data2, aes(x=Time, y=vpd))+
  geom_point(aes(colour=trt, shape=rangewtc), size=5,)+ 
  annotate("rect", fill = "black", alpha = 0.15, 
           xmin = end2, xmax = start2,
           ymin = -Inf, ymax = Inf)+
  scale_color_manual(name="Legend:",labels = c("Transpired Ambient Chamber", "Transpired Elevated Chamber"),values = c("black", "red"))+
  scale_shape_manual(name="Legend:",labels = c("in", "out" ),values = c(8,16))+
  geom_errorbar(aes(ymin=vpd-vpd.1, ymax=vpd+vpd.1), width=.2,
                position=position_dodge(.9))+
  labs(y="", x = "",element_text(size = 6))+
  theme_classic()+theme(panel.border = element_rect(fill = "NA", colour = "black", size = 2))+
  ggtitle("(d)")+ theme(legend.position = "none")+
  scale_y_continuous ( breaks=scales::pretty_breaks(n=5),
                       limits = c(0, 3.5))+
  theme(axis.text.y = element_text( color="black", size=30))+
  theme(axis.text.x = element_text( color="black", size=20, angle=45, hjust=1 , vjust=1))+
  theme(axis.title.y = element_text(color="black", size=45))+ 
  theme(plot.title = element_text(size = 40, face = "bold"))


#now Transpiration - here we need to denote which values are comprised (dew point close to air temp) with a *


wtc<-dfa
Cw<-wtc
Cw<-subset(Cw, Camp== 1)
Cw <- Cw[,c("rangewtc","Tdh","chamber","trt","Eleaf")]
Trtmean<- summaryBy(.~Tdh+trt+rangewtc,FUN=c(mean),keep.names=T,data=Cw)
#Trtmean<-filter(Trtmean, !grepl('<NA>', trt ));str(Trtmean)
se <- function(x) sqrt(var(x)/length(x))
Trtsd<- summaryBy(.~Tdh+trt+rangewtc,FUN=c(se),keep.names=T,data=Cw)
data<-cbind(Trtmean,Trtsd);data<-as.data.frame(data)
data['Time']<-(dmy_hm(data$Tdh, quiet=TRUE, tz="UTC"));str(data)
#######################
Cwx<-wtc
Cwx<-subset(Cwx, Camp== 3)
Cwx <- Cwx[,c("rangewtc","Tdh","chamber","trt","Eleaf")]
Trtmeanx<- summaryBy(.~Tdh+trt+rangewtc,FUN=c(mean),keep.names=T,data=Cwx)
#Trtmeanx<-filter(Trtmeanx, !grepl('<NA>', trt ))
se <- function(x) sqrt(var(x)/length(x))
Trtsdx<- summaryBy(.~Tdh+trt+rangewtc,FUN=c(se),keep.names=T,data=Cwx)
data2<-cbind(Trtmeanx,Trtsdx);data2<-as.data.frame(data2)
data2['Time']<-(dmy_hm(data2$Tdh, quiet=TRUE, tz="UTC"));str(data)
start = as.POSIXct('2016-10-24 19:00:00"', tz="UTC") 
end = as.POSIXct('2016-10-25 05:30:00', tz="UTC")
start2 = as.POSIXct('2016-11-20 19:00:00"', tz="UTC") 
end2 = as.POSIXct('2016-11-21 05:30:00', tz="UTC")
E1<-ggplot(data=data, aes(x=Time, y=Eleaf))+
  geom_point(aes(colour=trt, shape=rangewtc), size=5,)+ 
  annotate("rect", fill = "black", alpha = 0.15, 
           xmin = end, xmax = start,
           ymin = -Inf, ymax = Inf)+
  
  scale_color_manual(name="Legend:",labels = c("Transpired Ambient Chamber", "Transpired Elevated Chamber"),values = c("black", "red"))+
  scale_shape_manual(name="Legend:",labels = c("in", "out" ),values = c(8,16))+
  geom_errorbar(aes(ymin=Eleaf-Eleaf.1, ymax=Eleaf+Eleaf.1), width=.2,
                position=position_dodge(.9))+
  ylab(bquote(atop("Transpiration",
  '(mmol'~ m^-2~s^-1*')')))+
  xlab("")+
  # scale_x_datetime(breaks = scales::pretty_breaks(n = 12))
  theme_classic()+theme(panel.border = element_rect(fill = "NA", colour = "black", size = 2))+
  ggtitle("(e)")+ theme(legend.position = "none")+
  scale_y_continuous ( breaks=scales::pretty_breaks(n=5),
                       limits = c(0, 3))+
  theme(axis.text.y = element_text( color="black", size=30))+
  theme(axis.text.x = element_text( color="black", size=20, angle=45, hjust=1 , vjust=1))+
  theme(axis.title.y = element_text(color="black", size=45))+ 
  theme(plot.title = element_text(size = 40, face = "bold"))

E3<-ggplot(data=data2, aes(x=Time, y=Eleaf))+
  geom_point(aes(colour=trt, shape=rangewtc), size=5,)+ 
  annotate("rect", fill = "black", alpha = 0.15, 
           xmin = end2, xmax = start2,
           ymin = -Inf, ymax = Inf)+
  
  scale_color_manual(name="Legend:",labels = c("Transpired Ambient Chamber", "Transpired Elevated Chamber"),values = c("black", "red"))+
  scale_shape_manual(name="Legend:",labels = c("in", "out" ),values = c(8,16))+
  geom_errorbar(aes(ymin=Eleaf-Eleaf.1, ymax=Eleaf+Eleaf.1), width=.2,
                position=position_dodge(.9))+
  labs(y="", x = "",element_text(size = 6))+
  # scale_x_datetime(breaks = scales::pretty_breaks(n = 12))
  theme_classic()+theme(panel.border = element_rect(fill = "NA", colour = "black", size = 2))+
  ggtitle("(f)")+ theme(legend.position = "none")+
  scale_y_continuous ( breaks=scales::pretty_breaks(n=5),
                       limits = c(0, 3))+
  theme(axis.text.y = element_text( color="black", size=30))+
  theme(axis.text.x = element_text( color="black", size=20, angle=45, hjust=1 , vjust=1))+
  theme(axis.title.y = element_text(color="black", size=45))+ 
  theme(plot.title = element_text(size = 40, face = "bold"))



#and lastly gs (again with *'s) night time gs should not be that high

wtc<-dfa
Cw<-wtc
Cw<-subset(Cw, Camp== 1)
Cw <- Cw[,c("rangewtc","Tdh","chamber","trt","gs")]
Trtmean<- summaryBy(.~Tdh+trt+rangewtc,FUN=c(mean),keep.names=T,na.rm=TRUE,data=Cw)
#Trtmean<-filter(Trtmean, !grepl('<NA>', trt ));str(Trtmean)
se <- function(x) sqrt(var(x)/length(x))
Trtsd<- summaryBy(.~Tdh+trt+rangewtc,FUN=c(se),keep.names=T,data=Cw)
data<-cbind(Trtmean,Trtsd);data<-as.data.frame(data)
data['Time']<-(dmy_hm(data$Tdh, quiet=TRUE, tz="UTC"));str(data)
#######################
Cwx<-wtc
Cwx<-subset(Cwx, Camp== 3)
Cwx <- Cwx[,c("rangewtc","Tdh","chamber","trt","gs")]
Trtmeanx<- summaryBy(.~Tdh+trt+rangewtc,FUN=c(mean),keep.names=T,na.rm=TRUE,data=Cwx)
#Trtmeanx<-filter(Trtmeanx, !grepl('<NA>', trt ))
se <- function(x) sqrt(var(x)/length(x))
Trtsdx<- summaryBy(.~Tdh+trt+rangewtc,FUN=c(se),keep.names=T,data=Cwx)
data2<-cbind(Trtmeanx,Trtsdx);data2<-as.data.frame(data2)
data2['Time']<-(dmy_hm(data2$Tdh, quiet=TRUE, tz="UTC"));str(data)
start = as.POSIXct('2016-10-24 19:00:00"', tz="UTC") 
end = as.POSIXct('2016-10-25 05:30:00', tz="UTC")
start2 = as.POSIXct('2016-11-20 19:00:00"', tz="UTC") 
end2 = as.POSIXct('2016-11-21 05:30:00', tz="UTC")
gs1<-ggplot(data=data, aes(x=Time, y=gs))+
  geom_point(aes(colour=trt, shape=rangewtc), size=5,)+ 
  annotate("rect", fill = "black", alpha = 0.15, 
           xmin = end, xmax = start,
           ymin = -Inf, ymax = Inf)+
  scale_color_manual(name="Legend:",labels = c("Transpired Ambient Chamber", "Transpired Elevated Chamber"),values = c("black", "red"))+
  scale_shape_manual(name="Legend:",labels = c("in", "out" ),values = c(8,16))+
  geom_errorbar(aes(ymin=gs-gs.1, ymax=gs+gs.1), width=.2,
                position=position_dodge(.9))+
  ylab(bquote(atop("Total Leaf Conductance",
                   '(mol'~ m^-2~s^-1*')')))+
  xlab("")+
       # scale_x_datetime(breaks = scales::pretty_breaks(n = 12))
  theme_classic()+theme(panel.border = element_rect(fill = "NA", colour = "black", size = 2))+
  ggtitle("(g)")+ theme(legend.position = "none")+
  scale_y_continuous ( breaks=scales::pretty_breaks(n=5),
                       limits = c(0, 0.7))+
  theme(axis.text.y = element_text( color="black", size=30))+
  theme(axis.text.x = element_text( color="black", size=20, angle=45, hjust=1 , vjust=1))+
  theme(axis.title.y = element_text(color="black", size=45))+ 
  theme(plot.title = element_text(size = 40, face = "bold"))

gs3<-ggplot(data=data2, aes(x=Time, y=gs))+
  geom_point(aes(colour=trt, shape=rangewtc), size=5,)+ 
  annotate("rect", fill = "black", alpha = 0.15, 
           xmin = end2, xmax = start2,
           ymin = -Inf, ymax = Inf)+
  
  scale_color_manual(name="Legend:",labels = c("Transpired Ambient Chamber", "Transpired Elevated Chamber"),values = c("black", "red"))+
  scale_shape_manual(name="Legend:",labels = c("in", "out" ),values = c(8,16))+
  geom_errorbar(aes(ymin=gs-gs.1, ymax=gs+gs.1), width=.2,
                position=position_dodge(.9))+
  labs(y="", x = "",element_text(size = 6))+
  # scale_x_datetime(breaks = scales::pretty_breaks(n = 12))
  theme_classic()+theme(panel.border = element_rect(fill = "NA", colour = "black", size = 2))+
  ggtitle("(h)")+ theme(legend.position = "none")+
  scale_y_continuous ( breaks=scales::pretty_breaks(n=5),
                       limits = c(0, 0.7))+
  theme(axis.text.y = element_text( color="black", size=30))+
  theme(axis.text.x = element_text( color="black", size=20, angle=45, hjust=1 , vjust=1))+
  theme(axis.title.y = element_text(color="black", size=30))+ 
  theme(plot.title = element_text(size = 40, face = "bold"))


#We now just merge these diel obsevrations into one big figure 


##FIgure 2 enviro vars
Figure_2_non_isotope_data=plot_grid(t1,t3,vp1,vp3,E1,E3,gs1,gs3,  ncol = 2, align = 'hv')
#Figure_2_non_isotope_data=grid.arrange(t1,t3,vp1,vp3,E1,E3,gs1,gs3,  ncol = 2)
#and then ggsave it out -- I added axis labels etc later in adobe illustator 
#ggsave("NON_ISOTOPE_GRAPHTEST.png",Figure_2_non_isotope_data, width = 45, height = 70, units = "cm", dpi=100)

#save_plot("D:/PROJECTS/WTC_4_18O_E/R_results/general.png", Figure_2_non_isotope_data, ncol = 2)

#save_plot("D:/PROJECTS/WTC_4_18O_E/R_results/general.png", Figure_2_non_isotope_data, base_height = 8, base_aspect_ratio = 1)
