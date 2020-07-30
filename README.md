# Whole-Tree-Isoflux
Code to reproduce Whole Tree Isoflux 
The steps to recreate WTC4.

Step 1 Create Figure 2 - This involves exploring temporal data it uses 2 data frames (a water vapour oxygen isotope one (from the LGR) and a gas exchange one from WSU 
(portions of these dataframes are later macthed with wind and rain data to run stats) <br>

Each Panel of Figure 2 is created individually - but the process is more or less the same: <br>
#Load Packages <br>
require(lubridate);require(ggplot2);require(xts);require(dplyr);require(grid);require(cowplot);require(reshape2) <br>
require(plantecophys);require(akima);require(fields);require(tripack);require(autoimage)# Load Packages <br>
#Load Data <br>
googledriveWTC4ISOTOPEID <- "1RNzYApcCO1D1sr_mVQgQVSQFIWNWlqVm" <br>
df<- read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", googledriveWTC4ISOTOPEID)) # <br>
###Set data as time and assign set differnt simmaries e.g months weeks weeks and hour days etc: <br>
df["Td"]<-(dmy_hm(df$stime, quiet=TRUE, tz="UTC"));summary(df$Td) <br>
df$date <- as.Date(df$Td, format = "%Y-%m-%d") ; summary(df$date); str(df$date) <br>
df["month"]<-format(df$Td,"%m");str(df$month) <br>
df$hm <- format(df$Td, "%H") ; summary(df$hm) <br>
df["week"]<-strftime(df$Td,format="%W",tz="UTC");str(df$week) <br>
########################Extract flux data relevant to isotope <br>
start<-ymd_hms("2016-08-28 12:00:00 UTC");summary(start) <br>
end<-ymd_hms("2016-11-26 12:00:00") <br>
int<-interval(start, end); summary(int) <br>
df<-subset(df, df$Td %within% int);head(df$Td);tail(df$Td) <br>
##Filter out unrealisitic values <br>
df<-subset(df, df$d18O.corrected <= -8) <br>
df<-subset(df, df$d18O.corrected >= -20) <br>
#Make a managable data frame and get trt means <br>
dftrt<-data.frame(df$hm, df$month, df$d18O.corrected, df$T_treatment, df$Td);str(dftrt) <br>
dft<-dftrt %>% group_by(df.T_treatment, df.Td) %>% summarise_at(.vars = names(.)[3:3], na.rm=TRUE, .funs = c(mean)) <br>
##decide if looking at atm , hot or ambinet by un hashing  <br>
#dft<-subset(dft,df.T_treatment == "ambient") <br>
#dft<-subset(dft,df.T_treatment == "elevated") <br>
dft<-subset(dft,df.T_treatment == "atm") <br>

#Rereun time setting (this could improbed no need to do twice really) <br>
dft$date <- as.Date(dft$df.Td, format = "%Y-%m-%d")  <br>
dft["month"]<-format(dft$df.Td,"%m");str(dft$month);summary(dft$month) <br>
dft["monthday"]<-format(dft$df.Td,"%m%d");str(dft$monthday);summary(dft$monthday) <br>
dft<-transform(dft,day=as.numeric(factor(monthday))) #day <br>
dft<-as.data.frame(dft) <br>
dft$hm <- format(dft$df.Td, "%H") ; summary(dft$hm) <br>

##to make the heat map we need a dataframe with 3 vairables: 1) hour 2) day 3) a value (here it is d180atm) <br>
dfsml<-data.frame(dft$hm, dft$day, dft$df.d18O.corrected) <br>
#the exammple I followed used xyz so I changed my data frames names and we need to make some STR changes <br>
colnames(dfsml)<-c("x", "y","z") ; str(dfsml) <br>
dfsml$x<-as.numeric(paste(dfsml$x)) <br>
dfsml$y<-as.numeric(paste(dfsml$y));str(dfsml) <br>
x<-as.numeric(paste(dfsml$x)) <br>
y<-as.numeric(paste(dfsml$y)) <br>
z<-dfsml$z <br>

##We need then need to interpert the data: <br>

sample=as.data.frame(cbind(x,y,z)) <br>
names(sample) <- c("d", "m", "value") <br>
d1 <- with(sample, interp(x = d, y = m, z = value, nx = 250, ny=250, duplicate = "mean", extrap=FALSE)) <br>
d2 <- melt(d1$z, na.rm = TRUE) <br>
names(d2) <- c("x", "y", "z") <br>

d2$d<- d1$x[d2$x] <br>
d2$m <- d1$y[d2$y] <br>

#When plotting I use this colour map -  <br>
jet.colors <- colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000")) <br>

Then in ggplot we can make the graph  <br>
a<-ggplot(data = d2, aes(x = d, y = m, fill = z, z = z)) +  <br>
  geom_tile()+   <br>
  #scale_fill_viridis()+ <br>
  #+ scale_colour_gradient2() <br>
  scale_fill_gradientn(colours = jet.colors(7), limits =c(-20,-8))+ <br>
  labs(x="Hour", y="Day")+ <br>
  labs(fill=bquote(delta^18*O))+ <br>
  ggtitle((a)~delta^18*O~Atmospheric~Water~Vapour)+ <br>
  theme_bw()+ <br>
  theme(text = element_text(size=17))+ <br>
  theme(axis.text=element_text(size=17))+  <br>
  theme(legend.position='right') + <br>
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5))+ <br>
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5))+ <br>
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 62, ymax = 67, alpha = 1)+ <br>
  theme(plot.title = element_text(size = 13.5, face = "bold"))+ <br>
  annotation_custom(textGrob("*", gp = gpar(col = "black", fontsize=20)),  <br>
                    xmin=-0.15, xmax=-1,ymin=55.5, ymax=56.5) + <br>
  annotation_custom(textGrob("**", gp = gpar(col = "black", fontsize=20)),  <br>
                    xmin=-0.15, xmax=-1,ymin=82.5, ymax=83.5)   + <br>
  guides(fill=guide_colourbar(barwdith=0.5, barheight =18, nbin=100)) + <br>
  theme(plot.title = element_text(size = 20, face = "bold")) <br>
  
  Here is the output:

![](Fig2aeg.png)

This is done for all other panels (d18, temp and VPD, scripts in project)<br>

The next step is plotting the diurnal data from the intensive campaigns. <br>
FIgures 3,4 and s4 depend on the WTC Transpiration model being run:<br>

googledriveWTC4ISOTOPEIDCAMPAIGNDATA <- "d/10Y5-KjrbqeSaG6b_c6N7dJFGMsXSYqyq"<br>
df<- read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", googledriveWTC4ISOTOPEIDCAMPAIGNDATA))<br>
setwd("E:/From M.2/WTC4 Data/Dew")<br>
df<-read.csv(file="CDFWTC4.csv",header=TRUE, sep=",") #Campaign Data Frame WTC4<br>
######<br>
df["source"]=-3.28 ##from branch water<br>
###<br>
R<-287.05<br>
Rv<-461.95<br>
Pr<-101.3<br>
finr<-0.082058<br>
##diurnal d18ocond<br>
##Create Time class (I use Td == Time Date), I also create week and hour etc to explore data<br>
df["Td"]<-(dmy_hm(df$Td, quiet=TRUE, tz="UTC"));str(df$Td)<br>
df["hour"]<-format(df$Td,"%H");str(df$hour)<br>
df["month"]<-format(df$Td,"%m");str(df$month)<br>
df["md"]<-format(df$Td,"%m%d");str(df$md)<br>
df["week"]<-strftime(df$Td,format="%W",tz="UTC")<br>

##These equations are a combo of the WTC4 MS, John Drakes GBC Biology and Craigs orginal WTC4 Agmet paper<br>
####Pref###<br>
df["Pref"]=((df$AIRPRESS_mean*1000)-df$Vwat_mean)/(R*(df$Tair_al_mean+273))+((df$Vwat_mean)/(Rv*df$Vwat_mean+273))<br>
####Fin####<br>
df["fin"]=(df$kfactor_mean*(sqrt(df$DIFFP_mean*(1.2/df$Pref)))*(273/(df$Tair_al_mean+273))*(df$AIRPRESS_mean/Pr))<br>
###Finmol###<br>
df["finmol"]=((df$AIRPRESS_mean*0.00986923267)*df$fin)/(finr*(df$Tair_al_mean+273))<br>
##fout###<br>
df["fout"]=df$fin*(((df$AIRPRESS_mean)-df$Href_mean)/((df$Tair_al_mean)-df$Href_mean))<br>
##fout mol<br>
df["foutmol"]=((df$AIRPRESS_mean*0.00986923267)*df$fout)/(finr*(df$Tair_al_mean+273))<br>
#uoutwout<br>
df["uoutwout"]=df$foutmol*df$Vwat_mean<br>
#uinwin<br>
df["uinwin"]=df$finmol*df$Fwat_mean<br>
#condout<br>
df["condout"]=df$CONDH2O_mean<br>
##E<br>
df["E"]=df$uoutwout-df$uinwin+df$condout<br>
##E-wo<br>
df["e-wo"]=df$E-df$Vwat_mean<br>
##d18out<br>
df["d18out"]=df$d18O.corrected_WV*df$uoutwout<br>
#d18in<br>
df["d18in"]=df$d18O.corrected_WV.AMB.*df$uinwin<br>
##d18c<br>
df["d18c"]=df$d18O.corrected_Cond*df$condout<br>
##over e-wo<br>
df["d18oute"]=df$d18out/df$`e-wo` ;df["d18ine"]=df$d18in/df$`e-wo`; df["d18Ce"]=df$d18c/df$E<br>
##d trans<br>
df["dtrans"]=df$d18oute-df$d18ine+df$d18Ce ; mean(df$dtrans, na.rm = TRUE ) ;df$dtrans # this is d18oatm<br>

We need to express on a leaf area basis <br>

df$leafarea[df$chamber=="C01"]<-23.62101328 #M^2<br>
df$leafarea[df$chamber=="C02"]<- 17.17025483<br>
df$leafarea[df$chamber=="C03"]<- 12.37672299<br>
df$leafarea[df$chamber=="C04"]<-24.62392584<br>
df$leafarea[df$chamber=="C05"]<- 12.92316481<br>
df$leafarea[df$chamber=="C06"]<-29.71035506<br>
df$leafarea[df$chamber=="C07"]<- 14.61233375<br>
df$leafarea[df$chamber=="C08"]<- 24.42841037<br>
df$leafarea[df$chamber=="C09"]<- 17.33770004<br>
df$leafarea[df$chamber=="C10"]<- 17.13905693<br>
df$leafarea[df$chamber=="C11"]<- 9.63132978<br>
df$leafarea[df$chamber=="C12"]<- 27.27409398<br>
df$LACM<-df$leafarea / 10000<br>

#wi is the leaf intercellular vapour concentration (mol water vapour/mol moist air)<br>
#using Licor eqn  mole fraction of water vapor within the leaf, mmol H2O mol air-1.<br>
df["Ei"]<-6.13753*exp(df$leaftemp*((18.564-(df$leaftemp/254.4)))/(df$leaftemp+255.57))<br>
df$wi<-df$Ei / df$AIRPRESS_mean/10  #mol mol -1<br>
##make E a flux relative to tree lef area<br>
df$Emmol<-df$E*1000 #go to mmol<br>
df$Eleaf<-df$Emmol/df$leafarea ; plot(df$Eleaf) #(mmol H2O m-2 s-1)<br>

##Calculate VPD - good to use the plant ecophys package here <br>
df$esat<-esat(TdegC=df$Tair_al_mean, Pa = 101)<br>
df$vpd<-RHtoVPD(RH=df$RH_al_mean, TdegC=df$Tair_al_mean, Pa = 101) ; plot(df$vpd);summary(df$vpd) #the vapour pressure deficit of the chamber airspace (kPa)<br>

##Calculate gt -gt is total conductance to water vapour through the stomata and leaf boundary layer<br>
df$gs<-df$Eleaf/df$vpd/10 # mol<br>

##Calculate Leaf Water Residence Times##<br>
df$W<- 12 #mol m-2<br>
df$t<-df$W/(df$gs*df$wi) # in seconds<br>
df$tm<-df$t/60 ;mean(df$tm, na.rm=TRUE);min(df$tm, na.rm=TRUE);max(df$tm, na.rm=TRUE) # T in mins<br>
df$th<-df$t/3600 ;mean(df$th, na.rm=TRUE);min(df$th, na.rm=TRUE);max(df$th, na.rm=TRUE) # T in hour<br>
###Calculate for isotopes<br>
df$ek=0.027<br>
df$alphak<-1+df$ek<br>
df["Eplus"]<-2.644-(3.206*((10^3)/(df$Tair_al_mean+273.16)))+(1.534*((10^6)/((df$Tair_al_mean+273.16)^2)))<br>
df$betaplus<-(df$Eplus/1000)+ 1<br>
mean(df$alphak*df$betaplus)<br>
df$p<-1.2 ##See SI from MS<br>
##df$alphak*df$betaplus shold be around 1.040 (Table 1 Grahams and Lucas paper)<br>
df$tiso<-(df$W*df$alphak*df$betaplus)/(df$p*df$gs*df$wi)<br>
df$tmiso<-df$tiso/60 ;mean(df$tmiso, na.rm=TRUE);min(df$tmiso, na.rm=TRUE);max(df$tmiso, na.rm=TRUE) # T in mins<br>
df$thiso<-df$tiso/3600 ;mean(df$thiso, na.rm=TRUE);min(df$thiso, na.rm=TRUE);max(df$thiso, na.rm=TRUE)<br>

###calculate number obsertavtions of dtrans within 2 mil<br>
df$range<-ifelse(df$dtrans  >= -5.28 & df$dtrans <= -1.28, "within", "not");df$range<-as.factor(df$range)<br>
summary(df$range) # 107 not in 264 within = 367 total % witin 2 % is 258 /367<br>
264/(264+107)<br>

##The following script defines when chamber air temp and chamber dew point temp are within 1C or negative<br>
#We need to figure out if Dew Point deltas could be influecing gs and make a new colum which says if points are within<br>
df$dpdiffwtc<-df$Tair_al-df$DewPntC;df$dpdiffwtc<br>
df$rangewtc<-ifelse(df$dpdiffwtc  >= -1000000 & df$dpdiffwtc<= 1, "not", "within");df$rangewtc<-as.factor(df$rangewtc)<br>
summary(df$rangewtc) # 80 not in 520 within <br>

#Count how many data points are comprimised from each treatment<br>
sub<- df[df$trt == "ambient",] ;summary(sub$rangewtc) # 80 outisde out of 220 ambineto observatins or 36 % <br>
subx<- df[df$trt == "elevated ",] ;summary(subx$rangewtc) # 0 <br>

#We add a filter to define if it is day time or not and make data frames accoringly<br>
df$sun<- ifelse(df$PAR >=0.01, "day", "night")<br>
dfsun<-filter(df, df$sun == "day")<br>
dfnight<-filter(df, df$sun == "night")<br>

from here we get the daytime means <br>
summaryBy(.~trt+Camp,FUN=c(mean,sd),keep.names=T,data=dfsunmean,na.rm=TRUE);max(dfsun$dtrans,na.rm=TRUE)<br>
and finnaly i create a new dataframe to go and graph<br>

dfa<-df ##I make dfa to use in the code which makes Figures 2,3 and 4- These scripts will cal for WTC4 Ispflux model to be run<br>

if the above model is run and we have dfa in the global environment we can make all neccasry panels:<br>
For example here is the temperture code:<br>

wtc<-dfa<br>
Cw<-wtc<br>
Cw<-subset(Cw, Camp== 1)<br>
colnames(Cw)<br>
Cw <- Cw[,c("rangewtc","Tdh","chamber","trt","Tair_al_mean")]<br>
Trtmean<- summaryBy(.~Tdh+trt+rangewtc,FUN=c(mean),keep.names=T,data=Cw)<br>
#Trtmean<-filter(Trtmean, !grepl('<NA>', trt ));str(Trtmean)<br>
se <- function(x) sqrt(var(x)/length(x))<br>
Trtsd<- summaryBy(.~Tdh+trt+rangewtc,FUN=c(se),keep.names=T,data=Cw)<br>
data<-cbind(Trtmean,Trtsd);data<-as.data.frame(data)<br>

data['Time']<-(dmy_hm(data$Tdh, quiet=TRUE, tz="UTC"));str(data)<br>

#######################<br>
Cwx<-wtc<br>
Cwx<-subset(Cwx, Camp== 3)<br>
Cwx <- Cwx[,c("rangewtc","Tdh","chamber","trt","Tair_al_mean")]<br>
Trtmeanx<- summaryBy(.~Tdh+trt+rangewtc,FUN=c(mean),keep.names=T,data=Cwx)<br>
#Trtmeanx<-filter(Trtmeanx, !grepl('<NA>', trt ))<br><br>
se <- function(x) sqrt(var(x)/length(x))<br><br>
Trtsdx<- summaryBy(.~Tdh+trt+rangewtc,FUN=c(se),keep.names=T,data=Cwx)<br><br>
data2<-cbind(Trtmeanx,Trtsdx);data2<-as.data.frame(data2)<br>

data2['Time']<-(dmy_hm(data2$Tdh, quiet=TRUE, tz="UTC"));str(data)<br>

start = as.POSIXct('2016-10-24 19:00:00"', tz="UTC") <br>
end = as.POSIXct('2016-10-25 07:00:00', tz="UTC")<br>

start2 = as.POSIXct('2016-11-20 19:00:00"', tz="UTC") <br>
end2 = as.POSIXct('2016-11-21 07:00:00', tz="UTC")<br>

str(rects)<br>
str(data$Time)<br>


t1<-ggplot(data=data, aes(x=Time, y=Tair_al_mean))+<br>
  geom_point(aes(colour=trt, shape=trt), size=3.5,)+ <br>
  annotate("rect", fill = "black", alpha = 0.15, <br>
           xmin = end, xmax = start,<br>
           ymin = -Inf, ymax = Inf)+<br>
  
  scale_color_manual(name="Legend:",labels = c("Transpired Ambient Chamber", "Transpired Elevated Chamber"),values = c("black", "red"))+<br>
  scale_shape_manual(name="Legend:",labels = c("in", "out" ),values = c(16,16))+<br>
  geom_errorbar(aes(ymin=Tair_al_mean-Tair_al_mean.1, ymax=Tair_al_mean+Tair_al_mean.1), width=.2,<br>
                position=position_dodge(.9))+<br>
  labs(y="", x = "",element_text(size = 6))+<br>
  theme_classic()+theme(panel.border = element_rect(fill = "NA", colour = "black", size = 2))+<br>
  ggtitle("")+ theme(legend.position = "none")+<br>
  scale_y_continuous ( breaks=scales::pretty_breaks(n=5),<br>
                       limits = c(0, 40))+<br>
  theme(axis.text.y = element_text( color="black", size=25))+<br>
  theme(axis.text.x = element_text( color="black", size=16, angle=45, hjust=1 , vjust=1))<br>


t3<-ggplot(data=data2, aes(x=Time, y=Tair_al_mean))+<br>
  geom_point(aes(colour=trt, shape=rangewtc), size=3.5,)+ <br>
  annotate("rect", fill = "black", alpha = 0.15, <br>
           xmin = end2, xmax = start2,<br>
           ymin = -Inf, ymax = Inf)+<br>
  
  scale_color_manual(name="Legend:",labels = c("Transpired Ambient Chamber", "Transpired Elevated Chamber"),values = c("black", "red"))+<br>
  scale_shape_manual(name="Legend:",labels = c("in", "out" ),values = c(16,16))+<br>
  geom_errorbar(aes(ymin=Tair_al_mean-Tair_al_mean.1, ymax=Tair_al_mean+Tair_al_mean.1), width=.2,<br>
                position=position_dodge(.9))+<br>
  labs(y="", x = "",element_text(size = 6))+<br>

  theme_classic()+theme(panel.border = element_rect(fill = "NA", colour = "black", size = 2))+<br>
  ggtitle("")+ theme(legend.position = "none")+<br>
  scale_y_continuous ( breaks=scales::pretty_breaks(n=5),<br>
                       limits = c(0, 40))+<br>
  theme(axis.text.y = element_text( color="black", size=25))+<br>
  theme(axis.text.x = element_text( color="black", size=16, angle=45, hjust=1 , vjust=1))<br>
  
  here is t1 (which stands for temp - campiagn 1)
  ![](tempeg.png)
  each varible (temp , gs , Eleaf etc has its own script and then they are merged using cowplot::plot_grid) <br>
  figure s4 is slightly different in that it uses facet wrap on the chambers to produce the plots. 




