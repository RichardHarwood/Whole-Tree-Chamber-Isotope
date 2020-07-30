# Whole-Tree-Isoflux
Code to reproduce Whole Tree Isoflux 
The steps to recreate WTC4.

Step 1 Create Figure 2 - This involves exploring temporal data it uses 2 data frames (a water vapour oxygen isotope one (from the LGR) and a gas exchange one from WSU 
(portions of these dataframes are later macthed with wind and rain data to run stats)

Each Panel of Figure 2 is created individually - but the process is more or less the same:
#Load Packages
require(lubridate);require(ggplot2);require(xts);require(dplyr);require(grid);require(cowplot);require(reshape2)
require(plantecophys);require(akima);require(fields);require(tripack);require(autoimage)# Load Packages
#Load Data
googledriveWTC4ISOTOPEID <- "1RNzYApcCO1D1sr_mVQgQVSQFIWNWlqVm"
df<- read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", googledriveWTC4ISOTOPEID)) #
###Set data as time and assign set differnt simmaries e.g months weeks weeks and hour days etc:
df["Td"]<-(dmy_hm(df$stime, quiet=TRUE, tz="UTC"));summary(df$Td)
df$date <- as.Date(df$Td, format = "%Y-%m-%d") ; summary(df$date); str(df$date)
df["month"]<-format(df$Td,"%m");str(df$month)
df$hm <- format(df$Td, "%H") ; summary(df$hm)
df["week"]<-strftime(df$Td,format="%W",tz="UTC");str(df$week)
########################Extract flux data relevant to isotope
start<-ymd_hms("2016-08-28 12:00:00 UTC");summary(start)
end<-ymd_hms("2016-11-26 12:00:00")
int<-interval(start, end); summary(int)
df<-subset(df, df$Td %within% int);head(df$Td);tail(df$Td)
##Filter out unrealisitic values
df<-subset(df, df$d18O.corrected <= -8)
df<-subset(df, df$d18O.corrected >= -20)
#Make a managable data frame and get trt means
dftrt<-data.frame(df$hm, df$month, df$d18O.corrected, df$T_treatment, df$Td);str(dftrt)
dft<-dftrt %>% group_by(df.T_treatment, df.Td) %>% summarise_at(.vars = names(.)[3:3], na.rm=TRUE, .funs = c(mean))
##decide if looking at atm , hot or ambinet by un hashing 
#dft<-subset(dft,df.T_treatment == "ambient")
#dft<-subset(dft,df.T_treatment == "elevated")
dft<-subset(dft,df.T_treatment == "atm")

#Rereun time setting (this could improbed no need to do twice really)
dft$date <- as.Date(dft$df.Td, format = "%Y-%m-%d") 
dft["month"]<-format(dft$df.Td,"%m");str(dft$month);summary(dft$month)
dft["monthday"]<-format(dft$df.Td,"%m%d");str(dft$monthday);summary(dft$monthday)
dft<-transform(dft,day=as.numeric(factor(monthday))) #day
dft<-as.data.frame(dft)
dft$hm <- format(dft$df.Td, "%H") ; summary(dft$hm)

##to make the heat map we need a dataframe with 3 vairables: 1) hour 2) day 3) a value (here it is d180atm)
dfsml<-data.frame(dft$hm, dft$day, dft$df.d18O.corrected)
#the exammple I followed used xyz so I changed my data frames names and we need to make some STR changes
colnames(dfsml)<-c("x", "y","z") ; str(dfsml)
dfsml$x<-as.numeric(paste(dfsml$x))
dfsml$y<-as.numeric(paste(dfsml$y));str(dfsml)
x<-as.numeric(paste(dfsml$x))
y<-as.numeric(paste(dfsml$y))
z<-dfsml$z

##We need then need to interpert the data:

sample=as.data.frame(cbind(x,y,z))
names(sample) <- c("d", "m", "value")
d1 <- with(sample, interp(x = d, y = m, z = value, nx = 250, ny=250, duplicate = "mean", extrap=FALSE))
d2 <- melt(d1$z, na.rm = TRUE)
names(d2) <- c("x", "y", "z")

d2$d<- d1$x[d2$x]
d2$m <- d1$y[d2$y]

#When plotting I use this colour map - 
jet.colors <- colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))

Then in ggplot we can make the graph 
a<-ggplot(data = d2, aes(x = d, y = m, fill = z, z = z)) + 
  geom_tile()+  
  #scale_fill_viridis()+
  # scale_fill_gradientn(colours = c("blue","green", "yellow", "orange", "red"), limits=li)+
  # scale_fill_viridis(option = "E")+
  #+ scale_colour_gradient2()
  scale_fill_gradientn(colours = jet.colors(7), limits =c(-20,-8))+
  
  labs(x="Hour", y="Day")+
   labs(fill=bquote(delta^18*O))+
  #labs(fill=("kPa"))+
  ggtitle((a)~delta^18*O~Atmospheric~Water~Vapour)+
  # theme(axis.line=element_blank())+
  theme_bw()+
  theme(text = element_text(size=17))+
  theme(axis.text=element_text(size=17))+ 
  theme(legend.position='right') +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5))+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5))+
  # ylim(8.0,12 )
  #xlim(0,24)+ 
  
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 62, ymax = 67, alpha = 1)+
  theme(plot.title = element_text(size = 13.5, face = "bold"))+
  annotation_custom(textGrob("*", gp = gpar(col = "black", fontsize=20)), 
                    xmin=-0.15, xmax=-1,ymin=55.5, ymax=56.5) +
  annotation_custom(textGrob("**", gp = gpar(col = "black", fontsize=20)), 
                    xmin=-0.15, xmax=-1,ymin=82.5, ymax=83.5)   +
  guides(fill=guide_colourbar(barwdith=0.5, barheight =18, nbin=100)) +
  theme(plot.title = element_text(size = 20, face = "bold"))

a



