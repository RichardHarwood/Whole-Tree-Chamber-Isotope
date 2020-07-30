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

a
![](github%20images/figure 2a github.tiff)

