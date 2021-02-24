
# Isotopic steady state or non-steady state? Insights from whole tree transpiration

## This read me will document the analysis of a whole tree chamber expirment that used stable isotopes to investigate the isotopic composotion of tree transpiration.

There are a number of .CSVs hosted on google drive - the R scripts will download them automatically. To recreate the analysis internet is needed. 

Figure 2
Figure 2 plots the the isotope composition, air temperature and VPD of the atmosphere, and ambient and eleveted chambers. To do this I interpolated between data points.
The readme will show one example (Figure 2, Panel A) the "Figure 2.R" file will recreate teh whole plot
## Steps
Load Packages
```r
library(lubridate);library(ggplot2);library(xts);library(dplyr);library(grid);library(cowplot)
library(plantecophys);library(akima);library(doBy);library(reshape2);library(gdata);library(ggpubr);
library(data.table);library(rmarkdown);library(tinytex);library(knitr)
```
Load isotope data
```r
googledriveWTC4ISOTOPEID <- "1RNzYApcCO1D1sr_mVQgQVSQFIWNWlqVm"
df<- read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", googledriveWTC4ISOTOPEID))
```
Next I define "Time" and then create colums that are days and hours
```r
df["Td"]<-(dmy_hm(df$stime, quiet=TRUE, tz="UTC"));summary(df$Td)
df$date <- as.Date(df$Td, format = "%Y-%m-%d") ; summary(df$date); str(df$date)
df["month"]<-format(df$Td,"%m");str(df$month)
df$hm <- format(df$Td, "%H") ; summary(df$hm)
df["week"]<-strftime(df$Td,format="%W",tz="UTC");str(df$week)
```
Next I trim the data to the relevent timeframe
```r
start<-ymd_hms("2016-08-28 12:00:00 UTC");summary(start)
end<-ymd_hms("2016-11-26 12:00:00")
int<-interval(start, end); summary(int)
df<-subset(df, df$Td %within% int);head(df$Td);tail(df$Td)
```
Next I trim a few outliers
```r
df<-subset(df, df$d18O.corrected <= -8)
df<-subset(df, df$d18O.corrected >= -20)
```
Then I create a trimmed data frame with relevent columns 
```r
dftrt<-data.frame(df$hm, df$month, df$d18O.corrected, df$T_treatment, df$Td);str(dftrt)
```
Then I calculate treatment means (+ 3C, Ambient and Atmosphereic)
```r
dft<-dftrt %>% group_by(df.T_treatment, df.Td) %>% summarise_at(.vars = names(.)[3:3], na.rm=TRUE, .funs = c(mean))
```
Because panel a is the atmopshere I subset the atmopshere data 
```r
dft<-subset(dft,df.T_treatment == "atm") #Only take atmosphere data
```
The time class is lost in the averaging we we assign time structure 
```r
dft$date <- as.Date(dft$df.Td, format = "%Y-%m-%d") 
dft["month"]<-format(dft$df.Td,"%m");str(dft$month)
dft["monthday"]<-format(dft$df.Td,"%m%d");str(dft$monthday)
dft<-transform(dft,day=as.numeric(factor(monthday))) #day
dft<-as.data.frame(dft)
dft$hm <- format(dft$df.Td, "%H") 
```
I then create a subset again with hour, day and isotope data
```r
dfsml<-data.frame(dft$hm, dft$day, dft$df.d18O.corrected) 
```
The next step is to prepare for interpolataion   
```r
colnames(dfsml)<-c("x", "y","z") ; str(dfsml)
dfsml$x<-as.numeric(paste(dfsml$x))
dfsml$y<-as.numeric(paste(dfsml$y));str(dfsml)
x<-as.numeric(paste(dfsml$x))
y<-as.numeric(paste(dfsml$y))
z<-dfsml$z
sample=as.data.frame(cbind(x,y,z))
names(sample) <- c("d", "m", "value")
```
and then to interpolate
```r
d1 <- with(sample, interp(x = d, y = m, z = value, nx = 250, ny=250, duplicate = "mean", extrap=FALSE))
```
I then make the data 3 colums
```r
d2 <- melt(d1$z, na.rm = TRUE)
```
and prepare to plot
```r
names(d2) <- c("x", "y", "z")
d2$d<- d1$x[d2$x]
d2$m <- d1$y[d2$y]
```
Fristly I define a blue to red color map

```r
br=colorRampPalette(colors = c("#00007F","blue","#7F0000","red"))
```
and define some labels that will highlight the intensive campaigns 

```r
c1 <- textGrob("-", gp=gpar(fontsize=50, fontface="bold", col="black"))
c2<- textGrob("-", gp=gpar(fontsize=50, fontface="bold", col="grey"))
```
I use "ggplot" to plot

```r
ggplot(data = d2, aes(x = d, y = m, fill = z, z = z)) + 
  geom_tile()+  
  scale_fill_gradientn(colours = br(1000), limits =c(-20,-8))+
  labs(x="Hour", y="Day")+
  labs(fill=bquote(delta^18*O))+
  ggtitle(bquote((a)~delta^18*O[a]))+
  theme_bw()+
  theme(text = element_text(size=25))+
  theme(axis.text=element_text(size=25))+ 
  theme(legend.position='right') +
  scale_x_continuous(expand=c(0,0)) + 
  scale_y_continuous(expand=c(0,0))+
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 62, ymax = 67, alpha = 1,
           colour="black",fill="black")+
  guides(fill=guide_colourbar(barwdith=0.5, barheight =20, nbin=50)) +
  theme(plot.title = element_text(size = 20, face = "bold"))+
  annotation_custom(c1, xmin=23.7, xmax=23,ymin=55.5, ymax=56.5) +
  annotation_custom(c2, xmin=23.7, xmax=23,ymin=82.5, ymax=83.5) 
  ```
  ![Screenshot](  Images/Fig2A.png)









