library(tidyverse);library(plantecophys);library(gridExtra);library(cowplot)
#setwd("D:/PROJECTS/WTC_4_18O_E/R_results/")

wtc_df<-read.csv("D:/PROJECTS/WTC_4_18O_E/R_results/WTC_Data_w_storage_and_leaf_area.csv")

wtc_df<-
  wtc_df %>% 
  mutate(source=-3.28) %>% 
  mutate (F = Fwat_mean) %>% #F is the flow of water vapour into the chamber (mol s-1),
  mutate (V = Vwat_mean) %>% #V is the venting of water vapour out of the chamber (mol s-1),
  mutate (C = CONDH2O_mean) %>% #C is the rate of water removal from the chamber air by the condenser (mol s-1); 
  mutate (S = deltaH2O_mean) %>% #S is the change in storage of water vapour in the chamber air from one time step to the next (mol s-1).
  mutate(Scalculated=(((HWTC_mean-prevHWTC_mean)/(AIRPRESS_mean*1000))*(ChVolume_mean/22.4))*(1/3600)) %>% #storage term
  mutate(E=V-F+C+Scalculated) %>% #Transpiration
  mutate(deltas=((HWTC_mean*d18O.corrected_WV)-(HWTCPREVIOUS*d18O.corrected_WV_PREVIOUS))/(HWTC_mean-HWTCPREVIOUS)) %>%
  mutate(scaltimessdelta=(Scalculated*deltas)) %>% 
  
  mutate(dtrans=((V*d18O.corrected_WV)-(F*d18O.corrected_WV.AMB.)+(C*d18O.corrected_Cond)+(Scalculated*deltas))/E) %>%#180 of E
  
  mutate(Ei=(6.13753*exp(leaftemp*((18.564-(leaftemp/254.4)))/(leaftemp+255.57)))) %>% 
  mutate(wi = Ei /AIRPRESS_mean/10  ) %>% 
  mutate(Emmol = E*1000) %>% 
  mutate(Eleaf = Emmol/leafarea) %>%
  mutate(esat = esat(TdegC=Tair_al_mean, Pa = 101)) %>% 
  mutate(vpd = RHtoVPD(RH=RH_al_mean, TdegC=Tair_al_mean, Pa = 101)) %>%
  mutate(gs = Eleaf/vpd/10) %>%
  mutate(W = 12) %>% #mol m-2 water in the leaf
  mutate(t = W/(gs*wi)) %>% 
  mutate(th = t/3600) %>% 
  mutate(ek=0.027) %>% 
  mutate(alphak=1+ek) %>% 
  mutate(Eplus=2.644-(3.206*((10^3)/(Tair_al_mean+273.16)))+(1.534*((10^6)/((Tair_al_mean+273.16)^2)))) %>% 
  mutate(betaplus=(Eplus/1000)+ 1) %>% 
  mutate(p=1.2) %>% 
  mutate(tiso=(W*alphak*betaplus)/(p*gs*wi)) %>% 
  mutate(thiso=tiso/3600) %>% 
  mutate(dpdiffwtc=Tair_al_mean-DewPntC_mean) %>%  #define when chamber air temp and chamber dew point temp are within 1C or negative
  mutate(rangewtc=ifelse(dpdiffwtc  >= -1000000 & dpdiffwtc<= 1, "not", "within")) %>% 
  mutate(sun = ifelse(PAR_mean >=0.01, "day", "night")) %>% 
  mutate(ea=HWTC_mean/1000) %>% 
  mutate(ei=Ei/10) %>% 
  mutate(eai=ea/ei) %>% 
  mutate(ek_de=27) %>% 
  mutate(evapObs=((1+Eplus/1000)*((1+ek_de/1000)*(1+dtrans/1000)*(1-eai)+eai*(1+d18O.corrected_WV/1000))-1)*1000) %>% 
  mutate(evapSS=((1+Eplus/1000)*((1+ek_de/1000)*(1+-3.28/1000)*(1-eai)+eai*(1+d18O.corrected_WV/1000))-1)*1000)



mean(wtc_df$dtrans, na.rm=TRUE);max(wtc_df$dtrans, na.rm=TRUE)
#mean(df$dtrans, na.rm=TRUE);max(df$dtrans, na.rm=TRUE)

# Calculate Daily Means
small_wtc=
  wtc_df %>% 
  select(sun,rangewtc,dtrans,trt,Camp,chamber, Tdh,d18O.corrected_Cond,E,d18O.corrected_WV,
         d18O.corrected_WV.AMB.,gs,d18O.corrected_WV_PREVIOUS,d18O.corrected_WV_PREVIOUS,Scalculated, F,V,C,S)

wtc_df_filtered_all_dtrans<-
  wtc_df %>% 
  filter(sun == "day") %>% 
  filter(rangewtc == "within") %>% 
  select(sun,rangewtc,dtrans,trt,Camp,chamber, Tdh,d18O.corrected_Cond,E,d18O.corrected_WV,
         d18O.corrected_WV.AMB.,gs,d18O.corrected_WV_PREVIOUS,d18O.corrected_WV_PREVIOUS,Scalculated, F,V,C,S) %>% 
  na.omit()

wtc_df_filtered<-
  wtc_df %>% 
  filter(sun == "day") %>% 
  filter(rangewtc == "within") %>% 
  filter(dtrans >-25 & dtrans<25) %>% 
  select(sun,rangewtc,dtrans,trt,Camp,chamber, Tdh,d18O.corrected_Cond,E,d18O.corrected_WV,
         d18O.corrected_WV.AMB.,gs,d18O.corrected_WV_PREVIOUS,d18O.corrected_WV_PREVIOUS,Scalculated, F,V,C,S)%>% 
  na.omit()

means_df_raw<-wtc_df %>% 
  filter(sun == "day") %>% 
  filter(rangewtc == "within") %>% 
  filter(dtrans >-25 & dtrans<25) %>% 
  group_by(trt, Camp)%>% 
  summarise(WTC_mean  = mean(d18O.corrected_WV, na.rm=TRUE),
            WTC_SD  = sd(d18O.corrected_WV, na.rm=TRUE),
            ATM_mean  = mean(d18O.corrected_WV.AMB., na.rm=TRUE),
            ATM_SD  = sd(d18O.corrected_WV.AMB., na.rm=TRUE),
            Trans_mean  = mean(dtrans, na.rm=TRUE),
            Trans_SD  = sd(dtrans, na.rm=TRUE),
            Cond_mean  = mean(d18O.corrected_Cond, na.rm=TRUE),
            Cond_SD  = sd(d18O.corrected_Cond, na.rm=TRUE),
            Isotopic_time_mean  = mean(thiso, na.rm=TRUE),
            Isotopic_time_SD  = sd(thiso, na.rm=TRUE),
            difference_from_source= abs(Trans_mean)-abs(-3.28))

wtc_df %>% 
  filter(sun == "day") %>% 
  filter(rangewtc == "within") %>% 
  filter(dtrans >-25 & dtrans<25) %>% 
  summarise(WTC_mean  = mean(d18O.corrected_WV, na.rm=TRUE),
            WTC_SD  = sd(d18O.corrected_WV, na.rm=TRUE),
            ATM_mean  = mean(d18O.corrected_WV.AMB., na.rm=TRUE),
            ATM_SD  = sd(d18O.corrected_WV.AMB., na.rm=TRUE),
            Trans_mean  = mean(dtrans, na.rm=TRUE),
            Trans_SD  = sd(dtrans, na.rm=TRUE),
            Cond_mean  = mean(d18O.corrected_Cond, na.rm=TRUE),
            Cond_SD  = sd(d18O.corrected_Cond, na.rm=TRUE),
            Isotopic_time_mean  = mean(thiso, na.rm=TRUE),
            Isotopic_time_SD  = sd(thiso, na.rm=TRUE),
            difference_from_source= abs(Trans_mean)-abs(-3.28))



means_df<-wtc_df %>% 
  filter(sun == "day") %>% 
  filter(rangewtc == "within") %>% 
  filter(dtrans >-25 & dtrans<25) %>% 
  group_by(trt, Camp)%>% 
  summarise(WTC_mean  = mean(d18O.corrected_WV, na.rm=TRUE),
            WTC_SD  = sd(d18O.corrected_WV, na.rm=TRUE),
            ATM_mean  = mean(d18O.corrected_WV.AMB., na.rm=TRUE),
            ATM_SD  = sd(d18O.corrected_WV.AMB., na.rm=TRUE),
            Trans_mean  = mean(dtrans, na.rm=TRUE),
            Trans_SD  = sd(dtrans, na.rm=TRUE),
            Cond_mean  = mean(d18O.corrected_Cond, na.rm=TRUE),
            Cond_SD  = sd(d18O.corrected_Cond, na.rm=TRUE),
            Isotopic_time_mean  = mean(thiso, na.rm=TRUE),
            Isotopic_time_SD  = sd(thiso, na.rm=TRUE),
            difference_from_source= abs(Trans_mean)-abs(-3.3)) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  mutate(WTC_SD = paste0("(", WTC_SD, ")")) %>% 
  mutate(ATM_SD = paste0("(", ATM_SD, ")")) %>% 
  mutate(Trans_SD = paste0("(", Trans_SD, ")")) %>% 
  mutate(Cond_SD = paste0("(", Cond_SD, ")")) %>% 
  mutate(Isotopic_time_SD = paste0("(", Isotopic_time_SD, ")")) %>% 
  unite("δ v", WTC_mean:WTC_SD,sep = " ", remove = FALSE) %>% 
  unite("δ F", ATM_mean:ATM_SD,sep = "  ", remove = FALSE) %>% 
  unite("δ E", Trans_mean:Trans_SD,sep = "  ", remove = FALSE) %>% 
  unite("δ C", Cond_mean:Cond_SD,sep = "  ", remove = FALSE) %>%
  unite("τ i", Isotopic_time_mean:Isotopic_time_SD,sep = "  ", remove = FALSE) %>% 
  rename("$\\delta$E - $\\delta$source" =difference_from_source ) %>% 
  select(trt,Camp,"δ v","δ F","δ E","δ v","τ i" ) %>% 
  rename(Treatment = trt) %>% 
  rename(Campaign= Camp)%>% 
  mutate(Campaign=case_when(str_detect(Campaign, '3')~"2",
         str_detect(Campaign, '1')~"1"));means_df
 


#ET example 
wtc_df %>% 
  filter(sun == "day") %>% 
  filter(rangewtc == "within") %>% 
  filter(dtrans >-25 & dtrans<25) %>% 
  filter(Tdh == "25/10/2016 10:00") %>% 
  group_by(trt, Camp)%>% 
  summarise(WTC_mean  = mean(d18O.corrected_WV, na.rm=TRUE),
            WTC_SD  = sd(d18O.corrected_WV, na.rm=TRUE),
            ATM_mean  = mean(d18O.corrected_WV.AMB., na.rm=TRUE),
            ATM_SD  = sd(d18O.corrected_WV.AMB., na.rm=TRUE),
            Trans_mean  = mean(dtrans, na.rm=TRUE),
            Trans_max  = max(dtrans, na.rm=TRUE),
            Trans_SD  = sd(dtrans, na.rm=TRUE),
            difference_from_source= abs(Trans_mean)-abs(-3.3))

wtc10am<-wtc_df %>% 
  filter(sun == "day") %>% 
  filter(rangewtc == "within") %>% 
  filter(dtrans >-25 & dtrans<25) %>% 
  filter(Tdh == "25/10/2016 10:00") %>% 
  group_by(trt, Camp) %>% 
  select(trt, Camp,chamber, dtrans)

plot(wtc_df$d18O.corrected_WV.AMB., wtc_df$d18O.corrected_WV )


  
    
    
