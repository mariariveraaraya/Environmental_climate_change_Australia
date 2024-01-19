## ---- load-pkg-dia

library(here)
library(tidyverse)
library(dplyr)
library(tidyr)
library(analogue)


## ---- age

#or need to include read_chunk from a central file/better
ages5<-read.table("analysis/Radiocarbon/SAN8_2019_4_35_ages.txt",skip=1)

colnames(ages5)<-c("Depth","max","min","median","mean")

correctdepths<-read.csv("experiments/exp_ITRAX/data/replacement depths itrax_3.csv")

correctdepths<-correctdepths %>% rename(Depth=Real.depth)

agedepth<-merge(correctdepths,ages5,by="Depth")###FILE TO BE USED TO MERGE WITH OTHERS except itrax

agedepth$Identifier<- as.numeric(as.character(agedepth$Identifier))




## ---- dia-pre

diatoms_counts<-read.csv(here("experiments", "exp_diatoms","data", "Counts_diatoms_08_08_19.csv"))

diatoms_photo<-read.csv(here("experiments", "exp_diatoms","data", "Photos_silica.csv"))

###Concentration of diatoms
conc<-read.csv("experiments/exp_diatoms/data/Concentration diatoms.csv")

diatoms_counts$Species2 <- paste(diatoms_counts$Genus,diatoms_counts$Species)
diatoms_counts$final_morphotype <- paste(diatoms_counts$Morphotype,diatoms_counts$Morphotype_b)


diat_merge<-merge(diatoms_counts,conc,by="Identifier")
diat_merge$Species2[diat_merge$Species2 == "Caloneis 999"] <- "Caloneis"
diat_merge$Species2[diat_merge$Species2 == "Caloneis "] <- "Caloneis"


diat_merge3<-diat_merge%>%
        select(Identifier,Type,Genus,Species2,Counts,Total_sediment_analysed_g,Number_of_transects)%>%
        filter(Type=='Diatom')%>% 
        group_by(Identifier)%>%
        mutate(countT2=sum(Counts)) %>%
        mutate(per=paste0(round(100*Counts/countT2,2)))%>%
        mutate(d_g_wet_sed = countT2/Total_sediment_analysed_g)%>%
        filter(countT2 > 0)


diat_merge4<- diat_merge3%>%
        spread(Species2, per) 


diat_merge20<-select(diat_merge4,Identifier,11:29)


diat_merge11<- aggregate(x=diat_merge20[,2:20], by=list(Identifier=diat_merge4$Identifier), min, na.rm = TRUE)

diat_new<-left_join(diat_merge11,agedepth)

diat_new2<-select(diat_new,-(3:4))

diat_merge10<- aggregate(x=diat_new[,2:20], by=list(median=diat_new$median), min, na.rm = TRUE)
diat_merge100<- aggregate(x=diat_new[,2:20], by=list(Depth=diat_new$Depth), min, na.rm = TRUE)


diat_merge10[, c(1:20)] <- sapply(diat_merge10[, c(1:20)], as.numeric)

diat_merge10[is.na(diat_merge10)] <- 0

#diat_merge10<-diat_merge10[,-(1)]
diat_merge10<-diat_merge10[,-(20)]


diat_merge100[, c(1:19)] <- sapply(diat_merge100[, c(1:19)], as.numeric)

diat_merge100[is.na(diat_merge100)] <- 0

#diat_merge10<-diat_merge10[,-(1)]
diat_merge100<-diat_merge100[,-(20)]




con_diat_1<-diat_merge3%>%
        merge(agedepth)%>%
        filter(Identifier >0.1)

con_diat_2<-ggplot(con_diat_1,aes(y=median,(x=sqrt(d_g_wet_sed))))+geom_point() +  scale_y_reverse(breaks = seq(0, 32500, by = 5000)) + scale_x_continuous(limits=c(0, 600), breaks=c(0,200,400,600)) + ggtitle("")+xlab("Sqrt of Concentration (diat/g)")+ylab("Age (cal yr BP)")+theme_bw() + theme(axis.text.x=element_text(size=12),axis.title.x=element_text(size=12,face="bold"),axis.title.y = element_text(size=12,face="bold"))

#### --- gr-diat-rel

BAR2_merge2 <- Stratiplot(median~ . , data= chooseTaxa(diat_merge10, max.abun = 5, n.occ = 1),
                          type = c("h","g"), sort = "wa",xlab="Relative percentage (%)",ylab="Age (cal yr BP)")



#### ---- gr-diat-conc

print(con_diat_2)


######################################spicules


## ---- spi-pre

spi_2<-diat_merge%>%
        select(Identifier,Type,Genus,Species2,Counts,Total_sediment_analysed_g,Number_of_transects)%>%
        filter(Type=='Spicule')%>% 
        group_by(Identifier)%>%
        mutate(countT2=sum(Counts)) %>%
        mutate(per=paste0(round(100*Counts/countT2,2)))%>%
        mutate(spi_g_wet_sed = countT2/Total_sediment_analysed_g)%>%
        filter(countT2 > 0)





spi_222<-left_join(spi_2,agedepth)

spi_333 <- ggplot(spi_222,aes(y=median,(x=sqrt(spi_g_wet_sed))))+geom_point() +  scale_y_reverse(breaks = seq(0, 32500, by = 5000))  + ggtitle("")+xlab("Sqrt of Concentration (diat/g)")+ylab("Age (cal yr BP)")+theme_bw() + theme(axis.text.x=element_text(size=12),axis.title.x=element_text(size=12,face="bold"),axis.title.y = element_text(size=12,face="bold"))


#### ---- spi-conc

print(spi_333)




