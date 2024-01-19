# This spreadsheet is for the processing of radiocarbon dates using Bayesian modelling and the package Bacon.

library(ggplot2)
library(dplyr)
library(rbacon)
library(readxl)
library(here)


setwd(here("experiments", "exp_radiocarbon","data"))



#All hypy dates 
Bacon(core="SAN8_2019_7",cc=3)

##All hypy dates with no changes (hiatus in code/settings)
Bacon(core="SAN8_2019_11",cc=3)

## Dates of original model (SAN...10) plus OZY422 and OZY418 (hypy dates orginally removed for 10 to fit the model)
Bacon(core="SAN8_2019_12",cc=3,depths.file = TRUE)

## Dates without hypy, mod bulk organics and 423 (depth=3),refer to an_radiocarbon model_no_hyp, 82 % age- depth model
Bacon(core="SAN8_2019_13",cc=3)

## Dates without hypy,  and 423 (depth=3),refer to an_radiocarbon model_ABA_mod, only 69 % age-depth model
Bacon(core="SAN8_2019_14",cc=3)



Bacon(core="SAN8_2019_15",cc=3,d.min=0,d.max=172)

Bacon(core="SAN8_2019_19",d.min=0,d.max = 172.2) #bulk organics
Bacon(core="SAN8_2019_18",d.min=0,d.max = 172.2) #hypy
Bacon(core="SAN8_2019_20",d.min=0,depths.file = TRUE) #hypy

## ---- models


BO<-read.table('C:/Users/Maria Jose Rivera/OneDrive - James Cook University/Australia renamed/Sanamere/Thesis sections/PhD/experiments/exp_radiocarbon/data/Bacon_runs/SAN8_2019_19/SAN8_2019_19_35_ages.txt',header=T)


app2<-rep(c("Bulk organics"), times=173)

BO$fraction<-app2


e10<-ggplot(BO)+ geom_errorbar(data = BO, aes(x=depth,ymin = min, ymax = max,width=1.5))+ xlab("Depth (cm)")+ ylab("Calibrated date (BP)")+
        scale_x_continuous(breaks=c(2,6,12,22,32,41,55,65, 75, 85,95,105,115,125,135,144,160,170))+scale_y_continuous(breaks=seq(0, 32000, by=2000))+theme_bw()# scale_colour_manual(values=cbbPalette)


Fraction<-c("Pollen", "Pollen", "Cellulose")

depth<-c(146,6,42)

#min<-c(13745,5045,6755)

min<-c(13567,4894,6755)


#max<-c(14007,5305,7240)
max<-c(13944,5285,7240)


frac2<-data.frame(Fraction,depth,min,max)


frac_BO<-rbind(frac2,model_BO2)

cbbPalette2 <- c("#0000FF", "#330000", "#FFA200", "#008000", "#F3FA1A","#FF0000","#8B5F8D","#F3FA1A")#"#2A00E5""#56B4E9""#1FC910",

e10 +geom_errorbar(data=frac_BO,aes(x=depth, ymin=min,ymax=max, group= Fraction,colour=Fraction), width=5,size=1.5) +
        scale_fill_discrete(name="Fraction")+
        theme( axis.title.x = element_text(size=14, face="bold"),
               axis.title.y = element_text(size=14, face="bold"))+scale_colour_manual(values=cbbPalette2)



app<-rep(c("hypy"), times=173)#172 #1698

hypy<-read.table('experiments/exp_radiocarbon/data/Bacon_runs/SAN8_2019_18/SAN8_2019_18_35_ages.txt',header=T)


hypy$fraction<-app


e11<-ggplot(hypy)+ geom_errorbar(data = hypy, aes(y=depth,xmin = min/1000, xmax = max/1000,width=1.5))+ ylab("Depth (cm)")+ xlab("Calibrated date (ka)")+
        scale_y_reverse(breaks=c(6,12, 32,41,55,65, 75, 85,95,105,125,144,158,170))+scale_x_continuous(breaks=seq(0, 36, by=2))+theme_bw()

e12<-ggplot(hypy)+ geom_errorbar(data = hypy, aes(y=depth,xmin = min/1000, xmax = max/1000,width=1.5))+ ylab("Depth (cm)")+ xlab("Calibrated date (ka)")+
        scale_y_reverse(breaks=seq(0,172, by=10))+scale_x_continuous(breaks=seq(0, 36, by=4))+theme_bw()

png(
        "other/Hydro/age_model.png", 
        width = 7.5, 
        height = 5, 
        res = 300,
        units = "in"
)


efinal<- e12 +
        theme( axis.title.x = element_text(size=14, face="bold"),
               axis.title.y = element_text(size=14, face="bold"),axis.text = element_text(size=14))
print(efinal)




Fraction<-c("Charcoal > 63 um", "Charcoal > 63 um","Charcoal > 63 um", "Charcoal > 250 um", "hypy","hypy","Bulk organics (peroxide + ABA)", "Bulk organics (peroxide + ABA)")

depth<-c(3,6,82,6,23, 105,114,150)



min<-c(12742,4243,12666,4297,7798,21815, 27470,29655)

max<-c(13369,5040,13036,4845,8162,22540,27890,30683)

frac<-data.frame(Fraction,depth,min,max)

model_Hypy_2$Fraction <- gsub('SPAC', 'hypy', model_Hypy_2$Fraction)


hypy_22<-rbind(frac,model_Hypy_2)

both_models<-rbind(hypy_22,frac_BO)
both_models2<-rbind(hypy,BO)

both_models2<-mutate(both_models2,ymin=min/1000,ymax=max/1000)

both_models<-mutate(both_models,ymin=min/1000,ymax=max/1000)


e100<-ggplot(both_models2)+ geom_errorbar(data = both_models2, aes(x=depth,ymin = ymin, ymax = ymax,group= fraction,color=fraction,width=1.5))+ xlab("Depth (cm)")+ ylab("Calibrated date (ka BP)")+
        scale_x_reverse(breaks=c(2,32,55 ,105,135,170))+scale_y_continuous(breaks=seq(0, 36, by=4))+theme_bw(base_size = 20)+theme(legend.title=element_blank(),axis.title.x=element_text(size=22),axis.text.x=element_text(size=14),axis.title.y=element_text(size=22),axis.text.y=element_text(size=14))+
        coord_flip()#+theme(legend.position="none")

e1001<-ggplot(both_models2)+ geom_errorbar(data = both_models2, aes(x=depth,ymin = ymin, ymax = ymax,group= fraction,color=fraction,width=1.5))+ xlab("Depth (cm)")+ ylab("Calibrated date (ka)")+
        scale_x_reverse(breaks=seq(0,173, by=20))+scale_y_continuous(breaks=seq(0, 36, by=4))+theme_bw(base_size = 30)+theme(legend.title=element_blank(),axis.title.x=element_text(face="bold"),axis.title.y=element_text(face="bold"))+
        coord_flip()#+theme(legend.position="none")
png(
        "Figs/models_RC_2.png", 
        width = 14, 
        height = 8, 
        res = 300,
        units = "in"
)

rc_figure<-e1001 +geom_errorbar(data=both_models,aes(x=depth, ymin=ymin,ymax=ymax, group= Fraction,colour=Fraction), width=5,size=1.5) +
        theme( axis.title.x = element_text(face="bold"),
               axis.title.y = element_text( face="bold"))+scale_colour_manual(values=cbbPalette2)
# to change order of axis just change scale_y_continous vs reverse
 #ggsave(filename = "Figs/rc_figure.png", plot = rc_figure, width = 18, height = 10, dpi = 300, units = "cm")

print(rc_figure)

dev.off()

## ---- models2


print(rc_figure)

## ---- models3

#Hypy dates plus some other organics
Bacon(core="SAN8_2019_8",cc=3)

#all dates
Bacon(core="SAN8_2019_9",cc=3)

#hypy with hiatus
Bacon(core="SAN8_2019_7",cc=3, hiatus.depths=c(43,67))

#Bacon(core="SAN8_2019_7",cc=3, boundary.depths=c(43,67))

Bacon("SAN8_2019_2", cc=3)

Bacon("SAN8_2019_2",cc=3,depths.file=TRUE)

Bacon("SAN8_2019_3", cc=3)


### 2 charcoal and 1 bulk organics dates already not included here!

chronos<-read.csv('experiments/exp_radiocarbon/data/Bacon_runs/SAN8_2019_8/SAN8_2019_8.csv')


#Taking out first SPAC date
chronos2<-filter(chronos, labID!="OZX767", labID!="OZX768", labID!="OZX769", labID!="OZY132",labID!="OZY422", labID!="OZY419",labID!="OZY418",labID!="OZY131",labID!="OZY416")


#Taking out 
chronos3<-filter(chronos, labID!="OZX767", labID!="OZX768", labID!="OZX769", labID!="OZY132",labID!="OZY422", labID!="OZY419",labID!="OZY418",labID!="OZY131",labID!="OZX765")


chronos4<-filter(chronos, labID!="OZX767", labID!="OZX768", labID!="OZX769", labID!="OZY132",labID!="OZY422", labID!="OZY419",labID!="OZY418",labID!="OZY131",labID!="OZX765",labID!="OZX766")


chronos5<-chronos4%>%
        select(1)%>%
        rename("Laboratory Code"="labID")%>%
       left_join(all_first_table2,by="Laboratory Code")


Bacon("SAN8_2019_10",cc=3,depths.file=TRUE)

Bacon("SAN8_2019_10",cc=3,depths.file=TRUE)


###New model was done on Oct 23th 2019

Bacon("SAN8_2019_5", cc=3,depths.file=TRUE)


setwd("C:/Users/Maria Jose Rivera/OneDrive - James Cook University/Australia renamed/Sanamere/Radiocarbon")

###New model was done on Oct 23th 2019

Bacon("SAN8_2019_6", cc=3,depths.file=TRUE)


#With depths/ages for ITRAX

library(here)
data2<-read.csv(here("data", "SAN8_2019_compare_rainy2.csv"))



###The following graph compares the previous dates we have for SAN with mines.

compare_rainy2<-read.csv("C:/Users/Maria Jose Rivera/OneDrive - James Cook University/Australia renamed/Sanamere/Radiocarbon/SAN8_2019_compare_rainy2.csv")

compare_rainy2<-compare_rainy2[1:14,]

compare_rainy2$When <- as.character(as.numeric(compare_rainy2$When))


g10<-ggplot(compare_rainy2, aes(x=depth,y=age, group=When ,color=When ))+geom_line()+geom_point()+theme_bw()
print(g10)

####Radiocarbon dates

Rc.dates<-read.csv("C:/Users/Maria Jose Rivera/OneDrive - James Cook University/Australia renamed/Sanamere/Radiocarbon/Results/Book1.csv")

###cleaning names and taking out prefixes and suffixes

Rc.dates$ID<- gsub("SAN", "", Rc.dates$ID)
Rc.dates$ID<- gsub("- U1", "", Rc.dates$ID)
Rc.dates$ID<- gsub("- U2", "", Rc.dates$ID)
Rc.dates$ID<- gsub("\\s", "", Rc.dates$ID)
Rc.dates$Sample.Type<- gsub("Sediment", "", Rc.dates$Sample.Type)


###Include identifier so it can be merged
colnames(Rc.dates)[colnames(Rc.dates)=="ID"] <- "Identifier"

###Include real depths

merge.Rc.depths<-merge(Rc.dates,replacement_depths_itrax,by="Identifier")

str(replacement_depths_itrax)
str(Rc.dates)
Rc.dates$Identifier <- as.factor(as.factor(Rc.dates$Identifier))

###prepare for Oxcal Name, Date, Uncertainty, Depth
##Oxcal is not user friendly. To use it: go to file, open the project, go the "code" view (younger dates are at the bottom) and then file and run.
##Once it is finished, the output can be taken from View and tab delimited (txt file) or go to the menu in the right and select raw data then click save (csv) or plot, to do a personalized plot.

##This section of the code was intented to be used to import txt and csv to Oxcal..but it did not work,
##Select just Hypy
oxcal.hypy <- merge.Rc.depths[which(merge.Rc.depths$Sample.Type=="Hypy  "), ]
##Select columns of interest
oxcal.hypy2<-select(oxcal.hypy,Code,RCA,RCA.error,Real.depth)

##Change column names so it could be imported to Oxcal (it did work at the end)

colnames(oxcal.hypy2)[colnames(oxcal.hypy2)=="Code"] <- "Name"
colnames(oxcal.hypy2)[colnames(oxcal.hypy2)=="RCA"] <- "Date"
colnames(oxcal.hypy2)[colnames(oxcal.hypy2)=="RCA.error"] <- "Uncertainty"
colnames(oxcal.hypy2)[colnames(oxcal.hypy2)=="Real.depth"] <- "Depth"


###Delete commas in the dates

oxcal.hypy2$Date<- gsub(",", "", oxcal.hypy2$Date)

write.csv(oxcal.hypy2, '/Users/Maria Jose Rivera/OneDrive - James Cook University/Australia renamed/Sanamere/Radiocarbon/Results/oxcal.hypy2.csv',row.names = F)

###Now bulk organics

oxcal.org <- merge.Rc.depths[which(merge.Rc.depths$Sample.Type=="Bulk organics"), ]
oxcal.org22<-select(oxcal.org,Code,RCA,RCA.error,Real.depth)

###Combine bulk organics with hypy (rbind is used when you have the same columns)
comb.hypy.org<-rbind(oxcal.hypy,oxcal.org)##uncalibrated


###Compare bulk organics/hypy#calibrated#these files are the outputs from Oxcal

compare_org<-read.csv("C:/Users/Maria Jose Rivera/OneDrive - James Cook University/Australia renamed/Sanamere/Radiocarbon/Oxcal/SANOrg101U22.csv")
compare_hypy<-read.csv("C:/Users/Maria Jose Rivera/OneDrive - James Cook University/Australia renamed/Sanamere/Radiocarbon/Oxcal/SANHypy101U22.csv")

compare_pollen<-read.csv("C:/Users/Maria Jose Rivera/OneDrive - James Cook University/Australia renamed/Sanamere/Radiocarbon/Oxcal/Pollen.csv")

#compare_hypy1<-subset(compare_hypy,name!="depthModel"& z!=160)

##Delete modelled dates and stay just with the ones I need from the depths I measured
compare_hypy1<-subset(compare_hypy,name!="depthModel")
compare_hypy1<-mutate(compare_hypy1,Fraction="Hypy")
compare_org1<-subset(compare_org,name!="depthModel")
compare_org1<-mutate(compare_org1,Fraction="Organics")

compare_both<-rbind(compare_hypy1,compare_org1)

##Calculate the median
compare_both<-mutate(compare_both,mediana=((from_95_4)+(to_95_4))/2)

library(ggplot2)
###this is the one finalgraph
g3<-ggplot(compare_both)+ geom_errorbar(data = compare_both, aes(x=z,ymin = to_95_4, ymax = from_95_4, group=Fraction,color=Fraction, width = 7))+ xlab("Depth (cm)")+ ylab("Calibrated date (BP)")+
        scale_x_continuous(breaks=c(6,41,65,135,144,160))+scale_y_continuous(breaks=seq(0, 32000, by=5000))+theme_bw()

print(g3)

###Delete X column
compare_both<-compare_both[,-5]
####
###Charcoal

charcoal.dates<-read.table("C:/Users/Maria Jose Rivera/OneDrive - James Cook University/Australia renamed/Sanamere/Radiocarbon/Oxcal/SANcharcoal.txt")

colnames(charcoal.dates)[colnames(charcoal.dates)=="V1"] <- "name"
colnames(charcoal.dates)[colnames(charcoal.dates)=="V2"] <- "from_67"
colnames(charcoal.dates)[colnames(charcoal.dates)=="V3"] <- "to_67"
colnames(charcoal.dates)[colnames(charcoal.dates)=="V4"] <- "from_95_4"
colnames(charcoal.dates)[colnames(charcoal.dates)=="V5"] <- "to_95_4"


charcoal.dates<-mutate(charcoal.dates,Fraction=ifelse(name=="OZX765U1", "Charcoal_250","Charcoal_63"))
charcoal.dates<-mutate(charcoal.dates,z=6)
charcoal.dates<-charcoal.dates[,-2]
charcoal.dates<-charcoal.dates[,-2]


compare_both2<-compare_both[,-6]

compare.all<-rbind(charcoal.dates,compare_both2)

compare_all2<-rbind(compare.all,compare_pollen)

compare_mean2<-compare_all2%>%
        mutate(mean_2=((from_95_4)+ (to_95_4))/2)

g4<-ggplot(compare.all)+ geom_errorbar(data = compare_all2, aes(x=z,ymin = to_95_4, ymax = from_95_4, group=Fraction,color=Fraction, size=0.1, width = 0.5))+ xlab("Depth (cm)")+ ylab("Calibrated date (BP)")+
        scale_x_continuous(breaks=c(6,41,65,135,144,160))+scale_y_continuous(breaks=seq(0, 32000, by=5000))+theme_bw()


print(g4)

g5<-ggplot(compare.all)+ geom_errorbar(data = compare_all2, aes(x=z,ymin = to_95_4, ymax = from_95_4, group=Fraction,color=Fraction,width = 6))+ xlab("Depth (cm)")+ ylab("Calibrated date (BP)")+
        scale_x_continuous(breaks=c(6,41,65,135,144,160))+scale_y_continuous(breaks=seq(0, 32000, by=5000))+theme_bw()


print(g5)

g10<-ggplot(compare_mean2,aes(x=z,y=mean_2))+xlab("Depth(cm)")+ylab("Calibrated date BP")

g11<-g10+geom_point(aes(group=Fraction,color=Fraction))

g12<-g11+geom_point(aes(shape=Fraction,size=6))

g13<-g12+guides(size=FALSE)


print(g11)
