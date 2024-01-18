
library(tidyverse)
library(xlsx)
library(readxl)
library(plotly)

#Hypy section
source('preprocessing/pre_age_model.R')
source('preprocessing/pre_MAR.R')

Hypy.post<-read.csv('experiments/exp_EA/data/Copy of Maria 180831 reduction(19306).csv',na.strings=c("NA","#DIV/0!",""))


Hypy.post1<-read.csv('experiments/exp_Hypy/data/190314.PostHypy.csv',na.strings=c("NA","#DIV/0!",""))


colnames(Hypy.post1)[colnames(Hypy.post1)=="Row"] <- "Line"
colnames(Hypy.post1)[colnames(Hypy.post1)=="rArea.Flash.TCD"] <- "Area.44"



last<-read.csv(here("experiments", "exp_EA","data","EA_14_10_19.csv"),na.strings=c("NA","#DIV/0!",""))

last22<-filter(last,Identifier.2!='prehypy')
colnames(last22)[colnames(last22)=="Î.13C..â..VPDB."] <- "X.13C...VPDB."
colnames(last22)[colnames(last22)=="Î.15Î...â..Î.Î.R."] <- "X.15......R."
colnames(last22)[colnames(last22)=="ï..Line"] <- "Line"

colnames(last22)[colnames(last22)=="X..13C.....VPDB."] <- "X.13C...VPDB."
colnames(last22)[colnames(last22)=="X..15...........R."] <- "X.15......R."
colnames(last22)[colnames(last22)=="X...Line"] <- "Line"




merged.Hypypost<-rbind(Hypy.post,Hypy.post1,last22)




colnames(merged.Hypypost)[colnames(merged.Hypypost)=="X.13C...VPDB."] <- "d13CpostHypy"
colnames(merged.Hypypost)[colnames(merged.Hypypost)=="X.15......R."] <- "d15N"
colnames(merged.Hypypost)[colnames(merged.Hypypost)=="Identifier.1"] <- "Identifier"
##need to change the column anme clarifying this is post Hypy
colnames(merged.Hypypost)[colnames(merged.Hypypost)=="X.C.1"] <- "X.C.1postHypy"





##select column of interest
Hypyrawcombined.1<-select(merged.Hypypost,Identifier,Ampl..44,d13CpostHypy,d15N,X.C.1postHypy,X.N.1,C.N)


Hypyrawcombined.1<-subset(Hypyrawcombined.1,Identifier!="LOC"& Identifier!="HOC"& Identifier!="Flush"& Identifier!="Blank"& Identifier!="Taipan"& Identifier!="177_off"& Identifier!="Sorghum")

Hypyrawcombined.1$Identifier<- as.numeric(as.character(Hypyrawcombined.1$Identifier))


rawsubsetted<-read.csv("C:/Users/Maria Jose Rivera/OneDrive - James Cook University/Australia renamed/Sanamere/Thesis Sections/PhD/rawsubsetted.csv")
prepluspost3<-merge(Hypyrawcombined.1,rawsubsetted,by="Identifier")


merge_both <- prepluspost3%>% 
        select(Identifier,X.C.1postHypy,X.C.1,d13C,d13CpostHypy)%>%
        group_by(Identifier) %>% 
        summarize(av_d13C = mean(d13C),av_C=mean(X.C.1),av_Cpost=mean(X.C.1postHypy),av_d13Cpost=mean(d13CpostHypy))


dim(prepluspost)
dim(prepluspost.subsetted)

raw = read_excel('C:/Users/Maria Jose Rivera/OneDrive - James Cook University/Australia renamed/Sanamere/Hypy/Copy of HypyCalcs_Rainy_modified_04.04.xlsx')

raw2 <- raw[c(1:70),c(1:9)]



#raw3<-merge(raw2,prepluspost,by="Identifier")
raw3<-merge(raw2,merge_both,by="Identifier",all=TRUE)
#raw3<-subset(raw2[1:51,1:9])
#raw3 <- raw3[c(1:76),c(1:9,26,32)]
#raw3 <- raw[c(1:76),c(1:9,26,32)]


##fine?

colnames(raw3)[colnames(raw3)=="Sample Weight (g)"] <- "Sample.weight"
colnames(raw3)[colnames(raw3)=="Catalyst (g)"] <- "Catalyst"
colnames(raw3)[colnames(raw3)=="Pre HyPy Sample Weight (g)"] <- "Pre.Sample.Weight" 
colnames(raw3)[colnames(raw3)=="Post HyPy Sample Weight (g)"] <- "Post.Sample.Weight" 
#raw10<-subset(raw3[-12,])
#raw11<-subset(raw10[-28,])

####Need to put avoid all the NAs!



raw3<-mutate(raw3,PreHypy.X.C=((Sample.weight/100)*av_C/(Sample.weight+Catalyst))*100)
raw3<-mutate(raw3,C.pre.mg=((PreHypy.X.C/100)*Pre.Sample.Weight)*1000)
raw3<-mutate(raw3,C.post.mg=((av_Cpost/100)*Post.Sample.Weight)*1000)
raw3<-mutate(raw3,Ratio.BCTOC=(C.post.mg/C.pre.mg)*100)
raw3<-mutate(raw3,BlackCarbon.Perc=av_C*(Ratio.BCTOC/100))

raw3<-mutate(raw3,Corrected=(((BlackCarbon.Perc/av_C)/1.02)-0.004)*av_C)


raw3$Identifier<-as.numeric(raw3$Identifier)

merged.hypy.ages <- left_join(raw3,agedepth2)



###Correction for isotopic values/taken from other file

tbl.corre.d13c2<-merged.hypy.ages%>%
        select(av_C,av_d13C,av_Cpost,av_d13Cpost,Identifier)%>%
        filter(Identifier!=103,Identifier!=120,Identifier!=104)

#colnames(Hypyrawcombined.2)[colnames(Hypyrawcombined.2)=="Real.depth"] <- "Depth"


colnames(tbl.corre.d13c2)[colnames(tbl.corre.d13c2)=="av_C"] <- "CT"
colnames(tbl.corre.d13c2)[colnames(tbl.corre.d13c2)=="av_d13C"] <- "dT"
colnames(tbl.corre.d13c2)[colnames(tbl.corre.d13c2)=="av_Cpost"] <- "CR"
colnames(tbl.corre.d13c2)[colnames(tbl.corre.d13c2)=="av_d13Cpost"] <- "dR"


dat<-tbl.corre.d13c2

#dat<-dat[-39,]

# Total carbon in sample (%)
C_T <- dat$CT

# Error in total carbon measurement (%)
s.e.C_T <- 0.02*C_T

# d13C of C_T (per mil)
d_T <- dat$dT
dat.length <- length(d_T) # length of data (number of rows)

# Error in d13C of C_T (taken as constant at 1.0 per mil)
s.e.d_T <- rep(1.0,dat.length)

# Carbon in residue (%)
C_R <- dat$CR

# Error in residual carbon measurement (%)
s.e.C_R <-0.02*C_R

# d13C of C_R (per mil)
d_R <- dat$dR

# Error in d13C of d_R (taken as constant at 0.1 per mil)
s.e.d_R <- rep(0.1,dat.length)

Identifier<-dat$Identifier

# Bind new variables into one dataset
dat.bind <- cbind(C_T, s.e.C_T, d_T, s.e.d_T, C_R, s.e.C_R, d_R, s.e.d_R)

# Create blank vectors for quantile data created in loop later
lc.result <- vector("numeric", dat.length)
med.result <- vector("numeric", dat.length)
uc.result <- vector("numeric", dat.length)

# loop through each measurement, simulating 10000 values 
# assuming a normal distribution for C_T, d_T, C_R and d_R.
# phi (percentage of labile carbon remaining in residue) is simulated using a previously fit beta distribution (fig S1 above)

for (i in 1:dat.length){
        C_T.norm <- rnorm(10000,dat.bind[i,1], dat.bind[i,2])
        d_T.norm <- rnorm(10000,dat.bind[i,3], dat.bind[i,4])
        C_R.norm <- rnorm(10000,dat.bind[i,5], dat.bind[i,6])
        d_R.norm <- rnorm(10000,dat.bind[i,7], dat.bind[i,8])
        phi <- (rbeta(10000,1.18776,3.08183))*2.0
        # equation (1)in main text; phi converted to fraction
        d_P <- (C_R.norm*d_R.norm- phi/100*d_T.norm*C_T.norm)/(C_R.norm - phi/100*C_T.norm)
        # take quantiles and record in previously created blank vectors
        lc <- quantile(d_P, probs = 0.16, names = FALSE)
        med <- quantile(d_P, probs = 0.5, names = FALSE)
        uc<- quantile(d_P, probs = 0.84, names = FALSE)
        lc.result[i] <- lc
        med.result[i] <- med
        uc.result[i] <- uc}

# Combine quantiles into single data frame and print
stats <- cbind(lc.result,med.result,uc.result)
stats2<-cbind(stats,tbl.corre.d13c2)
stats3<-merge(stats2,merged.hypy.ages)
stats4<-stats3[,1:26]
#print(stats2)




#Hypy.selected<-select(Hypy,Identifier,Corrected..Wurster.et.al..2012.)
#Hypy.selected2<-merge(merged.datamass4,raw3,by="Identifier")
merged.datamass4$Identifier<- as.numeric(as.character(merged.datamass4$Identifier))
merged.hypy.ages$Identifier<- as.numeric(as.character(merged.hypy.ages$Identifier))


Hypy.selected4<-left_join(stats4, merged.datamass4,by='Identifier')
Hypy.selected4<-mutate(Hypy.selected4,PyCxMAR=((MAR*(Corrected)/100)/100)*1000) #ug/mm2yr
Hypy.selected4<-mutate(Hypy.selected4,PyCxMAR_2=((MAR*(Corrected)/100))*1000) 

Hypy.selected4<-subset(Hypy.selected4,Identifier!=177)

#Mar is in mg/cm2/y, corrected is a percentage

Hypy.selected5<-Hypy.selected4%>%
        
        group_by(Identifier) %>%
        slice(1)

dim(Hypy.selected5)
Hypy.selecna<-Hypy.selected5%>%
        filter(is.na(PyCxMAR))


####Together

#Age in x

PyCxMAR.graph<-ggplot(Hypy.selected5,aes(x=median,y=PyCxMAR))+geom_point() + ylab(expression(atop("PyC MAR",paste(mu~g~mm^{-2}~yr^{-1}))))+ xlab("Age (yr cal BP)")+ theme_bw()+ scale_x_continuous(breaks=seq(0, 32000, by=5000)) + scale_y_continuous(breaks=seq(0,0.6,by=0.2))+ theme(axis.text=element_text(size=12),axis.title=element_text(size=12,face="bold"))

PyCxMAR.graph2<-ggplot(Hypy.selected5,aes(x=median,y=PyCxMAR))+geom_point() + ylab(expression(atop("PyC MAR",paste(mu~g~mm^{-2}~yr^{-1}))))+ xlab("Age (yr cal BP)")+ theme_bw()+ scale_x_continuous(breaks=seq(0, 32000, by=5000)) + scale_y_continuous(breaks=seq(0,1,by=0.1))+ theme(axis.text=element_text(size=18),axis.title=element_text(size=18,face="bold"))


plot(PyCxMAR.graph2)

pydepth<-ggplot(Hypy.selected5,aes(x=PyCxMAR,y=median))+geom_point() + xlab(expression(atop("PyC MAR",paste(mu~g~mm^{-2}~yr^{-1}))))+ ylab("Depth")+ theme_bw()+ scale_y_reverse(breaks=seq(0, 180, by=10)) + scale_x_continuous(breaks=seq(0,0.6,by=0.2))+ theme(axis.text=element_text(size=12),axis.title=element_text(size=12,face="bold"))

pydepth
#NAs<-filter(Hypy.selected5,PyCxMAR=="NA")
#NAs


blackcarbon.age3<-ggplot(Hypy.selected5, aes(x=Corrected,y=median))+ xlab("% PyC")+ ylab("Calibrated date (BP)")+geom_point()+ theme_bw()  +scale_y_reverse(breaks=seq(0, 32000, by=2500))+ theme(axis.title.y=element_blank(),
                                                                                                                                                                                                    axis.text.y=element_blank(),axis.text.x=element_text(size=12),axis.title.x=element_text(size=12))
#Age in y

graph.corrected<-ggplot(Hypy.selected5, aes(x=med.result,y=median))+geom_point(size=2) + xlab(expression(paste(PyC~delta^{13}, C[VPDB], "(\u2030)")))+ ylab("Age (yr cal BP)")+ theme_bw()+ scale_y_reverse(breaks=seq(0, 32000, by=2500)) +  scale_x_continuous(breaks=seq(-26,-22,by=1))+ theme(axis.text.x=element_text(size=12),axis.title.x=element_text(size=12,face="bold"))

#Age in x
graph.corrected2<-ggplot(Hypy.selected5, aes(x=median,y=med.result))+geom_point(size=2) + ylab(expression(paste(PyC~delta^{13}, C[VPDB], "(\u2030)")))+ xlab("Age (yr cal BP)")+ theme_bw()+ scale_x_continuous(breaks=seq(0, 32000, by=5000)) +  scale_y_continuous(breaks=seq(-26,-22,by=1))+ theme(axis.text.x=element_text(size=12),axis.title.x=element_text(size=12,face="bold"))

graph.corrected3<-ggplot(Hypy.selected5, aes(x=median,y=med.result))+geom_point(size=2) + ylab(expression(paste(PyC~delta^{13}, C[VPDB], "(\u2030)")))+ xlab("Age (yr cal BP)")+ theme_bw()+ scale_x_continuous(breaks=seq(0, 32000, by=5000)) +  scale_y_continuous(breaks=seq(-26,-22,by=1))+ theme(axis.text=element_text(size=18),axis.title=element_text(size=18,face="bold"))

plot(graph.corrected3)

grid.arrange(PyCxMAR.graph, blackcarbon.age3, graph.corrected,nrow=1,ncol=3)
hypy_graph<-grid.arrange(graph.corrected, blackcarbon.age3, ncol=2)

ggplotly(PyCxMAR.graph3)


PyCxMAR.graph3<-ggplot(Hypy.selected5,aes(x=median,y=PyCxMAR))+geom_point() + ylab(expression(atop("PyC MAR",paste(mu~g~mm^{-2}~yr^{-1}))))+ xlab("Age (yr cal BP)")+ theme_bw()+ scale_x_continuous(breaks=seq(0, 32000, by=5000)) + scale_y_continuous(breaks=seq(0,1,by=0.1))+ theme(axis.text=element_text(size=18),axis.title=element_text(size=18,face="bold"))+ annotate("text",x=16000,y=0.45,label="Fire",size=20,color="red")
graph.corrected4<-ggplot(Hypy.selected5, aes(x=median,y=med.result))+geom_point(size=2) + ylab(expression(paste(PyC~delta^{13}, C[VPDB], "(\u2030)")))+ xlab("Age (yr cal BP)")+ theme_bw()+ scale_x_continuous(breaks=seq(0, 32000, by=5000)) +  scale_y_continuous(breaks=seq(-26,-22,by=1))+ theme(axis.text=element_text(size=18),axis.title=element_text(size=18,face="bold"))+ annotate("text",x=21000,y=-23,label="Vegetation",size=20,color="green4")


mod_hypy <- gam(PyCxMAR ~ s(median,k=15), data = Hypy.selected5, method = "REML")

plot(mod_hypy)

ggplotly(PyCgraph.corrected3)
PyCxMAR.graph3
graph.corrected4

##log%PYC vs %C

Cvslog<-ggplot(Hypy.selected5, aes(x=log(av_C),y=log(Corrected)))+geom_point(size=2) + ylab(expression(paste("log % PyC")))+ xlab("% C")+ theme_bw() + theme(axis.text=element_text(size=18),axis.title=element_text(size=18,face="bold"))
Cvslog

PyCxMAR.graph3<-ggplot(Hypy.selected5,aes(x=Depth,y=PyCxMAR))+geom_point() + ylab(expression(atop("PyC MAR",paste(mu~g~mm^{-2}~yr^{-1}))))+ xlab("Depth (cm)")+ theme_bw()+ scale_x_continuous(breaks=seq(0, 175, by=20)) + scale_y_continuous(breaks=seq(0,1,by=0.1))+ theme(axis.text=element_text(size=18),axis.title=element_text(size=18,face="bold"))
graph.corrected4<-ggplot(Hypy.selected5, aes(x=Depth,y=med.result))+geom_point(size=2) + ylab(expression(paste(PyC~delta^{13}, C[VPDB], "(\u2030)")))+ xlab("Depth (cm)")+ theme_bw()+ scale_x_continuous(breaks=seq(0, 175, by=20)) +  scale_y_continuous(breaks=seq(-26,-22,by=1))+ theme(axis.text=element_text(size=18),axis.title=element_text(size=18,face="bold"))

PyCxMAR.graph3


ggplotly(PyCxMAR.graph3)
graph.corrected4

plot_hypy <- Hypy.selected5 %>%
        select(Depth, PyCxMAR) %>%
        na.omit() %>%
        ggplot() +
        geom_point(aes(x = PyCxMAR, y = Depth), size = 1, alpha = 0.75) + geom_path(aes(x = PyCxMAR, y = Depth), size = 1, alpha = 0.75)+
        xlab("PyC MAR (ug mm-2/yr)") +
        theme_minimal() +
        scale_y_reverse()+
        theme(axis.title.x=element_text(size=12),
              axis.title.y = element_blank(),
              axis.text.y = element_blank())
plot_hypy
ggplotly(plot_hypy)



hypy_to_merge<-Hypy.selected5%>%
        select(Depth, av_d13Cpost,av_d13C,Corrected)


plot_hypy_median <- Hypy.selected5 %>%
        select(median, PyCxMAR) %>%
        na.omit() %>%
        ggplot() +
        geom_point(aes(x = PyCxMAR, y = median), size = 1, alpha = 0.75) + geom_path(aes(x = PyCxMAR, y = median), size = 1, alpha = 0.75)+
        xlab("PyC MAR (ug mm-2/yr)") +
        theme_minimal() +
        scale_y_reverse(breaks = seq(0, 33000, by = 1000))+
        theme(axis.title.x=element_text(size=12))
        #      axis.title.y = element_blank(),
         #     axis.text.y = element_blank())
plot_hypy_median
ggplotly(plot_hypy_median)

plot_hypy_toc <- Hypy.selected5 %>%
        select(av_d13C, av_d13Cpost)%>% 
        filter(av_d13C<(-20))%>%
        na.omit() %>%
        ggplot() +
        geom_point(aes(x = av_d13C, y = av_d13Cpost), size = 1, alpha = 0.75)+
        geom_smooth(aes(x = av_d13C, y = av_d13Cpost),method='lm')+
        theme_minimal() +
        scale_y_continuous()+
        theme(axis.title.x=element_text(size=12))
#      axis.title.y = element_blank(),
#     axis.text.y = element_blank())
print(plot_hypy_toc)

lm2<-Hypy.selected5%>%
        filter(median<10000 & median >4700)

lm22<-lm(lm2$av_d13Cpost ~ lm2$av_d13C)

lm<-lm(Hypy.selected5$av_d13C ~ Hypy.selected5$av_d13Cpost)

summary(lm22)
str(lm2)

plot_hypy_ab <- Hypy.selected5 %>%
        select(PyCxMAR, av_d13Cpost)%>% 
        na.omit() %>%
        ggplot() +
        geom_point(aes(x = PyCxMAR, y = av_d13Cpost), size = 1, alpha = 0.75)+
        geom_smooth(aes(x = PyCxMAR, y = av_d13Cpost),method='lm')+
        theme_minimal() +
        scale_y_continuous()+
        theme(axis.title.x=element_text(size=12))
#      axis.title.y = element_blank(),
#     axis.text.y = element_blank())
print(plot_hypy_ab)

lm_ab<-lm(Hypy.selected5$PyCxMAR ~ Hypy.selected5$av_d13Cpost)
summary(lm_ab)
summary(Hypy.selected5$av_d13Cpost)

plot_hypy_toc_hy <- Hypy.selected5 %>%
        select(Corrected,av_C)%>% 
        na.omit() %>%
        ggplot() +
        geom_point(aes(x = av_C, y = Corrected), size = 1, alpha = 0.75)+
        geom_smooth(aes(x = av_C, y =Corrected),method='lm')+
        theme_minimal() +
        scale_y_continuous()+
        theme(axis.title.x=element_text(size=12))
#      axis.title.y = element_blank(),
#     axis.text.y = element_blank())
print(plot_hypy_toc_hy)

lm_tochy<-lm(Hypy.selected5$Corrected ~ Hypy.selected5$av_C)
summary(lm_tochy)

#Spearman???? Need to convert to ranks!
cor.test(Hypy.selected5$Corrected,  Hypy.selected5$av_C,
          data=Hypy.selected5,
          method = "spearman",
          continuity = FALSE,
          conf.level = 0.95)

cor.test(Hypy.selected5$Corrected,  Hypy.selected5$av_C,
         data=Hypy.selected5,
         method = "kendall",
         continuity = FALSE,
         conf.level = 0.95)
