## ---- ages

library(tidyr)
library(dplyr)

# This creates a file with dates and depths so it can be used in other analyses
# Involves radiocarbon dating results 

## Wrangling depths from sediment core file 

correctdepths<-read.csv("C:/Users/Maria Jose Rivera/OneDrive - James Cook University/Australia renamed/Sanamere/Bulk density/replacement depths itrax_3.csv")
correctdepths<-correctdepths %>% rename(Depth=Real.depth)%>%
        filter(Depth <14 | Depth>21)
correctdepths2<-mutate(correctdepths,Identifier=ifelse(Depth==47, (75), Identifier))
correctdepths2<-mutate(correctdepths,Identifier=ifelse(Depth==48, (75.5), Identifier))
correctdepths$Identifier<-as.numeric(correctdepths$Identifier)
correctdepths_1<-read.csv("C:/Users/Maria Jose Rivera/OneDrive - James Cook University/Australia renamed/Sanamere/Bulk density/replacement depths itrax_3_3-LAPTOP-3NCFQGAR.csv")
correctdepths_1<-correctdepths_1 %>% rename(Depth=Real.depth)%>%
        filter(Depth <14 | Depth>21)
correctdepths_1$Identifier<-as.numeric(correctdepths_1$Identifier)


# The following file was created with Bacon software (for age correction)

ages_final<-read.table("C:/Users/Maria Jose Rivera/OneDrive - James Cook University/Australia renamed/Sanamere/Thesis Sections/PhD/experiments/exp_radiocarbon/data/Bacon_runs/SAN8_2019_18/SAN8_2019_18_35_ages.txt",skip=1)
colnames(ages_final)<-c("Depth","max","min","median","mean")


#FILE TO BE USED TO MERGE WITH OTHERS except itrax

agedepth<-merge(correctdepths_1,ages_final,by="Depth")
agedepth$Identifier<- as.numeric(as.character(agedepth$Identifier))
agedepth2<-mutate(agedepth,Identifier=ifelse(Depth==47, (75), Identifier))
agedepth2<-mutate(agedepth2,Identifier=ifelse(Depth==48, (75.5), Identifier))


## Age model ITRAX

ages_final_ITRAX<-read.table("C:/Users/Maria Jose Rivera/OneDrive - James Cook University/Australia renamed/Sanamere/Thesis Sections/PhD/experiments/exp_radiocarbon/data/Bacon_runs/SAN8_2019_20/SAN8_2019_20_35_ages.txt",skip=1)
colnames(ages_final_ITRAX)<-c("Depth","max","min","median","mean")
ages_final_ITRAX3<-merge(correctdepths_1,ages_final_ITRAX,by="Depth")###FILE TO BE USED TO MERGE WITH OTHERS except itrax
ages_final_ITRAX3$Identifier<- as.numeric(as.character(ages_final_ITRAX3$Identifier))
ages_final_ITRAX3<-mutate(ages_final_ITRAX3,Identifier=ifelse(Depth==47, (75), Identifier))
ages_final_ITRAX3<-mutate(ages_final_ITRAX3,Identifier=ifelse(Depth==48, (75.5), Identifier))
ages_final_ITRAX2<-ages_final_ITRAX%>%
        mutate(Depth.mm=Depth*10)

#write.csv(ages_final_ITRAX2,file=here("data","ages.csv"))
