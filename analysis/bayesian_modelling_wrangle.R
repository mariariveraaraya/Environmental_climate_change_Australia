## ---- load-pkg
library(here)
library(tidyverse)
library(data.table)
library(tidyr)
library(ggplot2)
library(magick)
library(kableExtra)
library(flextable)

## ---- tb-one-pre


dates_table<-read.csv(here("experiments", "exp_radiocarbon","data", "Compiled_Aug_19_corr.csv"),na.strings=c("NA","#DIV/0!",""))


dates_table$ID<- gsub("SAN", "", dates_table$ID)

dates_table$OZCode<- gsub(" ", "", dates_table$OZCode,fixed = TRUE)

colnames(dates_table)[colnames(dates_table)=="ID"] <- "Identifier"

dates_table$Identifier<- as.numeric(as.character(dates_table$Identifier))

dates_table<-dates_table[-15,]

correctdepths<-read.csv(here("experiments", "exp_radiocarbon","data", "replacement depths itrax_3.csv"))


correctdepths<-correctdepths %>% rename(Depth=Real.depth)

correctdepths$Identifier<- as.numeric(as.character(correctdepths$Identifier))

correctdepths<-filter(correctdepths, Depth < 14 | Depth > 15.5)





correctdepths<-correctdepths %>% rename(Depth=Real.depth)%>%
        filter(Depth <14 | Depth>21)

correctdepths2<-mutate(correctdepths,Identifier=ifelse(Depth==47, (75), Identifier))
correctdepths2<-mutate(correctdepths,Identifier=ifelse(Depth==48, (75.5), Identifier))


correctdepths$Identifier<-as.numeric(correctdepths$Identifier)


correctdepths_1<-read.csv(here("experiments", "exp_radiocarbon","data", "replacement depths itrax_3_2.csv"))

correctdepths_1<-correctdepths_1 %>% rename(Depth=Real.depth)%>%
        filter(Depth <14 | Depth>21)
correctdepths_1$Identifier<-as.numeric(correctdepths_1$Identifier)


ages_final<-read.table(here("experiments","exp_radiocarbon","data","Bacon_runs","SAN8_2019_18","SAN8_2019_18_35_ages.txt",skip=1))

setwd(here("experiments","exp_radiocarbon","data"))


temp = list.files(pattern="*.txt")

myfun <- function(x) {
        read.table(x,skip = 2,fill=TRUE)
}

myfiles = lapply(temp, myfun)


### check if pollen and charcoal dates got messed up (interchange them)

org<-as.data.frame(myfiles[[1]])
org<-org[1:7,]

cellulose<-as.data.frame(myfiles[[2]])
cellulose<-cellulose[1:1,]

hypy<-as.data.frame(myfiles[[3]])
hypy<-hypy[1:13,]

pollen<-as.data.frame(myfiles[[4]])
pollen<-pollen[1:2,]

charcoal<-as.data.frame(myfiles[[5]])
charcoal<-charcoal[1:4,]

data522<- rbind(hypy,pollen,charcoal,org,cellulose)

data522<-data522%>%
        plyr::rename(c("V1"="OZCode","V2"="from_67","V3"="to_67","V4"="from_95","V5"="to_95"))

setwd(here())

all_first_table<-data522%>%
        left_join(dates_table)%>%
        left_join(correctdepths_1)%>%
        select(OZCode,Depth,Conv_RC,Conv_RC_error,to_95,from_95,Carbon_fraction,Pretreatment)
        
model_no_hyp<- all_first_table%>%
        filter(Pretreatment!="Hypy")%>%
        filter(Pretreatment!="H2O2 + ABA")%>%
        filter(Depth!=3)%>%
        select(OZCode,Conv_RC,Conv_RC_error,Depth)%>%
        rename("labID"="OZCode","age"="Conv_RC","error"="Conv_RC_error","depth"="Depth")

#write.csv(model_no_hyp, '/Users/Maria Jose Rivera/OneDrive - James Cook University/Australia renamed/Sanamere/Thesis sections/PhD/experiments/exp_radiocarbon/data/Bacon_runs/SAN8_2019_13/SAN8_2019_13.csv',row.names = F)

model_BO<- all_first_table%>%
        filter(Carbon_fraction== "Bulk organics")%>%
        filter(Pretreatment!="H2O2 + ABA")%>%
        select(OZCode,Conv_RC,Conv_RC_error,Depth)%>%
        rename("labID"="OZCode","age"="Conv_RC","error"="Conv_RC_error","depth"="Depth")

#write.csv(model_BO, '/Users/Maria Jose Rivera/OneDrive - James Cook University/Australia renamed/Sanamere/Thesis sections/PhD/experiments/exp_radiocarbon/data/Bacon_runs/SAN8_2019_15/SAN8_2019_15.csv',row.names = F)

model_BO2<- all_first_table%>%
        filter(Carbon_fraction== "Bulk organics")%>%
        filter(Pretreatment!="H2O2 + ABA")%>%
        select(Carbon_fraction,Depth, to_95, from_95)%>%
        rename("Fraction"="Carbon_fraction","depth"="Depth","min"="to_95","max"="from_95")

model_Hypy<- all_first_table%>%
        filter(Carbon_fraction== "SPAC")%>%
        select(OZCode,Conv_RC,Conv_RC_error,Depth)%>%
        rename("labID"="OZCode","age"="Conv_RC","error"="Conv_RC_error","depth"="Depth")

#write.csv(model_Hypy, '/Users/Maria Jose Rivera/OneDrive - James Cook University/Australia renamed/Sanamere/Thesis sections/PhD/experiments/exp_radiocarbon/data/Bacon_runs/SAN8_2019_16/SAN8_2019_16.csv',row.names = F)

model_Hypy_2<- all_first_table%>%
        filter(Carbon_fraction== "SPAC")%>%
        select(Carbon_fraction,Depth, to_95, from_95)%>%
        rename("Fraction"="Carbon_fraction","depth"="Depth","min"="to_95","max"="from_95")




model_ABA_mod<- all_first_table%>%
        filter(Pretreatment!="Hypy")%>%
        filter(Depth!=3)%>%
        select(OZCode,Conv_RC,Conv_RC_error,Depth)%>%
        rename("labID"="OZCode","age"="Conv_RC","error"="Conv_RC_error","depth"="Depth")

#write.csv(model_ABA_mod, '/Users/Maria Jose Rivera/OneDrive - James Cook University/Australia renamed/Sanamere/Thesis sections/PhD/experiments/exp_radiocarbon/data/Bacon_runs/SAN8_2019_14/SAN8_2019_14.csv',row.names = F)


all_first_table2<-all_first_table%>%
        unite("Conventional radiocarbon dates",3:4, sep=" \u00B1 ")%>%
        unite("Calibrated age range (95 %)",4:5,sep=" \u002D ")%>%
        rename("Laboratory Code"="OZCode", "Carbon fraction"="Carbon_fraction")%>%
        filter("Conventional radiocarbon dates" != "25,450 ± 170")%>%
        arrange(Depth)


library(officer)  
library(flextable)

tabledates<-regulartable(all_first_table2)
tabledates2<-theme_vanilla(set_table_properties(tabledates, width=1,layout = "autofit"))


pp_rc<-all_first_table2%>%
        select(Depth,`Carbon fraction`,Pretreatment)%>%
        arrange(Depth)

final_model<-all_first_table%>%
        filter(Pretreatment=="Hypy")%>%
        unite("Conventional radiocarbon dates",3:4, sep=" \u00B1 ")%>%
        unite("Calibrated age range (95 %)",4:5,sep=" \u002D ")%>%
        rename("Laboratory Code"="OZCode", "Carbon fraction"="Carbon_fraction","Depth (cm)"="Depth")%>%
        filter("Conventional radiocarbon dates" != "25,450 ± 170")

        
hydro_1<-all_first_table2%>%
        rename("Depth (cm)"="Depth")%>%
       # filter("Laboratory Code"!="OZX767", "Laboratory Code"!="OZX768", "Laboratory Code"!="OZX769", "Laboratory Code"!="OZY132","Laboratory Code"!="OZY422", "Laboratory Code"!="OZY419","Laboratory Code"!="OZY418","Laboratory Code"!="OZY131","Laboratory Code"!="OZX765","Laboratory Code"!="OZX766")%>%
        filter(!grepl('OZX767', `Laboratory Code`))%>%
        filter(!grepl('OZX768', `Laboratory Code`))%>%
        filter(!grepl('OZX769', `Laboratory Code`))%>%
        filter(!grepl('OZY132', `Laboratory Code`))%>%
        filter(!grepl('OZY422', `Laboratory Code`))%>%
        filter(!grepl('OZY419', `Laboratory Code`))%>%
        filter(!grepl('OZY418', `Laboratory Code`))%>%
        filter(!grepl('OZY131', `Laboratory Code`))%>%
        filter(!grepl('OZX766', `Laboratory Code`))%>%
        filter(!grepl('OZY423', `Laboratory Code`))%>%
        filter(!grepl('OZY758', `Laboratory Code`))


ages_final_ITRAX222<-read.csv(here("data","ages.csv"))%>%
        select(-X)

table_str<-read.table(here("other","Hydro","Table_CONISS_ITRAX_2.csv",sep=",",skip=1))
#colnames(table_str)<-c("Depth (cm)","Ti","Si","Fe","Description","Layer","Unit","Distance to base (cm)","Age range (ka cal yr BP)")
colnames(table_str)<-c("Unit (cm)","Ti","Si","Fe","Description","Layer","Unit","Depth","Age range (ka)")

table_str_2<-select(table_str,-"Fe",-"Ti",-"Si")%>%
        left_join(ages_final_ITRAX222)%>%
        select(-`Age range (ka)`,-max,-min,-mean,-Depth.mm)%>%
        mutate(median=median/1000)%>%
        mutate(median=round(`median`,digits=1))

colnames(table_str_2)<-c("Depth (cm)","Description","Layer","Unit","Distance to base (cm)","Age (ka)")


table_sed<-table_str_2%>%
        select(-6)

table_sed2 <- table_sed%>% #changed layer rep "5", from 17  to 16
        mutate(Layer = c(rep("5", 16), rep("4", 3),rep("3",1),rep("2",4),rep("1",3)))%>%
        mutate(`Depth (cm)`=c(rep("0-43",16),rep("43-65",3),rep("65-71",1),rep("71-140",4),rep("140-172",3)))%>%
        mutate(Description=c(rep("2.5 YR 4/6 Dark silty peat",16),rep("5 YR 4/4 Silty clay",3),rep("7.5 YR 4/3 thinly stratified lenses (reddish bands)",1),rep("7.5 YR 4/3 Silty clay",4),rep("5YR 5/8 Silty clay with gravel",3)))

table_str_b<-read.table("C:/Users/Maria Jose Rivera/OneDrive - James Cook University/Australia renamed/Sanamere/Thesis Sections/PhD/other/Hydro/Table_CONISS_ITRAX_2.csv",sep=",",skip=1)
#colnames(table_str)<-c("Depth (cm)","Ti","Si","Fe","Description","Layer","Unit","Distance to base (cm)","Age range (ka cal yr BP)")
colnames(table_str_b)<-c("Unit (cm)","Ti","Si","Fe","Description","Layer","Unit","Depth","Age range (ka cal yr BP)")



table_str_b2<-select(table_str_b,-"Fe",-"Ti",-"Si")%>%
        left_join(ages_final_ITRAX222)%>%
        select(-`Age range (ka cal yr BP)`,-max,-min,-mean,-Depth.mm)%>%
        mutate(median=median/1000)%>%
        mutate(median=round(`median`,digits=1))

colnames(table_str_b2)<-c("Depth (cm)","Description","Layer","Unit","Distance to base (cm)","Age (ka)")


table_sed_b<-table_str_b2%>%
        select(-6)

table_sed2_b <- table_sed_b%>%
        mutate(Layer = c(rep("4", 17), rep("3", 3),rep("2",4),rep("1",3)))%>%
        mutate(`Depth (cm)`=c(rep("0-43",17),rep("43-65",3),rep("65-140",4),rep("140-172",3)))%>%
        mutate(Description=c(rep("2.5 YR 4/6 Dark silty peat",17),rep("5 YR 4/4 Silty clay",3),rep("7.5 YR 4/3 Silty clay",4),rep("5YR 5/8 Silty clay with gravel",3)))%>%
        select(1:3)%>%
        distinct()

table_str_22 <- table_str_b2%>%
        mutate(Layer = c(rep("4", 17), rep("3", 3),rep("2",4),rep("1",3)))%>%
        mutate(`Depth (cm)`=c(rep("0-43",17),rep("43-65",3),rep("65-140",4),rep("140-172",3)))%>%
        mutate(Description=c(rep("2.5 YR 4/6 Dark silty peat",17),rep("5 YR 4/4 Silty clay and 7.5 YR 4/3 thinly stratified lenses (reddish bands)",3),rep("7.5 YR 4/3 Silty clay",4),rep("5YR 5/8 Silty clay with gravel",3)))

sed<-read.csv(here("data","Table_CONISS_ITRAX_3.csv"))
colnames(sed)<-c("Depth (cm)","Description","Layer","Age (ka)")

library(officer)      

tablesed<-regulartable(sed)
tablesed2<-theme_vanilla(set_table_properties(tablesed, width=1,layout = "autofit"))


table5<-table_str_22

autecology<-read.csv('data/diatoms_autecology_2.csv')

autecology_2<-autecology%>%
        select(Species, Habitat,pH, Additional.information, References)

## ---- tb-one


all_first_table2%>%
       arrange(`Depth`)%>%
        knitr::kable("latex",booktabs = TRUE, caption = "(ref:tb-one)", linesep = "")%>%
        kableExtra::kable_styling(position = "center", latex_options= "scale_down")
        #as_image(all_first_table2)

## ---- tb- two

all<-data522%>%
        left_join(dates_table)%>%
        mutate(Carbon_fraction=ifelse(Pretreat_comments =="30 % H2O2 overnight, ABA",'Bulk organics (H2O2+ABA)', paste(dates_table$Carbon_fraction)))%>%
        left_join(correctdepths,by="Identifier")%>%
        select(OZCode,Depth,from_95,to_95,Carbon_fraction)    


## ---- tb-three
all<-data522%>%
        left_join(dates_table)
cbbPalette2 <- c("#0000FF", "#330000", "#FFA200", "#008000", "#F3FA1A","#FF0000","#8B5F8D","#F3FA1A")#"#2A00E5""#56B4E9""#1FC910",


all11<-mutate(all,Carbon_fraction=ifelse(Pretreat_comments =="30 % H2O2 overnight, ABA",'Bulk organics (H2O2+ABA)', paste(all$Carbon_fraction)))


all2<-all11%>%
        left_join(correctdepths,by="Identifier")%>%
        mutate(mean_2=((from_95)+ (to_95))/2)%>%
        select(OZCode,Depth,from_95,to_95,Carbon_fraction,mean_2)
young_old<-all2%>%
        group_by(Depth)%>%
        summarize(max_depth= max(mean_2),min_depth=min(mean_2))%>%
        mutate(diff=max_depth-min_depth)%>%
        filter(diff!=0)
young_old_corr<-all2%>%
        group_by(Depth)%>%
        unique()%>%
        summarize(max_depth= max(from_95),min_depth=min(to_95),length_Car=n())%>%
        mutate(diff=max_depth-min_depth)%>%
        filter(diff!=0)%>%
        merge(young_old,by="Depth")%>%
        select(1:5)

youn_old2_corr<-young_old_corr%>%
        left_join(all2)%>%
        select(Depth,min_depth.x,Carbon_fraction,to_95,length_Car)%>%
        filter(to_95==min_depth.x)%>%
        mutate(Fraction='Min')

youn_old22_corr<-young_old_corr%>%
        left_join(all2)%>%
        select(Depth,max_depth.x,mean_2,Carbon_fraction,from_95,length_Car)%>%
        filter(from_95==max_depth.x)%>%
        mutate(Fraction='Max')

#ff<-merge(youn_old2,youn_old22,by="Depth")

fff<-merge(youn_old2_corr,youn_old22_corr,by="Depth")

ff2<-fff%>%
        select(Depth,min_depth.x,Carbon_fraction.x,max_depth.x,Carbon_fraction.y,length_Car.x)

ff2$Carbon_fraction.x <- gsub("SPAC", "Hypy", ff2$Carbon_fraction.x)
ff2$Carbon_fraction.y <- gsub("SPAC", "Hypy", ff2$Carbon_fraction.y)


ff3<-ff2%>%
        merge(young_old,by="Depth")%>%
        select(Depth,min_depth.x,Carbon_fraction.x,max_depth.x,Carbon_fraction.y,diff,length_Car.x)%>%
        unique()%>%
        arrange(`Depth`)%>%
        rename("Depth (cm)"= "Depth", "Minimum age"="min_depth.x", "Carbon fraction (min)"="Carbon_fraction.x","Maximum age"="max_depth.x","Carbon fraction (max)"="Carbon_fraction.y","Offset"="diff","Number of dates"="length_Car.x")

cbbPalette <- c("#000000", "#2A00E5", "#56B4E9", "#1FC910", "#69D3A4", "#FFFF00", "#C91025", "#FFFF00")


tabledatesb<-regulartable(ff3)
tabledatesb2<-theme_vanilla(set_table_properties(tabledatesb, width=1,layout = "autofit"))
       

save_as_docx(tabledatesb2, path = here("data", "table_ch4_2.docx"))
#scale_colour_manual(values=cbbPalette)

g12<-ggplot(all2,aes(x=Depth,y=to_95,ymin= to_95,ymax=from_95,colour=Carbon_fraction))+geom_point(size=5)+ xlab("Depth (cm)")+ ylab("Calibrated date (BP)")+
        scale_x_continuous(breaks=c(3,6,12,43,67,76,82,90,105,114,137,146,162))+scale_y_continuous(breaks=seq(0, 32000, by=5000))+theme_bw()+xlab("Depth (cm)")+ scale_colour_manual(values=cbbPalette)

g12+ guides(color = guide_legend(override.aes = list(size=5))) + scale_fill_manual(name="",breaks=c("size"),labels=c(" "))


all3<-all2
all3$Carbon_fraction<- gsub("SPAC", "Hypy", all3$Carbon_fraction)


e3<-ggplot(all3)+ geom_errorbar(data = all3, aes(x=Depth,ymin = to_95/1000, ymax = from_95/1000, group=Carbon_fraction,color=Carbon_fraction),width=10,size=1)+ xlab("Depth (cm)")+ ylab("Calibrated date (ka BP)")+
        scale_x_reverse(breaks=c(6,23,41,65,105,135,160))+scale_y_continuous(breaks=seq(0, 33000, by=5))+theme_bw(base_size = 35)+theme(legend.title=element_blank(),axis.title.x=element_text(size=16,face="bold"),axis.text.x=element_text(size=14),axis.title.y=element_text(size=16,face="bold"),axis.text.y=element_text(size=14))+ scale_colour_manual(values=cbbPalette2,name="Carbon fraction")+
        coord_flip()

e33<-ggplot(all3)+ geom_errorbar(data = all3, aes(x=Depth,ymin = to_95/1000, ymax = from_95/1000, group=Carbon_fraction,color=Carbon_fraction),width=10,size=1)+ xlab("Depth (cm)")+ ylab("Calibrated date (ka)")+
        scale_x_reverse(breaks=seq(0,173, by=20))+scale_y_continuous(breaks=seq(0, 33000, by=5))+theme_bw(base_size = 30)+theme(legend.title=element_blank(),axis.title.x=element_text(face="bold"),axis.title.y=element_text(face="bold"))+ scale_colour_manual(values=cbbPalette2,name="Carbon fraction")+
        coord_flip()
e44<-e33 + guides(color = guide_legend(override.aes = list(size=5))) + scale_fill_manual(name="",breaks=c("size"),labels=c(" "))



e44<-e33 + guides(color = guide_legend(override.aes = list(size=5))) + scale_fill_manual(name="",breaks=c("size"),labels=c(" "))
# to save 811 x 549 (width/height) https://www.rapidtables.com/web/color/RGB_Color.html

#print(e44)

#dev.off()

## ---- tb-four
#ff3%>%
 #       arrange(`Depth (cm)`)%>%
  #      knitr::kable(booktabs = TRUE, caption = "(ref:tb4-sub)", linesep = "") %>%
   #     kableExtra::kable_styling(position = "center", latex_options= "hold_position")

ff3%>%
       arrange(`Depth (cm)`)%>%
       knitr::kable("latex",booktabs = TRUE, caption = "(ref:tb-four)", linesep = "") %>%
        kableExtra::kable_styling(position = "center", latex_options= "scale_down")
#ff4<-flextable(ff3)


#ff4
## ---- radiocarbon-gr1



## ---- sed-rate
source('C:/Users/Maria Jose Rivera/OneDrive - James Cook University/Australia renamed/Sanamere/Thesis sections/PhD/preprocessing/pre_MAR.R')
print(sed_rate)


## ---- tb-hy



final_model%>%
        arrange(`Depth (cm)`)%>%
        knitr::kable("latex",booktabs = TRUE, caption = "(ref:tb-hy)", linesep = "") %>%
        kableExtra::kable_styling(position = "center", latex_options= "scale_down")


## ---- tb-units


table_str_22%>%
       knitr::kable("latex",booktabs = T, align = "c",caption = "(ref:tb-units)") %>%
        kableExtra::kable_styling(position = "center", latex_options= "scale_down")%>%
        column_spec(2, width = "4cm")%>%
        collapse_rows(columns = 1:3, valign = "middle")


## ---- tb-units2


## ---- tb-sed

table_sed2_b%>%
        knitr::kable("latex",booktabs = T, align = "c",caption = "(ref:tb-sed)") %>%
        kableExtra::kable_styling(position = "center")

## ---- tb-aut

autecology_2%>%
        knitr::kable("latex", caption = "(ref:tb-aut)", linesep = "", longtable=TRUE)%>%
        kable_styling(latex_options = c("repeat_header"))%>%#"latex"
       kableExtra::column_spec(4, width = "10cm")%>%
        kableExtra::column_spec(2, width = "2cm")%>%
        kableExtra::column_spec(3, width = "2cm")%>%
        kableExtra::column_spec(1, width = "2cm", italic=T)%>%
        kableExtra::column_spec(5, width = "3cm")%>%
        landscape()


setwd('here()')


Element<- c("Aluminum", "Silicon", "Phosphorous", "Sulfur", "Chloride","Potassium", "Calcium", "Scandium","Titanium","Vanadium","Chromium","Manganese", "Iron", "Nickel","Copper", "Zinc", "Bromide","Rubidium", "Strontium", "Yttrium", "Zirconium","Palladium", "Barium", "Lanthanum", "Cerium", "Lead") 
Abbreviation<-c("Al", "Si", "P", "S", "Cl", "K", "Ca", "Sc", "Ti", "V", "Cr","Mn", "Fe", "Ni", "Cu", "Zn", "Br", "Rb", "Sr", "Y", "Zr", "Pd", "Ba", "La", "Ce", "Pb")

element<-data.frame(Element,Abbreviation)

element2<-regulartable(element)
element3<-theme_vanilla(set_table_properties(element2, width=0.5,layout = "autofit"))
#table8<-italic(table8,j=1)

#save_as_docx(element3, path = here("data", "table_ch3_1.docx"))
