## ---- load-pkg-veg
library(here)
library(tidyverse)
library(plotly)
library(rioja)
library(vegan)
library(analogue)

source('C:/Users/Maria Jose Rivera/OneDrive - James Cook University/Australia renamed/Sanamere/Thesis sections/PhD/preprocessing/pre_phy.R')

##---- tb-one-veg

#### fill out table1 and table2_a with species names and Poaceae identification...

#### agedepth file final!!!! include
##Include phy class for modern phyt
##group them by plant group and meaning see other thesis

#Paleoecological potential of phytoliths from lake sediment records from the tropical lowlands of Bolivia


#main reference: mcz064_suppl Nomenclature 2.0, Neumann et al.
##Should include depths with low counts????
#Should reorganize to include the corrected version with concentration
#Stratplot with corrected counts by conc is ready and also diat, phy and sp conc
#Should group the counts by gssc/poaceae, woody (Dicotyle), 3D=blocky, mono vs dico difference?

source('C:/Users/Maria Jose Rivera/OneDrive - James Cook University/Australia renamed/Sanamere/Thesis sections/PhD/preprocessing/pre_age_model.R')



morphotype<-c("Globular echinate","Elongate echinate", "Elongate sinuous", "Globular psilate", "Hair", "Tracheids", "Blocky faceted", "Blocky polyhedron","Globular decorated","Parallelepiped blocky","Cyperaceae","Elongate psilate","Elongate facetated")

plant_group<-c("Woody dicotyledons (Arecaceae?)","Non_diagnostic","Non_diagnostic","Non_diagnostic", "Non_diagnostic","Non_diagnostic","Woody Dicotyledons", "Woody Dicotyledons","Woody Dicotyledons","Woody Dicotyledons","Woody Dicotyledons","Grasses and sedges","Grasses and sedges")

df<-data.frame(morphotype,plant_group)


#next step is to count "wood" or globular granulate phytoliths
#check thesis to see what it means to have the different types ??? of phytoliths


diatoms_counts<-read.csv(here("experiments", "exp_diatoms","data", "Counts_diatoms_08_08_19.csv"))

diatoms_photo<-read.csv(here("experiments", "exp_diatoms","data", "Photos_silica.csv"))

###Concentration of diatoms
conc<-read.csv("C:/Users/Maria Jose Rivera/OneDrive - James Cook University/Australia renamed/Sanamere/Diatoms spicules/Concentration diatoms.csv")
diatoms_counts$Species2 <- paste(diatoms_counts$Genus,diatoms_counts$Species)
diatoms_counts$final_morphotype <- paste(diatoms_counts$Morphotype,diatoms_counts$Morphotype_b)


#Merging counts and concentration
diat_merge<-merge(diatoms_counts,conc,by="Identifier")


#####Modern reference

#### --- phy-pre

#Phytoliths
#phy_2<-diat_merge%>%
#     #select(Identifier,Type,Genus,Species2,Counts,Total_sediment_analysed_g,Number_of_transects)%>%
#      filter(Type=='Phytoliths')%>% 
#     group_by(Identifier)%>%
#    mutate(Corrected_counts=Counts/Total_sediment_analysed_g)%>%
#   mutate(countT= sum(Corrected_counts),countT2=sum(Counts)) %>%
#   mutate(per=paste0(round(100*Corrected_counts/countT,2)))%>%
#   mutate(phy_g_wet_sed = countT2/Total_sediment_analysed_g)%>%
#   mutate(corr_trans= countT2/Number_of_transects)%>%
#  filter(countT2 > 0)

morphotype<-c("Globular echinate","Elongate echinate", "Elongate sinuous", "Globular psilate", "Hair", "Tracheids", "Blocky faceted", "Blocky polyhedron","Globular decorated","Parallelepiped blocky","Cyperaceae","Elongate psilate","Elongate facetated")

plant_group<-c("Woody dicotyledons (Arecaceae?)","Non_diagnostic","Non_diagnostic","Non_diagnostic", "Non_diagnostic","Non_diagnostic","Woody Dicotyledons", "Woody Dicotyledons","Woody Dicotyledons","Woody Dicotyledons","Woody Dicotyledons","Grasses and sedges","Grasses and sedges")

df<-data.frame(morphotype,plant_group)

#next step is to count "wood" or globular granulate phytoliths
#check thesis to see what it means to have the different types ??? of phytoliths


#At least for cluster analysis...filter non_diag and total counts >50?

phy_2<-diat_merge%>%
        select(Identifier,Type,Genus,Species2,Counts,Total_sediment_analysed_g,Number_of_transects,final_morphotype)%>%
        filter(Type=='Phytoliths')%>% 
        filter(Identifier!=64)%>%
        group_by(Identifier)%>%
        mutate(countT2=sum(Counts)) %>%
        mutate(Corrected_counts=Counts/Total_sediment_analysed_g)%>%
        mutate(per=paste0(round(100*Counts/countT2,2)))%>%
        mutate(phy_g_wet_sed = countT2/Total_sediment_analysed_g)
       # filter(countT2 > 50)

        
phy_factor<-phy_2

phy_factor$final_morphotype<-as.factor(phy_factor$final_morphotype)

str(phy_factor$final_morphotype)

phy_2$final_morphotype<- gsub("3D","blocky faceted", phy_2$final_morphotype)
phy_2$final_morphotype<- gsub("blocky faceted","Blocky faceted", phy_2$final_morphotype)
phy_2$final_morphotype<- gsub("cylindroid","Elongate entire cylindrical", phy_2$final_morphotype)
phy_2$final_morphotype<- gsub("999","Unidentified", phy_2$final_morphotype)
phy_2$final_morphotype<- gsub("cylindroid ","Elongate entire cylindrical", phy_2$final_morphotype)


phy_2$per <- as.numeric(as.character(phy_2$per))

phy_2_age<-left_join(phy_2,agedepth2)

phy_all<-ggplot(phy_2_age, aes(x=median,y=per, color=final_morphotype))+geom_point()+ scale_color_hue(h = c(80, 1000))

phy_all

ggplotly(phy_all)

str(phy_2$per)
#silica skeleton/elongate psilate
phy21$final_morphotype

phy_2_<-phy_2

phy_2_$final_morphotype<- gsub("Acicular","non_diag", phy_2_$final_morphotype)
phy_2_$final_morphotype<- gsub("Elongate echinate","non_diag", phy_2_$final_morphotype)
phy_2_$final_morphotype<- gsub("Elongate sinuous","non_diag", phy_2_$final_morphotype)
phy_2_$final_morphotype<- gsub("Elongate sinuate","non_diag", phy_2_$final_morphotype)
phy_2_$final_morphotype<- gsub("Tracheids","non_diag", phy_2_$final_morphotype)
phy_2_$final_morphotype<- gsub("Unidentified","non_diag", phy_2_$final_morphotype)
phy_2_$final_morphotype<- gsub("non_diag ","non_diag", phy_2_$final_morphotype)
#phy_2_$final_morphotype<- gsub("Elongate","non_diag", phy_2_$final_morphotype)

phy_mor<-phy_2_%>%
        group_by(Identifier,final_morphotype)%>%
        mutate(tot=sum(per))%>%
        select(Identifier,final_morphotype,tot)%>%
        distinct()%>%
        spread(final_morphotype,tot)%>%
        select(-'Elongate ')%>%
        left_join(agedepth2)


phy_mor[is.na(phy_mor)] <- 0

df <- data.frame(matrix(ncol = 13, nrow = 0))
#x <- c("name", "age", "gender")
x<-colnames(phy_mor)

x<-x[-1]
#x<-colnames(phy_mor)



#Create vector to group some of them

x2<-c("Saddle ","Bilobate ","Trapezoid oblong"," bulliform","Cuneiform bulliform","Elongate entire cylindrical ", "Elongate psilate","Elongate blocky","Elongate rugose", "Elongate facetated","Globular granulate","Blocky faceted ","non_diag")
svar<-x2

Zones=c(8800,28100)

BAR2_merge2 <- Stratiplot(Identifier~ . , data= chooseTaxa(phy_mor, max.abun = 2, n.occ = 1),
                          type = c("h","g"), sort = "var",svar=x2,xlab="Relative percentage (%)",ylab="Age (cal yr BP)")


BAR2_merge3 <- Stratiplot(median ~ `Saddle ` + `Bilobate `+ `Trapezoid oblong`+` bulliform`+ `Cuneiform bulliform`+`Elongate entire cylindrical `+`Elongate psilate`+`Elongate blocky`+`Elongate rugose`+`Elongate facetated`+`Globular granulate`+`Blocky faceted `+`non_diag`, data= chooseTaxa(phy_mor, max.abun = 2, n.occ = 1),
                          type = c("h","g"), sort = "var",xlab="Relative percentage (%)",ylab="Age (cal yr BP)",zones=Zones,drawLegend=TRUE)


phy21<-phy_2
#gssc<-c("Saddle","Bilobate","Trapezoid oblong")
#poaceae<-c("bulliform","Cuneiform bulliform")
#non_diag<-c("Acicular","Elongate echinate", "Elongate sinuous", "Elongate sinuate","Tracheids")
#grasses_sedges<-c("cylindroid", "Elongate entire", "Elongate psilate", "Elongate facetated")
#woody<-c("Globular granulate", "3D")

#globular granulate = globular decorated

#elongate blocky = elongate psilate = elongate rugose = elongate entire

####Transform to ICPN 2.0
#https://academic.oup.com/aob/article/124/2/189/5537002


phy_gssc<-filter(phy_2, final_morphotype == 'Saddle '|final_morphotype == 'Bilobate '|final_morphotype == 'Trapezoid oblong' )


phy_globular<-filter(phy_2, final_morphotype == 'Globular granulate')



str(phy_2)

phy21$final_morphotype<- gsub("Saddle","gssc", phy21$final_morphotype)
phy21$final_morphotype<- gsub("Bilobate","gssc", phy21$final_morphotype)
phy21$final_morphotype<- gsub("Trapezoid oblong","gssc", phy21$final_morphotype)



phy21$final_morphotype<- gsub("bulliform","poaceae", phy21$final_morphotype)
phy21$final_morphotype<- gsub("Cuneiform bulliform","poaceae", phy21$final_morphotype)
phy21$final_morphotype<- gsub("Cuneiform poaceae","poaceae", phy21$final_morphotype)


phy21$final_morphotype<- gsub("Acicular","non_diag", phy21$final_morphotype)
phy21$final_morphotype<- gsub("Elongate echinate","non_diag", phy21$final_morphotype)
phy21$final_morphotype<- gsub("Elongate sinuous","non_diag", phy21$final_morphotype)
phy21$final_morphotype<- gsub("Elongate sinuate","non_diag", phy21$final_morphotype)
phy21$final_morphotype<- gsub("Tracheids","non_diag", phy21$final_morphotype)
phy21$final_morphotype<- gsub("Unidentified","non_diag", phy21$final_morphotype)


phy21$final_morphotype<- gsub("Elongate entire cylindrical","grasses_sedges", phy21$final_morphotype)
phy21$final_morphotype<- gsub("Elongate entire","grasses_sedges", phy21$final_morphotype)
phy21$final_morphotype<- gsub("Elongate psilate","grasses_sedges", phy21$final_morphotype)

phy21$final_morphotype<- gsub("Elongate blocky","grasses_sedges", phy21$final_morphotype)
phy21$final_morphotype<- gsub("Elongate rugose","grasses_sedges", phy21$final_morphotype)

phy21$final_morphotype<- gsub("Globular granulate","woody", phy21$final_morphotype)#Palms
phy21$final_morphotype<- gsub("Elongate facetated","grasses_sedges", phy21$final_morphotype)#grasses_sedges? or woody
phy21$final_morphotype<- gsub("Blocky faceted","woody", phy21$final_morphotype)#or grasses_sedges

phy21$final_morphotype<- gsub("Elongate","non_diag", phy21$final_morphotype)
#phy21$final_morphotype<- gsub("Elongate","grasses_sedges", phy21$final_morphotype)

phy21$final_morphotype<- gsub("grasses_sedges ","grasses_sedges", phy21$final_morphotype)
phy21$final_morphotype<- gsub("non_diag ","non_diag", phy21$final_morphotype)
phy21$final_morphotype<- gsub("gssc ","gssc", phy21$final_morphotype)
phy21$final_morphotype<- gsub("poaceae ","poaceae", phy21$final_morphotype)
phy21$final_morphotype<- gsub(" poaceae","poaceae", phy21$final_morphotype)

phy21$final_morphotype<- gsub("woody ","woody", phy21$final_morphotype)

phy_2$final_morphotype<-as.factor(phy_2$final_morphotype)

#Graph by morphotype
phy21$final_morphotype <- as.factor(phy21$final_morphotype) 
phy211<-phy21%>%
        select(Identifier,Counts,final_morphotype,Corrected_counts, Total_sediment_analysed_g, Number_of_transects)%>%
        group_by(Identifier,final_morphotype)%>%
        mutate(countT= sum(Corrected_counts))%>%
        select(Identifier,Counts,final_morphotype,countT,Total_sediment_analysed_g,Number_of_transects)
        
phy211_a<-phy21%>%
        select(Identifier,Counts,final_morphotype,Corrected_counts, Total_sediment_analysed_g, Number_of_transects)%>%
        filter(final_morphotype!="non_diag")%>%
        group_by(Identifier)%>%
        mutate(T_counts_depth= sum(Counts))%>%
        ungroup()%>%
        group_by(final_morphotype)%>%
        mutate(perTfinal= (Counts/T_counts_depth)*100)%>%
        select(Identifier,Counts,final_morphotype,T_counts_depth,Total_sediment_analysed_g,Number_of_transects,perTfinal)%>%
        unique()

phy211_b<-phy21%>%
        select(Identifier,Counts,final_morphotype,Corrected_counts, Total_sediment_analysed_g, Number_of_transects)%>%
        group_by(final_morphotype)%>%
        group_by(Identifier)%>%
        mutate(T_counts_depth= sum(Counts))%>%
        unique()%>%
        ungroup()%>%
        group_by(final_morphotype)%>%
        mutate(perTfinal= (Counts/T_counts_depth)*100)%>%
        select(Identifier,final_morphotype,perTfinal)%>%
        unique()%>%
        group_by(final_morphotype)


gssc<-phy211_a%>%
        filter(final_morphotype=="gssc"|final_morphotype=="poaceae")


cluster_phy<-phy211_a%>%
        filter(Identifier!=102)%>%
        filter(Identifier!=75)%>%
        select(Identifier,final_morphotype,perTfinal)%>%
        group_by(Identifier,final_morphotype)%>%
        mutate(total=sum(perTfinal))%>%
        select(Identifier,final_morphotype,total)%>%
        unique()
        

        #spread(final_morphotype,perTfinal)%>%
        #left_join(agedepth2)%>%
        #select(Identifier,grasses_sedges,gssc,non_diag,poaceae,woody,median)
       
#cluster_group<-cluster_phy%>%
 #       group_by(Identifier,final_morphotype)%>%
  #      mutate(new=sum(perTfinal))%>%
   #     select(Identifier,final_morphotype,new)%>%
    #    unique()


cluster_group<- cluster_phy%>%
        spread(final_morphotype,total)%>%
        ungroup()

#cluster_phy[is.na(cluster_phy)] <- 0


#cluster_phy2<-group_by(cluster_phy,Identifier)

#cluster_phy2<- aggregate(x=cluster_phy[,2:6], by=list(Identifier=cluster_phy$Identifier), median,na.rm = TRUE)

#cluster_phy2<- aggregate()
#cluster_phy2<- gsub(cluster_phy2,"NaN","0")
cluster_phy2<-cluster_group

cluster_phy2[is.na(cluster_phy2)] = 0

cluster_phy2<-left_join(cluster_phy2,agedepth2)
### Counts

cluster_phy3<-select(cluster_phy2,grasses_sedges,gssc,woody,poaceae)#remove non_diag
#cluster_phy33<-filter(cluster_phy3,Identifier!=75)
cluster_phy3<-as.data.frame(cluster_phy3)

dissim_phy<- vegdist(cluster_phy3, method="bray")
#dissimilarity matrix computation

clust_phy <- chclust(dissim_phy, method="coniss")

#cluster33<-cluster_phy3
#cluster_phy33<-select(cluster_phy3,-Identifier)



# Ball and Stick
groups1<- bstick(clust_phy,ng=10) #determine the appropriate number of significant clusters
ngroups1<- groups1$nGroups[which(groups1$dispersion <= groups1$bstick)]
ngroups1<- ngroups1[2]
cc1<- cutree(clust_phy, k = ngroups1)

# STRAT plot
par(oma=c(2,1,1,1.2))
strat.plot(cluster_phy3, yvar=cluster_phy2$Identifier, clust=clust_phy, y.rev=TRUE, cex.axis=0.6, cex.yaxis=0.8, cex.ylabel=0.8, cex.lab=0.6, ylab="cal BP (yr)", col.line="black", col.bar="black", las=3, mgp=c(3,1,0))
# check that strat plot is ok
#?par
#diat_merge100<-mutate(diat_merge10,age_ka=median/1000)

x<-strat.plot(cluster_phy3, yvar=cluster_phy4$median, clust=clust_phy, y.rev=TRUE, cex.axis=0.8, cex.yaxis=0.8, cex.ylabel=0.8, cex.lab=0.8, ylab="ka cal BP (yr)",col.line="black", col.bar="black", las=3, mgp=c(3,1,0))

#?strat.plot


z<-as.matrix(1:ngroups1)

addClustZone(x, clust_phy, nZone=ngroups1, col=rainbow(length(z))[rank(z)])
c1 <- cutree(clust_phy, k=ngroups1)




#x<-strat.plot(cluster_phy3, yvar=cluster_phy4$median, clust=clust_phy, y.rev=TRUE, cex.axis=0.8, cex.yaxis=0.8, cex.ylabel=0.8, cex.lab=0.8, ylab="ka cal BP (yr)",col.line="black", col.bar="black", las=3, mgp=c(3,1,0))

#?strat.plot

cluster_phy4<-cluster_phy3

cluster_phy4$median <- cluster_phy2$median
cluster_phy4$clust <- c1

#addClustZone(x, clust_phy, nZone=ngroups1, col=rainbow(length(z))[rank(z)])
#c1 <- cutree(clust_phy, k=ngroups1)

cluster_phy4$age <- ages_final_ITRAX2$median
cluster_phy3$depth <- diat_depth$Depth




#levels(phy211$final_morphotype)

phy_222<-left_join(phy211,agedepth2)%>%
        filter(final_morphotype!="non_diag")
phy_223<-ggplot(phy_222, aes(x=phy_222$median,y=countT, color=final_morphotype))+geom_point()+ scale_color_hue(h = c(80, 1000))

phy_223

phy_non1<-left_join(phy211,agedepth2)

phy_non<-ggplot(phy_non1, aes(x=phy_non1$median,y=countT, color=final_morphotype))+geom_point()+ scale_color_hue(h = c(80, 1000))

ggplotly(phy_non)



ggplotly(phy_223)


phy_per<-phy21%>%
        #select(Identifier,Type,Genus,Species2,Counts,Total_sediment_analysed_g,Number_of_transects)%>%
        group_by(Identifier)%>%
        mutate(Corrected_counts=Counts/Total_sediment_analysed_g)%>%
        mutate(countT= sum(Corrected_counts),countT2=sum(Counts)) %>%
        mutate(per=as.numeric(paste0(round(100*Corrected_counts/countT,2))))%>%
        select(Identifier,final_morphotype,per)%>%
        group_by(Identifier,final_morphotype)%>%
        mutate(perT= sum(per))%>%
        select(Identifier,final_morphotype,perT)%>%
        unique()
        
        
        #select(Identifier,per,final_morphotype)



#phy_per$per<-as.numeric(as.character(phy_per$per))
       
#phy_per<-phy_per%>%
#        group_by(Identifier,final_morphotype)%>%
 #       mutate(perT= sum(per))


phy_2222<-left_join(phy_per,agedepth2)%>%
        filter(final_morphotype!="non_diag")
phy_2233<-ggplot(phy_2222, aes(x=phy_2222$median,y=perT, color=final_morphotype))+geom_point()+ scale_color_hue(h = c(80, 1000))

phy_2233

phy_224<-left_join(phy_per,agedepth2)%>%
        drop_na()

phy_24<-ggplot(phy_224, aes(x=median,y=perT, fill=final_morphotype))+geom_bar(stat = "identity")+ scale_color_hue(h = c(80, 1000))

phy_24


graph_phy<- phy_224%>%
        spread(final_morphotype, perT) 

graph_phy[is.na(graph_phy)] <- 0

theme_new <- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), # remove grids
                   panel.background = element_blank(), axis.line = element_line(colour = "black"),
                   strip.text.x = element_text(size=10, angle=90, vjust=0), # Taxa names
                   strip.background = element_blank(),
                   strip.text.y = element_text(angle = 0),
                   legend.position="none",panel.border = element_blank(),
                   axis.text.x=element_text(angle=45,hjust=1)) # Axis tick label angle


phy_g1<-ggplot(phy_224)+
        geom_line(aes(median,perT))+
        geom_area(aes(median,perT))+
        scale_x_reverse(breaks=seq(0,33000,2000))+
        scale_y_continuous()+
        xlab("Age (cal. BP)")+ylab("%")+
        coord_flip()+
        theme_new+
        facet_grid(~phy_224$final_morphotype,scales = "free", space = "free")

phy_g1

ggplotly(phy_g1)

phy_2$final_morphotype<-as.factor(phy_2$final_morphotype)


phy_2age<-left_join(phy_2,agedepth2)%>%
        filter(final_morphotype!="Elongate blocky")%>%
        filter(final_morphotype!="Trapezoid oblong")%>%
        filter(final_morphotype!="Saddle ")%>%
        filter(final_morphotype!=" bulliform")%>%
        filter(final_morphotype!="Elongate sinuous")%>%
        filter(final_morphotype!="Unidentified ")%>%
        filter(final_morphotype!="Elongate entire cylindrical ")
                       

phy_g2<-ggplot(phy_2age)+
        geom_line(aes(median,per))+
        geom_area(aes(median,per))+
        scale_x_reverse()+
        scale_y_continuous()+
        xlab("Age (cal. BP)")+ylab("%")+
        coord_flip()+
        theme_new+
        facet_grid(~phy_2age$final_morphotype,scales = "free", space = "free")


phy_g2

ggplotly(phy_g1)
str(phy_per$final_morphotype)

#str(phy_222)
phy_222$final_morphotype<-as.factor(phy_222$final_morphotype)


#Concentration graph

phy_conc<-left_join(phy21,agedepth2)%>%
        filter(Identifier!=64)

phy_444<-ggplot(phy_conc, aes(y=median,(x=sqrt(phy_g_wet_sed))))+geom_point() +  
        geom_path()+
        scale_y_reverse(breaks = seq(0, 32500, by = 2000))  + ggtitle("")+xlab("Sqrt of Concentration \n(diat/g)")+ylab("Age (cal yr BP)")+theme_bw() + theme(axis.text.x=element_text(size=12),axis.title.x=element_text(size=12,face="bold"),axis.title.y = element_text(size=12,face="bold"))

ggplotly(phy_444)

phy_444


#write.table(together, file = "C:/Users/Maria Jose Rivera/OneDrive - James Cook University/Australia renamed/Sanamere/Thesis sections/PhD/experiments/exp_grain/data/together.csv",
#sep = ",", col.names = NA,
#            qmethod = "double")

tog2<-read.csv("C:/Users/Maria Jose Rivera/OneDrive - James Cook University/Australia renamed/Sanamere/Thesis sections/PhD/experiments/exp_grain/data/together.csv",header = TRUE, sep = ",", row.names = 1)
#write.table(Hypy.selected5, file = "C:/Users/Maria Jose Rivera/OneDrive - James Cook University/Australia renamed/Sanamere/Thesis sections/PhD/experiments/exp_Hypy/data/Hypy.selected5.csv",
#            sep = ",", col.names = NA,
#           qmethod = "double")

hypy_phy<-read.csv("C:/Users/Maria Jose Rivera/OneDrive - James Cook University/Australia renamed/Sanamere/Thesis sections/PhD/experiments/exp_Hypy/data/Hypy.selected5.csv",header = TRUE, sep = ",", row.names = 1)

tog22<-tog2%>%
        select(Identifier,averaged.C.N,averaged.C,median)

tog3<-hypy_phy%>%
        select(Identifier,PyCxMAR, Corrected,av_d13Cpost,av_d13C,median)


hypy_phy12<-phy_conc%>%
        select(Identifier,median,phy_g_wet_sed)%>%
     full_join(tog3)%>%
        full_join(tog22)%>%
        unique()%>%
        arrange()


plot_conc1 <- hypy_phy12%>%
        select(median, phy_g_wet_sed)%>% 
        filter(Identifier!=132)%>%
        na.omit()%>% 
        ggplot(aes(x = sqrt(phy_g_wet_sed) , y = median)) +
        geom_point(aes(x = sqrt(phy_g_wet_sed) , y = median), size = 1, alpha = 0.75) +
        geom_path(aes(x = sqrt(phy_g_wet_sed) , y = median), size = 1, alpha = 0.75)+
        ylab("Age cal yr BP") +
        xlab("Phytolith concentration")+
        theme_minimal()+
        scale_y_reverse(limits=c(33000,0),breaks = seq(0, 33000, by = 2000))+
        theme(axis.title.x=element_text(size=12))
#              axis.title.y = element_blank(),
 #             axis.text.y = element_blank())
plot_conc1

plot_conc2 <- hypy_phy12%>%
        select(median, PyCxMAR)%>% 
        na.omit()%>%
        arrange(median)%>%
        ggplot(aes(x = PyCxMAR , y = median)) +
        geom_point(aes(x = PyCxMAR , y = median), size = 1, alpha = 0.75) +
        geom_path(aes(x = PyCxMAR, y = median), size = 1, alpha = 0.75)+
        ylab("Age cal yr BP")  +
        xlab("PyC MAR (ug mm^-2^/yr)")+
        theme_minimal()+
        scale_y_reverse(limits=c(33000,0),breaks = seq(0, 33000, by = 2000))+
        theme(axis.title.x=element_text(size=14))
          #    axis.title.y = element_blank(),
           #   axis.text.y = element_blank())
plot_conc2

plot_conc3 <- hypy_phy12%>%
        select(median, averaged.C.N)%>% 
        na.omit() %>%
        arrange(median)%>%
        ggplot(aes(x = averaged.C.N , y = median)) +
        geom_point(aes(x = averaged.C.N , y = median), size = 1, alpha = 0.75) +
        geom_path(aes(x = averaged.C.N, y = median), size = 1, alpha = 0.75)+
        ylab("") +
        xlab("C:N ratio")+
        theme_minimal()+
        scale_y_reverse(limits=c(33000,0),breaks = seq(0, 33000, by = 2000))+
        theme(axis.title.x=element_text(size=14),
              axis.title.y = element_blank(),
              axis.text.y = element_blank())
plot_conc3

plot_conc4 <- hypy_phy12%>%
        select(median, averaged.C)%>% 
        na.omit() %>%
        arrange(median)%>%
        ggplot(aes(x = averaged.C , y = median)) +
        geom_point(aes(x = averaged.C , y = median), size = 1, alpha = 0.75) +
        geom_path(aes(x = averaged.C , y = median), size = 1, alpha = 0.75)+
        ylab("") +
        xlab("% C")+
        theme_minimal()+
        scale_y_reverse(limits=c(33000,0),breaks = seq(0, 33000, by = 2000))+
        theme(axis.title.x=element_text(size=14),
              axis.title.y = element_blank(),
              axis.text.y = element_blank())

plot_conc7 <- hypy_phy12%>%
        select(median, av_d13C)%>% 
        na.omit() %>%
        arrange(median)%>%
        ggplot(aes(x = av_d13C , y = median)) +
        geom_point(aes(x = av_d13C , y = median), size = 1, alpha = 0.75) +
        geom_path(aes(x = av_d13C , y = median), size = 1, alpha = 0.75)+
        ylab("") +
        xlab(expression(paste(delta^{13}, C[VPDB],"(\u2030)")))+ 
        theme_minimal()+
        scale_y_reverse(limits=c(33000,0),breaks = seq(0, 33000, by = 2000))+
        theme(axis.title.x=element_text(size=14),
              axis.title.y = element_blank(),
              axis.text.y = element_blank())

plot_conc7

plot_conc8 <- hypy_phy12%>%
        select(median, av_d13Cpost)%>% 
        na.omit() %>%
        arrange(median)%>%
        ggplot(aes(x = av_d13Cpost , y = median)) +
        geom_point(aes(x = av_d13Cpost , y = median), size = 1, alpha = 0.75) +
        geom_path(aes(x = av_d13Cpost , y = median), size = 1, alpha = 0.75)+
        ylab("") +
        xlab(expression(paste(delta^{13}, PyC[VPDB],"(\u2030)"))) +
        theme_minimal()+
        scale_y_reverse(limits=c(33000,0),breaks = seq(0, 33000, by = 2000))+
        theme(axis.title.x=element_text(size=16),
              axis.title.y = element_blank(),
              axis.text.y = element_blank())

plot_conc8
grid.arrange(plot_conc1,plot_conc2,plot_conc3,plot_conc4,nrow=1,ncol=4)
grid.arrange(plot_conc1,plot_conc2,plot_conc3,plot_conc4,plot_conc7,plot_conc8,nrow=1,ncol=6)


fig3<-grid.arrange(plot_conc2,plot_conc3,plot_conc4,plot_conc7,plot_conc8,nrow=1,ncol=5)

plot_conc5 <- hypy_phy12%>%
        select(Corrected, averaged.C)%>% 
        na.omit() %>%
        
        ggplot(aes(x = averaged.C , y = Corrected)) +
        geom_point(aes(x = averaged.C , y = Corrected), size = 1, alpha = 0.75) +
        #geom_path(aes(x = averaged.C , y = Corrected), size = 1, alpha = 0.75)+
        ylab("PyC abundance (%)") +
        xlab( "% TOC")+
        theme_minimal()+
        #scale_y_reverse(limits=c(33000,0),breaks = seq(0, 33000, by = 2000))+
        theme(axis.title=element_text(size=16),axis.text.x=element_text(size=12))
             # axis.title.y = element_blank(),
              #axis.text.y = element_blank())

plot_conc5

plot_conc6 <- hypy_phy12%>%
        select(av_d13C,av_d13Cpost)%>% 
        filter(av_d13C<(-20))%>%
        na.omit() %>%
        ggplot(aes(x = av_d13C , y = av_d13Cpost)) +
        geom_point(aes(x = av_d13C , y = av_d13Cpost), size = 1, alpha = 0.75) +
        #geom_path(aes(x = averaged.C , y = median), size = 1, alpha = 0.75)+
        ylab(expression(paste(delta^{13}, PyC[VPDB],"(\u2030)"))) +
        xlab(expression(paste(delta^{13}, C[VPDB],"(\u2030)"))) +
        theme_minimal()+
        #scale_y_reverse(limits=c(33000,0),breaks = seq(0, 33000, by = 2000))+
        theme(axis.title=element_text(size=16),axis.text.x=element_text(size=12))
         #     axis.title.y = element_blank(),
          #    axis.text.y = element_blank())
plot_conc6


fig4<-grid.arrange(plot_conc5, plot_conc6,nrow=1,ncol=2)



lm_toc<-lm(hypy_phy12$av_d13Cpost ~ hypy_phy12$av_d13C)

summary(lm_toc)

hypy_trial<-hypy_phy12%>%
        select(median,PyCxMAR,phy_g_wet_sed)%>%
        filter(median>10000)%>%
        na.omit()

lm_toc2<-lm(hypy_trial$phy_g_wet_sed ~ hypy_trial$PyCxMAR)

summary(lm_toc2)

phy_445<-ggplot(phy_222, aes(y=Depth,(x=sqrt(phy_g_wet_sed))))+geom_point() +  scale_y_reverse(breaks = seq(0, 175, by = 10))  + ggtitle("")+xlab("Sqrt of Concentration (diat/g)")+ylab("Age (cal yr BP)")+theme_bw() + theme(axis.text.x=element_text(size=12),axis.title.x=element_text(size=12,face="bold"),axis.title.y = element_text(size=12,face="bold"))

phy_445


hypy_phy<-merge(phy_conc,Hypy.selected5)%>%
        select(phy_g_wet_sed,PyCxMAR,Depth, median)%>%
        drop_na()%>%
        unique()


hypy_phy_22<-full_join(Hypy.selected5,phy_conc)%>%
        select(phy_g_wet_sed,PyCxMAR,Depth, median,Identifier)%>%
        filter(Identifier!=97)%>%
        filter(Identifier!=132)
        #drop_na()%>%
       # unique()

#?left_join

plot_hypy_median_b <- hypy_phy_22 %>%
        select(median, PyCxMAR) %>%
        na.omit() %>%
        ggplot() +
        geom_point(aes(x = PyCxMAR, y = median), size = 1, alpha = 0.75) +
        geom_path(aes(x = PyCxMAR, y = median), size = 1, alpha = 0.75)+
        xlab("PyC MAR (ug mm-2/yr)") +
        theme_minimal() +
        scale_y_reverse(breaks = seq(0, 33000, by = 1000))+
        theme(axis.title.x=element_text(size=12))
#      axis.title.y = element_blank(),
#     axis.text.y = element_blank())

plot_hypy_median_b

ggplotly(plot_hypy_median_b)

hypy_phy_223<-full_join(phy_conc,Hypy.selected5)%>%
        select(phy_g_wet_sed,PyCxMAR,Depth, median,Identifier)%>%
        filter(Identifier!=97)%>%
        filter(Identifier!=132)

plot_hypy_median_c <- hypy_phy_223 %>%
        select(median, phy_g_wet_sed)%>% 
        na.omit()%>%
        ggplot() +
        geom_point(aes(x = sqrt(phy_g_wet_sed), y = median), size = 1, alpha = 0.75) +
        geom_path(aes(x = sqrt(phy_g_wet_sed), y = median), size = 1, alpha = 0.75)+
        xlab("phy conc") +
        theme_minimal() +
        scale_y_reverse(breaks = seq(0, 33000, by = 1000))+
        theme(axis.title.x=element_text(size=12),
         axis.title.y = element_blank(),
     axis.text.y = element_blank())



plot_hypy_median_c

grid.arrange(plot_hypy_median_b,plot_hypy_median_c,nrow=1,ncol=2)

#grid.arrange(plot_hypy_median_b,phy_g2,nrow=1,ncol=2)

h<-cor(hypy_phy)

library(corrplot)


#corrplot(h,method = "number")

#hypy_phy2<-ggplot(hypy_phy, aes(y=PyCxMAR,(x=phy_g_wet_sed)))+geom_line()  + ggtitle("")+xlab("Sqrt of Concentration (diat/g)")+ylab("Age (cal yr BP)")+theme_bw() + theme(axis.text.x=element_text(size=12),axis.title.x=element_text(size=12,face="bold"),axis.title.y = element_text(size=12,face="bold"))

#hypy_phy2

######Phytoliths just globular granulate
phy_4<-diat_merge%>%
        #select(Identifier,Type,Genus,Species2,Counts,Total_sediment_analysed_g,Number_of_transects)%>%
        filter(final_morphotype=='Globular granulate')%>% 
        group_by(Identifier)%>%
        mutate(Corrected_counts=Counts/Total_sediment_analysed_g)%>%
        mutate(countT= sum(Corrected_counts),countT2=sum(Counts)) %>%
        mutate(per=paste0(round(100*Corrected_counts/countT,2)))%>%
        mutate(glob_g_wet_sed = countT2/Total_sediment_analysed_g)%>%
        mutate(corr_trans= countT2/Number_of_transects)%>%
        filter(countT2 > 0)

phy_44<-left_join(phy_4,agedepth2)
phy_443<-ggplot(phy_44, aes(x=phy_44$median,y=sqrt(glob_g_wet_sed)))+geom_point()

ggplotly(phy_443)

phy_333 <- ggplot(phy_222, aes(x=phy_222$median,y=spi_g_wet_sed))+geom_point() 
phy_333





# Modern reference

phy_modern<-read.csv(here("experiments", "exp_phy","data","Phytolith_modern.csv"))

phy_modern$Distance_from_lake <- gsub("SAN", "", phy_modern$Distance_from_lake)
phy_modern$Distance_from_lake <- gsub("51", "50", phy_modern$Distance_from_lake)
phy_modern$Distance_from_lake <- as.numeric(as.character(phy_modern$Distance_from_lake))   

phy_modern$Sci_name <- gsub("pandanus", "Pandanus tectorius", phy_modern$Sci_name)


#table1
table1<-phy_modern%>%
        select(Distance_from_lake,Common_name,Sci_name,Family,Dominance)%>%
        arrange(Distance_from_lake)%>%
        filter(Common_name!="Unknown")
#        gsub("SAN","", Distance_from_lake)

#table1$Distance_from_lake <- gsub("SAN", "", table1$Distance_from_lake)

#table1$Distance_from_lake <- as.numeric(as.character(table1$Distance_from_lake))

#maybe include plant part sampled and type (shrub, tree)


#table2
table2<-phy_modern%>%
        filter(Phy_Presence==1)%>%
        select(Distance_from_lake,Common_name,Sci_name,Family)%>%
        arrange(Distance_from_lake)

#table2$Distance_from_lake <- gsub("SAN", "", table2$Distance_from_lake)

#table2$Distance_from_lake <- as.numeric(as.character(table2$Distance_from_lake))        


#table2$Common_name <- gsub("", "Pandanus", table2$Common_name)
#table2<-table2%>%
 #       mutate(`Common_name`==ifelse(Sci_name=="Pandanus tectorius","Pandanus",`Common_name`))

Morphology<-c("Elongate sinuate","Elongated","Globular echinate","Elongate sinuate","Silica skeleton","Stomata")

table22<-fct_recode(table2$Common_name,"Pandanus"="")

table222<-data.frame(table2,table22)

table2222<-select(table222,-Common_name)


table2_a<-data.frame(table2,Morphology)



phy_modern2<-phy_modern%>%
        filter(Sci_name!="")


# 20 different species
length(unique(phy_modern2$Sci_name))

#no scientific name: 2 poaceae, Melaleuca, Eucalyptus y Acacia; orange creeper?
phy_modern3<-phy_modern%>%
        filter(Sci_name=="")


phy_modern5<-phy_modern%>%
        filter(Phy_Presence=="1"|Phy_Presence=="0"|Phy_Presence=="X")

#length(unique(phy_modern4))
#https://davekimble.net/rainforest/pandanus.htm
#with phytoliths/ 2 poaceae, pandanus, palm
print(unique(phy_modern4$Family))

#table include Commom_name, Family, Sci_name, Phy_abundance, Modern_reference,Phy_presence
#add morphotype from pics same morphotypes in thesis_sci


cluster_stacked<-cluster_phy4%>%
        gather(Group,Percentage,-median,-clust)

ggplot(cluster_stacked, aes(x=median, y=Percentage, fill=Group)) + 
        geom_area()

      