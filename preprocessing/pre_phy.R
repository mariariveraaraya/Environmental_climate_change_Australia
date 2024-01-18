# Phytolith analysis

## ---- load-pkg-veg  
library(here)
library(tidyverse)
library(plotly)
library(rioja)
library(vegan)
library(analogue)
library(corrplot)

## ---- phy-pre


source('C:/Users/Maria Jose Rivera/OneDrive - James Cook University/Australia renamed/Sanamere/Thesis sections/PhD/preprocessing/pre_age_model.R')

morphotype<-c("Globular echinate","Elongate echinate", "Elongate sinuous", "Globular psilate", "Hair", "Tracheids", "Blocky faceted", "Blocky polyhedron","Globular decorated","Parallelepiped blocky","Cyperaceae","Elongate psilate","Elongate facetated")

plant_group<-c("Woody dicotyledons (Arecaceae?)","Non_diagnostic","Non_diagnostic","Non_diagnostic", "Non_diagnostic","Non_diagnostic","Woody Dicotyledons", "Woody Dicotyledons","Woody Dicotyledons","Woody Dicotyledons","Woody Dicotyledons","Grasses and sedges","Grasses and sedges")

df<-data.frame(morphotype,plant_group)


diatoms_counts<-read.csv(here("experiments", "exp_diatoms","data", "Counts_diatoms_08_08_19.csv"))

diatoms_photo<-read.csv(here("experiments", "exp_diatoms","data", "Photos_silica.csv"))

###Concentration of diatoms
conc<-read.csv("C:/Users/Maria Jose Rivera/OneDrive - James Cook University/Australia renamed/Sanamere/Diatoms spicules/Concentration diatoms.csv")
diatoms_counts$Species2 <- paste(diatoms_counts$Genus,diatoms_counts$Species)
diatoms_counts$final_morphotype <- paste(diatoms_counts$Morphotype,diatoms_counts$Morphotype_b)


#Merging counts and concentration
diat_merge<-merge(diatoms_counts,conc,by="Identifier")



morphotype<-c("Globular echinate","Elongate echinate", "Elongate sinuous", "Globular psilate", "Hair", "Tracheids", "Blocky faceted", "Blocky polyhedron","Globular decorated","Parallelepiped blocky","Cyperaceae","Elongate psilate","Elongate facetated")

plant_group<-c("Woody dicotyledons (Arecaceae?)","Non_diagnostic","Non_diagnostic","Non_diagnostic", "Non_diagnostic","Non_diagnostic","Woody Dicotyledons", "Woody Dicotyledons","Woody Dicotyledons","Woody Dicotyledons","Woody Dicotyledons","Grasses and sedges","Grasses and sedges")

df<-data.frame(morphotype,plant_group)



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
tri<-phy_2%>%
        mutate(Genus=ifelse(Type=='Diatom',Genus,fct_recode("hola")))


test <- ifelse(phy_2$Type %in% Phytoliths, 1, temp$Feature1)
test
dat = phy_2 %>% mutate(Genus = factor(ifelse(Type == "Phytoliths" & Species2 == "Monocot ", "hola", "no")))


#tri$Genus[which(tri$Type=="Phytoliths")] <- 'hola'
phy_factor<-phy_2

phy_factor$final_morphotype<-as.factor(phy_factor$final_morphotype)

#x_2<-phy_factor$final_morphotype

x_3<-fct_collapse(phy_factor$final_morphotype, woody = c("Globular granulate", "Trapezoid oblong"),
                  poaceae = c("3D ", "Bilobate "), NULL = "999")


#x_4<-phy_factor%>%
#        fct_collapse(final_morphotype, woody = c("Globular granulate", "Trapezoid oblong"),
#                     poaceae = c("3D ", "Bilobate "), NULL = "H")

phy_5<-data.frame(phy_factor,x_3)

#fun <- function(z) {
#z[z == "Y"] <- "Yes"
#z[z == "N"] <- "No"
#z[!(z %in% c("Yes", "No"))] <- NA
#z
#}
#fct_relabel(factor(x), fun)


#str(phy_factor$final_morphotype)

phy_2$final_morphotype<- gsub("3D","blocky faceted", phy_2$final_morphotype)
phy_2$final_morphotype<- gsub("blocky faceted","Blocky faceted", phy_2$final_morphotype)
phy_2$final_morphotype<- gsub("cylindroid","Elongate entire cylindrical", phy_2$final_morphotype)
phy_2$final_morphotype<- gsub("999","Unidentified", phy_2$final_morphotype)
phy_2$final_morphotype<- gsub("cylindroid ","Elongate entire cylindrical", phy_2$final_morphotype)


phy_2$per <- as.numeric(as.character(phy_2$per))

phy_2_age<-left_join(phy_2,agedepth2)

phy_all<-ggplot(phy_2_age, aes(x=median,y=per, color=final_morphotype))+geom_point()+ scale_color_hue(h = c(80, 1000))


phy_2_<-phy_2

phy_2_$final_morphotype<- gsub("Acicular","non_diag", phy_2_$final_morphotype)
phy_2_$final_morphotype<- gsub("Elongate echinate","non_diag", phy_2_$final_morphotype)
phy_2_$final_morphotype<- gsub("Elongate sinuous","non_diag", phy_2_$final_morphotype)
phy_2_$final_morphotype<- gsub("Elongate sinuate","non_diag", phy_2_$final_morphotype)
phy_2_$final_morphotype<- gsub("Tracheids","non_diag", phy_2_$final_morphotype)
phy_2_$final_morphotype<- gsub("Unidentified","non_diag", phy_2_$final_morphotype)
phy_2_$final_morphotype<- gsub("non_diag ","non_diag", phy_2_$final_morphotype)
#phy_2_$final_morphotype<- gsub("Elongate","non_diag", phy_2_$final_morphotype)

phy_2_$final_morphotype<- gsub("Blocky faceted ","non_diag", phy_2_$final_morphotype)

phy_und<-phy_2_%>%
        group_by(Identifier,final_morphotype)%>%
        mutate(tot=sum(per))%>%
        select(Identifier,final_morphotype,tot)%>%
        distinct()%>%
        spread(final_morphotype,tot)%>%
        left_join(correctdepths_1)%>%
        left_join(ages_final_ITRAX2)%>%
        select(-17,-18,-20,-21)

phy_und[is.na(phy_und)] <- 0


phy_mor10<-phy_und%>%
        rename(`Non diagnostic`= `non_diag`, `Bulliform`= ` bulliform`)

phy_mor101<-phy_mor10%>%
        rename(`Globular psilate`= `Globular granulate`, `Elongate entire`= `Elongate `)
zones4=c(7.3,8,18.7,20.6)


BAR2_merge10 <- Stratiplot(median/1000 ~ `Saddle ` + `Bilobate `+ `Trapezoid oblong`+`Bulliform`+ `Cuneiform bulliform`+`Elongate entire cylindrical `+`Elongate psilate`+`Elongate blocky`+`Elongate rugose`+`Elongate facetated`+`Globular granulate`+`Blocky faceted `+`Non diagnostic`+`Elongate `, data= chooseTaxa(phy_mor10, max.abun = 2, n.occ = 1),
                          type = c("h","g"), sort = "var",xlab="Relative percentage (%)",ylab="Age (cal yr BP)",zones=zones4,drawLegend=TRUE)

BAR2_merge101 <- Stratiplot(median/1000 ~ `Saddle ` + `Bilobate `+ `Trapezoid oblong`+`Bulliform`+ `Cuneiform bulliform`+`Elongate entire cylindrical `+`Elongate psilate`+`Elongate blocky`+`Elongate rugose`+`Elongate facetated`+`Globular psilate`+`Blocky faceted `+`Non diagnostic`+`Elongate entire`, data= chooseTaxa(phy_mor101, max.abun = 2, n.occ = 1),
                           type = c("h","g"), sort = "var",xlab="Relative percentage (%)",ylab="Age (cal yr BP)",drawLegend=TRUE)


phy_mor<-phy_2_%>%
        group_by(Identifier,final_morphotype)%>%
        mutate(tot=sum(per))%>%
        select(Identifier,final_morphotype,tot)%>%
        distinct()%>%
        spread(final_morphotype,tot)%>%
        select(-'Elongate ')%>%
        left_join(correctdepths_1)%>%
        left_join(ages_final_ITRAX2)%>%
        select(-15,-16,-18,-19)


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
zones2=c(20600,18700,8000,7300)
zones3=c(7300,8000,18700,20600)
zones4=c(7.3,8,18.7,20.6)

zones5=c(9.7,19.8,29)

zones5=c(9.7,18.2,29)


phy_mor2<-phy_mor%>%
        rename(`Non diagnostic`= `non_diag`, `Bulliform`= ` bulliform`)%>%
        ungroup()

phy_mor23<-phy_mor2%>%
        left_join(select(hypy_phy12,phy_g_wet_sed,Identifier))%>%
        rename("Concentration"=phy_g_wet_sed)%>%
        mutate(Concentration=sqrt(Concentration))%>%
        
        ungroup()%>%
        rename("Spheroid ornate"="Globular granulate", "Bulliform flabellate"="Cuneiform bulliform",
               "Elongate entire"= "Elongate entire cylindrical ")



phy_mor23[is.na(phy_mor23)] <- 0

phy_mor3<-phy_mor23%>%
        pivot_longer(2:13,names_to = "Morphotype", values_to = "Counts")%>%
        mutate(id2= fct_collapse(Morphotype, "Grasses/Sedges"= c("Bulliform","Saddle ", "Bilobate ","Trapezoid oblong", "Bulliform flabellate","Elongate entire","Elongate psilate","Elongate blocky","Elongate rugose","Elongate facetated"), "Arboreal"=c("Spheroid ornate"), "Non diagnostic"=c("Non diagnostic"))) 
        

ppp<-pivot_wider(phy_mor3, names_from = "id2",values_from = "Counts")%>%
        select(-Morphotype)%>%
        distinct()

ppp[is.na(ppp)] <- 0

pp2<-ppp%>%
        distinct()

phy_mor4<-phy_mor3%>%
        select(-Morphotype,-Concentration)%>%
        distinct()


phy_mor5<-phy_mor3%>%
        pivot_longer(4,names_to = "Conc",values_to = "conc2")
#write.csv(phy_mor4,"phy_mor4.csv")
png(
        "other/Phy/phy_coll.png", 
        width = 7.5, 
        height = 6, 
        res = 300,
        units = "in"
)


phy_fig<-phy_mor4 %>%
        ggplot(aes(y = median/1000, x = Counts)) +
        #geom_point() +
       # geom_path()+
       # geom_area(orientation = "y")+
        geom_segment(aes(xend = 0, yend = median/1000), lwd = 1) +
        facet_wrap(~id2, scales = "free_x", ncol=3) +
        #theme(strip.text.x = element_text(size = 14, colour = "red"))+
        scale_y_reverse(limits=c(33,0),breaks = seq(0, 33, by = 2)) +
        labs(x = "Relative abundance (%)", y = "Age (ka)")+
        theme_bw(base_size = 15)+
 theme(axis.title =element_text(),panel.grid.major = element_blank())
print(phy_fig)

dev.off()


conc<-phy_mor5%>%
        ggplot(aes(y = median/1000, x = conc2)) +
        geom_point() +
        geom_path()+
        # geom_area(orientation = "y")+
       # geom_segment(aes(xend = 0, yend = median/1000), lwd = 1) +
        facet_wrap(~Conc, scales = "free_x", ncol=1) +
      #  theme(strip.text.x = element_text(size = 14, colour = "red"))+
        scale_y_reverse(limits=c(33,0),breaks = seq(0, 33, by = 2)) +
       # scale_x_continuous(position = "top") +
        labs(x = "(phytoliths/g)", y = "Age (cal ka yr BP)")+
        theme_minimal(base_size = 30)+
        theme(axis.title.x =element_text(face="bold"),  axis.text.y = element_blank(), axis.title.y = element_blank())

conc

a<- grid.arrange(phy_fig, conc, nrow=1, ncol=2, widths = c(2.5, 1))
BAR2_merge44 <- Stratiplot(median/1000 ~ `Grasses/Sedges` + `Arboreal`+ `Non diagnostic`+ `Concentration`,data= chooseTaxa(pp2, max.abun = 2, n.occ = 1),
                         type = c("h"), varTypes = c(rep("absolute", 1)),sort = "var",xlab="Relative percentage (%)",ylab="Age (k cal yr BP)",zones=zones5,drawLegend=TRUE)



BAR2_merge3 <- Stratiplot(median/1000 ~ `Saddle ` + `Bilobate `+ `Trapezoid oblong`+`Bulliform`+ `Cuneiform bulliform`+`Elongate entire cylindrical `+`Elongate psilate`+`Elongate blocky`+`Elongate rugose`+`Elongate facetated`+`Globular granulate`+`Blocky faceted `+`Non diagnostic`, data= chooseTaxa(phy_mor2, max.abun = 2, n.occ = 1),
                          type = c("h","g"), sort = "var",xlab="Relative percentage (%)",ylab="Age (ka)",zones=zones4,drawLegend=TRUE, cex= 4)

BAR2_merge4 <- Stratiplot(median/1000 ~ `Saddle ` + `Bilobate `+ `Trapezoid oblong`+`Bulliform`+ `Cuneiform bulliform`+`Elongate entire cylindrical `+`Elongate psilate`+`Elongate blocky`+`Elongate rugose`+`Elongate facetated`+`Globular granulate`+`Blocky faceted `+`Non diagnostic`, data= chooseTaxa(phy_mor2, max.abun = 2, n.occ = 1),
                         type = c("h","g"), sort = "var",xlab="Relative percentage (%)",ylab="Age (ka)",zones=zones5,drawLegend=TRUE)

#BAR2_merge4 <- Stratiplot(median/1000 ~ `Saddle ` + `Bilobate `+ `Trapezoid oblong`+`Bulliform`+ `Cuneiform bulliform`+`Elongate entire cylindrical `+`Elongate psilate`+`Elongate blocky`+`Elongate rugose`+`Elongate facetated`+`Globular granulate`+`Blocky faceted `+`Non diagnostic`, data= chooseTaxa(phy_mor2, max.abun = 2, n.occ = 1),
 #                         type = c("h","g"),sort = "var",xlab="Relative percentage (%)",ylab="Age (cal yr BP)",zones=zones4,drawLegend=TRUE,scales=list(x=list(cex=1.2), y=list(cex=1.5)))


write.csv(phy_mor23,"phy_mor23.csv")

phy_mor23<-read.csv("phy_mor23.csv")

phy_mor233<-phy_mor23%>%
        clean_names()

png(
        "other/Phy/phy_sep_202.png", 
        width = 7.5, 
        height = 5, 
        res = 300,
        units = "in"
)


#Stratiplot(median/1000 ~ `Saddle ` + `Bilobate `+ `Trapezoid oblong`+`Bulliform`+ `Cuneiform bulliform`+`Elongate entire cylindrical `+`Elongate psilate`+`Elongate blocky`+`Elongate rugose`+`Elongate facetated`+`Globular granulate`+`Blocky faceted `+`Non diagnostic`+`Concentration`, data= chooseTaxa(phy_mor23, max.abun = 2, n.occ = 1),
 #                 type = c("h"),varTypes = c(rep("absolute", 1)), sort = "var",xlab="Relative percentage (%)",ylab="Age (ka)",col="black",drawLegend=TRUE, zones=zones5)
Stratiplot(median/1000 ~ `Saddle ` + `Bilobate `+ `Trapezoid oblong`+`Bulliform`+ `Bulliform flabellate`+`Elongate entire`+`Elongate psilate`+`Elongate blocky`+`Elongate rugose`+`Elongate facetated`+`Spheroid ornate`+`Non diagnostic`+`Concentration`, data= chooseTaxa(phy_mor23, max.abun = 2, n.occ = 1),
           type = c("h"),varTypes = c(rep("absolute", 1)), sort = "var",xlab="Relative percentage (%)",ylab="Age (ka)",col="black",drawLegend=TRUE, zones=zones5)

dev.off()

#Stratiplot(median/1000 ~ `saddle` + `bilobate `+ `trapezoid oblong`+`bulliform`+ `cuneiform_bulliform`+`elongate_entire_cylindrical `+`elongate_psilate`+`Elongate blocky`+`Elongate rugose`+`Elongate facetated`+`Globular granulate`+`Blocky faceted `+`Non diagnostic`+`Concentration`, data= chooseTaxa(phy_mor23, max.abun = 2, n.occ = 1),
 #          type = c("h"),varTypes = c(rep("absolute", 1)), sort = "var",xlab="Relative percentage (%)",ylab="Age (ka)",col="black",drawLegend=TRUE)



?Stratiplot
phy21<-phy_2


phy_gssc<-filter(phy_2, final_morphotype == 'Saddle '|final_morphotype == 'Bilobate '|final_morphotype == 'Trapezoid oblong' )


phy_globular<-filter(phy_2, final_morphotype == 'Globular granulate')



#str(phy_2)

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
?gsub

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
        

cluster_group<- cluster_phy%>%
        spread(final_morphotype,total)%>%
        ungroup()


cluster_phy2<-cluster_group

cluster_phy2[is.na(cluster_phy2)] = 0


cluster_phy2<-cluster_phy2%>%
        merge(correctdepths_1)%>%
        left_join(ages_final_ITRAX2)
### Counts

cluster_phy3<-select(cluster_phy2,grasses_sedges,gssc,woody,poaceae)#remove non_diag
#cluster_phy33<-filter(cluster_phy3,Identifier!=75)
cluster_phy3<-as.data.frame(cluster_phy3)

dissim_phy<- vegdist(cluster_phy3, method="bray")
#dissimilarity matrix computation

clust_phy <- chclust(dissim_phy, method="coniss")

cluster_phy4<-cluster_phy3
#cluster33<-cluster_phy3
#cluster_phy33<-select(cluster_phy3,-Identifier)



# Ball and Stick
groups1<- bstick(clust_phy,ng=10) #determine the appropriate number of significant clusters
ngroups1<- groups1$nGroups[which(groups1$dispersion <= groups1$bstick)]
ngroups1<- ngroups1[2]
cc1<- cutree(clust_phy, k = ngroups1)

# STRAT plot
par(oma=c(2,1,1,1.2))
strat.plot(cluster_phy3, yvar=cluster_phy2$median, clust=clust_phy, y.rev=TRUE, cex.axis=0.6, cex.yaxis=0.8, cex.ylabel=0.8, cex.lab=0.6, ylab="cal BP (yr)", col.line="black", col.bar="black", las=3, mgp=c(3,1,0))
# check that strat plot is ok
#?par
#diat_merge100<-mutate(diat_merge10,age_ka=median/1000)

x<-strat.plot(cluster_phy3, yvar=cluster_phy4$median, clust=clust_phy, y.rev=TRUE, cex.axis=0.8, cex.yaxis=0.8, cex.ylabel=0.8, cex.lab=0.8, ylab="ka cal BP (yr)",col.line="black", col.bar="black", las=3, mgp=c(3,1,0))

#?strat.plot


z<-as.matrix(1:ngroups1)

#addClustZone(x, clust_phy, nZone=ngroups1, col=rainbow(length(z))[rank(z)])
c1 <- cutree(clust_phy, k=ngroups1)






cluster_phy4$median <- cluster_phy2$median
cluster_phy4$clust <- c1





#levels(phy211$final_morphotype)

phy_222<-left_join(phy211,agedepth2)%>%
        filter(final_morphotype!="non_diag")
phy_223<-ggplot(phy_222, aes(x=phy_222$median,y=countT, color=final_morphotype))+geom_point()+ scale_color_hue(h = c(80, 1000))

phy_223

phy_non1<-left_join(phy211,agedepth2)

phy_non<-ggplot(phy_non1, aes(x=phy_non1$median,y=countT, color=final_morphotype))+geom_point()+ scale_color_hue(h = c(80, 1000))

#ggplotly(phy_non)



#ggplotly(phy_223)


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

#ggplotly(phy_g1)
#str(phy_per$final_morphotype)

#str(phy_222)
phy_222$final_morphotype<-as.factor(phy_222$final_morphotype)


#Concentration graph

phy_conc<-left_join(phy21,agedepth2)%>%
        filter(Identifier!=64)

phy_444<-ggplot(phy_conc, aes(y=median,(x=sqrt(phy_g_wet_sed))))+geom_point() +  
        geom_path()+
        scale_y_reverse(breaks = seq(0, 32500, by = 2000))  + ggtitle("")+xlab("Sqrt of Concentration \n(diat/g)")+ylab("Age (cal yr BP)")+theme_bw() + theme(axis.text.x=element_text(size=12),axis.title.x=element_text(size=12,face="bold"),axis.title.y = element_text(size=12,face="bold"))

#ggplotly(phy_444)

phy_444


#write.table(together, file = "C:/Users/Maria Jose Rivera/OneDrive - James Cook University/Australia renamed/Sanamere/Thesis sections/PhD/experiments/exp_grain/data/together2.csv",
#sep = ",", col.names = NA,
#            qmethod = "double")

#tog2<-read.csv("C:/Users/Maria Jose Rivera/OneDrive - James Cook University/Australia renamed/Sanamere/Thesis sections/PhD/experiments/exp_grain/data/together.csv",header = TRUE, sep = ",", row.names = 1)
tog2<-read.csv("experiments/exp_grain/data/together2.csv",header = TRUE, sep = ",", row.names = 1)


#write.table(Hypy.selected5, file = "C:/Users/Maria Jose Rivera/OneDrive - James Cook University/Australia renamed/Sanamere/Thesis sections/PhD/experiments/exp_Hypy/data/Hypy.selected5.csv",
#            sep = ",", col.names = NA,
#           qmethod = "double")

hypy_phy<-read.csv("experiments/exp_Hypy/data/Hypy.selected5.csv",header = TRUE, sep = ",", row.names = 1)

tog22<-tog2%>%
        select(Identifier,averaged.C.N,averaged.C)

tog22$Identifier<-as.numeric(tog22$Identifier)

tog3<-hypy_phy%>%
        select(Identifier,PyCxMAR, Corrected,av_d13Cpost,av_d13C)


tog4<-tog3%>%
        full_join(tog22)%>%
        drop_na(Identifier)

str(tog22)

hypy_phy12<-phy_conc%>%
        select(Identifier,phy_g_wet_sed)%>%
        full_join(tog4,by="Identifier")%>%
        #merge(tog22,by="")%>%
        # select()%>%
        unique()%>%
        arrange()%>%
        left_join(correctdepths_1)%>%
        left_join(ages_final_ITRAX2)
str(correctdepths_1)
#ggplotly(plot_conc2)


pyc<- hypy_phy12%>% ungroup()%>%
        select(median,2:8)%>%
        select(-Corrected, -phy_g_wet_sed)%>%
        mutate(averaged.C=log(averaged.C))%>%
        pivot_longer(-median,names_to= "param",values_to = "count")%>%
       # drop_na()%>%
        unique()

pyc$param <- as.factor(pyc$param)

write.csv(pyc, "pyc.csv")
pyc2<-pyc%>%
        drop_na()

png(
        "other/Phy/precom2_2.png", 
        width = 20, 
        height = 8, 
        res = 300,
        units = "in"
)



pyc_fig<-pyc2 %>%
        mutate(facet_label=fct_recode(
                param,
                "C"= "averaged.C",
            #    "Concentration"= "phy_g_wet_sed",
                "C:N"= "averaged.C.N",
                "delta ^13*C "= "av_d13C",
                "delta ^13*PyC"= "av_d13Cpost",
                "'PyC MAR'"= "PyCxMAR"
        ))%>%
       
        ggplot(aes(y = median/1000, x = count)) +
       # geom_line()+
        geom_point() +
       # geom_path()+
        facet_wrap(~facet_label, scales = "free_x", labeller=label_parsed, ncol=5) +
       # theme(strip.text.x = element_text(size = 8, colour = "red"))+
       # theme(plot.margin = margin(10, 2, 2, 2, "cm"))+
        scale_y_reverse(limits=c(33,0),breaks = seq(0, 33, by = 2)) +
        labs(x = NULL, y = "Age (ka)")+
        theme_bw(base_size = 25)+ theme(panel.grid.major = element_blank())+
 theme(axis.title.y =element_text(size=24),
        strip.text = element_text(size = 24))

print(pyc_fig)

dev.off()



plot_conc1 <- hypy_phy12%>%
        select(median, phy_g_wet_sed)%>% 
        #filter(Identifier!=132)%>%
        na.omit()%>% 
        ggplot(aes(x = sqrt(phy_g_wet_sed) , y = median/1000)) +
        geom_point(aes(x = sqrt(phy_g_wet_sed) , y = median/1000), size = 1, alpha = 0.75) +
        geom_path(aes(x = sqrt(phy_g_wet_sed) , y = median/1000), size = 1, alpha = 0.75)+
        ylab("Age k cal yr BP") +
        xlab("Phytolith concentration")+
        theme_minimal()+
        scale_y_reverse(limits=c(33,0),breaks = seq(0,33, by = 2))+
        theme(axis.title.x=element_text(size=10),
            axis.title.y = element_blank(),
              axis.text.y = element_blank())

plot_conc2 <- hypy_phy12%>%
        select(median, PyCxMAR)%>% 
        na.omit()%>%
        arrange(median)%>%
        ggplot(aes(x = PyCxMAR , y = median/1000)) +
        geom_point(aes(x = PyCxMAR , y = median/1000), size = 1, alpha = 0.75) +
        geom_path(aes(x = PyCxMAR, y = median/1000), size = 1, alpha = 0.75)+
        ylab("Age cal yr BP")  +
        xlab("PyC MAR \n (ug mm^-2^/yr)")+# (ug mm^-2^/yr)
        theme_minimal(base_size = 20)+
        scale_y_reverse(limits=c(33,0),breaks = seq(0, 33, by = 2))+scale_x_continuous(breaks = seq(0, 0.8, by = 0.2))+scale_x_continuous(position = "top")+
        theme(axis.title.x=element_text(size=12))
          #    axis.title.y = element_blank(),
           #   axis.text.y = element_blank())

plot_conc22 <- hypy_phy12%>%
        select(median, PyCxMAR)%>% 
        na.omit()%>%
        arrange(median)%>%
        ggplot(aes(x = PyCxMAR , y = median/1000)) +
        geom_point(aes(x = PyCxMAR , y = median/1000), size = 1, alpha = 0.75) +
        geom_path(aes(x = PyCxMAR, y = median/1000), size = 1, alpha = 0.75)+
        ylab("Age cal yr BP")  +
        xlab(bquote("PyC MAR (ug mm"^"-2"/"yr)"))+# (ug mm^-2^/yr)
        theme_minimal(base_size = 20)+
        scale_y_reverse(limits=c(33,0),breaks = seq(0, 33, by = 2))+scale_x_continuous(breaks = seq(0, 0.8, by = 0.2))+scale_x_continuous(position = "top")+
        theme(axis.title.x=element_text(size=12))
#    axis.title.y = element_blank(),
#   axis.text.y = element_blank())

plot_conc22


p_pyc<-plot_conc2+ scale_x_continuous(sec.axis = dup_axis(~.,name=bquote("ug mm"^"-2"/"yr")), position = "top")

plot_conc2

plot_conc3 <- hypy_phy12%>%
        select(median, averaged.C.N)%>%
        na.omit() %>%
        arrange(median)%>%
        ggplot(aes(x = averaged.C.N , y = median/1000)) +
        geom_point(aes(x = averaged.C.N , y = median/1000), size = 1, alpha = 0.75) +
        geom_path(aes(x = averaged.C.N, y = median/1000), size = 1, alpha = 0.75)+
        ylab("") +
        xlab("C:N ratio")+
        theme_minimal(base_size = 20)+
        scale_y_reverse(limits=c(33,0),breaks = seq(0, 33, by = 2))+ scale_x_continuous(position = "top")+
        theme(#axis.title.x=element_text(size=12),
              axis.title.y = element_blank(),
              axis.text.y = element_blank())
plot_conc3
p_cn<-plot_conc3+ scale_x_continuous(sec.axis = dup_axis(~.,name=bquote("")), position = "top")


plot_conc4 <- hypy_phy12%>%
        select(median, averaged.C)%>% 
        na.omit() %>%
        arrange(median)%>%
        ggplot(aes(x = averaged.C , y = median/1000)) +
        geom_point(aes(x = averaged.C , y = median/1000), size = 1, alpha = 0.75) +
        geom_path(aes(x = averaged.C , y = median/1000), size = 1, alpha = 0.75)+
        ylab("") +
        xlab(bquote(atop("C", "()")))+
        theme_minimal(base_size = 20)+
        scale_y_reverse(limits=c(33,0),breaks = seq(0, 33, by = 2))+ scale_x_continuous(position = "top")+
        theme(#axis.title.x=element_text(size=12),
              axis.title.y = element_blank(),
              axis.text.y = element_blank())

plot_conc44 <- hypy_phy12%>%
        select(median, averaged.C)%>% 
        na.omit() %>%
        arrange(median)%>%
        ggplot(aes(x = averaged.C , y = median/1000)) +
        geom_point(aes(x = averaged.C , y = median/1000), size = 1, alpha = 0.75) +
        geom_path(aes(x = averaged.C , y = median/1000), size = 1, alpha = 0.75)+
        ylab("") +
        xlab("C (%)")+
        theme_minimal(base_size = 20)+
        scale_y_reverse(limits=c(33,0),breaks = seq(0, 33, by = 2))+ scale_x_continuous(position = "top")+
        theme(#axis.title.x=element_text(size=12),
                axis.title.y = element_blank(),
                axis.text.y = element_blank())


p_c<-plot_conc4+ scale_x_continuous(sec.axis = dup_axis(~.,name=bquote("(%)")), position = "top")


plot_conc7 <- hypy_phy12%>%
        select(median, av_d13C)%>% 
        na.omit() %>%
        #filter(av_d13C<(-19))%>%
        arrange(median)%>%
        ggplot(aes(x = av_d13C , y = median/1000)) +
        geom_point(aes(x = av_d13C , y = median/1000), size = 1, alpha = 0.75) +
        geom_path(aes(x = av_d13C , y = median/1000), size = 1, alpha = 0.75)+
        ylab("") +
        xlab(bquote(atop("\u2030"^"13"*"C", "()")))+ 
       # xlab(expression(paste("Histogram of ",hat(mu), ",cat("\n") Bootstrap samples, Allianz"))))+
        theme_minimal(base_size = 20)+
        scale_y_reverse(limits=c(33,0),breaks = seq(0, 33, by = 2))+  scale_x_continuous(position = "top")
       # theme(#axis.title.x=element_text(size=10),
          ##    axis.title.y = element_blank(),
          #    axis.text.y = element_blank())
       
#bquote("Average Conc of S- on plates ("~mu~"Moles/cm"^"2"*")"))
#main=expression(paste("Histogram of ",hat(mu), ",cat("\n") Bootstrap samples, Allianz")))
plot_conc7
p_c13<-plot_conc7+ scale_x_continuous(sec.axis = dup_axis(~.,name=bquote("VPDB (\u2030)")), position = "top")

plot_conc77 <- hypy_phy12%>%
        select(median, av_d13C)%>% 
        na.omit() %>%
        #filter(av_d13C<(-19))%>%
        arrange(median)%>%
        ggplot(aes(x = av_d13C , y = median/1000)) +
        geom_point(aes(x = av_d13C , y = median/1000), size = 1, alpha = 0.75) +
        geom_path(aes(x = av_d13C , y = median/1000), size = 1, alpha = 0.75)+
        ylab("") +
        xlab("\u2030")+ 
        # xlab(expression(paste("Histogram of ",hat(mu), ",cat("\n") Bootstrap samples, Allianz"))))+
        theme_minimal(base_size = 20)+
        scale_y_reverse(limits=c(33,0),breaks = seq(0, 33, by = 2))+  scale_x_continuous(position = "top")
p_c13
plot_conc8 <- hypy_phy12%>%
        select(median, av_d13Cpost)%>% 
        na.omit() %>%
        arrange(median)%>%
        ggplot(aes(x = av_d13Cpost , y = median/1000)) +
        geom_point(aes(x = av_d13Cpost , y = median/1000), size = 1, alpha = 0.75) +
        geom_path(aes(x = av_d13Cpost , y = median/1000), size = 1, alpha = 0.75)+
        ylab("") +
        xlab(expression(paste(delta^{13}, PyC))) +
        theme_minimal(base_size = 20)+
        scale_y_reverse(limits=c(33,0),breaks = seq(0, 33, by = 2))+scale_x_continuous(limits=c(-26,-23),breaks = seq(-26,-23, by = 1))+  scale_x_continuous(position = "top")+
        theme(#axis.title.x=element_text(face="bold"), axis.title.x.bottom  = element_text(face="bold"),
              axis.title.y = element_blank(),
              axis.text.y = element_blank())

p_pyc13<-plot_conc8+ scale_x_continuous(sec.axis = dup_axis(breaks=waiver(),name=bquote("VPDB(\u2030)")), position = "top")

plot_conc88 <- hypy_phy12%>%
        select(median, av_d13Cpost)%>% 
        na.omit() %>%
        arrange(median)%>%
        ggplot(aes(x = av_d13Cpost , y = median/1000)) +
        geom_point(aes(x = av_d13Cpost , y = median/1000), size = 1, alpha = 0.75) +
        geom_path(aes(x = av_d13Cpost , y = median/1000), size = 1, alpha = 0.75)+
        ylab("") +
        xlab(expression(paste(delta^{13}, PyC))) +
        theme_minimal(base_size = 20)+
        scale_y_reverse(limits=c(33,0),breaks = seq(0, 33, by = 2))+
        scale_x_continuous(expression(paste(delta^{13}, PyC)),limits=c(-26,-23),breaks = seq(-26,-23, by = 1),sec.axis = dup_axis(breaks=waiver(),name=bquote("VPDB(\u2030)")), position = "top")+
        theme(#axis.title.x=element_text(face="bold"), axis.title.x.bottom  = element_text(face="bold"),
                axis.title.y = element_blank(),
                axis.text.y = element_blank())

plot_conc88
p_pyc13
plot_conc8
grid.arrange(plot_conc1,plot_conc2,plot_conc3,plot_conc4,nrow=1,ncol=4)
grid.arrange(plot_conc1,plot_conc2,plot_conc3,plot_conc4,plot_conc7,plot_conc8,nrow=1,ncol=6)


fig3<-grid.arrange(plot_conc2,plot_conc3,plot_conc4,plot_conc7,plot_conc8,nrow=1,ncol=5)
fig33<-grid.arrange(plot_conc2,plot_conc3,plot_conc4,plot_conc1, plot_conc7,plot_conc8,nrow=1,ncol=6)
fig33
fig33<-grid.arrange(p_pyc,p_cn,p_c, p_c13,plot_conc88,nrow=1,ncol=5)

ggarrange(p_pyc,p_cn,p_c, p_c13,p_pyc13,widths = c(18,18,18,18,28))

ggsave(file="fig33.png", fig33,path="C:/Users/Maria Jose Rivera/OneDrive - James Cook University/Australia renamed/Sanamere/Thesis sections/PhD/other/Phy", width=24,height=12,units="cm")#750, 400

grid.arrange(plot_conc22,plot_conc3,plot_conc44,plot_conc77,plot_conc8,nrow=1,ncol=5)

plot_conc5 <- hypy_phy12%>%
        select(Corrected, averaged.C)%>% 
        na.omit() %>%
        
        ggplot(aes(x = averaged.C , y = Corrected)) +
        geom_point(aes(x = averaged.C , y = Corrected), size = 1, alpha = 0.75) +
        #geom_path(aes(x = averaged.C , y = Corrected), size = 1, alpha = 0.75)+
        ylab("PyC abundance (%)") +
        xlab( "% TOC")+
        theme_bw()+
        #scale_y_reverse(limits=c(33000,0),breaks = seq(0, 33000, by = 2000))+
        theme(axis.title=element_text(size=16),axis.text.x=element_text(size=12), panel.grid.minor = element_blank())
             # axis.title.y = element_blank(),
              #axis.text.y = element_blank())
str(hypy_phy12)
plot_conc5

plot_conc6 <- hypy_phy12%>%
        select(av_d13C,av_d13Cpost)%>% 
        filter(av_d13C<(-20))%>%
        na.omit() %>%
        ggplot(aes(x = av_d13C , y = av_d13Cpost)) +
        geom_point(aes(x = av_d13C , y = av_d13Cpost), size = 1, alpha = 0.75) +
        #geom_path(aes(x = averaged.C , y = median/1000), size = 1, alpha = 0.75)+
        ylab(expression(paste(delta^{13}, PyC[VPDB],"(\u2030)"))) +
        xlab(expression(paste(delta^{13}, C[VPDB],"(\u2030)"))) +
        theme_bw()+
        #scale_y_reverse(limits=c(33000,0),breaks = seq(0, 33000, by = 2000))+
        theme(axis.title=element_text(size=16),axis.text.x=element_text(size=12), panel.grid.minor = element_blank())
         #     axis.title.y = element_blank(),
          #    axis.text.y = element_blank())
plot_conc6


fig4<-grid.arrange(plot_conc5, plot_conc6,nrow=2,ncol=1)
library(egg)
#fig4<-egg::ggarrange(plot_conc5, plot_conc6, heights = c(0.50, 0.50))

#fig4
#ggsave(file="fig44.png", fig4,path="C:/Users/Maria Jose Rivera/OneDrive - James Cook University/Australia renamed/Sanamere/Thesis sections/PhD/other/Phy", width=15,height=15,units="cm")#750, 400


lm_toc<-lm(hypy_phy12$av_d13Cpost ~ hypy_phy12$av_d13C)

summary(lm_toc)

lm_toc2<-lm(hypy_phy12$averaged.C ~ hypy_phy12$Corrected)
summary(lm_toc2)

hypy_trial<-hypy_phy12%>%
        select(median,PyCxMAR,phy_g_wet_sed)%>%
        filter(median>10000)%>%
        na.omit()



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
        scale_y_reverse(breaks = seq(0, 33, by = 1000))+
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
        scale_y_reverse(breaks = seq(0, 33, by = 1000))+
        theme(axis.title.x=element_text(size=12),
         axis.title.y = element_blank(),
     axis.text.y = element_blank())



plot_hypy_median_c

grid.arrange(plot_hypy_median_b,plot_hypy_median_c,nrow=1,ncol=2)

#grid.arrange(plot_hypy_median_b,phy_g2,nrow=1,ncol=2)

h<-cor(hypy_phy)


## ---- modern-reference

# Isachne confusa was added as SAN 350 Poaceae and Thaumastochloa major SAN 450
phy_modern<-read.csv(here("experiments", "exp_phy","data","Phytolith_modern.csv"))

phy_modern$Distance_from_lake <- gsub("SAN", "", phy_modern$Distance_from_lake)
phy_modern$Distance_from_lake <- gsub("51", "50", phy_modern$Distance_from_lake)
phy_modern$Distance_from_lake <- as.numeric(as.character(phy_modern$Distance_from_lake))   

phy_modern$Sci_name <- gsub("pandanus", "Pandanus tectorius", phy_modern$Sci_name)

table1<-phy_modern%>%
        select(Distance_from_lake,Common_name,Sci_name,Family,Dominance)%>%
        arrange(Distance_from_lake)%>%
        filter(Common_name!="Unknown")

table11<-fct_recode(table1$Common_name,"Pandanus"="")
table111<-fct_recode(table1$Dominance,"minor"="","minor"="Minor","major"="Major")

table1_1<-data.frame(table1,table11,table111)
table1_2<-table1_1%>%
        select(Distance_from_lake,table11,Sci_name,Family,table111)%>%
        rename(`Distance from lake (m)`=Distance_from_lake, `Common name`= table11,`Scientific name`=Sci_name,`Dominance`=table111)%>%
        filter(`Common name`!="Orange creeper")
        

#?rename
table2<-phy_modern%>%
        filter(Phy_Presence==1)%>%
        select(Distance_from_lake,Common_name,Sci_name,Family)%>%
        arrange(Distance_from_lake)

Morphology<-c("Elongate sinuate","Elongated","Globular echinate","Elongate sinuate","Silica skeleton","Stomata")

table22<-fct_recode(table2$Common_name,"Pandanus"="")

table222<-data.frame(table2,table22)

table2222<-select(table222,-Common_name)


table2_a<-data.frame(table2222,Morphology)
table2_aa<-table2_a%>%
        select(Distance_from_lake,table22,Sci_name,Family,Morphology)%>%
        rename(`Distance from lake (m)`=Distance_from_lake, `Common name`= table22,`Scientific name`=Sci_name)%>%
        filter(`Common name`!="Unknown")


## ---- tb-one-veg


table1_2%>%
        arrange(`Distance from lake (m)`)%>%
        knitr::kable("latex",booktabs = TRUE, caption = "(ref:tb-one-veg)", linesep = "")%>%
        kableExtra::kable_styling(position = "center", latex_options= "scale_down")%>%
        column_spec(3, italic  = T) 

## ---- tb-two-veg

table2_aa%>%
        arrange(`Distance from lake (m)`)%>%
        knitr::kable("latex",booktabs = TRUE, caption = "(ref:tb-two-veg)", linesep = "")%>%
        kableExtra::kable_styling(position = "center", latex_options= "scale_down")


##---- extra

phy_modern2<-phy_modern%>%
        filter(Sci_name!="")


# 20 different species
length(unique(phy_modern2$Sci_name))

#no scientific name: 2 poaceae, Melaleuca, Eucalyptus y Acacia; orange creeper?
phy_modern3<-phy_modern%>%
        filter(Sci_name=="")


phy_modern5<-phy_modern%>%
        filter(Phy_Presence=="1"|Phy_Presence=="0"|Phy_Presence=="X")


print(unique(phy_modern4$Family))

#table include Commom_name, Family, Sci_name, Phy_abundance, Modern_reference,Phy_presence
#add morphotype from pics same morphotypes in thesis_sci


cluster_stacked<-cluster_phy4%>%
        gather(Group,Percentage,-median,-clust)

ggplot(cluster_stacked, aes(x=median, y=Percentage, fill=Group)) + 
        geom_area()

#LGM<-cluster_phy4%>%
 #       filter(median>18200 & median<19800)

# Factors

data <- data.frame( Name=c('Alice','Bruce','Charlie'), Grade = c('A','B','B') ) %>%
        mutate( Grade = factor(Grade) ) %>%
        mutate( Grade = fct_expand(Grade, 'C')) %>%
        mutate( Grade = if_else(Name == 'Charlie', factor('C', levels=levels(Grade)), Grade ) )
data  