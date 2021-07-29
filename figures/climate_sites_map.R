library(here)
library(tidyverse)

# last cleaned versio ready for map? cleaned_back3


# Using climate_1_2 do an extra column: "Indicator": Climate/vegetation; fire
# use climate sites_7 as a base and join with climate_sites_3
# maybe add the extra ones to climate_sites_5 or 7 and charcoal, add Emma's


climate<-read.csv(here("data","climate_sites_3.csv"))


climate_1<-climate%>%
        select(-X,-X.3)%>%
        filter(Latitude > -20)%>% #before it was -23
        filter(Latitude < -7)%>%
        filter(Record!="Forams")%>%
        filter(Record!="Coral")%>%
        rename(Type='X.1', Length='X.2')%>%
        filter(Longitude<160)



climate_1_1<-climate%>%
        select(-X,-X.3)%>%
        filter(Latitude > -20)%>% #before it was -23
        filter(Latitude < -7)%>%
        filter(Record!="Forams")%>%
        filter(Record!="Coral")%>%
        rename(Type='X.1', Length='X.2')%>%
        filter(Longitude>128 & Longitude<147)

climate_1_3<-climate%>%
        select(-X,-X.3)%>%
        filter(Latitude > -20)%>% #before it was -23
        filter(Latitude < -7)%>%
       # filter(Record!="Forams")%>%
#        filter(Record!="Coral")%>%
        rename(Type='X.1', Length='X.2')%>%
        filter(Longitude>136 & Longitude<151)









write.csv(climate_1,file=(here("data","climate_sites_4.csv")))


climate_2<-read.csv(here("data","climate_sites_5.csv"))

write.csv(climate_2,file=(here("data","climate_sites_7.csv")))



merged_climate<- climate_1_3%>%
        left_join(climate_2)




climate_1_2<-climate_2%>% #PDP20 and lake euramoo repeated, delete gilbert river?; de deckker 2001 to lc gc2; zurath is 6520;delete Boigu Gawat Core 2? waruid is 6 ka, badu 15 is 8 ka;
        # include bentick 2.4 ka and lizard island 8 ka in the lit review
       # select(-X,-X.3)%>%
        filter(Latitude > -20)%>% #before it was -23
        filter(Latitude < -7)%>%
        filter(Record!="Forams")%>%
        filter(Record!="Coral")%>%
       # rename(Type='X.1', Length='X.2')%>%
        filter(Longitude>135.15 & Longitude<147)





Site<-c("Bentinck Island", "Lizard Island")
Latitude<-c(-17.0666, -14.665)
Longitude<-c(139.483, 145.463)
Type<-c("coastal", "coastal")
Record<-c("lacustrine", "swamp")
Length<-c(2.4, 8)
References<-c("Mackenzie et. al, 2017; Mackenzie et al., 2020", "Proske and Haberle, 2012")
#Charcoal<- c(1, 1)

c_m<- data.frame(Site, Latitude, Longitude, Type, Record, Length, References)  
        
#c_m<-cbind(Site, Latitude, Longitude, Type, Record, Length, References)
#c_m<-as.data.frame(c_m)


back22<-full_join(climate_1_2,c_m)

back2<-back22%>%
        filter(Site!="ODP Site 820")%>% # add charcoal
        filter(Site!="Lake Euramoo, QLD")


back2$Length[back2$Length== 11.83]<- 6.5
back2$Length[back2$Length== 7.2]<- 6
back2$Length[back2$Length== 4.19]<- 9
back2$Length[back2$Length== 13.82]<- 2.6
back2$Length[back2$Length== 4.57]<- 2.7



back2$Type<-as.factor(back2$Type)
back2$Site<-as.factor(back2$Site)
back2$Record<-as.factor(back2$Record)
back2$References<-as.factor(back2$References)

write.csv(back2,file=here("data","back2.csv"))

cleaned_back<- read.csv(here("data","back2.csv"))


cleaned_back2<- full_join(cleaned_back,c_m)

cleaned_back3<-cleaned_back2%>%
        filter(Site!= "Lynchs Crater (holocene core)")%>%
        filter(Site!="Talita Kupai")%>%
        select(-X)


cleaned_final<- cleaned_back3%>%
        select(-Charcoal, -Record)%>%
        filter(Site!= "Gilbert River Delta, QLD")%>%
        filter(Site!= "Badu, Torres Strait")%>%
        filter(Site!="Carpentaria dune field")

cleaned_qgis<- cleaned_back3%>%
        select(-Record)%>%
        filter(Site!= "Gilbert River Delta, QLD")%>%
        filter(Site!= "Badu, Torres Strait")%>%
        filter(Site!="Carpentaria dune field")
        

write.csv(cleaned_qgis,file=here("data","cleaned_qgis.csv"))

#cleaned_final$Site <- sub('\\ .,*' , "", cleaned_final$Site)

#table22<-fct_recode(cleaned_final$Site,"ODP 820"="ODP820")


#cf<-cbind(table22,cleaned_final)


write.csv(cleaned_final,file=here("data","cleaned_final.csv"))
#climate_1_3<-fct_recode(climate_1_2$Site,"Big Willum"= c("Big Willium"))

climate_22<-cbind(climate_1_2,climate_1_3)

write.csv(climate_1_2,file=(here("data","climate_sites_8.csv")))

str(climate_1_2)

library(ggmap)
map <- get_map(location = 'Oceania', zoom = 5)
ggmap::register_google(key = "AIzaSyAYjWh9pLpk_fVgKEdGX4QFTP2oD6b0u2s")

mapPoints <- ggmap(map) +
         geom_point(aes(x = Longitude, y = Latitude), data = climate_1_2, alpha = .5)
mapPoints
