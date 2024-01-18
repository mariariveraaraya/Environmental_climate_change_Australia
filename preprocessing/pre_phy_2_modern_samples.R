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

library(officer)      

table_plants<-regulartable(table1_2)
table_plants2<-theme_vanilla(set_table_properties(table_plants, width=1,layout = "autofit"))
table_plants3<-italic(table_plants2,j=3)
  

save_as_docx(table_plants3, path = here("data", "table_ch6_1.docx"))


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

table_plants_a<-regulartable(table2_aa)
table_plants_b<-theme_vanilla(set_table_properties(table_plants_a, width=1,layout = "autofit"))
table_plants_c<-italic(table_plants_b,j=3)


save_as_docx(table_plants_c, path = here("data", "table_ch6_2.docx"))
## ---- tb-veg


table1_2%>%
        arrange(`Distance from lake (m)`)%>%
        knitr::kable("latex",booktabs = TRUE, caption = "(ref:tb-veg)", linesep = "")%>%
        kableExtra::kable_styling(position = "center", latex_options= "scale_down")%>%
        column_spec(3, italic  = T) 

## ---- tb-two-veg

table2_aa%>%
        arrange(`Distance from lake (m)`)%>%
        knitr::kable("latex",booktabs = TRUE, caption = "(ref:tb-two-veg)", linesep = "")%>%
        kableExtra::kable_styling(position = "center", latex_options= "scale_down")%>%
        column_spec(3, italic  = T) 
