library(zoo)


plot_ <- together_z_1 %>%
        select(median, Value,Element) %>%
        na.omit() %>%
        mutate(facet_label=fct_recode(
                Element,
                "Al"= "NorAl_roll",
                "Fe"= "NorFe_roll",
                "Si"= "NorSi_roll",
                "Si:Ti"= "NorSi_Ti_roll",
                "PrC"= "NorPrC_roll",
                "Inc/Coh" = "NorMoroll"
        ))%>%
        ggplot(aes(y = median/1000, x = Value)) +
        geom_point()+
        geom_path()+
        xlab("") +
        theme_bw(base_size = 20) +
        facet_wrap(~facet_label,scales = "free_x", labeller=label_parsed, ncol=5)+
        scale_y_reverse(limits=c(33,4),breaks = seq(4, 33, by = 2)) +
        labs(x = "Normalized ratio", y = "Age (ka)")
      #  theme(axis.title.x=element_text(size=22),axis.text.x=element_text(size=16),
  

     #       axis.title.y = element_blank(),
        #      axis.text.y = element_blank())


png(
                "other/Hydro/elements_1.png", 
                width = 12, 
                height = 8, 
                res = 300,
                units = "in"
        )


print(plot_)

dev.off()
together_z<- together_1%>%
        select(median, NorFe, NorSi, PrC, NorAl, Si_Ti)%>%
        mutate(NorFe_roll=rollmean(NorFe,k=10, fill = TRUE))%>%
        mutate(NorSi_roll=rollmean(NorSi,k=10, fill = TRUE))%>%
        mutate(NorPrC_roll=rollmean(PrC,k=10, fill = TRUE))%>%
        mutate(NorAl_roll=rollmean(NorAl,k=10, fill = TRUE))%>%
        mutate(NorSi_Ti_roll=rollmean(Si_Ti,k=10, fill = TRUE))%>%
        filter(median>4600 & median<32000)%>%
        select(median,NorAl_roll, NorFe_roll,NorSi_roll,NorSi_Ti_roll,NorPrC_roll)%>%
        gather(Element, Value, -median)%>%
        drop_na()
        
together_z_1<- together_1%>%
        select(median, NorFe, NorSi, PrC, NorAl, Si_Ti)%>%
        mutate(NorFe_roll=rollmean(NorFe,k=10, fill = TRUE))%>%
        mutate(NorSi_roll=rollmean(NorSi,k=10, fill = TRUE))%>%
        mutate(NorPrC_roll=rollmean(PrC,k=10, fill = TRUE))%>%
        mutate(NorAl_roll=rollmean(NorAl,k=10, fill = TRUE))%>%
        mutate(NorSi_Ti_roll=rollmean(Si_Ti,k=10, fill = TRUE))%>%
        filter(median>4600 & median<32000)%>%
        left_join(organic3)%>%
        select(median,NorSi_Ti_roll,NorPrC_roll, NorMoroll)%>%
        gather(Element, Value, -median)%>%
        drop_na()

geom_fig<-geochemical %>%
        mutate(facet_label=fct_recode(
                param,
                "'C (%)'"= "averaged.C",
                "'N (%)'"= "averaged.N",
                "C:N"= "averaged.C.N",
                "delta ^13*C"= "averaged.d13C",
                "delta ^15*N"= "averaged.d15N"
        ))%>%
        ggplot(aes(y = median/1000, x = count)) +
        geom_path() +
        geom_point() +
        facet_wrap(~facet_label, scales = "free_x", labeller=label_parsed, ncol=5) +
        #theme(strip.text.x = element_text(size = 14, colour = "red"))+
        scale_y_reverse(limits=c(33,0),breaks = seq(0, 33, by = 2)) +
        labs(x = NULL, y = "Age (cal ka yr BP)")+
        theme_minimal(base_size = 30)
# theme(axis.title.x =element_text(size=16,face="bold"))

#a<-together_z%>%
#        mutate(across(2:6,rollmean(.)))
#v<-sapply(together_z,zoo:rollmean(2:6))

plot_2 <- prcurve_ITRAX4 %>%
        select(1:7,9)%>%
        gather(Element,counts,1:7,-8)%>%
        na.omit() %>%
        mutate(facet_label=fct_recode(
                Element,
                "Al"= "NorAl",
                "Fe"= "NorFe",
                "Si"= "NorSi",
                "Ca"= "NorCa",
                "K"= "NorK",
                "Sr"= "NorSr",
                "Ti"= "NorTi"
        ))%>%
        ggplot(aes(y = median/1000, x = counts)) +
        geom_point()+
        geom_path()+
        xlab("") +
        theme_bw(base_size = 20) +
        facet_wrap(~facet_label, scales = "fixed",labeller=label_parsed, ncol=7)+
        scale_y_reverse(limits=c(33,4),breaks = seq(4, 33, by = 2)) +
      # scale_x_continuous(labels = scales::number_format(accuracy = 0.1))%>%
        labs(x = "Normalized ratio", y = "Age (ka)")
#  theme(axis.title.x=element_text(size=22),axis.text.x=element_text(size=16),
plot_2

plot_2+scale_x_continuous(breaks=c(0.2, 0.6,1),
        labels = scales::number_format(accuracy = 0.1)) + 
        geom_hline(yintercept=c(9.7, 18.2,29.1))



plot_2+scale_x_continuous(labels=scaleFUN, breaks=seq(by=0.1))
#Our transformation function
scaleFUN <- function(x) sprintf("%.1f", x)
#       axis.title.y = element_blank(),
#      axis.text.y = element_blank())


png(
        "other/Hydro/elements_1.png", 
        width = 12, 
        height = 8, 
        res = 300,
        units = "in"
)


print(plot_)

together_z2<- together_1%>%
        select(median, NorFe, NorSi, PrC, NorAl, Si_Ti)%>%
        mutate(NorFe_roll=rollmean(NorFe,k=10, fill = TRUE))%>%
        mutate(NorSi_roll=rollmean(NorSi,k=10, fill = TRUE))%>%
        mutate(NorPrC_roll=rollmean(PrC,k=10, fill = TRUE))%>%
        mutate(NorAl_roll=rollmean(NorAl,k=10, fill = TRUE))%>%
        mutate(NorSi_Ti_roll=rollmean(Si_Ti,k=10, fill = TRUE))%>%
        filter(median>4600 & median<32000)%>%
        select(NorAl_roll, NorFe_roll,NorSi_roll,NorSi_Ti_roll, NorPrC_roll)%>%
        drop_na()

together_z3<- together_1%>%
        select(median, NorFe, NorSi, PrC, NorAl, Si_Ti)%>%
        mutate(NorFe_roll=rollmean(NorFe,k=10, fill = TRUE))%>%
        mutate(NorSi_roll=rollmean(NorSi,k=10, fill = TRUE))%>%
        mutate(NorPrC_roll=rollmean(PrC,k=10, fill = TRUE))%>%
        mutate(NorAl_roll=rollmean(NorAl,k=10, fill = TRUE))%>%
        mutate(NorSi_Ti_roll=rollmean(Si_Ti,k=10, fill = TRUE))%>%
        filter(median>4600 & median<32000)%>%
        select(NorAl_roll, NorFe_roll,NorSi_roll,median)%>%
        drop_na()

rolling2<-san_final_model10%>%
        mutate(roll_Ti=rollmean(NorTi, k=10, fill=NA))%>%
        mutate(roll_Si=rollmean(NorSi, k=10, fill=NA))%>%
        mutate(roll_Rb=rollmean(NorRb, k=10, fill=NA))%>%
       # mutate(roll_Zr=rollmean(NorZr, k=10, fill=NA))%>%
        left_join(ages_final_ITRAX2,all=T)%>%
        select(roll_Rb,roll_Si,roll_Ti,median)
rename(median_2=="median")
rolling2<-rolling%>%
        filter(is.na(roll_Ti))%>%
        select(Depth,roll_Ti,NorTi)
