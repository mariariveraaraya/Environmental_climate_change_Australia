library(here)
library(tidyverse)

together_1<-read.csv(here("processed_data","together.csv"))

together<-together_1


ele<-together_1%>%
    select(Depth,NorTi,NorFe,NorSi)%>%
    # filter(Water>0)%>%
    #mutate(averaged.C=log(averaged.C))%>%
    #filter(Water!=-12.34453, Water!=70.29578)%>%
    pivot_longer(-Depth,names_to= "param",values_to = "count")%>%
    drop_na()%>%
    unique()

strat<-together%>%
    select(Depth,Water, Dry_bulk_density2, averaged.C,d.0.5)%>%
    # filter(Water>0)%>%
    mutate(averaged.C=log(averaged.C))%>%
    #filter(Water!=-12.34453, Water!=70.29578)%>%
    pivot_longer(-Depth,names_to= "param",values_to = "count")%>%
    drop_na()%>%
    unique()

water<-together%>%
    filter(Water>70)

ele_fig<-ele %>%
    #filter(param!="Water" & count!=70.29578)%>%
    mutate(facet_label=fct_recode(
        param,
        "'Ti'"= "NorTi",
        "'Fe'"= "NorFe",
        "'Si'"= "NorSi"
    ))%>%
    ggplot(aes(y = Depth, x = count)) +
    geom_path() +
    geom_point() +
    facet_wrap(~facet_label, scales = "free_x", labeller=label_parsed, ncol=5) +
    #theme(strip.text.x = element_text(size = 14, colour = "red"))+
    scale_y_reverse(limits=c(172.2,0),breaks = seq(0, 172.2, by = 10)) +
    scale_x_continuous(labels = scales::number_format(accuracy = 0.1))+
    labs(x = NULL, y = "Depth (cm)")+
    theme_bw(base_size = 20)+ theme(panel.grid.minor = element_blank())


print(ele_fig)


ggsave("ele_fig.png",ele_fig,path=here("Figs"),width=8,height=10)



plot_w <- together %>%
    select(Depth, Water) %>%
    na.omit() %>%
    filter(Water>0, Water<60)%>%
    ggplot() +
    geom_point(aes(x = Water, y = Depth), size = 1, alpha = 0.75) + geom_path(aes(x = Water, y = Depth), size = 1, alpha = 0.75)+
    xlab("Water (%)") +
    theme_minimal(base_size = 20) +
    scale_y_reverse()+
    theme(
        axis.title.y = element_blank(),
        axis.text.y = element_blank())
ggplotly(plot_w)
plot_bd <- together %>%
    select(Depth, Dry_bulk_density2) %>%
    na.omit() %>%
    ggplot() +
    geom_point(aes(x = Dry_bulk_density2, y = Depth), size = 1, alpha = 0.75) + geom_path(aes(x = Dry_bulk_density2, y = Depth), size = 1, alpha = 0.75)+
    xlab(bquote('Bulk density ('*g/cm^-3*')'))+
    theme_minimal(base_size = 20) +
    scale_y_reverse()+
    theme(
        axis.title.y = element_blank(),
        axis.text.y = element_blank())

plot_C <- together %>%
    select(Depth, averaged.C) %>%
    na.omit() %>%
    ggplot() +
    geom_point(aes(x = averaged.C, y = Depth), size = 1, alpha = 0.75) + geom_path(aes(x = averaged.C, y = Depth), size = 1, alpha = 0.75)+
    xlab("Carbon (%)") +
    theme_minimal(base_size = 20) +
    scale_y_reverse()+
    theme(
        axis.title.y = element_blank(),
        axis.text.y = element_blank())

plot_05 <- together %>%
    select(Depth, d.0.5) %>%
    na.omit() %>%
    ggplot() +
    geom_point(aes(x = d.0.5, y = Depth), size = 1, alpha = 0.75) + geom_path(aes(x = d.0.5, y = Depth), size = 1, alpha = 0.75)+
    xlab("Median grain size (\u03BCm)") +
    theme_minimal(base_size = 20) +
    scale_y_reverse()+
    theme(
        axis.title.y = element_blank(),
        axis.text.y = element_blank())
ggplotly(plot_05)
plot_05

cowplot::plot_grid(plot_w, plot_bd, plot_C, align = "h", nrow = 1, rel_heights = c(0.33,0.33,0.33))

cowplot::plot_grid(plot_w, plot_bd, plot_C, plot_05,align = "h", nrow = 1, rel_heights = c(0.25,0.25,0.25,0.25))


plot_frac <- together %>%
    select(Depth, Fraction,Percentage) %>%
    na.omit() %>%
    unique()%>%
    ggplot() +
    geom_point(aes(x = Percentage , y = Depth, color=Fraction))+ geom_path(aes(x = Percentage, y = Depth, color=Fraction))+
    xlab("Percentage (%)") +
    scale_y_reverse(limits=c(172.2,0),breaks = seq(0, 172.2, by = 25))+
    theme_minimal()+
    theme(axis.title.x=element_text(size=18),axis.text.x=element_text(size=14),
          axis.title.y = element_blank(),
          axis.text.y = element_blank(), legend.position="none")+ annotate("text",x=60,y=20,label="Silt",size=8,color="blue")+
    annotate("text",x=40,y=100,label="Clay",size=8,color="green")+
    annotate("text",x=35,y=160,label="Sand",size=6,color="red")
ggplotly(plot_frac_time)
graphpp333<-ggplot(plot_frac, aes(x=Percentage,y=Depth,fill=Fraction))+ geom_area()+ theme_bw()+ theme(panel.border = element_blank()) + theme(axis.text=element_text(size=12), axis.title=element_text(size=12,face="bold"))+ylab("Percentage of the total (%)")+xlab("Age (cal yr BP)")+  scale_x_continuous()


graphpp333   
aa<-read.table("C:/Users/Maria Jose Rivera/OneDrive - James Cook University/Australia renamed/Sanamere/Thesis sections/PhD/other/table1.txt",header=TRUE,sep = ",")


plot_frac_time <- together %>%
    select(median, Fraction,Percentage) %>%
    na.omit() %>%
    unique()%>%
    ggplot() +
    geom_point(aes(x = Percentage , y = (median/1000), color=Fraction))+ geom_path(aes(x = Percentage, y = (median/1000), color=Fraction))+
    xlab("Percentage (%)") +
    scale_y_reverse(limits=c(33,0),breaks = seq(0, 33, by = 2))+
    theme_minimal()+
    theme(axis.title.x=element_text(size=18),axis.text.x=element_text(size=12),
          axis.title.y = element_blank(),
          axis.text.y = element_blank(), legend.position="none")+ annotate("text",x=60,y=20,label="Silt",size=6,color="blue")+
    annotate("text",x=10,y=5,label="Clay",size=6,color="green")+
    annotate("text",x=35,y=16,label="Sand",size=6,color="red")

plot_frac_time

###Fig2


plot_N <- together %>%
    select(Depth, averaged.N)%>% 
    na.omit() %>%
    ggplot() +
    geom_point(aes(x = averaged.N, y = Depth), size = 1, alpha = 0.75) + geom_path(aes(x = averaged.N, y = Depth), size = 1, alpha = 0.75)+
    xlab("Nitrogen (%)") +
    theme_minimal() +
    scale_y_reverse(breaks = seq(0, 172.2, by = 25))+
    theme(axis.title.x=element_text(size=16),
          axis.title.y = element_blank(),
          axis.text.y = element_blank())


plot_d13C <- together %>%
    select(Depth, averaged.d13C) %>%
    na.omit() %>%
    ggplot() +
    geom_point(aes(x = averaged.d13C, y = Depth), size = 1, alpha = 0.75) + geom_path(aes(x = averaged.d13C, y = Depth), size = 1, alpha = 0.75)+
    xlab(expression(paste(delta^{13}, C[VPDB],"(\u2030)"))) +
    theme_minimal() +
    scale_y_reverse(breaks = seq(0, 172.2, by = 25))+
    theme(axis.title.x=element_text(size=22),axis.text.x=element_text(size=16),
          axis.title.y = element_blank(),
          axis.text.y = element_blank())
#expression(paste(delta^{13}, C[VPDB],"(???)")))

"\u03B4 ^13^ C[VPDB]\u2030"
plot_d15N <- together %>%
    select(Depth, averaged.d15N) %>%
    na.omit() %>%
    ggplot() +
    geom_point(aes(x = averaged.d15N, y = Depth), size = 1, alpha = 0.75) + geom_path(aes(x = averaged.d15N, y = Depth), size = 1, alpha = 0.75)+
    xlab(expression(paste(delta^{15}, N[VPDB],"(\u2030)")))  +
    theme_minimal() +
    scale_y_reverse(breaks = seq(0, 172.2, by = 25))+
    theme(axis.title.x=element_text(size=22),axis.text.x=element_text(size=16),
          axis.title.y = element_blank(),
          axis.text.y = element_blank())



plot_sed <- together %>%
    select(Depth, sedrate.mm) %>%
    na.omit() %>%
    ggplot() +
    geom_point(aes(x = sedrate.mm, y = Depth), size = 1, alpha = 0.75) + geom_path(aes(x = sedrate.mm, y = Depth), size = 1, alpha = 0.75)+
    xlab("Sed rate (mm/yr)") +
    theme_minimal() +
    scale_y_reverse(limits=c(172.2,0),breaks = seq(0, 172.2, by = 25))+
    theme(axis.title.x=element_text(size=18,face="bold"),axis.text.x=element_text(size=14))
#  axis.title.y = element_blank(),
# axis.text.y = element_blank())
plot_sed_time <- together %>%
    select(median, sedrate.mm) %>%
    na.omit() %>%
    ggplot() +
    geom_point(aes(x = sedrate.mm, y = (median/1000)), size = 1, alpha = 0.75) + geom_path(aes(x = sedrate.mm, y = (median/1000)), size = 1, alpha = 0.75)+
    xlab("Sed rate (mm/yr)") +
    ylab("Age (k cal yr BP)")+
    theme_minimal() +
    scale_y_reverse(limits=c(33,0),breaks = seq(0, 33, by = 2))+
    theme(axis.title=element_text(size=18),axis.text=element_text(size=12))
#  axis.title.y = element_blank(),
# axis.text.y = element_blank())
ggplotly(plot_05)
plot_05 <- together %>%
    select(Depth, d.0.5) %>%
    na.omit() %>%
    ggplot() +
    geom_point(aes(x = d.0.5, y = Depth), size = 1, alpha = 0.75) + geom_path(aes(x = d.0.5, y = Depth), size = 1, alpha = 0.75)+
    xlab("Median grain size (\u03BCm)") +
    theme_minimal() +
    scale_y_reverse(limits=c(172.2,0),breaks = seq(0, 172.2, by = 25))+
    theme(axis.title.x=element_text(size=18),axis.text.x=element_text(size=14),
          axis.title.y = element_blank(),
          axis.text.y = element_blank())



plot_05_time <- together %>%
    select(median, d.0.5) %>%
    na.omit() %>%
    ggplot() +
    geom_point(aes(x = d.0.5, y = median/1000), size = 1, alpha = 0.75) + geom_path(aes(x = d.0.5, y = median/1000), size = 1, alpha = 0.75)+
    xlab("Median grain size (\u03BCm)") +
    theme_minimal() +
    scale_y_reverse(limits=c(33,0),breaks = seq(0, 33, by = 2))+
    theme(axis.title.x=element_text(size=18),axis.text.x=element_text(size=12),
          axis.title.y = element_blank(),
          axis.text.y = element_blank())

plot_05
plot_conc_2 <- together %>%
    select(Depth, d_g_wet_sed) %>%
    na.omit() %>%
    ggplot() +
    geom_point(aes(y = Depth, x = sqrt(d_g_wet_sed)), size = 1, alpha = 0.75) + geom_path(aes(y = Depth, x = sqrt(d_g_wet_sed)), size = 1, alpha = 0.75)+
    xlab("Diatoms per gram") +
    theme_minimal()+
    scale_y_reverse(limits= c(172.2,0),breaks = seq(0, 172.2, by = 25))+
    scale_x_continuous(breaks = seq(0, 2500, by = 1000))+
    theme(axis.title.x=element_text(size=22),axis.text.x=element_text(size=16),
          axis.title.y = element_blank(),
          axis.text.y = element_blank())


ggplotly(plot_conc_2)
plot_conc_2

#cowplot::plot_grid(plot_sed, plot_C, plot_N, plot_d13C,plot_d15N, align = "h", nrow = 1, rel_heights = c(0.2,0.2,0.2,0.2,0.2))

grid.arrange(plot_sed, plot_C, plot_d13C,ncol=3,nrow=1)

grid.arrange(plot_d15N, plot_N, ncol=2,nrow=1)

grid.arrange(plot_d15N, plot_N, plot_05,ncol=3,nrow=1)

grid.arrange(plot_sed,plot_05,plot_frac,ncol=3,nrow=1)

p33<-grid.arrange(plot_sed,plot_05,plot_frac,ncol=3,nrow=1)
## ---- FINAL FIG 2

p3<-grid.arrange(plot_sed_time,plot_05_time,plot_frac_time,ncol=3,nrow=1)
ggsave("fig2.png",p3,width=8,height=10,path=here("Figs"))


#grid.arrange(plot_sed, plot_C, plot_d13C,plot_05, ncol=4,nrow=1)
plot_C_N <- together %>%
    select(Depth, averaged.C.N) %>%
    na.omit() %>%
    ggplot() +
    geom_point(aes(y = Depth, x = averaged.C.N)) + geom_path(aes(y = Depth, x = averaged.C.N))+
    ylab("C:N ratio") +
    theme_minimal()+
    scale_y_reverse(breaks = seq(0, 172.2, by = 25))+
    theme(axis.title.x=element_text(size=16),
          axis.title.y = element_blank(),
          axis.text.y = element_blank())

plot_C_N
ggplotly(plot_C_N)
###Fig3

#together<- together_1

plot_Ti <- together %>%
    select(Depth, NorTi) %>%
    na.omit() %>%
    ggplot() +
    geom_point(aes(x = NorTi, y = Depth), size = 1, alpha = 0.75) + geom_path(aes(x = NorTi, y = Depth), size = 1, alpha = 0.75)+
    xlab("Ti") +
    theme_minimal() +
    scale_y_reverse()+
    theme(axis.title.x=element_text(size=22),axis.text.x=element_text(size=16),
          axis.title.y = element_blank(),
          axis.text.y = element_blank())

plot_Si <- together %>%
    select(Depth, NorSi) %>%
    na.omit() %>%
    ggplot() +
    geom_point(aes(x = NorSi, y = Depth), size = 1, alpha = 0.75) + geom_path(aes(x = NorSi, y = Depth), size = 1, alpha = 0.75)+
    xlab("Si") +
    theme_minimal() +
    scale_y_reverse()+
    theme(axis.title.x=element_text(size=22),axis.text.x=element_text(size=16),
          axis.title.y = element_blank(),
          axis.text.y = element_blank())

plot_Fe <- together %>%
    select(Depth, NorFe) %>%
    na.omit() %>%
    ggplot() +
    geom_point(aes(x = NorFe, y = Depth), size = 1, alpha = 0.75) + geom_path(aes(x = NorFe, y = Depth), size = 1, alpha = 0.75)+
    xlab("Fe") +
    theme_minimal() +
    scale_y_reverse()+
    scale_x_continuous(breaks = seq(0, 1, by = 0.3))+
    theme(axis.title.x=element_text(size=22),axis.text.x=element_text(size=16),
          axis.title.y = element_blank(),
          axis.text.y = element_blank())

plot_FeTi <- together %>%
    select(Depth, Fe_Ti) %>%
    na.omit() %>%
    ggplot() +
    geom_point(aes(x = Fe_Ti, y = Depth), size = 1, alpha = 0.75) + geom_path(aes(x = Fe_Ti, y = Depth), size = 1, alpha = 0.75)+
    xlab("Fe:Ti ratio") +
    theme_minimal() +
    scale_y_reverse()+
    theme(axis.title.x=element_text(size=22),axis.text.x=element_text(size=16),
          axis.title.y = element_blank(),
          axis.text.y = element_blank())
plot_SiTi <- together %>%
    select(Depth, Si_Ti) %>%
    na.omit() %>%
    ggplot() +
    geom_point(aes(x = Si_Ti, y = Depth), size = 1, alpha = 0.75) + geom_path(aes(x = Si_Ti, y = Depth), size = 1, alpha = 0.75)+
    xlab("Si:Ti ratio") +
    theme_minimal() +
    scale_y_reverse()+
    theme(axis.title.x=element_text(size=22),axis.text.x=element_text(size=16),
          axis.title.y = element_blank(),
          axis.text.y = element_blank())

plot_prc <- together %>%
    select(Depth, PrC) %>%
    na.omit() %>%
    ggplot() +
    geom_point(aes(x = PrC, y = Depth), size = 1, alpha = 0.75) + geom_path(aes(x = PrC, y = Depth), size = 1, alpha = 0.75)+
    xlab("Principal curve") +
    theme_minimal() +
    scale_y_reverse()+
    theme(axis.title=element_text(size=22),axis.text=element_text(size=16),
          axis.title.y = element_blank(),
          axis.text.y = element_blank())

plot_Al <- together %>%
    select(Depth, NorAl) %>%
    na.omit() %>%
    ggplot() +
    geom_point(aes(x = NorAl, y = Depth), size = 1, alpha = 0.75) + geom_path(aes(x = NorAl, y = Depth), size = 1, alpha = 0.75)+
    xlab("Al") +
    theme_minimal() +
    scale_y_reverse()+
    theme(axis.title.x=element_text(size=22), axis.text=element_text(size=16),
          axis.title.y = element_blank(),
          axis.text.y = element_blank())
together_2<-together%>%
    left_join(Al_Si_1cm, by='median')

si_ti_2<-together_2%>% 
    select(Depth,roll_Si_Al)%>%
    na.omit()%>%
    ggplot(aes(x = roll_Si_Al, y = Depth)) +
    # geom_path() +
    geom_point() +
    labs(x="Si:Al",y="Age (ka)")+
    theme_minimal()+
    #   geom_smooth(method = "gam")+
    scale_y_reverse()+
    theme(axis.title.x=element_text(size=22), axis.text=element_text(size=16),
          axis.title.y = element_blank(),
          axis.text.y = element_blank())
# theme_bw(base_size = 20)
si_ti_2
#ggsave("siti_fig.png",si_ti,path=here("Figs"),width=6,height=10,dpi=300)

plot_C_C <- together %>%
    select(Depth, averaged.C) %>%
    na.omit() %>%
    ggplot() +
    geom_point(aes(x = averaged.C, y = Depth), size = 1, alpha = 0.75) + geom_path(aes(x = averaged.C, y = Depth), size = 1, alpha = 0.75)+
    xlab("Carbon (%)") +
    theme_minimal() +
    scale_y_reverse()+
    scale_x_continuous(breaks = seq(0, 44, by = 10))+
    theme(axis.title.x=element_text(size=22),axis.text.x=element_text(size=18),
          axis.title.y = element_blank(),
          axis.text.y = element_blank())

plot_C_C

grid.arrange(plot_prc,plot_Ti, plot_Si, plot_Fe, plot_FeTi, plot_SiTi,ncol=6,nrow=1)

## ---- FIG 5

grid.arrange(plot_Ti, plot_Si, plot_Fe, plot_FeTi, plot_SiTi, plot_prc, plot_Al, ncol=7,nrow=1)

grid.arrange(plot_Ti, plot_Si, plot_Fe, plot_FeTi, plot_SiTi, plot_prc, plot_Al, si_ti_2,ncol=8,nrow=1)


grid.arrange(plot_Ti, plot_Si, plot_Fe,ncol=3,nrow=1)


plot_FeTi_2 <- together %>%
    select(age_k, Fe_Ti) %>%
    na.omit() %>%
    ggplot() +
    geom_point(aes(x = Fe_Ti, y = age_k), size = 1, alpha = 0.75) + geom_path(aes(x = Fe_Ti, y = age_k), size = 1, alpha = 0.75)+
    xlab("Fe:Ti ratio") +
    theme_minimal() +
    scale_y_reverse()+
    theme(axis.title.x=element_text(size=22),axis.text.x=element_text(size=18),
          axis.title.y = element_blank(),
          axis.text.y = element_blank())


plot_SiTi_2 <- together %>%
    select(age_k, Si_Ti) %>%
    na.omit() %>%
    ggplot() +
    geom_point(aes(x = Si_Ti, y = age_k), size = 1, alpha = 0.75) + geom_path(aes(x = Si_Ti, y = age_k), size = 1, alpha = 0.75)+
    xlab("Si:Ti ratio") +
    theme_minimal() +
    scale_y_reverse()+
    theme(axis.title.x=element_text(size=22),axis.text.x=element_text(size=18),
          axis.title.y = element_blank(),
          axis.text.y = element_blank())

plot_prc_2 <- together %>%
    select(age_k, PrC) %>%
    na.omit() %>%
    ggplot() +
    geom_point(aes(x = PrC, y = age_k), size = 1, alpha = 0.75) + geom_path(aes(x = PrC, y = age_k), size = 1, alpha = 0.75)+
    xlab("Principal curve") + ylab("Calibrated age (ka yr BP)")+
    theme_minimal() +
    scale_y_reverse(breaks = seq(0, 33, by = 5))+
    theme(axis.title=element_text(size=22),axis.text=element_text(size=18))
#       axis.title.y = element_blank(),
#      axis.text.y = element_blank())




grid.arrange(plot_prc_2, plot_FeTi_2, plot_SiTi_2,ncol=3,nrow=1)

plot_C

grid.arrange(plot_Ti, plot_Si, plot_Fe, ncol=3,nrow=1)



plot_N_log <- together %>%
    select(Depth, averaged.N)%>% 
    na.omit() %>%
    ggplot() +
    geom_point(aes(x = log(averaged.N), y = Depth), size = 1, alpha = 0.75) + geom_path(aes(x = log(averaged.N), y = Depth), size = 1, alpha = 0.75)+
    xlab("log Nitrogen (%)") +
    theme_minimal() +
    scale_y_reverse(breaks = seq(0, 172.2, by = 25))+
    theme(axis.title.x=element_text(size=22),axis.text.x=element_text(size=16),
          axis.title.y = element_blank(),
          axis.text.y = element_blank())

plot_C_log <- together %>%
    select(Depth, averaged.C) %>%
    na.omit() %>%
    ggplot() +
    geom_point(aes(x = log(averaged.C), y = Depth), size = 1, alpha = 0.75) + geom_path(aes(x = log(averaged.C), y = Depth), size = 1, alpha = 0.75)+
    xlab("log Carbon (%)") +
    theme_minimal() +
    scale_y_reverse(breaks = seq(0, 172.2, by = 25))+
    theme(axis.title.x=element_text(size=22),axis.text.x=element_text(size=16),
          axis.title.y = element_blank(),
          axis.text.y = element_blank())


plot_N_log_time <- together %>%
    select(median, averaged.N)%>% 
    na.omit() %>%
    ggplot() +
    geom_point(aes(x = log(averaged.N), y = median/1000), size = 1, alpha = 0.75) + geom_path(aes(x = log(averaged.N), y = median/1000), size = 1, alpha = 0.75)+
    xlab("log Nitrogen (%)") +
    ylab("Age (k cal yr BP)")+
    theme_minimal() +
    scale_y_reverse(limits=c(33,0),breaks = seq(0, 33, by = 2))+
    theme(axis.title=element_text(size=16,face="bold"),axis.text=element_text(size=16))
#  axis.title.y = element_blank(),
# axis.text.y = element_blank())

plot_C_log_time <- together %>%
    select(median, averaged.C) %>%
    na.omit() %>%
    ggplot() +
    geom_point(aes(x = log(averaged.C), y = median/1000), size = 1, alpha = 0.75) + geom_path(aes(x = log(averaged.C), y = median/1000), size = 1, alpha = 0.75)+
    xlab("log Carbon (%)") +
    theme_minimal() +
    scale_y_reverse(limits=c(33,0),breaks = seq(0, 33, by = 2))+
    theme(axis.title.x=element_text(size=16,face="bold"),axis.text.x=element_text(size=16),
          axis.title.y = element_blank(),
          axis.text.y = element_blank())


plot_C_N_time <- together %>%
    select(median, averaged.C.N) %>%
    na.omit() %>%
    ggplot() +
    geom_point(aes(y = median/1000, x = averaged.C.N), size = 1, alpha = 0.75)  + geom_path(aes(y = median/1000, x = averaged.C.N), size = 1, alpha = 0.75) +
    xlab("C:N ratio") +
    theme_minimal()+
    scale_y_reverse(limits=c(33,0),breaks = seq(0, 33, by = 2))+
    theme(axis.title.x=element_text(size=16,face = "bold"),axis.text.x=element_text(size=16),
          axis.title.y = element_blank(),
          axis.text.y = element_blank())

plot_d13C_time <- together %>%
    select(median, averaged.d13C) %>%
    na.omit() %>%
    ggplot() +
    geom_point(aes(x = averaged.d13C, y = median/1000), size = 1, alpha = 0.75) + geom_path(aes(x = averaged.d13C, y = median/1000), size = 1, alpha = 0.75)+
    xlab(expression(paste(delta^{13}, C[VPDB],"(\u2030)"))) +
    ylab("Age (k cal yr BP)")+
    theme_minimal() +
    scale_y_reverse(limits=c(33,0),breaks = seq(0, 33, by = 2))+
    theme(axis.title=element_text(size=16,face="bold"),axis.text=element_text(size=16))
#   axis.title.y = element_blank(),
#  axis.text.y = element_blank())

"\u03B4 ^13^ C[VPDB]\u2030"
plot_d15N_time <- together %>%
    select(median, averaged.d15N) %>%
    na.omit() %>%
    ggplot() +
    geom_point(aes(x = averaged.d15N, y = median/1000), size = 1, alpha = 0.75) + geom_path(aes(x = averaged.d15N, y = median/1000), size = 1, alpha = 0.75)+
    xlab(expression(paste(delta^{15}, N[VPDB],"(\u2030)")))  +
    theme_minimal() +
    scale_y_reverse(limits=c(33,0),breaks = seq(0, 33, by = 2))+
    theme(axis.title.x=element_text(size=16,face="bold"),axis.text.x=element_text(size=16),
          axis.title.y = element_blank(),
          axis.text.y = element_blank())


plot_conc_2_time <- together %>%
    select(median, d_g_wet_sed) %>%
    na.omit() %>%
    filter(median>216)%>%
    ggplot() +
    geom_point(aes(y = median/1000, x = sqrt(d_g_wet_sed)), size = 1, alpha = 0.75) + geom_path(aes(y = median/1000, x = sqrt(d_g_wet_sed)), size = 1, alpha = 0.75)+
    xlab("Diatom conc") +
    theme_minimal()+
    scale_y_reverse(limits= c(33,0),breaks = seq(0, 33, by = 2))+
    scale_x_continuous(breaks = seq(0, 600, by = 250))+
    theme(axis.title.x=element_text(size=16,face="bold"),axis.text.x=element_text(size=16),
          axis.title.y = element_blank(),
          axis.text.y = element_blank())

ggplotly(plot_conc_2_time)
ggplotly(plot_C_log)
log_C_N<- grid.arrange(plot_N_log,plot_C_log,ncol=2,nrow=1)

fig8<- grid.arrange(plot_N_log_time,plot_C_log_time,plot_C_N_time,plot_conc_2_time,ncol=4,nrow=1)
#fig8<- grid.arrange(plot_N_log_time,plot_C_log_time, plot_d13C_time,plot_d15N_time,plot_C_N_time,plot_conc_2_time,ncol=6,nrow=1)

fig8b<- grid.arrange(plot_d13C_time,plot_d15N_time,ncol=2,nrow=1)

ggsave("pyc_geom.png",fig8,path=here("Figs"),width=8,height=5)

ggsave("geom.png",fig8b,path=here("Figs"),width=6,height=5)


#png(filename="C:/Users/Maria Jose Rivera/OneDrive - James Cook University/Australia renamed/Sanamere/Thesis sections/PhD/other/log.png",width=250,height=700)
#plot(log_C_N)
#dev.off()
#?png


plot_C_N <- together %>%
    select(Depth, averaged.C.N) %>%
    na.omit() %>%
    ggplot() +
    geom_point(aes(y = Depth, x = averaged.C.N), size = 1, alpha = 0.75)  + geom_path(aes(y = Depth, x = averaged.C.N), size = 1, alpha = 0.75) +
    xlab("C:N ratio") +
    theme_minimal()+
    scale_y_reverse(breaks = seq(0, 172.2, by = 25))+
    theme(axis.title.x=element_text(size=22),axis.text.x=element_text(size=16),
          axis.title.y = element_blank(),
          axis.text.y = element_blank())
plot_C_N
ggplotly(plot_C_N)


with_hypy<-together%>%
    left_join(hypy_to_merge)

plot_TOC_hypy <- with_hypy%>%
    select(av_d13Cpost, averaged.d13C) %>%
    filter(averaged.d13C < -20)%>%
    na.omit() %>%
    ggplot(aes(x = averaged.d13C , y = av_d13Cpost)) +
    geom_smooth(method = "lm", formula = x ~ y)+
    ylab("C:N ratio") +
    theme_minimal()+
    #scale_y_reverse(breaks = seq(0, 172.2, by = 25))+
    theme(axis.title.x=element_text(size=16),
          axis.title.y = element_blank(),
          axis.text.y = element_blank())
plot_TOC_hypy
ggplotly(plot_TOC_hypy)

grid.arrange(plot_d15N, plot_d13C,ncol=2,nrow=1)
grid.arrange(plot_N_log, plot_C_log,ncol=2,nrow=1)

grid.arrange(plot_sed,plot_frac, ncol=2,nrow=1)
plot_05

table_str<-read.table("C:/Users/Maria Jose Rivera/OneDrive - James Cook University/Australia renamed/Sanamere/Thesis Sections/PhD/other/Hydro/Table_CONISS_ITRAX.csv",sep=",",skip=1)
colnames(table_str)<-c("Depth (cm)","Ti","Si","Fe","Description","Layer","Unit","Distance to base (cm)","Age range (ka cal yr BP)")

table_str_2<-select(table_str,-"Fe",-"Ti",-"Si")


table_str_22 <- table_str_2%>%
    mutate(Layer = c(rep("5", 17), rep("4", 3),rep("3",1),rep("2",4),rep("1",3)))

kable(table_str_22, "latex", booktabs = T, align = "c") %>%
    collapse_rows(columns = 3:4, latex_hline = "major", valign = "middle")


together_filter<-filter(together, averaged.d13C < -19)

plot_d15N_simple <- together %>%
    select(median, averaged.d15N) %>%
    na.omit() %>%
    ggplot() +
    geom_point(aes(x = averaged.d15N, y = median), size = 1, alpha = 0.75) + geom_path(aes(x = averaged.d15N, y = median), size = 1, alpha = 0.75)+
    xlab("d15N")  +
    theme_minimal() +
    scale_y_reverse()+
    theme(axis.title.x=element_text(size=22),axis.text.x=element_text(size=16),
          axis.title.y = element_blank(),
          axis.text.y = element_blank())
#ggplotly(plot_d15N_simple)
plot_ab <- with_hypy%>%
    select(averaged.C, Corrected)%>% 
    na.omit() %>%
    ggplot(aes(x = averaged.C , y = Corrected)) +
    geom_point(aes(x = averaged.C, y = Corrected), size = 1, alpha = 0.75) +
    ylab("") +
    theme_minimal()+
    #scale_y_reverse(breaks = seq(0, 172.2, by = 25))+
    theme(axis.title.x=element_text(size=16),
          axis.title.y = element_blank(),
          axis.text.y = element_blank())
plot_ab

water<-together%>%
    select(Water,Depth)%>%
    na.omit()%>%
    ggplot(aes(x = Water , y = Depth)) +
    geom_point(aes(x = Water , y = Depth), size = 1, alpha = 0.75) +
    ylab("") +
    theme_minimal()+
    scale_y_reverse(limits=c(172.2,0),breaks = seq(0, 172.2, by = 20))+
    theme(axis.title.x=element_text(size=16),
          axis.title.y = element_blank(),
          axis.text.y = element_blank())

print(water)

d_b_den<-together%>%
    select(Dry_bulk_density2,Depth)%>%
    na.omit()%>%
    ggplot(aes(x = Dry_bulk_density2 , y = Depth)) +
    geom_point(aes(x = Dry_bulk_density2 , y = median/1000), size = 1, alpha = 0.75) +
    ylab("") +
    theme_minimal()+
    scale_y_reverse(limits=c(33,0),breaks = seq(0, 33, by = 2))+
    theme(axis.title.x=element_text(size=16),
          axis.title.y = element_blank(),
          axis.text.y = element_blank())
print(d_b_den)

##---- str-sed

plot_water <- together %>%
    select(Depth, Water) %>%
    filter(Water> 0, Depth!=86)%>%
    na.omit() %>%
    ggplot() +
    geom_point(aes(x = Water, y = Depth), size = 1, alpha = 0.75) +  geom_path(aes(x = Water, y = Depth), size = 1, alpha = 0.75)+
    ylab("Depth (cm)")  +
    xlab("Water (%)") +
    theme_minimal(base_size = 15)+
    scale_y_reverse(limits=c(172.2,0),breaks = seq(0, 172.2, by = 20))+
    theme(axis.title=element_text(size=16,face="bold"),axis.text=element_text(size=16))
#   axis.title.y = element_blank(),
#  axis.text.y = element_blank())

plot_density <- together %>%
    select(Depth, Dry_bulk_density2) %>%
    #   filter(Wat)%>%
    na.omit() %>%
    ggplot() +
    geom_point(aes(x = Dry_bulk_density2, y = Depth), size = 1, alpha = 0.75) +  geom_path(aes(x = Dry_bulk_density2, y = Depth), size = 1, alpha = 0.75)+
    ylab("Depth (cm)")  +
    xlab("Dry bulk density") +
    theme_minimal(base_size = 15)+
    scale_y_reverse(limits=c(172.2,0),breaks = seq(0, 172.2, by = 20))+
    theme(axis.title=element_text(size=16,face="bold"),axis.text=element_text(size=16),
          axis.title.y = element_blank(),
          axis.text.y = element_blank())


plot_frac_2 <- together %>%
    select(Depth, Fraction,Percentage) %>%
    na.omit() %>%
    unique()%>%
    ggplot() +
    geom_point(aes(x = Percentage , y = Depth, color=Fraction))+ geom_path(aes(x = Percentage, y = Depth, color=Fraction))+
    xlab("Percentage (%)") +
    scale_y_reverse(limits=c(172.2,0),breaks = seq(0, 172.2, by = 20))+
    theme_minimal(base_size = 15)+
    theme(axis.title.x=element_text(size=18,face="bold"),axis.text.x=element_text(size=14),
          axis.title.y = element_blank(),
          axis.text.y = element_blank(), legend.position="none")+ annotate("text",x=60,y=20,label="Silt",size=8,color="blue")+
    annotate("text",x=40,y=100,label="Clay",size=8,color="green")+
    annotate("text",x=35,y=160,label="Sand",size=6,color="red")

plot_05_a <- together %>%
    select(Depth, d.0.5) %>%
    na.omit() %>%
    ggplot() +
    geom_point(aes(x = d.0.5, y = Depth), size = 1, alpha = 0.75) + geom_path(aes(x = d.0.5, y = Depth), size = 1, alpha = 0.75)+
    xlab("Median grain size (\u03BCm)") +
    theme_minimal(base_size = 15) +
    scale_y_reverse(limits=c(172.2,0),breaks = seq(0, 172.2, by = 20))+
    theme(axis.title.x=element_text(size=18,face="bold"),axis.text.x=element_text(size=14),
          axis.title.y = element_blank(),
          axis.text.y = element_blank())

#?png

str_sed<-grid.arrange(plot_water,plot_density,plot_frac_2,plot_05_a,nrow=1,ncol=4)

str_sed_2<-grid.arrange(plot_sed,plot_frac_2,plot_05_a,nrow=1,ncol=3)

ggsave("fig_str.png",str_sed,width=12,height=8,path=here("Figs"))




'/Users/Maria Jose Rivera/OneDrive - James Cook University/Australia renamed/Sanamere/Thesis sections/PhD/experiments/exp_radiocarbon/data/model_BO2.csv