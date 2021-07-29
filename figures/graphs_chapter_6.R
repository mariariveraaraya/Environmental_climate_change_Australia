together_3<-together_2%>%
        left_join(sizes3_2)%>%
        left_join(tial)

#write.csv(together_3, file="together_3.csv")

together_3<-read.csv(here("together_3.csv"))
sea_level<-read.csv(here("data","sea_level_north_AU.csv"))

sea_level_all<-read.csv(here("data","all_sea_level.csv"))

level_graph<- sea_level_all%>% ggplot() +
        geom_point(aes(x = level, y = age), size = 1, alpha = 0.75) + 
        xlab("Sea level (m)") +
        ylab("Age (ka)")+
        theme_bw(base_size = 20) +
        scale_y_reverse(limits=c(33,0),breaks = seq(0, 33, by = 2))+
       theme(
              axis.title.y = element_blank(), axis.ticks.y= element_blank(),
              axis.text.y = element_blank()) 
        #   geom_hline(yintercept=29, color="red")+
       # annotate("rect", xmin=c(-150),xmax=c(Inf),ymin=c(29) , ymax=c(33), alpha=0.2, color="blue", fill="blue")

        #scale_x_continuous(limits=c(0,2),breaks = seq(0, 2, by = 1))+
        #theme(axis.title=element_text(size=22), axis.text=element_text(size=16))+ 
       # geom_hline(yintercept=29, color="red")

level_graph_2<- sea_level_all%>% ggplot() +
        geom_point(aes(x = age, y = level), size = 1, alpha = 0.75) + 
        xlab("Age (ka)") +
        ylab("Sea level (m)")+
        theme_bw(base_size = 20) +
        scale_y_continuous(limits=c(-150,2),breaks = seq(-150, 2, by = 25))+
        theme(axis.title=element_text(face="bold"))

png(
        "Figs/Background/sea_level_3.png", 
        width = 6, 
        height = 6, 
        res = 200,
        units = "in"
)

level_graph_2


dev.off()

##### original

plot_sed_time_2 <- together_3 %>%
        select(median, sedrate.mm) %>%
        na.omit() %>%
        ggplot() +
        geom_point(aes(x = sedrate.mm, y = (median/1000)), size = 1, alpha = 0.75) + geom_path(aes(x = sedrate.mm, y = (median/1000)), size = 1, alpha = 0.75)+
        xlab("Sed rate (mm/yr)") +
        ylab("Age (ka)")+
        theme_minimal(base_size = 20) +
        scale_y_reverse(limits=c(33,0),breaks = seq(0, 33, by = 2))+
        scale_x_continuous(limits=c(0,2),breaks = seq(0, 2, by = 1))
        theme(axis.title=element_text(size=22), axis.text=element_text(size=16))
       # geom_hline(yintercept=29, color="red")
     #   annotate("rect", xmin=c(0),xmax=c(Inf),ymin=c(29) , ymax=c(33), alpha=0.2, color="blue", fill="blue")

si_ti_2<-together_3%>% 
        select(median,roll_Si_Al)%>%
        na.omit()%>%
        ggplot(aes(x = roll_Si_Al, y = median/1000)) +
        # geom_path() +
        geom_point() +
        labs(x="Si:Al",y="Age (ka)")+
        theme_bw(base_size = 20)+
        #   geom_smooth(method = "gam")+
        scale_y_reverse(limits=c(33,0),breaks = seq(0, 33, by = 2))
       # theme(axis.title.x=element_text(size=22), axis.text=element_text(size=16),
        #      axis.title.y = element_blank(),
         #     axis.text.y = element_blank()) 
    #    geom_hline(yintercept=29, color="red")


coarse_sand<-together_3%>% 
        select(median,sumdepth_sand_coarse)%>%
        na.omit()%>%
        ggplot(aes(x = sumdepth_sand_coarse, y = median/1000)) +
        # geom_path() +
        geom_point() +
        labs(x="Coarse sand (%)",y="Age (ka)")+
        theme_bw(base_size = 20)+
        #   geom_smooth(method = "gam")+
        scale_y_reverse(limits=c(33,0),breaks = seq(0, 33, by = 2))+
        theme(axis.title.x=element_text(size=22), axis.text=element_text(size=16),
              axis.title.y = element_blank(), axis.ticks.y= element_blank(),
              axis.text.y = element_blank()) 
      #  geom_hline(yintercept=29, color="red")
coarse_sand<-sizes3_2%>% 
        select(median,sumdepth_sand_coarse)%>%
        na.omit()%>%
        ggplot(aes(x = sumdepth_sand_coarse, y = median/1000)) +
        # geom_path() +
        geom_point() +
        labs(x="Coarse sand (%)",y="Age (ka)")+
        theme_bw(base_size = 20)+
        #   geom_smooth(method = "gam")+
        scale_y_reverse(limits=c(33,0),breaks = seq(0, 33, by = 2))+
        theme(axis.title.x=element_text(size=22), axis.text=element_text(size=16),
              axis.title.y = element_blank(), axis.ticks.y= element_blank(),
              axis.text.y = element_blank()) 

Ti<-together_3%>% 
        select(median,NorTi.x)%>%
        na.omit()%>%
        ggplot(aes(x = NorTi.x, y = median/1000)) +
        # geom_path() +
        geom_point() +
        labs(x="Ti",y="Age (ka)")+
        theme_bw(base_size = 20)+
        #   geom_smooth(method = "gam")+
        scale_y_reverse(limits=c(33,0),breaks = seq(0, 33, by = 2))+
        theme(axis.title.x=element_text(size=22), axis.text=element_text(size=16),
              axis.title.y = element_blank(), axis.ticks.y= element_blank(),
              axis.text.y = element_blank()) 
      #  geom_hline(yintercept=29, color="red")

png(
        "other/Hydro/erosion.png", 
        width = 11, 
        height = 6, 
        res = 200,
        units = "in"
)



grid.arrange(si_ti_2,coarse_sand,Ti,level_graph,ncol=4,nrow=1)

dev.off()

sea<- sea_level%>%
        ggplot(aes(x = Level, y = Age)) +
        # geom_path() +
        geom_point() +
        labs(x="Sea Level(RSL)",y="Age (ka)")+
        theme_minimal()+
        #   geom_smooth(method = "gam")+
        scale_y_reverse(limits=c(33,0),breaks = seq(0, 33, by = 2))+
        theme(axis.title.x=element_text(size=22), axis.text=element_text(size=16),
              axis.title.y = element_blank(),
              axis.text.y = element_blank())+ 
        geom_hline(yintercept=29, color="red")

grid.arrange(plot_sed_time_2,si_ti_2,coarse_sand,Ti,ncol=4,nrow=1)

png(
        "other/Hydro/sed.png", 
        width = 7.5, 
        height = 6, 
        res = 200,
        units = "in"
)


grid.arrange(plot_sed_time_2,si_ti_2,coarse_sand,Ti,ncol=4,nrow=1)


dev.off()

#### Rectangle

plot_sed_time_2 <- together_3 %>%
        select(median, sedrate.mm) %>%
        na.omit() %>%
        ggplot() +
        geom_point(aes(x = sedrate.mm, y = (median/1000)), size = 1, alpha = 0.75) + geom_path(aes(x = sedrate.mm, y = (median/1000)), size = 1, alpha = 0.75)+
        xlab("Sed rate (mm/yr)") +
        ylab("Age (k cal yr BP)")+
        theme_minimal() +
        scale_y_reverse(limits=c(33,0),breaks = seq(0, 33, by = 2))+
        theme(axis.title=element_text(size=22), axis.text=element_text(size=16))+ 
     #   geom_hline(yintercept=29, color="red")
 annotate("rect", xmin=c(0),xmax=c(Inf),ymin=c(29) , ymax=c(33), alpha=0.2, color="blue", fill="blue")

si_ti_2<-together_3%>% 
        select(median,roll_Si_Al)%>%
        na.omit()%>%
        ggplot(aes(x = roll_Si_Al, y = median/1000)) +
        # geom_path() +
        geom_point() +
        labs(x="Si:Al",y="Age (ka)")+
        theme_minimal()+
        #   geom_smooth(method = "gam")+
        scale_y_reverse(limits=c(33,0),breaks = seq(0, 33, by = 2))+
        theme(axis.title.x=element_text(size=22), axis.text=element_text(size=16),
              axis.title.y = element_blank(),
              axis.text.y = element_blank())+ 
      #  geom_hline(yintercept=29, color="red")+
annotate("rect", xmin=c(0),xmax=c(Inf),ymin=c(29) , ymax=c(33), alpha=0.2, color="blue", fill="blue")

coarse_sand<-together_3%>% 
        select(median,sumdepth_sand_coarse)%>%
        na.omit()%>%
        ggplot(aes(x = sumdepth_sand_coarse, y = median/1000)) +
        # geom_path() +
        geom_point() +
        labs(x="Coarse sand",y="Age (ka)")+
        theme_minimal()+
        #   geom_smooth(method = "gam")+
        scale_y_reverse(limits=c(33,0),breaks = seq(0, 33, by = 2))+
        theme(axis.title.x=element_text(size=22), axis.text=element_text(size=16),
              axis.title.y = element_blank(),
              axis.text.y = element_blank())+ 
     #   geom_hline(yintercept=29, color="red")+
        annotate("rect", xmin=c(0),xmax=c(Inf),ymin=c(29) , ymax=c(33), alpha=0.2, color="blue", fill="blue")


Ti<-together_3%>% 
        select(median,NorTi.x)%>%
        na.omit()%>%
        ggplot(aes(x = NorTi.x, y = median/1000)) +
        # geom_path() +
        geom_point() +
        labs(x="Ti",y="Age (ka)")+
        theme_minimal()+
        #   geom_smooth(method = "gam")+
        scale_y_reverse(limits=c(33,0),breaks = seq(0, 33, by = 2))+
        theme(axis.title.x=element_text(size=22), axis.text=element_text(size=16),
              axis.title.y = element_blank(),
              axis.text.y = element_blank())+ 
   #     geom_hline(yintercept=29, color="red")+
        annotate("rect", xmin=c(0),xmax=c(Inf),ymin=c(29) , ymax=c(33), alpha=0.2, color="blue", fill="blue")

level_graph_a<- sea_level_all%>% ggplot() +
        geom_point(aes(x = level, y = age), size = 1, alpha = 0.75) + 
        xlab("Sea level (m)") +
        ylab("Age (k cal yr BP)")+
        theme_minimal(base_size = 20) +
        scale_y_reverse(limits=c(33,0),breaks = seq(0, 33, by = 2))+
        theme(axis.title.x=element_text(size=22), axis.text=element_text(size=16),
              axis.title.y = element_blank(),
              axis.text.y = element_blank())+ 
        #   geom_hline(yintercept=29, color="red")+
        annotate("rect", xmin=c(-150),xmax=c(Inf),ymin=c(29) , ymax=c(33), alpha=0.2, color="blue", fill="blue")


grid.arrange(plot_sed_time_2,si_ti_2,coarse_sand,Ti,ncol=4,nrow=1)

#### Rectangle 2

plot_sed_time_2 <- together_3 %>%
        select(median, sedrate.mm) %>%
        na.omit() %>%
        ggplot() +
        geom_point(aes(x = sedrate.mm, y = (median/1000)), size = 1, alpha = 0.75) + geom_path(aes(x = sedrate.mm, y = (median/1000)), size = 1, alpha = 0.75)+
        xlab("Sed rate (mm/yr)") +
        ylab("Age (k cal yr BP)")+
        theme_minimal() +
        scale_y_reverse(limits=c(33,0),breaks = seq(0, 33, by = 2))+
        theme(axis.title=element_text(size=22), axis.text=element_text(size=16))+ 
        #   geom_hline(yintercept=29, color="red")
        annotate("rect", xmin=c(0),xmax=c(Inf),ymin=c(20) , ymax=c(29), alpha=0.2, color="blue", fill="blue")

si_ti_2<-together_3%>% 
        select(median,roll_Si_Al)%>%
        na.omit()%>%
        ggplot(aes(x = roll_Si_Al, y = median/1000)) +
        # geom_path() +
        geom_point() +
        labs(x="Si:Al",y="Age (ka)")+
        theme_minimal()+
        #   geom_smooth(method = "gam")+
        scale_y_reverse(limits=c(33,0),breaks = seq(0, 33, by = 2))+
        theme(axis.title.x=element_text(size=22), axis.text=element_text(size=16),
              axis.title.y = element_blank(),
              axis.text.y = element_blank())+ 
        #  geom_hline(yintercept=29, color="red")+
        annotate("rect", xmin=c(0),xmax=c(Inf),ymin=c(20) , ymax=c(29), alpha=0.2, color="blue", fill="blue")


coarse_sand<-together_3%>% 
        select(median,sumdepth_sand_coarse)%>%
        na.omit()%>%
        ggplot(aes(x = sumdepth_sand_coarse, y = median/1000)) +
        # geom_path() +
        geom_point() +
        labs(x="Coarse sand",y="Age (ka)")+
        theme_minimal()+
        #   geom_smooth(method = "gam")+
        scale_y_reverse(limits=c(33,0),breaks = seq(0, 33, by = 2))+
        theme(axis.title.x=element_text(size=22), axis.text=element_text(size=16),
              axis.title.y = element_blank(),
              axis.text.y = element_blank())+ 
        #   geom_hline(yintercept=29, color="red")+
        annotate("rect", xmin=c(0),xmax=c(Inf),ymin=c(20) , ymax=c(29), alpha=0.2, color="blue", fill="blue")



Ti<-together_3%>% 
        select(median,NorTi.x)%>%
        na.omit()%>%
        ggplot(aes(x = NorTi.x, y = median/1000)) +
        # geom_path() +
        geom_point() +
        labs(x="Ti",y="Age (ka)")+
        theme_minimal()+
        #   geom_smooth(method = "gam")+
        scale_y_reverse(limits=c(33,0),breaks = seq(0, 33, by = 2))+
        theme(axis.title.x=element_text(size=22), axis.text=element_text(size=16),
              axis.title.y = element_blank(),
              axis.text.y = element_blank())+ 
        #     geom_hline(yintercept=29, color="red")+
        annotate("rect", xmin=c(0),xmax=c(Inf),ymin=c(20) , ymax=c(29), alpha=0.2, color="blue", fill="blue")



grid.arrange(plot_sed_time_2,si_ti_2,coarse_sand,Ti,ncol=4,nrow=1)

#### Rectangle 3

plot_sed_time_2 <- together_3 %>%
        select(median, sedrate.mm) %>%
        na.omit() %>%
        ggplot() +
        geom_point(aes(x = sedrate.mm, y = (median/1000)), size = 1, alpha = 0.75) + geom_path(aes(x = sedrate.mm, y = (median/1000)), size = 1, alpha = 0.75)+
        xlab("Sed rate (mm/yr)") +
        ylab("Age (k cal yr BP)")+
        theme_minimal() +
        scale_y_reverse(limits=c(33,0),breaks = seq(0, 33, by = 2))+
        theme(axis.title=element_text(size=22), axis.text=element_text(size=16))+ 
        #   geom_hline(yintercept=29, color="red")
        annotate("rect", xmin=c(0),xmax=c(Inf),ymin=c(20) , ymax=c(10), alpha=0.2, color="blue", fill="blue")

si_ti_2<-together_3%>% 
        select(median,roll_Si_Al)%>%
        na.omit()%>%
        ggplot(aes(x = roll_Si_Al, y = median/1000)) +
        # geom_path() +
        geom_point() +
        labs(x="Si:Al",y="Age (ka)")+
        theme_minimal()+
        #   geom_smooth(method = "gam")+
        scale_y_reverse(limits=c(33,0),breaks = seq(0, 33, by = 2))+
        theme(axis.title.x=element_text(size=22), axis.text=element_text(size=16),
              axis.title.y = element_blank(),
              axis.text.y = element_blank())+ 
        #  geom_hline(yintercept=29, color="red")+
        annotate("rect", xmin=c(0),xmax=c(Inf),ymin=c(20) , ymax=c(10), alpha=0.2, color="blue", fill="blue")


coarse_sand<-together_3%>% 
        select(median,sumdepth_sand_coarse)%>%
        na.omit()%>%
        ggplot(aes(x = sumdepth_sand_coarse, y = median/1000)) +
        # geom_path() +
        geom_point() +
        labs(x="Coarse sand",y="Age (ka)")+
        theme_minimal()+
        #   geom_smooth(method = "gam")+
        scale_y_reverse(limits=c(33,0),breaks = seq(0, 33, by = 2))+
        theme(axis.title.x=element_text(size=22), axis.text=element_text(size=16),
              axis.title.y = element_blank(),
              axis.text.y = element_blank())+ 
        #   geom_hline(yintercept=29, color="red")+
        annotate("rect", xmin=c(0),xmax=c(Inf),ymin=c(20) , ymax=c(10), alpha=0.2, color="blue", fill="blue")



Ti<-together_3%>% 
        select(median,NorTi.x)%>%
        na.omit()%>%
        ggplot(aes(x = NorTi.x, y = median/1000)) +
        # geom_path() +
        geom_point() +
        labs(x="Ti",y="Age (ka)")+
        theme_minimal()+
        #   geom_smooth(method = "gam")+
        scale_y_reverse(limits=c(33,0),breaks = seq(0, 33, by = 2))+
        theme(axis.title.x=element_text(size=22), axis.text=element_text(size=16),
              axis.title.y = element_blank(),
              axis.text.y = element_blank())+ 
        #     geom_hline(yintercept=29, color="red")+
        annotate("rect", xmin=c(0),xmax=c(Inf),ymin=c(20) , ymax=c(10), alpha=0.2, color="blue", fill="blue")



grid.arrange(plot_sed_time_2,si_ti_2,coarse_sand,Ti,ncol=4,nrow=1)


#### Rectangle 4

plot_sed_time_2 <- together_3 %>%
        select(median, sedrate.mm) %>%
        na.omit() %>%
        ggplot() +
        geom_point(aes(x = sedrate.mm, y = (median/1000)), size = 1, alpha = 0.75) + geom_path(aes(x = sedrate.mm, y = (median/1000)), size = 1, alpha = 0.75)+
        xlab("Sed rate (mm/yr)") +
        ylab("Age (k cal yr BP)")+
        theme_minimal() +
        scale_y_reverse(limits=c(33,0),breaks = seq(0, 33, by = 2))+
        theme(axis.title=element_text(size=22), axis.text=element_text(size=16))+ 
        #   geom_hline(yintercept=29, color="red")
        annotate("rect", xmin=c(0),xmax=c(Inf),ymin=c(0) , ymax=c(9.7), alpha=0.2, color="blue", fill="blue")

si_ti_2<-together_3%>% 
        select(median,roll_Si_Al)%>%
        na.omit()%>%
        ggplot(aes(x = roll_Si_Al, y = median/1000)) +
        # geom_path() +
        geom_point() +
        labs(x="Si:Al",y="Age (ka)")+
        theme_minimal()+
        #   geom_smooth(method = "gam")+
        scale_y_reverse(limits=c(33,0),breaks = seq(0, 33, by = 2))+
        theme(axis.title.x=element_text(size=22), axis.text=element_text(size=16),
              axis.title.y = element_blank(),
              axis.text.y = element_blank())+ 
        #  geom_hline(yintercept=29, color="red")+
        annotate("rect", xmin=c(0),xmax=c(Inf),ymin=c(0) , ymax=c(9.7), alpha=0.2, color="blue", fill="blue")


coarse_sand<-together_3%>% 
        select(median,sumdepth_sand_coarse)%>%
        na.omit()%>%
        ggplot(aes(x = sumdepth_sand_coarse, y = median/1000)) +
        # geom_path() +
        geom_point() +
        labs(x="Coarse sand",y="Age (ka)")+
        theme_minimal()+
        #   geom_smooth(method = "gam")+
        scale_y_reverse(limits=c(33,0),breaks = seq(0, 33, by = 2))+
        theme(axis.title.x=element_text(size=22), axis.text=element_text(size=16),
              axis.title.y = element_blank(),
              axis.text.y = element_blank())+ 
        #   geom_hline(yintercept=29, color="red")+
        annotate("rect", xmin=c(0),xmax=c(Inf),ymin=c(0) , ymax=c(9.7), alpha=0.2, color="blue", fill="blue")



Ti<-together_3%>% 
        select(median,NorTi.x)%>%
        na.omit()%>%
        ggplot(aes(x = NorTi.x, y = median/1000)) +
        # geom_path() +
        geom_point() +
        labs(x="Ti",y="Age (ka)")+
        theme_minimal()+
        #   geom_smooth(method = "gam")+
        scale_y_reverse(limits=c(33,0),breaks = seq(0, 33, by = 2))+
        theme(axis.title.x=element_text(size=22), axis.text=element_text(size=16),
              axis.title.y = element_blank(),
              axis.text.y = element_blank())+ 
        #     geom_hline(yintercept=29, color="red")+
        annotate("rect", xmin=c(0),xmax=c(Inf),ymin=c(0) , ymax=c(9.7), alpha=0.2, color="blue", fill="blue")



grid.arrange(plot_sed_time_2,si_ti_2,coarse_sand,Ti,ncol=4,nrow=1)
