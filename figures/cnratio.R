

cn<-together_1%>%
        select(Depth, averaged.C.N, averaged.d15N)%>%
        ggplot(aes(y = averaged.C.N, x = averaged.d15N)) +
        geom_path() +
        geom_point() +
        #theme(strip.text.x = element_text(size = 14, colour = "red"))+
       # scale_y_reverse(limits=c(172.2,0),breaks = seq(0, 172.2, by = 10)) +
      #  scale_x_continuous(labels = scales::number_format(accuracy = 0.1))+
        labs(x = "d15N", y = "CN ratio")+
        theme_bw(base_size = 20)+ theme(panel.grid.minor = element_blank())


cn2<-together_1%>%
        select(Depth, averaged.d13C, averaged.d15N)%>%
        ggplot(aes(x = averaged.d13C, y = averaged.d15N)) +
        geom_path() +
        geom_point() +
        #theme(strip.text.x = element_text(size = 14, colour = "red"))+
        # scale_y_reverse(limits=c(172.2,0),breaks = seq(0, 172.2, by = 10)) +
        #  scale_x_continuous(labels = scales::number_format(accuracy = 0.1))+
        labs(y = "d15N", x = "d13C")+
        theme_bw(base_size = 20)+ theme(panel.grid.minor = element_blank())

cn3<-together_1%>%
        select(Depth, averaged.C, averaged.N)%>%
        ggplot(aes(x = averaged.C, y = averaged.N)) +
        geom_path() +
        geom_point() +
        #theme(strip.text.x = element_text(size = 14, colour = "red"))+
        # scale_y_reverse(limits=c(172.2,0),breaks = seq(0, 172.2, by = 10)) +
        #  scale_x_continuous(labels = scales::number_format(accuracy = 0.1))+
        labs(y = "% N", x = " % C")+
        theme_bw(base_size = 20)+ theme(panel.grid.minor = element_blank())


cn4<-together_1%>%
        select(Depth, averaged.C, averaged.N)%>%
        ggplot(aes(x = Depth, y = averaged.N)) +
        geom_path() +
        geom_point() +
        #theme(strip.text.x = element_text(size = 14, colour = "red"))+
        # scale_y_reverse(limits=c(172.2,0),breaks = seq(0, 172.2, by = 10)) +
        #  scale_x_continuous(labels = scales::number_format(accuracy = 0.1))+
        labs(y = "% N", x = "Depth (cm)")+
        theme_bw(base_size = 20)+ theme(panel.grid.minor = element_blank())


