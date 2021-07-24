
cbbPalette <- c("#000000", "#2A00E5", "#56B4E9", "#1FC910", "#69D3A4", "#C91025", "#CCCC00", "#FFFFCC")




e3<-ggplot(all2)+ geom_errorbar(data = all2, aes(x=Depth,ymin = to_95, ymax = from_95, group=Carbon_fraction,color=Carbon_fraction),width=10,size=1)+ xlab("Depth (cm)")+ ylab("Calibrated date (BP)")+
        scale_x_continuous(breaks=c(6,12,32,41,65,135,144,160))+scale_y_continuous(breaks=seq(0, 32000, by=5000))+theme_bw()+ scale_colour_manual(values=cbbPalette)


e3 + guides(color = guide_legend(override.aes = list(size=5))) + scale_fill_manual(name="",breaks=c("size"),labels=c(" "))


#################################

maria2<-ggplot(EA2, aes(y=averaged.C,x=mean))+geom_point()+ ggtitle("")+ylab("%Carbon")+xlab("Identifier")+geom_point()+ theme_bw() +
        theme(axis.text.y=element_text(size=12),axis.title.y=element_text(size=12,face="bold"))

maria2

maria3 <- ggplot(EA2, aes(x=averaged.d13C,y=mean))+geom_point() +  scale_y_reverse(breaks = seq(0, 32500, by = 2500)) + scale_x_continuous(limits=c(-28, -22), breaks=c(-28, -26, -24, -22)) + ggtitle("")+xlab("\u03B4^13")+ylab("Age (cal yr BP)")+theme_bw() + theme(axis.text.x=element_text(size=12),axis.title.x=element_text(size=12,face="bold"),axis.title.y = element_text(size=12,face="bold"))

maria3 <- ggplot(EA2, aes(x=averaged.d13C,y=mean))+geom_point() +  scale_y_reverse(breaks = seq(0, 32500, by = 2500)) + scale_x_continuous(limits=c(-28, -22), breaks=c(-28, -26, -24, -22)) + ggtitle("")+ labs(x= expression("\u03B4"^13), y="Age")+theme_bw() + theme(axis.text.x=element_text(size=12),axis.title.x=element_text(size=12,face="bold"),axis.title.y = element_text(size=12,face="bold"))


maria3 <- ggplot(EA2, aes(x=Depth,y=averaged.d13C))+geom_point() +  scale_x_continuous(breaks = seq(0, 175, by = 20)) + scale_y_continuous(limits=c(-28, -22), breaks=c(-28, -26, -24, -22)) + ggtitle("")+ labs(y= expression(bold(delta^13)),y = "Depth (cm)")+theme_bw() + xlab("Depth (cm)")+ theme(axis.text.x=element_text(size=12),axis.title.x=element_text(size=12,face="bold"),axis.title.y = element_text(size=12,face="bold"))


#print(p10)

#p1 <- ggplot(merged.ages, aes(x=d13C,y=mean))+geom_point() +  scale_y_reverse() + scale_x_continuous(limits=c(-28, -22), breaks=c(-28, -26, -24, -22)) + ggtitle("")

#p1 <- ggplot(merged.ages, aes(x=mean,y=d13C))+geom_point() + scale_y_continuous(limits=c(-28, -22), breaks=c(-28, -26, -24, -22)) + ggtitle("")



maria4 <- ggplot(EA2, aes(x=Depth,y=averaged.C))+geom_point()+  scale_y_continuous(breaks = seq(0, 50, by = 10)) +scale_x_continuous(breaks = seq(0, 175, by = 20)) +ylab("% C")+xlab("Calibrated date BP")+ggtitle("")+theme_bw()+ theme(axis.title.x=element_blank(),
                                                                                                                                                                                                                                          axis.text.x=element_blank(),axis.text.y=element_text(size=12),axis.title.y=element_text(size=12,face="bold"))


#print(p2)
#grid.arrange(p10,p2,ncol=2)

###to delete y axis not neccesary here (maybe)

maria5<-ggplot(EA2, aes(x=Depth,y=averaged.C.N))+geom_point()+ scale_y_continuous(limits=c(0, 40), breaks=seq(0,40,by=10)) + ggtitle("") +ylab("C/N ratio")+xlab("cal yr BP")+theme_bw()+ scale_x_continuous(breaks = seq(0, 175, by = 20))+theme(axis.title.x=element_blank(),
                                                                                                                                                                                                                                                  axis.text.x=element_blank(),axis.text.y=element_text(size=12),axis.title.y=element_text(size=12,face="bold"))


grid.arrange(maria5,maria4,maria3,nrow=3)


#####################
