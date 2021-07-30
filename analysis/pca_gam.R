# Principal component analysis, generalised additive models


san_final_model10<-read.csv("processed_data","san_final_model10.csv")

prcurve_ITRAX_depth<-san_final_model10%>%
    select(13:17,28:32)%>%
    select(-7,-8)%>%
    drop_na()

prcurve_ITRAX3<-select(prcurve_ITRAX,(-(8:10)))

prcurve_ITRAX_filter<-select(prcurve_ITRAX3,-NorK,-NorCa,-NorSr)

prcurve_ITRAX2<-prcurve(prcurve_ITRAX3,method='ca',trace = TRUE,plotit = TRUE,vary = TRUE,penalty=1.4,smoother = smoothGAM)


varExpl(prcurve_ITRAX2)
# Extract pricipal curve scores
scrs<-scores(prcurve_ITRAX2)
# Plot curve vs. Age
plot(scrs~prcurve_ITRAX$median, type="l")



prc_curve_scores<-as.data.frame(scrs,prcurve_ITRAX$median)
prc_curve_scores_2<-as.data.frame(scrs,prcurve_ITRAX$Depth)

setDT(prc_curve_scores, keep.rownames = TRUE)[]
prc_curve_scores$rn<- as.numeric(as.character(prc_curve_scores$rn))

prc_curve_scores2<-rename(prc_curve_scores,"median"="rn")

setDT(prc_curve_scores_2, keep.rownames = TRUE)[]
prc_curve_scores_2$Depth<- as.numeric(as.character(prc_curve_scores_2$Depth))

d1s <- scale(prcurve_ITRAX3, center=T, scale=T)

pca <- prcomp(prcurve_ITRAX3, center=T, scale.=T)
dissim<- vegdist(prcurve_ITRAX3, method="bray")
plot(pca1$PC1~prcurve_ITRAX$mean, type="l")


pca_filter<-prcomp(prcurve_ITRAX_filter)
autoplot(pca_filter)
plot(prcomp(prcurve_ITRAX3))

pca1
print(pca1)
pca1<-as.data.frame(pca$x)

plot(pca22$PC1~pca22$mean,type="l")

#mod_pca <- gam(PC1~ s(mean,k=30), data = pca22, method = "REML")

# Generalised additive model

mod_pc1 <- gam(PC1 ~ s(mean,k=30), data = pca22, method = "REML")


plot(mod_pc1)



pca <- prcomp(d1s, center=F, scale.=F)

#dissim<- vegdist(d1s, method="bray")

#dissimilarity matrix computation

clust <- chclust(dissim, method="coniss")

# Ball and Stick
#groups<- bstick(clust, ng=50, plot=T) #determine the appropriate number of significant clusters
groups<- bstick(clust,ng=100)#ng=100
ngroups<- groups$nGroups[which(groups$dispersion <= groups$bstick)]
ngroups<- ngroups[2]
cc<- cutree(clust, k = ngroups)


prcurve_ITRAX4<-mutate(prcurve_ITRAX,age_ka=mean/1000)

prcurve_ITRAX333<-prcurve_ITRAX3%>%
    rename( "Si"="NorSi","Ti"="NorTi","Ca"="NorCa","K"="NorK","Al"="NorAl","Fe"="NorFe","Sr"="NorSr")
#png("cluster_graph.png",width=500,height=500)

png(
    "other/Hydro/ITRAX_clus_1.png", 
    width = 11, 
    height = 5, 
    res = 300,
    units = "in"
)

# STRAT plot
par(oma=c(2,1,1,1.2))
#strat.plot(prcurve_ITRAX333, yvar=prcurve_ITRAX4$age_ka, clust=clust, y.rev=TRUE, cex.axis=0.6, cex.yaxis=0.8, cex.ylabel=0.8, cex.lab=0.6, ylab="ka cal yr BP",col.line="black", col.bar="black", las=3, mgp=c(3,1,0))
# check that strat plot is ok

#x<-strat.plot(prcurve_ITRAX333, yvar=prcurve_ITRAX4$age_ka, clust=clust, y.rev=TRUE, cex.axis=0.7, cex.yaxis=0.8, cex.ylabel=1, cex.lab=0.8, ylab="ka cal yr BP",col.line="black", col.bar="black", las=3, mgp=c(3,1,0))

x<-strat.plot(prcurve_ITRAX333, yvar=prcurve_ITRAX4$age_ka, clust=clust, y.rev=TRUE, cex.xlabel = 2,cex.axis=1, cex.yaxis=1, cex.ylabel=2, cex.lab=1, ylab="Age (ka)",col.line="black", col.bar="black", las=3, mgp=c(3,1,0))


#?png#?strat.plot
z<-as.matrix(1:ngroups)

#cluster_graph<-addClustZone(x, clust, nZone=ngroups, col=rainbow(length(z))[rank(z)])#ngroups
addClustZone(x, clust, nZone=ngroups, col=rainbow(length(z))[rank(z)])#ngroups

#png("cluster_graph.png",path=here("Figs"),width=8,height=5)

#dev.off()

#ggsave("cluster_graph.png",cluster_graph,path=here("Figs"),width=8,height=5)


c <- cutree(clust, k=ngroups)
prcurve_ITRAX3$clust <- c

prcurve_ITRAX3$age <- prcurve_ITRAX$mean
prcurve_ITRAX3$depth <- prcurve_ITRAX$Depth

title(main=strat_title, cex.main=2)

dissim<- vegdist(prcurve_ITRAX_depth_2, method="bray")
clust <- chclust(dissim, method="coniss")

groups<- bstick(clust,ng=50)
ngroups<- groups$nGroups[which(groups$dispersion <= groups$bstick)]
ngroups<- ngroups[2]
cc<- cutree(clust, k = ngroups)
par(oma=c(2,1,1,1.2))
strat.plot(prcurve_ITRAX_depth_2, yvar=prcurve_ITRAX_depth$Depth, clust=clust, y.rev=TRUE, cex.axis=0.6, cex.yaxis=0.8, cex.ylabel=0.8, cex.lab=0.6, ylab="Depth (cm)", col.line="black", col.bar="black", las=3, mgp=c(3,1,0),ylim=c(0,180))

strat.plot(prcurve_ITRAX_depth_2, yvar=prcurve_ITRAX_depth$Depth, y.rev=TRUE, cex.axis=0.6, cex.yaxis=0.8, cex.ylabel=0.8, cex.lab=0.6, ylab="Depth (cm)", col.line="black", col.bar="black", las=3, mgp=c(3,1,0),ylim=c(0,180))

# check that strat plot is ok
z<-as.matrix(1:ngroups)

x<-strat.plot(prcurve_ITRAX_depth_2, yvar=prcurve_ITRAX_depth$Depth, clust=clust, y.rev=TRUE, cex.axis=0.6, cex.yaxis=0.8, cex.ylabel=0.8, cex.lab=0.6, ylab="Depth (cm)",col.line="black", col.bar="black", las=3, mgp=c(3,1,0),ylim=c(0,180))
addClustZone(x, clust, nZone=ngroups, col=rainbow(length(z))[rank(z)])
c <- cutree(clust, k=ngroups)


x_depth<-strat.plot(prcurve_ITRAX_depth_2, yvar=prcurve_ITRAX_depth$Depth, y.rev=TRUE, cex.axis=0.6, cex.yaxis=0.8, cex.ylabel=0.8, cex.lab=0.6, ylab="Depth (cm)",col.line="black", col.bar="black", las=3, mgp=c(3,1,0),ylim=c(0,180))


library(FactoMineR)
library(factoextra)

res.pca <- PCA(prcurve_ITRAX3, graph = FALSE)
dimdesc(res.pca,axes=1)

plot(res.pca, choix = 'varcor')

res.pca$contrib

res <- PCA(prcurve_ITRAX3, scale.unit = FALSE, graph = FALSE)
loadings<-sweep(res$var$coord,2,sqrt(res$eig[1:5,1]),FUN="/")
print(loadings)

#Plot factormine package

eig.val <- get_eigenvalue(res.pca)
eig.val

fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 50))

#Scores with Factormine. Similar to x (in prcomp package). 

res.pca$ind$coord

#Contributions of variables to PC1

fviz_contrib(res.pca, choice='var', axes=1, top=26)

#Contributions of variables to PC2

fviz_contrib(res.pca, choice='var', axes=2, top=26)


d1ttt <- scale(prcurve_ITRAX3, center=T, scale=T)




library(mgcv)


mod_Si <- gam(NorSi ~ s(mean,k=15), data = prcurve_ITRAX, method = "REML")

mod_Fe <- gam(NorFe ~ s(mean,k=15), data = prcurve_ITRAX, method = "REML")

mod_Ti <- gam(NorTi ~ s(mean,k=15), data = prcurve_ITRAX, method = "REML")

mod_K <- gam(NorK ~ s(mean,k=15), data = prcurve_ITRAX, method = "REML")

plot(mod_Si)

prcurve_ITRAX_depth_2<-select(prcurve_ITRAX_depth,(-8))
prcurve_ITRAX_depth_2<-rename(prcurve_ITRAX_depth_2, "Si"="NorSi","Ti"="NorTi","Ca"="NorCa","K"="NorK","Al"="NorAl","Fe"="NorFe","Sr"="NorSr")


#prc_curve_scores_2<-merge(prc_curve_scores,prcurve_ITRAX, by="row.names")

plot_pr <- prc_curve_scores %>%
    select(rn, PrC) %>%
    na.omit() %>%
    ggplot() +
    geom_point(aes(x = rn, y = PrC), size = 1, alpha = 0.75)+ geom_path(aes(x = rn, y = PrC), size = 1, alpha = 0.75)+
    ylab("PrC") +
    theme_minimal()+
    scale_x_continuous(breaks=seq(0, 32500, by=5000))
#theme(axis.title.x = element_blank(),
#axis.text.x = element_blank())
str(prc_curve_scores)

pr3<-prc_curve_scores%>%
    rename("median"="rn")%>%
    left_join(ages_final_ITRAX3)

plot_pr2<- prc_curve_scores%>%
    select(rn, PrC) %>%
    na.omit() %>%
    ggplot() +
    geom_point(aes(x = PrC, y = (rn/1000)), size = 1, alpha = 0.75) + geom_path(aes(x = PrC, y = (rn/1000)), size = 1, alpha = 0.75)+
    xlab("Principal curve") +
    ylab("Age (k cal yr BP)")+
    theme_minimal(base_size = 20) +
    scale_y_reverse(limits=c(33,0),breaks = seq(0, 33, by = 2))+
    scale_x_continuous(limits=c(0,2),breaks = seq(0, 4, by = 1))

plot_pr3<- pr3%>%
    select(Depth, PrC) %>%
    na.omit() %>%
    ggplot() +
    geom_point(aes(x = PrC, y = (Depth)), size = 1, alpha = 0.75) + geom_path(aes(x = PrC, y = Depth), size = 1, alpha = 0.75)+
    xlab("Principal curve") +
    ylab("Depth (cm)")+
    theme_minimal(base_size = 20) +
    scale_y_reverse(limits=c(173,0),breaks = seq(0, 173, by = 10))+
    scale_x_continuous(limits=c(0,2),breaks = seq(0, 4, by = 1))+
    theme(axis.title.x=element_text(size=22), axis.text=element_text(size=16),
          axis.title.y = element_blank(),
          axis.text.y = element_blank()) 

ggplotly(plot_pr)