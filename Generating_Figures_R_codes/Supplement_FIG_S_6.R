names=c("Psi[beta]","Psi[gamma]","Psi[mu]", "Method")
#names=c("Transmission rate","Recovery rate", "Waning immunity rate", "Method")

#load hyper-beta parameter data:
all_hb=readMat('all_hie_hb_all.mat')
all_hb=all_hb$hb
all_hb=t(all_hb[,5001:6000])
all_hb=data.frame(all_hb,"Method"=rep("One group",1000))



g1_hb=readMat('2C_hb_g1_all.mat')
g1_hb=g1_hb$hb
g1_hb=t(g1_hb[,5001:6000])
g1_hb=data.frame(g1_hb,"Method"=rep("Cluster 2 of 2",1000))


g2_hb=readMat('2C_hb_g2_all.mat')
g2_hb=g2_hb$hb
g2_hb=t(g2_hb[,5001:6000])
g2_hb=data.frame(g2_hb,"Method"=rep("Cluster 1 of 2",1000))




colnames(all_hb)=colnames(g1_hb)=colnames(g2_hb)<-names

dt=rbind(all_hb,g1_hb,g2_hb)
dd=melt(dt)

#group means:
me_b=c(2.6977,3.4767,1.9426, 2.6737)
me_g=c(1.010,1.011,1.007,1.013)
me_e=c(0.059,0.061,0.059,0.056)

mes=data.frame(rbind(me_b,me_g,me_e),c("Psi[beta]","Psi[gamma]","Psi[mu]"))
colnames(mes)<-c("One group","Group A","Group B","Group C","variable")
mms=melt(mes)
colnames(mms)<-c("variable","Method","value")


lv=c("One group","Cluster 1 of 2","Cluster 2 of 2")
dd$Method=factor(dd$Method,levels = lv)

p1=ggplot()+
  #  geom_histogram(data=dd,aes(x=value,y=..density..,fill=factor(Method)),binwidth = 0.005,alpha=0.67,position="identity")+
  geom_density(data=dd,aes(x=value,fill=factor(Method)),alpha=0.5,adjust =3)+
  #geom_point(data=mms,aes(x=value,y=-1,colour=factor(Method)), size = 3.5,shape=8)+
  facet_wrap(~factor(variable), scales="free", labeller = label_parsed)+
  scale_fill_brewer(palette = "Set1")+
  scale_color_brewer(palette = "Set1")+
  theme_classic(14) +
  ylab("Density")+
  xlab("Parameter space")+
    ggtitle("Use of two clusters")+
  theme(plot.background=element_blank(),
        strip.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        #axis.title.y=element_blank(),
        strip.text.x = element_text(),
        strip.text.y = element_text())+
  theme(legend.title=element_blank(),legend.background = element_blank()) +
  theme(legend.position="bottom") +
  theme(plot.title = element_text(hjust = 0.5))

p1


names=c("sigma[beta]","sigma[gamma]","sigma[mu]", "Method")
#names=c("Transmission rate","Recovery rate", "Waning immunity rate", "Method")

#load hyper-beta parameter data:
all_hb=readMat('all_hie_hb_all.mat')
all_hb=all_hb$hb
all_hb=t(all_hb[,5001:6000])
all_hb=data.frame(all_hb,"Method"=rep("One group",1000))



g1_hb=readMat('C3_hb_g1_all.mat')
g1_hb=g1_hb$hb
g1_hb=t(g1_hb[,5001:6000])
g1_hb=data.frame(g1_hb,"Method"=rep("Cluster 3 of 3",1000))


g2_hb=readMat('C3_hb_g2_all.mat')
g2_hb=g2_hb$hb
g2_hb=t(g2_hb[,5001:6000])
g2_hb=data.frame(g2_hb,"Method"=rep("Cluster 1 of 3",1000))

g3_hb=readMat('C3_hb_g3_all.mat')
g3_hb=g3_hb$hb
g3_hb=t(g3_hb[,5001:6000])
g3_hb=data.frame(g3_hb,"Method"=rep("Cluster 2 of 3",1000))

colnames(all_hb)=colnames(g1_hb)=colnames(g2_hb)=colnames(g3_hb)<-names

dt=rbind(all_hb,g1_hb,g2_hb,g3_hb)
dd=melt(dt)

lv=c("One group","Cluster 1 of 3","Cluster 2 of 3","Cluster 3 of 3")
dd$Method=factor(dd$Method,levels = lv)

#group means:
me_b=c(2.6977,3.4767,1.9426, 2.6737)
me_g=c(1.010,1.011,1.007,1.013)
me_e=c(0.059,0.061,0.059,0.056)

mes=data.frame(rbind(me_b,me_g,me_e),c("Psi[beta]","Psi[gamma]","Psi[mu]"))
colnames(mes)<-c("All groups","Group A","Group B","Group C","variable")
mms=melt(mes)
colnames(mms)<-c("variable","Method","value")

p2=ggplot()+
  #  geom_histogram(data=dd,aes(x=value,y=..density..,fill=factor(Method)),binwidth = 0.005,alpha=0.67,position="identity")+
  geom_density(data=dd,aes(x=value,fill=factor(Method)),alpha=0.5,adjust = 2.5)+
  #geom_point(data=mms,aes(x=value,y=-1,colour=factor(Method)), size = 3.5,shape=8)+
  facet_wrap(~factor(variable), scales="free", labeller = label_parsed)+
  scale_fill_brewer(palette = "Set1")+
  scale_color_brewer(palette = "Set1")+
  theme_classic(14) +
  ylab("Density")+
  xlab("Parameter space")+
   ggtitle("Use of three clusters")+
  theme(plot.background=element_blank(),
        strip.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        #axis.title.y=element_blank(),
        strip.text.x = element_text(),
        strip.text.y = element_text())+
  theme(legend.title=element_blank(),legend.background = element_blank()) +
  theme(legend.position="bottom") +
  theme(plot.title = element_text(hjust = 0.5))

p2


#library(ggpubr)
#ggarrange(p1, p2, nrow=2, common.legend = TRUE, legend="bottom")

plot_grid(p1,p2,nrow= 2)

ggsave("appendix_group_cluster_hypers.png",last_plot(),height = 6,width = 8)
