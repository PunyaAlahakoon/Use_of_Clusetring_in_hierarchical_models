library(R.matlab)
library(ggplot2)
library(tidyverse)
library(cowplot)
library(reshape2)
library(RColorBrewer)
library(ggthemes)
library(robustbase)
library(ggridges)
library(wesanderson)

subs<-sprintf('%d ',1:30)
#pop_names=paste("Sub-popuation ",subs)
pop_names=subs

#all hierarchical: 3 datasets
hie_bet=readMat('hie_123_beta_d123.mat')

A_hie_bet=hie_bet$beta.smc
colnames(A_hie_bet)<-pop_names
A_hie=data.frame(A_hie_bet,"Estimation"=rep("No clustering",1000))
colnames(A_hie)=c(pop_names,"Estimation")
m=melt(A_hie)



#load group 1 data:
g1_beta=readMat('hie_cluster3_beta_g1.mat')
g1_beta=g1_beta$beta.smc

g2_beta=readMat('hie_cluster3_beta_g2.mat')
g2_beta=g2_beta$beta.smc

g3_beta=readMat('hie_cluster3_beta_g3.mat')
g3_beta=g3_beta$beta.smc


#g2_names=c(sprintf('%d ',1:10),"21","23","24","25","26")
#g1_names=c(sprintf('%d ',11:20),"22","27","28","29","30")

g1=data.frame(g1_beta,"Estimation"=rep("Cluster 3 of 3",1000))

#colnames(g1)=c(g1_names,"Estimation")
m1=melt(g1)

g2=data.frame(g2_beta,"Estimation"=rep("Cluster 1 of 3",1000))
#colnames(g2)=c(g2_names,"Estimation")
m2=melt(g2)

g3=data.frame(g3_beta,"Estimation"=rep("Cluster 2 of 3",1000))
#colnames(g2)=c(g2_names,"Estimation")
m3=melt(g3)

data=rbind(m,m1,m2,m3)



data$variable=c(rep(1:30,each=1000),rep(c(4,9,21,23,24,26,27,29),each=1000),rep(c(1:3,5:8,10),each=1000),
                rep(c(11:20,22,25,28,30),each=1000))

levels(data$variable)<-1:30

p2<-ggplot(data,aes(x=value,y=factor(variable),fill=factor(Estimation)))+
  geom_density_ridges(scale=1,alpha=0.8,color = NA)+
  #geom_density_ridges(stat = "binline", bins = 250,scale=1, draw_baseline = FALSE,alpha=0.8,color = NA)+
  scale_vline_size_continuous(limits = c(0,4))+
  # geom_segment(data =true_betas,mapping=aes(x = tr_be, xend = tr_be, y = Var2,
  #yend = Var2+0.5))+
  scale_fill_manual(values = c("#D69C4E", "#046C9A",  "#00A08A","#8D8680"))+
  # geom_vline(xintercept=2, linetype="dotted")+
  
  #facet_wrap(~Pop)+
  theme_ridges()+
  xlim(1,5.5)+
  theme_minimal(base_size = 55) +
  ylab("Sub-population")+
  xlab(expression(beta))+
  #ggtitle(expression(paste("Marginal posteriors of ",beta,"")))+
  theme(plot.background=element_blank(),
        strip.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title.y=element_text(size = 50),
        axis.title.x=element_text(size = 50),
        strip.text.x = element_text(size = 20),
        strip.text.y = element_text(size = 20))+
  theme(legend.text=element_text(size=rel(0.75)))+
  theme(legend.title=element_blank(),legend.background = element_blank()) +
  theme(legend.position="bottom") 

# theme(plot.title = element_text(hjust = 0.5))
p2


plot_grid(p1,p2,ncol = 2,labels = c("A","B"),label_size = 60)

ggsave(filename = "ridge_betas_cluster2_3_groups.png", last_plot(),
       width = 39.5, height = 45, units = "in", device='png')
