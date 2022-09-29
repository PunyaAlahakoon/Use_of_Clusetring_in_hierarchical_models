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
hie_bet=readMat('hie_123_gamma_d123.mat')

A_hie_bet=hie_bet$gamma.smc
colnames(A_hie_bet)<-pop_names
A_hie=data.frame(A_hie_bet,"Estimation"=rep("No clustering",1000))
colnames(A_hie)=c(pop_names,"Estimation")
m=melt(A_hie)




#load group 1 data:
g1_beta=readMat('hie_cluster_beta_g1.mat')
g1_beta=g1_beta$beta.smc

g1_gam=readMat('hie_cluster_gamma_g1.mat')
g1_gam=g1_gam$gamma.smc

g1_eps=readMat('hie_cluster_epsilon_g1.mat')
g1_eps=g1_eps$epsilon.smc

g2_beta=readMat('hie_cluster_beta_g2.mat')
g2_beta=g2_beta$beta.smc

g2_gam=readMat('hie_cluster_gamma_g2.mat')
g2_gam=g2_gam$gamma.smc

g2_eps=readMat('hie_cluster_epsilon_g2.mat')
g2_eps=g2_eps$epsilon.smc

g2_names=c(sprintf('%d ',1:8),"10","21","26")
g1_names=c("9",sprintf('%d ',11:20),sprintf('%d ',22:25),sprintf('%d ',27:30))

g1=data.frame(g1_gam,"Estimation"=rep("Cluster 2 of 2",1000))
colnames(g1)=c(g1_names,"Estimation")
m1=melt(g1)
g2=data.frame(g2_gam,"Estimation"=rep("Cluster 1 of 2",1000))
colnames(g2)=c(g2_names,"Estimation")
m2=melt(g2)


data=rbind(m,m2,m1)

data$variable=c(rep(sprintf('%d ',1:30),each=1000), rep(c(sprintf('%d ',1:8),"10","21","26"),each=1000),
                rep(c("9",sprintf('%d ',11:20),sprintf('%d ',22:25),sprintf('%d ',27:30)),each=1000))


data$variable=c(rep(1:30,each=1000),rep(c(1:8,10,21,26),each=1000),
                rep(c(9,11:20,22:25,27:30),each=1000))

levels(data$variable)<-1:30

p1<-ggplot(data,aes(x=value,y=factor(variable),fill=factor(Estimation)))+
  geom_density_ridges(scale=1,alpha=0.8,color = NA)+
  #geom_density_ridges(stat = "binline", bins = 250,scale=1, draw_baseline = FALSE,alpha=0.8,color = NA)+
  scale_vline_size_continuous(limits = c(0,4))+
  # geom_segment(data =true_betas,mapping=aes(x = tr_be, xend = tr_be, y = Var2,
  #yend = Var2+0.5))+
  scale_fill_manual(values = c( "#D69C4E", "#046C9A", "#8D8680"))+
  # geom_vline(xintercept=2, linetype="dotted")+
  #facet_wrap(~Pop)+
  theme_ridges()+
  xlim(0.65,1.5)+
  theme_clean(base_size = 55) +
  ylab("Sub-population")+
  xlab(expression(gamma))+
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
p1


