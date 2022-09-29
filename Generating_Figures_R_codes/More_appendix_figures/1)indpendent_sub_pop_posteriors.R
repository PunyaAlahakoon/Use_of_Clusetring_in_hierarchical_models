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

subs<-sprintf('%d ',1:10)
subs2<-sprintf('%d ',11:20)
subs3<-sprintf('%d ',21:30)

pop_names1=paste("Sub-population ",subs)
pop_names2=paste("Sub-population ",subs2)
pop_names3=paste("Sub-population ",subs3)

#load data:

b1=readMat('independent_beta_d1.mat')
b1=b1$beta.smc
b1=data.frame(b1,rep("Sub-group A",1000))
colnames(b1)<-c(pop_names1,"Group")

b2=readMat('independent_beta_d2.mat')
b2=b2$beta.smc
b2=data.frame(b2,rep("Sub-group B",1000))
colnames(b2)<-c(pop_names2,"Group")


b3=readMat('independent_beta_d3.mat')
b3=b3$beta.smc
b3=data.frame(b3,rep("Sub-group C",1000))
colnames(b3)<-c(pop_names3,"Group")

m1=melt(b1)
m2=melt(b2)
m3=melt(b3)

m=rbind(m1,m2,m3)


p1=ggplot(m)+
  geom_density(aes(x=value,fill=factor(Group)),alpha=1)+
  facet_wrap(~factor(variable),ncol=5)+
  theme_classic(16) +
  scale_fill_manual(values = c("#D69C4E", "#046C9A",  "#00A08A","#8D8680"))+
  ylab("Density")+
  xlab(expression(beta))+
  theme(plot.background=element_blank(),
        strip.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        #axis.title.y=element_blank(),
        strip.text.x = element_text(),
        strip.text.y = element_text())+
  theme(legend.title=element_blank(),legend.background = element_blank()) +
  theme(legend.position="bottom") 

p1

ggsave(filename = "independent_betas.png", last_plot(),
       width =15, height = 10, units = "in", device='png')

####gammas

b1=readMat('independent_gamma_d1.mat')
b1=b1$gamma.smc
b1=data.frame(b1,rep("Sub-group A",1000))
colnames(b1)<-c(pop_names1,"Group")

b2=readMat('independent_gamma_d2.mat')
b2=b2$gamma.smc
b2=data.frame(b2,rep("Sub-group B",1000))
colnames(b2)<-c(pop_names2,"Group")


b3=readMat('independent_gamma_d3.mat')
b3=b3$gamma.smc
b3=data.frame(b3,rep("Sub-group C",1000))
colnames(b3)<-c(pop_names3,"Group")

m1=melt(b1)
m2=melt(b2)
m3=melt(b3)

m=rbind(m1,m2,m3)


p1=ggplot(m)+
  geom_density(aes(x=value,fill=factor(Group)),alpha=1)+
  facet_wrap(~factor(variable),ncol=5)+
  theme_classic(16) +
  scale_fill_manual(values = c("#D69C4E", "#046C9A",  "#00A08A","#8D8680"))+
  ylab("Density")+
  xlab(expression(gamma))+
  theme(plot.background=element_blank(),
        strip.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        #axis.title.y=element_blank(),
        strip.text.x = element_text(),
        strip.text.y = element_text())+
  theme(legend.title=element_blank(),legend.background = element_blank()) +
  theme(legend.position="bottom") 

p1

ggsave(filename = "independent_gammas.png", last_plot(),
       width =15, height = 10, units = "in", device='png')


####mu

b1=readMat('independent_epsilon_d1.mat')
b1=b1$epsilon.smc
b1=data.frame(b1,rep("Sub-group A",1000))
colnames(b1)<-c(pop_names1,"Group")

b2=readMat('independent_epsilon_d2.mat')
b2=b2$epsilon.smc
b2=data.frame(b2,rep("Sub-group B",1000))
colnames(b2)<-c(pop_names2,"Group")


b3=readMat('independent_epsilon_d3.mat')
b3=b3$epsilon.smc
b3=data.frame(b3,rep("Sub-group C",1000))
colnames(b3)<-c(pop_names3,"Group")

m1=melt(b1)
m2=melt(b2)
m3=melt(b3)

m=rbind(m1,m2,m3)


p1=ggplot(m)+
  geom_density(aes(x=value,fill=factor(Group)),alpha=1)+
  facet_wrap(~factor(variable),ncol=5)+
  theme_classic(16) +
  scale_fill_manual(values = c("#D69C4E", "#046C9A",  "#00A08A","#8D8680"))+
  ylab("Density")+
  xlab(expression(mu))+
  theme(plot.background=element_blank(),
        strip.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        #axis.title.y=element_blank(),
        strip.text.x = element_text(),
        strip.text.y = element_text())+
  theme(legend.title=element_blank(),legend.background = element_blank()) +
  theme(legend.position="bottom") 

p1

ggsave(filename = "independent_mus.png", last_plot(),
       width =15, height = 10, units = "in", device='png')

