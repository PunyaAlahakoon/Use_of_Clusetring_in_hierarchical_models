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



#load data:

b11=readMat('separate_hie_beta_d1.mat')
b11=b11$beta.smc
b11=data.frame(b11,rep("Sub-group A",1000))
colnames(b11)<-c(pop_names1,"Group")

b22=readMat('separate_hie_beta_d2.mat')
b22=b22$beta.smc
b22=data.frame(b22,rep("Sub-group B",1000))
colnames(b22)<-c(pop_names2,"Group")


b33=readMat('separate_hie_beta_d3.mat')
b33=b33$beta.smc
b33=data.frame(b33,rep("Sub-group C",1000))
colnames(b33)<-c(pop_names3,"Group")



#load data:

b_all=readMat('hie_123_beta_d123.mat')
b_all=b_all$beta.smc
b_all=data.frame(b_all,rep("All",1000))
colnames(b_all)<-c(pop_names1,pop_names2,pop_names3,"Group")



m1=melt(b1)
m2=melt(b2)
m3=melt(b3)

m4=melt(b11)
m5=melt(b22)
m6=melt(b33)

m=rbind(m1,m2,m3)
mbb=rbind(m4,m5,m6)
ma=melt(b_all)

p1=ggplot()+
  geom_density(data=ma,aes(x=value),fill="#D95F02")+
  geom_density(data=m,aes(x=value,fill=factor(Group)),alpha=0.3)+
  geom_density(data=mbb,aes(x=value,fill=factor(Group)),alpha=1)+
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

ggsave(filename = "Hie_betas.png", last_plot(),
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

#load data:

b11=readMat('separate_hie_gamma_d1.mat')
b11=b11$gamma.smc
b11=data.frame(b11,rep("Sub-group A",1000))
colnames(b11)<-c(pop_names1,"Group")

b22=readMat('separate_hie_gamma_d2.mat')
b22=b22$gamma.smc
b22=data.frame(b22,rep("Sub-group B",1000))
colnames(b22)<-c(pop_names2,"Group")


b33=readMat('separate_hie_gamma_d3.mat')
b33=b33$gamma.smc
b33=data.frame(b33,rep("Sub-group C",1000))
colnames(b33)<-c(pop_names3,"Group")



#load data:

b_all=readMat('hie_123_gamma_d123.mat')
b_all=b_all$gamma.smc
b_all=data.frame(b_all,rep("All",1000))
colnames(b_all)<-c(pop_names1,pop_names2,pop_names3,"Group")



m1=melt(b1)
m2=melt(b2)
m3=melt(b3)

m4=melt(b11)
m5=melt(b22)
m6=melt(b33)

m=rbind(m1,m2,m3)
mbb=rbind(m4,m5,m6)
ma=melt(b_all)


p1=ggplot()+
  geom_density(data=ma,aes(x=value),fill="#D95F02")+
  geom_density(data=m,aes(x=value,fill=factor(Group)),alpha=0.3)+
  geom_density(data=mbb,aes(x=value,fill=factor(Group)),alpha=1)+
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

ggsave(filename = "Hie_gammas.png", last_plot(),
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

#load data:

b11=readMat('separate_hie_epsilon_d1.mat')
b11=b11$epsilon.smc
b11=data.frame(b11,rep("Sub-group A",1000))
colnames(b11)<-c(pop_names1,"Group")

b22=readMat('separate_hie_epsilon_d2.mat')
b22=b22$epsilon.smc
b22=data.frame(b22,rep("Sub-group B",1000))
colnames(b22)<-c(pop_names2,"Group")


b33=readMat('separate_hie_epsilon_d3.mat')
b33=b33$epsilon.smc
b33=data.frame(b33,rep("Sub-group C",1000))
colnames(b33)<-c(pop_names3,"Group")



#load data:

b_all=readMat('hie_123_epsilon_d123.mat')
b_all=b_all$epsilon.smc
b_all=data.frame(b_all,rep("All",1000))
colnames(b_all)<-c(pop_names1,pop_names2,pop_names3,"Group")



m1=melt(b1)
m2=melt(b2)
m3=melt(b3)

m4=melt(b11)
m5=melt(b22)
m6=melt(b33)

m=rbind(m1,m2,m3)
mbb=rbind(m4,m5,m6)
ma=melt(b_all)

p1=ggplot()+
  geom_density(data=ma,aes(x=value),fill="#D95F02")+
  geom_density(data=m,aes(x=value,fill=factor(Group)),alpha=0.3)+
  geom_density(data=mbb,aes(x=value,fill=factor(Group)),alpha=1)+
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

ggsave(filename = "Hie_mus.png", last_plot(),
       width =15, height = 10, units = "in", device='png')

