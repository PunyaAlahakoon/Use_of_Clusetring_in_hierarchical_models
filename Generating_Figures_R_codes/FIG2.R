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
#pop_names=subs

g1=readMat('sc1_data.mat')
g1<-g1$data[1:30,]

g2=readMat('sc2_data.mat')
g2=g2$data[1:30,]

g3=readMat('sc3_data.mat')
g3=g3$data[1:30,]

colnames(g1)<-pop_names1
colnames(g2)<-pop_names2
colnames(g3)<-pop_names3

m1=melt(g1)
m2=melt(g2)
m3=melt(g3)

p1=ggplot(m1,aes(x=Var1,y=value))+
  geom_line(color="#666666")+
facet_wrap(~factor(Var2),ncol=5)+
  theme_minimal(18)+
  ylim(0,350)+
  ylab("Prevalance")+
  xlab("Day")+
   ggtitle("Sub-group A")+
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


p2=ggplot(m2,aes(x=Var1,y=value))+
  geom_line(color="#666666")+
  facet_wrap(~factor(Var2),ncol=5)+
  theme_minimal(18)+
  ylim(0,350)+
  ylab("Prevalance")+
  xlab("Day")+
  ggtitle("Sub-group B")+
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


p3=ggplot(m3,aes(x=Var1,y=value))+
  geom_line(color="#666666")+
  facet_wrap(~factor(Var2),ncol=5)+
  theme_minimal(18)+
  ylim(0,350)+
  ylab("Prevalance")+
  xlab("Day")+
  ggtitle("Sub-group C")+
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
p3


plot_grid(p1,p2,p3,ncol=1)


ggsave('data_all.png',last_plot(),height =14,width = 12)
