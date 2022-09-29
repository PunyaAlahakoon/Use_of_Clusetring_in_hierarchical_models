library(R.matlab)
library(ggplot2)
library(tidyverse)
library(cowplot)
library(reshape2)
library(ggthemes)
#library(wesanderson)
#library(RColorBrewer)
library(colorspace)
library(lubridate)

library(readr)
library(grid)
library(gridExtra)
library(GGally)
library(fossil)
library(data.table)
library(factoextra)
#load data:
subs<-sprintf('%d ',1:30)
#pop_names=paste("Sub-popuation ",subs)
pop_names=subs

#independent 
ind_bet_d1=readMat('independent_beta_d1.mat')
ind_bet_d1=ind_bet_d1$beta.smc
colnames(ind_bet_d1)<-pop_names[1:10]
#ind_bet_d1=melt(ind_bet_d1)



ind_bet_d2=readMat('independent_beta_d2.mat')
ind_bet_d2=ind_bet_d2$beta.smc
colnames(ind_bet_d2)<-pop_names[11:20]
#ind_bet_d2=melt(ind_bet_d2)

ind_bet_d3=readMat('independent_beta_d3.mat')
ind_bet_d3=ind_bet_d3$beta.smc
colnames(ind_bet_d3)<-pop_names[21:30]


#all parameters for datsets 1 and 2:
betas=data.frame(cbind(ind_bet_d1,ind_bet_d2))
colnames(betas)<-pop_names


me=data.frame(apply(betas, 2, median),apply(betas, 2, quantile,probs=0.025),apply(betas, 2, quantile,probs=0.975))
#me=b=data.frame(apply(betas, 2, mean))

k2 <- kmeans(me, centers =2, nstart = 25) 
str(k2)
k2

p1<-fviz_cluster(k2, data =me)+
  theme_classic()+ggtitle(label='') +
  #scale_color_brewer(palette = "Set2")+
  #scale_fill_brewer(palette = "Set1")
  scale_color_manual(values = c("#D69C4E","#046C9A","#00A08A","#8D8680"))+
  scale_fill_manual(values = c( "#D69C4E","#046C9A","#00A08A","#D69C4E",   "#8D8680"))
p1



k2 <- kmeans(me, centers =3, nstart = 25) 
str(k2)
k2

p2<-fviz_cluster(k2, data =me)+
  theme_classic()+ggtitle(label='') +
  #scale_color_brewer(palette = "Set2")+
  #scale_fill_brewer(palette = "Set1")
  scale_color_manual(values = c("#046C9A","#D69C4E","#00A08A"))+
  scale_fill_manual(values = c("#046C9A","#D69C4E","#00A08A","#8D8680"))
p2

plot_grid(p1,p2,ncol=2,labels = c("Use of two clusters","Use of three clusters"))

ggsave("Clusters.png",last_plot(),
       width = 12, height = 6, units = "in", device='png')  



#find optimal clusters 

p3<-fviz_nbclust(me, kmeans, method = "wss")+
  theme_classic()
p3
p4<-fviz_nbclust(me, kmeans, method = "silhouette")+
  theme_classic()
p4

plot_grid(p3,p4)

ggarrange(p1, p2, p3, p4, ncol=2, nrow=2, common.legend = F,labels = c("A","B","C","D"))

ggsave("Clusters_appndix.png",last_plot(),
       width = 12, height = 12, units = "in", device='png') 
