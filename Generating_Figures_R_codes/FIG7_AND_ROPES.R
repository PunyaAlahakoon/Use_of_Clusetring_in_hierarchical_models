library(R.matlab)
Sys.setenv(JAGS_HOME="C:\\Program Files\\JAGS\\JAGS-4.3.0")
library(BEST)
library(ggplot2)

subs<-sprintf('%d ',1:30)

#load true parameters:
t_betas=readMat('123_betas.mat')
t_betas=t_betas$t.betas
colnames(t_betas)<-subs

#indepepndent betas:
inds=readMat('independent_beta_d123.mat')
inds=inds$beta.smc
colnames(inds)<-subs

#load individual hie groups
g1=readMat('separate_hie_beta_d1.mat')
g1=g1$beta.smc

g2=readMat('separate_hie_beta_d2.mat')
g2=g2$beta.smc

g3=readMat('separate_hie_beta_d3.mat')
g3=g3$beta.smc

gs=cbind(g1,g2,g3)
colnames(gs)<-subs

#load all hie data:
hie_bet=readMat('hie_123_beta_d123.mat')
hie_bet=hie_bet$beta.smc
colnames(hie_bet)<-subs


#cluster2s 
c21=readMat('hie_cluster_beta_g1.mat')
c21=c21$beta.smc

c22=readMat('hie_cluster_beta_g2.mat')
c22=c22$beta.smc

c2=data.frame(c22[,1:8],c21[,1],c22[,9],c21[,2:11],c22[,10],c21[,12:15],c22[,11],c21[,16:19])
colnames(c2)<-subs

#cluster3s:
c31=readMat('hie_cluster3_beta_g1.mat')
c31=c31$beta.smc

c32=readMat("hie_cluster3_beta_g2.mat")
c32=c32$beta.smc

c33=readMat('hie_cluster3_beta_g3.mat')
c33=c33$beta.smc

c3=data.frame(c32[,1:3],c31[,1] ,c32[,4:7],c31[,2],c32[,8],c33[,1:10],c31[,3],c33[,11],c31[,4:5],c33[,12],c31[,6:7],c33[,13],c31[,8],c33[,14])
colnames(c3)<-subs

i=1

par(mfrow=c(2,3))



ind_estimation=inds[,i]
True_group=gs[,i]
all_hie=hie_bet[,i]
clusters_2=c2[,i]
clusters_3=c3[,i]

#png("2ROPES_sub-pop.png", width = 500, height = 1000)
plotPost(ind_estimation, credMass = 0.95, compVal=t_betas[i], ROPE=c(t_betas[i]-0.5, t_betas[i]+.5),HDItextPlace = 1, showMode = T,showCurve=T, xlim=c(0.5,7),xlab=expression(beta),main="Independent estimation")
plotPost(all_hie, credMass = 0.95, compVal=t_betas[i], ROPE=c(t_betas[i]-.5, t_betas[i]+.5),HDItextPlace = 1, showMode = T,showCurve=T, xlim=c(0.5,7),xlab=expression(beta),main="Hierarchical \n (One group)" )
plotPost(True_group, credMass = 0.95, compVal=t_betas[i], ROPE=c(t_betas[i]-.5, t_betas[i]+.5),HDItextPlace = 1, showMode = T,showCurve=T, xlim=c(0.5,7),xlab=expression(beta),main="Hierarchical \n (True sub-group)")
plotPost(clusters_2, credMass = 0.95, compVal=t_betas[i], ROPE=c(t_betas[i]-.5, t_betas[i]+.5),HDItextPlace = 1, showMode = T,showCurve=T, xlim=c(0.5,7),xlab=expression(beta),main="Hierarchical \n (Two clusters)")
plotPost(clusters_3, credMass = 0.95, compVal=t_betas[i], ROPE=c(t_betas[i]-.5, t_betas[i]+.5),HDItextPlace = 1, showMode = T,showCurve=T, xlim=c(0.5,7),xlab=expression(beta),main="Hierarchical \n (Three clusters)")

#ggsave('ROPES_sub-pop1.png',last_plot())

#dev.off()

ggsave('2ROPES_sub-pop1.png',last_plot(),height=4,width=8)


plotAreaInROPE(gs[,1], credMass = 0.95, compVal = t_betas[1],maxROPEradius = 1)
