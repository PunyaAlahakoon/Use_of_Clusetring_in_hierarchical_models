library(overlapping)
library(R.matlab)

#load data:
hie_bet=readMat('hie_123_beta_d123.mat')
hie_bet=hie_bet$beta.smc

g1_beta=readMat('hie_cluster_beta_g1.mat')
c2_g1_beta=g1_beta$beta.smc

colnames(c2_g1_beta)<-c("9",sprintf('%d ',11:20),sprintf('%d ',22:25),sprintf('%d ',27:30))


dt=list(hie_bet[,9],c2_g1_beta[,1])

out<-boot.overlap(dt,1000)
out$OVboot_stats

Y <- stack( data.frame( out$OVboot_dist ))
ggplot( Y, aes( values ))  + geom_density()



