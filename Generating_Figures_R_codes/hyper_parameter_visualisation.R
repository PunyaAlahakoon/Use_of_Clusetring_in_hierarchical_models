
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

names=c("Transmission rate","Recovery rate", "Waning immunity rate", "Method")

#load hyper parameter data:
#without clustering 
all_hb=readMat('all_hie_hb_all.mat')
all_hb=all_hb$hb
all_hb=t(all_hb[,5001:10000])
all_hb=data.frame(all_hb,"Method"=rep("No clustering",5000))
colnames(all_hb)<-names

all_sig=readMat('all_hie_hb_sig_all.mat')
all_sig=all_sig$hb.sig
all_sig=t(all_sig[,5001:10000])
all_sig=data.frame(all_sig,"Method"=rep("No clustering",5000))
colnames(all_sig)<-names

#true clustering:
#dataset 1:
d1_hb=readMat('d1_hie_hb_all.mat')
d1_hb=d1_hb$hb
d1_hb=t(d1_hb[,5001:10000])
d1_hb=data.frame(d1_hb,"Method"=rep("True clustering dataset 1",5000))
colnames(d1_hb)<-names

d1_sig=readMat('d1_hie_hb_sig_all.mat')
d1_sig=d1_sig$hb.sig
d1_sig=t(d1_sig[,5001:10000])
d1_sig=data.frame(d1_sig,"Method"=rep("True clustering dataset 1",5000))
colnames(d1_sig)<-names

#dataset2:
d2_hb=readMat('d2_hie_hb_all.mat')
d2_hb=d2_hb$hb
d2_hb=t(d2_hb[,5001:10000])
d2_hb=data.frame(d2_hb,"Method"=rep("True clustering dataset 2",5000))
colnames(d2_hb)<-names

d2_sig=readMat('d2_hie_hb_sig_all.mat')
d2_sig=d2_sig$hb.sig
d2_sig=t(d2_sig[,5001:10000])
d2_sig=data.frame(d2_sig,"Method"=rep("True clustering dataset 2",5000))
colnames(d2_sig)<-names

#psi_beta:

psi_means=rbind(melt(all_hb),melt(d1_hb),melt(d2_hb))

#summary statistics:
med=data.frame(rbind(apply(all_hb[,1:3],2,median),apply(d1_hb[,1:3],2,median),
                     apply(d2_hb[,1:3],2,median)),"Mehod"=c("No clustering","True clustering dataset 1",
                                                            "True clustering dataset 2"))
colnames(med)<-c("Transmission rate","Recovery rate","Waning immunity rate","Method")

m=data.frame(melt(med),"Type"=rep("Median",9))

tru_vals=data.frame("Transmission rate"=c(NA,3.5,2),"Recovery rate"=c(1,1,1), "Waning immunity rate"=c(0.06,0.06,0.06),
                    "Mehod"=c("No clustering","True clustering dataset 1",
                              "True clustering dataset 2"))
colnames(tru_vals)<-c("Transmission rate","Recovery rate","Waning immunity rate","Method")
m2=melt(tru_vals)
m2$Method<-as.factor(m2$Method)
m$Method<-as.factor(m$Method)
m2$variable<-as.factor(m2$variable)
m$variable<-as.factor(m$variable)
#m2=data.frame(melt(tru_vals),"Type"=rep("True parameter",9))


#########EDIT FROM HERE!!!!! 



  p1<-ggplot()+
  geom_histogram(data=psi_means,aes(x=value,y=..density..,fill="#1B9E77"),binwidth = 0.005,alpha=1,position="identity")+
    #geom_vline(data =m, aes(xintercept =value,colour ="#D95F02"),size=0.4, linetype ="twodash")+
    geom_vline(data =m2, aes(xintercept =value,colour = "#08519C" ),size=0.4, linetype ="twodash")+
    #geom_text(data = true_betas,mapping = aes(x=3,y=4,label=paste(round(tr_be,3))),size=2,inherit.aes = FALSE)+
    scale_colour_manual(values=c( "#08519C"),
                        labels = c("True parameter value"))+
                       facet_grid(factor(Method)~factor(variable))+
    scale_fill_manual(values = "#1B9E77",labels="Posterior distributin")+
  theme_classic(10) +
  #ylab("Sub-population")+
  xlab("Parameter space")+
    ggtitle("Hyper-parameters---mean")+
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
  
  ggsave("hyper_means.png",last_plot())  


  ##sigma
  
  psi_sigs=rbind(melt(all_sig),melt(d1_sig),melt(d2_sig))
  
  #summary statistics:
  med=data.frame(rbind(apply(all_sig[,1:3],2,median),apply(d1_sig[,1:3],2,median),
                       apply(d2_sig[,1:3],2,median)),"Mehod"=c("No clustering","True clustering dataset 1",
                                                              "True clustering dataset 2"))
  colnames(med)<-c("Transmission rate","Recovery rate","Waning immunity rate","Method")
  
  m=data.frame(melt(med),"Type"=rep("Median",9))
  
  tru_vals=data.frame("Transmission rate"=c(NA,0.15,0.15),"Recovery rate"=c(0.1,0.1,0.1), "Waning immunity rate"=c(0.05,0.05,0.05),
                      "Mehod"=c("No clustering","True clustering dataset 1",
                                "True clustering dataset 2"))
  colnames(tru_vals)<-c("Transmission rate","Recovery rate","Waning immunity rate","Method")
  m2=melt(tru_vals)
  m2$Method<-as.factor(m2$Method)
  m$Method<-as.factor(m$Method)
  m2$variable<-as.factor(m2$variable)
  m$variable<-as.factor(m$variable)
  #m2=data.frame(melt(tru_vals),"Type"=rep("True parameter",9))
  
  
  #########EDIT FROM HERE!!!!! 
  
  
  
  p2<-ggplot()+
    geom_histogram(data=psi_sigs,aes(x=value,y=..density..,fill="#1B9E77"),binwidth = 0.005,alpha=1,position="identity")+
    #geom_vline(data =m, aes(xintercept =value,colour ="#D95F02"),size=0.4, linetype ="twodash")+
    geom_vline(data =m2, aes(xintercept =value,colour = "#08519C" ),size=0.4, linetype ="twodash")+
    #geom_text(data = true_betas,mapping = aes(x=3,y=4,label=paste(round(tr_be,3))),size=2,inherit.aes = FALSE)+
    scale_colour_manual(values=c( "#08519C"),
                        labels = c("True parameter value"))+
    facet_grid(factor(Method)~factor(variable))+
    scale_fill_manual(values = "#1B9E77",labels="Posterior distributin")+
    theme_classic(10) +
    #ylab("Sub-population")+
    xlab("Parameter space")+
    ggtitle("Hyper-parameters -- standard deviation")+
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
ggsave("hyper_sigmas.png",last_plot())  
  





