library(ggplot2)

names=c("Sub-population","Independenet","True group","All","two","three")

names=c("Sub-population","Independenet","Hierarchical \n (One group)","Hierarchical \n (True group) ","Hierarchical \n (Two clusters)","Hierarchical \n (Three clusters)")


#load data
hdp1=read.csv('hdp_0.25.csv')

hdp2=read.csv('hdp_0.35.csv')

hdp3=read.csv('hdp_0.5.csv')

hdp4=read.csv('hdp_0.65.csv')

colnames(hdp1)=colnames(hdp2)=colnames(hdp3)=colnames(hdp4)=names


m1=melt(hdp1,id="Sub-population")
m2=melt(hdp2,id="Sub-population")
m3=melt(hdp3,id="Sub-population")
m4=melt(hdp4,id="Sub-population")


p1 <- ggplot(m1, aes(x=variable, y=value)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=8,
               outlier.size=4)+
  ggtitle("True parameter +-0.25")+
  ylim(0,100)+
  ylab("Percentage")+
  xlab("Estimation method")+
  theme_classic()

p1

p2 <- ggplot(m2, aes(x=variable, y=value)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=8,
               outlier.size=4)+
  ylim(0,100)+
  ylab("Percentage")+
  xlab("Estimation method")+
  ggtitle("True parameter +-0.35")+
  theme_classic()

p2

p3 <- ggplot(m3, aes(x=variable, y=value)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=8,
               outlier.size=4)+
  ylim(0,100)+
  ylab("Percentage")+
  xlab("Estimation method")+
  ggtitle("True parameter +-0.5")+
  theme_classic()

p3

p4 <- ggplot(m4, aes(x=variable, y=value)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=8,
               outlier.size=4)+
  ylim(0,100)+
  ylab("Percentage")+
  xlab("Estimation method")+
  ggtitle("True parameter +-0.65")+
  theme_classic()

p4


plot_grid(p1,p2,p3,p4,ncol=2)

ggsave('ROPE_box_plots.png',last_plot(),height = 8,width = 10)

ggsave('ROPE_box_plot_0.5.png',last_plot(),height = 5,width = 6)
