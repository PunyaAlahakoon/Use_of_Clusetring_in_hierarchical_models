
library(ggpubr)

names=c("Sub-population","Independenet","True group","All","Two","Three")

names=c("Sub-population","Independenet","Hierarchical \n (One group)","Hierarchical \n (True group) ","Hierarchical \n (Two clusters)","Hierarchical \n (Three clusters)")


#load data
hdp1=read.csv('hdp_0.25.csv')

hdp2=read.csv('hdp_0.35.csv')

hdp3=read.csv('hdp_0.5.csv')

hdp4=read.csv('hdp_0.65.csv')

colnames(hdp1)=colnames(hdp2)=colnames(hdp3)=colnames(hdp4)=names


#m1=melt(hdp1,id="Sub-population")
#m2=melt(hdp2,id="Sub-population")
#m3=melt(hdp3,id="Sub-population")
#m4=melt(hdp4,id="Sub-population")

#for hdp3:
d1=data.frame(hdp3[,2],hdp3[,3])
colnames(d1)<-c("Independenet","Hierarchical \n (One group)")

 p1= ggpaired(d1, cond1  = "Independenet", cond2= "Hierarchical \n (One group)",line.color = "gray", line.size = 0.4)+
   xlab("Estimation method")+
   ylim(0,100)+
   ylab("Percentage ")+
   
   theme_classic()
 p1

 d2=data.frame(hdp3[,2],hdp3[,4])
 colnames(d2)<-c("Independenet","Hierarchical \n (True group)")
 
 p2= ggpaired(d2, cond1  = "Independenet", cond2= "Hierarchical \n (True group)",line.color = "gray", line.size = 0.4)+
   xlab("Estimation method")+
   ylab("Percentage ")+
   ylim(0,100)+
   theme_classic()
 p2

 d3=data.frame(hdp3[,2],hdp3[,5])
 colnames(d3)<-c("Independenet","Hierarchical \n (Two clusters)")
 
 p3= ggpaired(d3, cond1  = "Independenet", cond2= "Hierarchical \n (Two clusters)",line.color = "gray", line.size = 0.4)+
   xlab("Estimation method")+
   ylab("Percentage ")+
   ylim(0,100)+
   theme_classic()
 p3
 
 d4=data.frame(hdp3[,2],hdp3[,6])
 colnames(d4)<-c("Independenet","Hierarchical \n (Three clusters)")
 
 p4= ggpaired(d4, cond1  = "Independenet", cond2= "Hierarchical \n (Three clusters)",line.color = "gray", line.size = 0.4)+
   
   xlab("Estimation method")+
   ylab("Percentage ")+
   ylim(0,100)+
   theme_classic()
 p4

px<- plot_grid(p1,p2,p3,p4,ncol=2)  
 title <- ggdraw() + draw_label("True parameter+-0.5", fontface='bold')
 px
 
   px3<-plot_grid(title, px, ncol=1, rel_heights=c(0.05, 1)) 

   ggsave("ROPE_percentages_0.5.png",height = 8,width = 10,px) 
   
   
   #for hdp4
   hdp3=hdp4
   d1=data.frame(hdp3[,2],hdp3[,3])
   colnames(d1)<-c("Independenet","Hierarchical \n (One group)")
   
   p1= ggpaired(d1, cond1  = "Independenet", cond2= "Hierarchical \n (One group)",line.color = "gray", line.size = 0.4)+
     xlab("Estimation method")+
     ylim(0,100)+
     ylab("Percentage ")+
     theme_classic()
   p1
   
   d2=data.frame(hdp3[,2],hdp3[,4])
   colnames(d2)<-c("Independenet","Hierarchical \n (True group)")
   
   p2= ggpaired(d2, cond1  = "Independenet", cond2= "Hierarchical \n (True group)",line.color = "gray", line.size = 0.4)+
     xlab("Estimation method")+
     ylab("Percentage ")+
     ylim(0,100)+
     theme_classic()
   p2
   
   d3=data.frame(hdp3[,2],hdp3[,5])
   colnames(d3)<-c("Independenet","Hierarchical \n (Two clusters)")
   
   p3= ggpaired(d3, cond1  = "Independenet", cond2= "Hierarchical \n (Two clusters)",line.color = "gray", line.size = 0.4)+
     xlab("Estimation method")+
     ylab("Percentage ")+
     ylim(0,100)+
     theme_classic()
   p3
   
   d4=data.frame(hdp3[,2],hdp3[,6])
   colnames(d4)<-c("Independenet","Hierarchical \n (Three clusters)")
   
   p4= ggpaired(d4, cond1  = "Independenet", cond2= "Hierarchical \n (Three clusters)",line.color = "gray", line.size = 0.4)+
     
     xlab("Estimation method")+
     ylab("Percentage ")+
     ylim(0,100)+
     theme_classic()
   p4
   
   py<- plot_grid(p1,p2,p3,p4,ncol=2)  
   title <- ggdraw() + draw_label("True parameter+-0.65", fontface='bold')
   
   py4<-plot_grid(title, py, ncol=1, rel_heights=c(0.05, 1)) 
   
   
   #for hdp1
   hdp3=hdp1
   d1=data.frame(hdp3[,2],hdp3[,3])
   colnames(d1)<-c("Independenet","Hierarchical \n (One group)")
   
   p1= ggpaired(d1, cond1  = "Independenet", cond2= "Hierarchical \n (One group)",line.color = "gray", line.size = 0.4)+
     xlab("Estimation method")+
     ylim(0,100)+
     ylab("Percentage ")+
     
     theme_classic()
   p1
   
   d2=data.frame(hdp3[,2],hdp3[,4])
   colnames(d2)<-c("Independenet","Hierarchical \n (True group)")
   
   p2= ggpaired(d2, cond1  = "Independenet", cond2= "Hierarchical \n (True group)",line.color = "gray", line.size = 0.4)+
     xlab("Estimation method")+
     ylab("Percentage ")+
     ylim(0,100)+
     theme_classic()
   p2
   
   d3=data.frame(hdp3[,2],hdp3[,5])
   colnames(d3)<-c("Independenet","Hierarchical \n (Two clusters)")
   
   p3= ggpaired(d3, cond1  = "Independenet", cond2= "Hierarchical \n (Two clusters)",line.color = "gray", line.size = 0.4)+
     xlab("Estimation method")+
     ylab("Percentage ")+
     ylim(0,100)+
     theme_classic()
   p3
   
   d4=data.frame(hdp3[,2],hdp3[,6])
   colnames(d4)<-c("Independenet","Hierarchical \n (Three clusters)")
   
   p4= ggpaired(d4, cond1  = "Independenet", cond2= "Hierarchical \n (Three clusters)",line.color = "gray", line.size = 0.4)+
     
     xlab("Estimation method")+
     ylab("Percentage ")+
     ylim(0,100)+
     theme_classic()
   p4
   
   py<- plot_grid(p1,p2,p3,p4,ncol=2)  
   title <- ggdraw() + draw_label("True parameter+-0.25", fontface='bold')
   
   py1<-plot_grid(title, py, ncol=1, rel_heights=c(0.05, 1)) 
   
   py1
   
   
   #for hdp2
   hdp3=hdp2
   d1=data.frame(hdp3[,2],hdp3[,3])
   colnames(d1)<-c("Independenet","Hierarchical \n (One group)")
   
   p1= ggpaired(d1, cond1  = "Independenet", cond2= "Hierarchical \n (One group)",line.color = "gray", line.size = 0.4)+
     xlab("Estimation method")+
     ylim(0,100)+
     ylab("Percentage ")+
     
     theme_classic()
   p1
   
   d2=data.frame(hdp3[,2],hdp3[,4])
   colnames(d2)<-c("Independenet","Hierarchical \n (True group)")
   
   p2= ggpaired(d2, cond1  = "Independenet", cond2= "Hierarchical \n (True group)",line.color = "gray", line.size = 0.4)+
     xlab("Estimation method")+
     ylab("Percentage ")+
     ylim(0,100)+
     theme_classic()
   p2
   
   d3=data.frame(hdp3[,2],hdp3[,5])
   colnames(d3)<-c("Independenet","Hierarchical \n (Two clusters)")
   
   p3= ggpaired(d3, cond1  = "Independenet", cond2= "Hierarchical \n (Two clusters)",line.color = "gray", line.size = 0.4)+
     xlab("Estimation method")+
     ylab("Percentage ")+
     ylim(0,100)+
     theme_classic()
   p3
   
   d4=data.frame(hdp3[,2],hdp3[,6])
   colnames(d4)<-c("Independenet","Hierarchical \n (Three clusters)")
   
   p4= ggpaired(d4, cond1  = "Independenet", cond2= "Hierarchical \n (Three clusters)",line.color = "gray", line.size = 0.4)+
     
     xlab("Estimation method")+
     ylab("Percentage ")+
     ylim(0,100)+
     theme_classic()
   p4
   
   py<- plot_grid(p1,p2,p3,p4,ncol=2)  
   title <- ggdraw() + draw_label("True parameter+-0.35", fontface='bold')
   
   py2<-plot_grid(title, py, ncol=1, rel_heights=c(0.05, 1)) 
   
   py2
   
   
   
   
   
   plot_grid(py1,py2,px3,py4)
   
   ggsave("ROPE_percentages.png",height = 12,width = 14,last_plot()) 
   
  # ggsave("ROPE_percentages_0.65.png",height = 8,width = 10,last_plot()) 
   