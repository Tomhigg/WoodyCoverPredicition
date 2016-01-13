# Packages ----------------------------------------------------------------
library(ggplot2)
library(gridExtra)
library(dplyr)

std <- function(x) sd(x)/sqrt(length(x))
std_h  <- function(x) +mean(x)+(sd(x)/sqrt(length(x)))
std_l  <- function(x) mean(x)-(sd(x)/sqrt(length(x)))


stat_sum_single <- function(fun, geom="point", ...) {
  stat_summary(fun.y=fun, colour="red", geom=geom, size = 3, ...)
}
stat_sum_single2 <- function(fun, geom="point", ...) {
  stat_summary(fun.y=fun, colour="blue", geom=geom, size = 3,shape= 95, ...)
}

# VarImp Plot  ------------------------------------------------------------

pdf("VariableImport.pdf")

ggplot(Variable.importance, aes(x= VariableLabel, y=Overall,fill=classvi))+ 
  geom_bar(stat="identity") + coord_flip() + scale_y_continuous(limits=c(0,70))+
  theme_bw() + scale_fill_manual(values=c("darkolivegreen3", "darkolivegreen4", "brown","blue","blue4"))+
  ylab("Scaled Variable Importance")+ xlab("Variable")+
  geom_rect(aes(xmin = 18 + 0.5, xmax = 11 - 0.5, ymin = 0 , ymax = 70 - 0.5),
            fill = "transparent", color = "black", size = 1,linetype=2)+
  theme(legend.title=element_blank())  

# RFE Rsquare & RMSE plot -------------------------------------------------
# ggplot(rfeMetrics,aes(x = Variables,y = value))+
#   facet_wrap(~variable,nrow = 2,ncol = 1,scales = "free")+ 
#   geom_point()+
#   geom_smooth(,aes(group=1))+
#   ylab("Cross Validated Value")+xlab("Number of Variables")+
#   theme_bw()


stat_sum_single <- function(fun, geom="point", ...) {
  stat_summary(fun.y=fun, colour="red", geom=geom, size = 3, ...)
}
stat_sum_single2 <- function(fun, geom="point", ...) {
  stat_summary(fun.y=fun, colour="blue", geom=geom, size = 3,shape= 95, ...)
}


 plot1<- ggplot(filter(rfeMetrics,variable=="Rsquared"),aes(x = Variables,y = value))+
  geom_line(col="darkgrey",size=1)+
  scale_shape_identity() +
  stat_sum_single(mean)+
  stat_sum_single2(std_h)+
  stat_sum_single2(std_l)+
  ylab(bquote('Adjusted'~ R^2))+xlab("")+
  theme_bw()+theme(text = element_text(size=20))+
  geom_hline(yintercept=0.65298831,linetype=2,col= "darkgreen",size=1)+
  geom_hline(yintercept=0.65298831*0.95,linetype=2,col= "darkgreen",size=1)
 
  plot2 <- ggplot(filter(rfeMetrics,variable=="RMSE"),aes(x = Variables,y = value))+
  geom_line(col="darkgrey",size=1)+
  scale_shape_identity() +
    stat_sum_single(mean)+
  stat_sum_single2(std_h)+
  stat_sum_single2(std_l)+
  ylab("RMSE")+xlab("Number of Variables")+
  theme_bw()+theme(text = element_text(size=20))+
  geom_hline(yintercept=0.09367232,linetype=2,col= "darkgreen",size=1)+
  geom_hline(yintercept=0.09367232*1.05,linetype=2,col= "darkgreen",size=1)+
  scale_y_reverse()

pdf(file = "new_figs/rfe.pdf",width = 15,height = 12)
grid.arrange(plot1, plot2, ncol=1)
dev.off()

# Heat Scatter ------------------------------------------------------------
pdf(file = "heatscatter.pdf",width = 8,height = 6)
 ggplot(coverValues, aes(x=Predicted, y=Actual)) +
  stat_binhex(bins=75)+
  #scale_fill_gradientn(colours=c("yellow","green","peachpuff","red","darkred","brown"),name = "Frequency",na.value=NA)+
  scale_fill_gradient(low="bisque", high="magenta4",name = "Frequency",na.value=NA)+
    #geom_point(size=1, position="jitter",alpha = 0.2) +facet_wrap(~variable)+
  #scale_colour_hue(l=50) + # Use a slightly darker palette than normal
  geom_smooth(method="lm")  +  # Don't add shaded confidence region
  scale_y_continuous(limits=c(0,1))+
  scale_x_continuous(limits=c(0,1))+
    geom_abline(intercept = 0, slope = 1, color="red", 
              linetype="dashed", size=1)+ theme_bw()+theme(text = element_text(size=20))+
  #theme(strip.text.x = element_text(size=20,face="bold"),axis.text=element_text(size=20,face="bold"),axis.title=element_text(size=20,face="bold"))+
  labs(x="Predicted Value",y="Actual Value")



dev.off()

# sample size test --------------------------------------------------------
pdf("SampleSizeTests.pdf")

plot1.sampsize <- ggplot(filter(sample.runsMelt.rfemod.FORPLOT,variable=="Rsq"),aes(factor(Sample.No),y = value))+
  geom_line(col="darkgrey",size=1)+
  scale_shape_identity() +
  stat_sum_single(mean)+
  stat_sum_single2(std_h)+
  stat_sum_single2(std_l)+
  theme_bw()+ theme(text = element_text(size=20))+
  ylab(bquote('Adjusted'~ R^2))+xlab("")+
  scale_x_discrete(breaks=seq(500, 30000, 2500))+
  #geom_hline(yintercept=0.5834400+7.180483e-06,linetype=2,col= "darkgreen",size=1)+
  #geom_hline(yintercept=0.5834400+7.180483e-06,linetype=2,col= "darkgreen",size=1)


plot2.sampsize <- ggplot(filter(sample.runsMelt.rfemod.FORPLOT,variable=="RMSE"),aes(factor(Sample.No),y = value))+
  geom_line(col="darkgrey",size=1)+
  scale_shape_identity() +
  stat_sum_single(mean)+
  stat_sum_single2(std_h)+
  stat_sum_single2(std_l)+
  theme_bw()+ theme(text = element_text(size=20))+
  xlab("Number of Samples")+
  ylab("RMSE")+
  scale_x_discrete(breaks=seq(500, 30000, 2500))+
  scale_y_reverse()+
  geom_hline(yintercept= 0.1024951+7.180483e-06,linetype=2,col= "darkgreen",size=1)
  
pdf(file = "samplesize.pdf",width = 15,height = 12)
grid.arrange(plot1.sampsize, plot2.sampsize, ncol=1)
dev.off()

# All  Model combinations ------------------------------------------------------
  
model.test.r2.forplot <-    model.test %>% select(Model, Rsq) %>% group_by(Model) %>% summarise(mn=mean(Rsq)) %>% arrange(mn)
model.test.r2.forplot$Model  <- factor(model.test.r2.forplot$Model,levels=unique(model.test.r2.forplot$Model))
model.test.r2.forplot$grp  <- "grp"

model.test.rmse.forplot <-    model.test %>% select(Model, RMSE) %>% group_by(Model) %>% summarise(mn=mean(RMSE)) %>% arrange(mn)
model.test.rmse.forplot$Model  <- factor(model.test.rmse.forplot$Model,levels=unique(model.test.rmse.forplot$Model))
model.test.rmse.forplot$grp  <- "grp"

plot1.combs<- ggplot(model.test.r2.forplot,aes(x = Model,y = mn, group=grp))+
  geom_point(size=4,col="red")+ geom_line(size=1,col="darkgray",linetype="dashed")+
  geom_hline(yintercept= 0.6470302,linetype=2,col= "darkgreen",size=1)+
  geom_hline(yintercept= 0.6470302*0.95,linetype=2,col= "darkgreen",size=1)+
  ylab(bquote('Adjusted'~ R^2))+xlab("")+
  theme_bw()+theme(text = element_text(size=20),axis.text.x=element_text(size=10, angle=25,hjust=1),plot.margin= unit(rep(.5, 4), "lines"))


plot2.combs<- ggplot(model.test.rmse.forplot,aes(x = Model,y = mn, group=grp))+
  geom_point(size=4,col="red")+ geom_line(size=1,col="darkgray",linetype="dashed")+
  geom_hline(yintercept= 0.09433350,linetype=2,col= "darkgreen",size=1)+
  geom_hline(yintercept= 0.09433350*1.05,linetype=2,col= "darkgreen",size=1)+
  ylab(bquote("RMSE"))+xlab("")+
  theme_bw()+theme(text = element_text(size=20),axis.text.x=element_text(size=10, angle=25,hjust=1),plot.margin= unit(rep(.5, 4), "lines"))+
  scale_y_reverse()+
  scale_x_discrete(limits = rev(levels(model.test.rmse.forplot$Model)))
  

pdf(file = "new_figs/models.pdf",width = 15,height = 12)
grid.arrange(plot1.combs, plot2.combs, ncol=1)
dev.off()

# Density of residuals ----------------------------------------------------

ggplot(coverValues, aes(Residual,)) +
  geom_density()+
  geom_vline(xintercept = 0.00,linetype=2,colour="red",size=2)+  
  theme_bw()

