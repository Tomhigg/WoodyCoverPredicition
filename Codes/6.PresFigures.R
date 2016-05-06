# *------------------------------------------------------------------
# | SCRIPT NAME: 5.PresentationFigures.R
# | DATE:        01st March 2016 
# | CREATED BY:  Tom Higginbottom 
# | PROJECT:     Predictive Modelling of Savannah Woody Cover

# *----------------------------------------------------------------
# | SessionInfo:   

#  R version 3.2.2 (2015-08-14)
#  Platform: x86_64-w64-mingw32/x64 (64-bit)
#  Running under: Windows 7 x64 (build 7601) Service Pack 1
# *----------------------------------------------------------------
# | PURPOSE:         
# |
# |  1: Make Figure for EGU 2016 Presentation
# *------------------------------------------------------------------
# | COMMENTS AND TODO:               
# |
# |  1:  Add standard error functions to zebu-DONE
# |  2:  Change sample size to within "5%" not standard errors 
# |  3: 

# |*------------------------------------------------------------------
# | CONTENTS:               
# |
# |  PART 1:  1. Variable Importance Plot
# |  PART 2:  2. RFE Output Plots
# |  PART 3:  3. Heatscatter
# |  PART 4:  4. Model Combinations
# |  PART 5:  5. Sample Size Test
# *-----------------------------------------------------------------
# | UPDATES:               
# |  06/05/2016 theme_solarized_presentation added to zebu
# |
# |*------------------------------------------------------------------
# | Packages Used:               
library(ggplot2)
library(gridExtra)
library(dplyr)
library(ggthemes)
library(viridis)
library(reshape2)
library(zebu)
#functions to calculate standard errors
std <- function(x) sd(x)/sqrt(length(x))
std_h  <- function(x) mean(x)+(sd(x)/sqrt(length(x)))
std_l  <- function(x) mean(x)-(sd(x)/sqrt(length(x)))
#functions to plot standard errors 
stat_sum_single <- function(fun, geom="point", ...) {
  stat_summary(fun.y=fun, colour="red", geom=geom, size = 3, ...)
}
stat_sum_single2 <- function(fun, geom="point", ...) {
  stat_summary(fun.y=fun, colour="steelblue", geom=geom, size = 6,shape= 95, ...)
}


# |
# |*------------------------------------------------------------------
# | Data Used:    
# | Variable.importance - from 2.MachineLearningRFE.R
# | rfeMetrics - from 2.MachineLearningRFE.R
# | coverValues - from 2.MachineLearningRFE.R
# | model.test.r2.forplot - from 3.VariableCombinations.R
# | model.test.rmse.forplot - from 3.VariableCombinations.R
# | sample.runs.melt.forplot - from SampleSizeTesting.R

# 1. Variable Importance Plot  ------------------------------------------------------------

pdf("VariableImport.pdf",width =11 ,height = 8)


ggplot(Variable.importance, aes(x= VariableLabel, y=Overall,fill=classvi))+ 
  geom_bar(stat="identity") + coord_flip() + scale_y_continuous(limits=c(0,70))+
  scale_fill_manual(values=c("springgreen3", "springgreen4", "violetred4","steelblue1","steelblue4"))+
  ylab("Scaled Variable Importance")+ 
  geom_rect(aes(xmin = 18 + 0.5, xmax = 12 - 0.5, ymin = 0 , ymax = 70 - 0.5),
            fill = "transparent", color = "cornsilk", size = 2,linetype=2,show.legend = F)+
  ylab("Variable Importance (%)")+
  xlab("")+
  theme_solarized_presentation(base_size=22, light=FALSE)+
  theme(legend.title=element_blank(),legend.justification = c(1, 0),legend.position = c(1, 0), legend.direction= "vertical") 
  
  
dev.off()

# 2. RFE Output Plots -----------------------------------------------------

plot1<- ggplot(filter(rfeMetrics,variable=="Rsquared"),aes(x = Variables,y = value))+
  geom_line(colour= "turquoise",size=1)+
  scale_shape_identity() +
  stat_sum_single(mean)+
  #stat_sum_single2(std_h)+
  #stat_sum_single2(std_l)+
  ylab("")+xlab("")+
  ggtitle("R-Squared")+
  geom_hline(yintercept=0.65298831,linetype=2,col= "green",size=1)+
  geom_hline(yintercept=0.65298831*0.95,linetype=2,col= "green",size=1)+
  theme_solarized_presentation(base_size=22, light=FALSE)

plot2 <- ggplot(filter(rfeMetrics,variable=="RMSE"),aes(x = Variables,y = value))+
  geom_line(col="turquoise",size=1)+
  scale_shape_identity() +
  stat_sum_single(mean)+
  #stat_sum_single2(std_h)+
  #stat_sum_single2(std_l)+
  ggtitle("Root Mean Squared Error")+
  xlab("Number of Variables")+
  ylab("")+
  geom_hline(yintercept=0.09367232,linetype=2,col= "green",size=1)+
  geom_hline(yintercept=0.09367232*1.05,linetype=2,col= "green",size=1)+
  scale_y_reverse()+
  theme_solarized_presentation(base_size=22, light=FALSE)


pdf(file = "rfe.pdf",width = 15,height = 12)
grid.arrange(plot1, plot2, ncol=1)
dev.off()

# 3. Heatscatter ----------------------------------------------------------

pdf(file = "heatscatter.pdf",width = 12,height = 7)
ggplot(coverValues, aes(x=Predicted, y=Actual)) +
  stat_binhex(bins=75)+
  geom_density_2d(bins = 3,colour = "white",lwd=1)+
  scale_fill_viridis(name = "Frequency",na.value=NA)+
  geom_smooth(method="lm")  +  
  scale_y_continuous(limits=c(0,1))+
  scale_x_continuous(limits=c(0,1))+
  geom_abline(intercept = 0, slope = 1, color="red",linetype="dashed", size=1)+ 
  labs(x="Predicted Value",y="Actual Value")+
  theme_solarized_presentation(base_size=22, light=FALSE)
dev.off()

# 4. Model Combinations ---------------------------------------------------

plot1.combs<- ggplot(model.test.r2.forplot,aes(x = Model,y = mn, group=grp))+
  geom_point(size=4,col="red")+ 
  geom_line(size=1,col="cornsilk",linetype="dashed")+
  geom_hline(yintercept= 0.6470302,linetype=2,col= "green",size=1)+
  geom_hline(yintercept= 0.6470302*0.95,linetype=2,col= "green",size=1)+
  xlab("")+ylab("")+
  theme_solarized_presentation(base_size=22, light=FALSE)+
  theme(axis.text.x=element_text(angle=25,hjust=1),plot.margin= unit(rep(.5, 4), "lines"))+
  ggtitle("R-Squared")

plot2.combs<- ggplot(model.test.rmse.forplot,aes(x = Model,y = mn, group=grp))+
  geom_point(size=4,col="red")+ geom_line(size=1,col="cornsilk",linetype="dashed")+
  geom_hline(yintercept= 0.09433350,linetype=2,col= "green",size=1)+
  geom_hline(yintercept= 0.09433350*1.05,linetype=2,col= "green",size=1)+
  xlab("")+ylab("")+
  theme_solarized_presentation(base_size=22, light=FALSE)+
  theme(axis.text.x=element_text(angle=25,hjust=1),plot.margin= unit(rep(.5, 4), "lines"))+
  scale_y_reverse()+
  scale_x_discrete(limits = rev(levels(model.test.rmse.forplot$Model)))+
  ggtitle("Root Mean Squared Error")

pdf(file = "models.pdf",width = 15,height = 17)
grid.arrange(plot1.combs, plot2.combs, ncol=1)
dev.off()

# 5. Sample Size Test -----------------------------------------------------


plot1.sampsize <- ggplot(filter(sample.runs.all,variable=="Rsq"),aes(factor(Sample.No),y = value))+
  geom_line(col="cornsilk",size=1)+
  scale_shape_identity() +
  stat_sum_single(mean)+
  #stat_sum_single2(std_h)+
  #stat_sum_single2(std_l)+
  theme_bw()+ theme(text = element_text(size=20))+
  ylab(bquote('Adjusted'~ R^2))+xlab("")+
  scale_x_discrete(breaks=seq(500, 30000, 2500))+
  #geom_hline(yintercept=0.5834400+7.180483e-06,linetype=2,col= "darkgreen",size=1)+
  #geom_hline(yintercept=0.5834400+7.180483e-06,linetype=2,col= "darkgreen",size=1)
theme_solarized_presentation()
  
  plot2.sampsize <- ggplot(filter(sample.runs.all,variable=="RMSE"),aes(factor(Sample.No),y = value))+
  geom_line(col="cornsilk",size=1)+
  scale_shape_identity() +
  stat_sum_single(mean)+
  #stat_sum_single2(std_h)+
  #stat_sum_single2(std_l)+
  xlab("Number of Samples")+
  ylab("RMSE")+
  scale_x_discrete(breaks=seq(500, 30000, 2500))+
  scale_y_reverse()+
  #geom_hline(yintercept= 0.1024951+7.180483e-06,linetype=2,col= "darkgreen",size=1)
    theme_solarized_presentation(base_size=22, light=FALSE)
  
  
pdf(file = "D:/Projects_new/woodycovermodels/CodeData/Data/Outputs/5.Figures/samplesize.pdf",width = 15,height = 12)
grid.arrange(plot1.sampsize, plot2.sampsize, ncol=1)
dev.off()

# Rainfall plot -----------------------------------------------------------

rain <- read.csv(file = "c:/Users/55110140/Google Drive/laptopSync/LandsatComp_figures/SA_NP_RFE.csv",header = TRUE)
rain$Period <- rain$Period*10


rain.melt <- melt(data = rain[2:17],id.vars = "Period")

rain.stats <- rain.melt %>% group_by(Period)%>% summarise(mean=mean(value,na.rm = TRUE),
                                                        sdH =mean+sd(value,na.rm = TRUE),
                                                        sdL =mean-sd(value,na.rm=TRUE))

pdf("rainfall.pdf",width =14 ,height = 8)

ggplot(rain.stats,aes(x = Period))+
  scale_x_continuous(breaks = c(50,100,150,200,250,300,350))+
  coord_cartesian(ylim = c(5, 120))+ 
  geom_ribbon(aes(ymin=sdL, ymax=sdH),fill = "steelblue4")+
  geom_point(data = rain.melt,aes(x = Period,y = value),colour="steelblue1", size= 3)+
  geom_line(aes(y = mean),size=2,colour= "violetred4" )+
  xlab("Day")+ylab("Rainfall (mm)")+
  theme(plot.title = element_text(hjust =0 ))+
  geom_vline(xintercept = 130,linetype=2,size=2,colour="springgreen3")+
  geom_vline(xintercept = 280,linetype=2,size=2,colour = "springgreen3")+
  theme_solarized_presentation(base_size=22, light=FALSE)

dev.off()

