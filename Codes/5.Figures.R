# *------------------------------------------------------------------
# | SCRIPT NAME: 5.Figures.R
# | DATE:        18th January 2016 (new format, orginally summer 2015)
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
# |  1: Make Figure for publication
# *------------------------------------------------------------------
# | COMMENTS AND TODO:               
# |
# |  1:  Add standard error functions to zebu
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
# | 
# |
# |*------------------------------------------------------------------
# | Packages Used:               
library(ggplot2)
library(gridExtra)
library(dplyr)

#functions to calculate standard errors
std <- function(x) sd(x)/sqrt(length(x))
std_h  <- function(x) mean(x)+(sd(x)/sqrt(length(x)))
std_l  <- function(x) mean(x)-(sd(x)/sqrt(length(x)))
#functions to plot standard errors 
stat_sum_single <- function(fun, geom="point", ...) {
  stat_summary(fun.y=fun, colour="red", geom=geom, size = 3, ...)
}
stat_sum_single2 <- function(fun, geom="point", ...) {
  stat_summary(fun.y=fun, colour="blue", geom=geom, size = 7,shape= 95, ...)
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

pdf("Data/Outputs/5.Figures/VariableImport.pdf")

ggplot(Variable.importance, aes(x= VariableLabel, y=Overall,fill=classvi))+ 
  geom_bar(stat="identity") + coord_flip() + scale_y_continuous(limits=c(0,70))+
  theme_bw() + scale_fill_manual(values=c("darkolivegreen3", "darkolivegreen4", "brown","blue","blue4"))+
  ylab("Scaled Variable Importance")+ xlab("Variable")+
  geom_rect(aes(xmin = 18 + 0.5, xmax = 12 - 0.5, ymin = 0 , ymax = 70 - 0.5),
            fill = "transparent", color = "black", size = 1,linetype=2,show.legend = F)+
  theme(legend.title=element_blank(),legend.justification = c(1, 0),legend.position = c(1, 0))  

  
dev.off()

# 2. RFE Output Plots -----------------------------------------------------

plot1<- ggplot(filter(rfeMetrics,variable=="Rsquared"),aes(x = Variables,y = value))+
  geom_line(col="darkgrey",size=1)+
  scale_shape_identity() +
  stat_sum_single2(std_h)+
  stat_sum_single2(std_l)+
  stat_sum_single(mean)+
  ylab(bquote(~ R^2))+xlab("")+
  theme_bw(base_size = 22)+
  geom_hline(yintercept=0.65298831,linetype=2,col= "darkgreen",size=1)+
  geom_hline(yintercept=0.65298831*0.95,linetype=2,col= "darkgreen",size=1)

plot2 <- ggplot(filter(rfeMetrics,variable=="RMSE"),aes(x = Variables,y = value))+
  geom_line(col="darkgrey",size=1)+
  scale_shape_identity() +
  stat_sum_single2(std_h)+
  stat_sum_single2(std_l)+
  stat_sum_single(mean)+
  ylab("RMSE")+xlab("Number of Variables")+
  theme_bw(base_size = 22)+
  geom_hline(yintercept=0.09367232,linetype=2,col= "darkgreen",size=1)+
  geom_hline(yintercept=0.09367232*1.05,linetype=2,col= "darkgreen",size=1)+
  scale_y_reverse()

pdf(file = "rfe.pdf",width = 15,height = 12)
grid.arrange(plot1, plot2, ncol=1)
dev.off()

# 3. Heatscatter ----------------------------------------------------------

pdf(file = "heatscatter.pdf",width = 8,height = 6)
ggplot(coverValues, aes(x=Predicted, y=Actual)) +
  stat_binhex(bins=75)+
  scale_fill_gradient(low="bisque", high="magenta4",name = "Frequency",na.value=NA)+
  geom_smooth(method="lm")+  
  scale_y_continuous(limits=c(0,1))+
  scale_x_continuous(limits=c(0,1))+
  geom_abline(intercept = 0, slope = 1, color="red", 
              linetype="dashed", size=1)+ 
  theme_bw(base_size = 22)+
  labs(x="Predicted Value",y="Actual Value")+
  guides(fill=guide_legend(title="Count"))
dev.off()

# 4. Model Combinations ---------------------------------------------------

plot1.combs<- ggplot(model.test.r2.forplot,aes(x = Model,y = mn, group=grp))+
  geom_point(size=4,col="red")+ geom_line(size=1,col="darkgray",linetype="dashed")+
  geom_hline(yintercept= 0.6470302,linetype=2,col= "darkgreen",size=1)+
  geom_hline(yintercept= 0.6470302*0.95,linetype=2,col= "darkgreen",size=1)+
  ylab(bquote('Adjusted'~ R^2))+xlab("")+
  theme_bw(base_size = 22)+theme(axis.text.x=element_text(angle=25,hjust=1),plot.margin= unit(rep(.5, 4), "lines"))


plot2.combs<- ggplot(model.test.rmse.forplot,aes(x = Model,y = mn, group=grp))+
  geom_point(size=4,col="red")+ geom_line(size=1,col="darkgray",linetype="dashed")+
  geom_hline(yintercept= 0.09433350,linetype=2,col= "darkgreen",size=1)+
  geom_hline(yintercept= 0.09433350*1.05,linetype=2,col= "darkgreen",size=1)+
  ylab(bquote("RMSE"))+xlab("")+
  theme_bw(base_size = 22)+theme(axis.text.x=element_text(angle=25,hjust=1),plot.margin= unit(rep(.5, 4), "lines"))+
  scale_y_reverse()+
  scale_x_discrete(limits = rev(levels(model.test.rmse.forplot$Model)))


pdf(file = "models.pdf",width = 15,height = 12)
grid.arrange(plot1.combs, plot2.combs, ncol=1)
dev.off()

# 5. Sample Size Test -----------------------------------------------------


plot1.sampsize <- ggplot(filter(sample.runs.all,variable=="Rsq"),aes(factor(Sample.No),y = value))+
  geom_line(col="darkgrey",size=1)+
  scale_shape_identity() +
  stat_sum_single(mean)+
  stat_sum_single2(std_h)+
  stat_sum_single2(std_l)+
  theme_bw()+ theme(text = element_text(size=20))+
  ylab(bquote('Adjusted'~ R^2))+xlab("")+
  scale_x_discrete(breaks=seq(500, 30000, 2500))
  #geom_hline(yintercept=0.5834400+7.180483e-06,linetype=2,col= "darkgreen",size=1)+
  #geom_hline(yintercept=0.5834400+7.180483e-06,linetype=2,col= "darkgreen",size=1)
  
  
  plot2.sampsize <- ggplot(filter(sample.runs.all,variable=="RMSE"),aes(factor(Sample.No),y = value))+
  geom_line(col="darkgrey",size=1)+
  scale_shape_identity() +
  stat_sum_single(mean)+
  stat_sum_single2(std_h)+
  stat_sum_single2(std_l)+
  theme_bw()+ theme(text = element_text(size=20))+
  xlab("Number of Samples")+
  ylab("RMSE")+
  scale_x_discrete(breaks=seq(500, 30000, 2500))+
  scale_y_reverse()
  #geom_hline(yintercept= 0.1024951+7.180483e-06,linetype=2,col= "darkgreen",size=1)

#pdf(file = "D:/Projects_new/woodycovermodels/CodeData/Data/Outputs/5.Figures/samplesize.pdf",width = 15,height = 12)
grid.arrange(plot1.sampsize, plot2.sampsize, ncol=1)
dev.off()


