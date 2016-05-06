# *------------------------------------------------------------------
# | SCRIPT NAME: 4.SampleSizeTesting.R
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
# |  1:  Run models varying sample size
# |  2:  Calculate Summary Statistics for sample size groups
# |  3:  Convert output to a plotable object

# *------------------------------------------------------------------
# | COMMENTS AND TODO:               
# |
# |  1:  Fix fracCover function so don't need to repeat code block
# |  2:  Run script with different model combinations- used all varibles as best model 

# |*------------------------------------------------------------------
# | CONTENTS:               
# |
# |  PART 1:  Make subset and list objects
# |  PART 2:  Run Sample Size Test Loop
# |  PART 3:  Tidy outputs
# *-----------------------------------------------------------------
# | UPDATES:               
# | 1: 11/2/2016 Move regressionStats from Rsenal to zebu 
# |
# |*------------------------------------------------------------------
# | Packages Used:        

#library(randomForest)
library(doParallel)
library(dplyr)
library(zebu)
library(reshape2)
library(devtools)

# |*------------------------------------------------------------------
# | Data Used:               
# | PCtrain from 1.SampleCreation.R
# | PCtest  from 1.SampleCreation.R

PCtrain <- read.csv(file = "D:/Projects_new/woodycovermodels/CodeData/Data/Training.csv",header = TRUE)
PCtest <- read.csv(file = "D:/Projects_new/woodycovermodels/CodeData/Data/Testing.csv",header = TRUE)

#drop x,y and rownumbers colls
PCtrain <- PCtrain[,4:22]
PCtest <- PCtest[,4:22]

# Make Vector of samples sizes to test --------------------------------------------
sample.test.vector  <- c(rep(seq(from = 10000,to = 29000,by = 3000),10))
sample.test.vector2  <- c(rep(seq(from = 1000,to = 9000,by = 1000),10))


#make new data frame that will be subsetting
#need to test this with different models
#predictors
samples.training  <- PCtrain[,4:22]
#Y
#samples.training$pc  <- PCtrain

#make new data fram for validation subsetting 
#need to test this with different models
#Predictors
samples.validation  <- PCtest[,5:22]
#Y
samples.validation.actual  <- PCtest$pc

#make cluster object for parallel-isation
cl <- makeCluster(outfilmc <- getOption("cl.cores", 11),outfile="")
registerDoParallel(cl, cores=11)
clusterExport(cl=cl, varlist=c("regressionStats2","samples.training", "samples.validation","samples.validation.actual"),envir=environment())
#clusterEvalQ(cl, library(zebu))
clusterEvalQ(cl, library(randomForest))


# Run Sample Size Test Loop -----------------------------------------------

sample.runs  <-as.data.frame(do.call(rbind, parLapply(cl = cl,X = sample.test.vector, 
                                                      fun = function(i){
                                                        # select samples from the training data set, i 
                                                        train.sample<- samples.training[sample(nrow(samples.training), i),]
                                                        # run random forest on the training sample
                                                        randfor <- randomForest(pc ~ . , data=train.sample)
                                                        # use the model to predict onto the validation data  
                                                        val.predicted <- stats::predict(randfor,samples.validation)  
                                                        #subset the "real" validation values  
                                                        #run regression stats (Marburg Rsenel) and populate data frame   
                                                        stats <- regressionStats2(val.predicted, samples.validation.actual, adj.rsq = TRUE, plot = FALSE)
                                                        return(stats)})))
#stop cluster of breakage
stopCluster(cl)




cl <- makeCluster(outfilmc <- getOption("cl.cores", 11),outfile="")
registerDoParallel(cl, cores=11)
clusterExport(cl=cl, varlist=c("regressionStats2","samples.training", "samples.validation","samples.validation.actual"),envir=environment())
#clusterEvalQ(cl, library(zebu))
clusterEvalQ(cl, library(randomForest))


# Run Sample Size Test Loop -----------------------------------------------

sample.runs2  <-as.data.frame(do.call(rbind, parLapply(cl = cl,X = sample.test.vector2, 
                                                      fun = function(i){
                                                        # select samples from the training data set, i 
                                                        train.sample<- samples.training[sample(nrow(samples.training), i),]
                                                        # run random forest on the training sample
                                                        randfor <- randomForest(pc ~ . , data=train.sample)
                                                        # use the model to predict onto the validation data  
                                                        val.predicted <- stats::predict(randfor,samples.validation)  
                                                        #subset the "real" validation values  
                                                        #run regression stats (Marburg Rsenel) and populate data frame   
                                                        stats <- regressionStats2(val.predicted, samples.validation.actual, adj.rsq = TRUE, plot = FALSE)
                                                        return(stats)})))
#stop cluster of breakage
stopCluster(cl)

sample.runs2$Sample.No  <- sample.test.vector2
plot(sample.runs2$Rsq~sample.runs2$Sample.No)
sample.runs.melt2  <- melt(sample.runs2,id.vars = "Sample.No")


# Tidy outputs ------------------------------------------------------------

#Add sample size collumn to data frame
sample.runs$Sample.No  <- sample.test.vector

plot(sample.runs$Rsq~sample.runs$Sample.No)

#melt to make plot-abl
sample.runs.melt  <- melt(sample.runs,id.vars = "Sample.No")



sample.runs.all <- rbind(sample.runs,sample.runs2)
sample.runs.all <- melt(sample.runs.all,id.vars = "Sample.No")


plot(sample.runs.all$Sample.No, sample.runs.all$Rsq)

# stats -------------------------------------------------------------------



sample.size.stats <- sample.runs %>% group_by(Sample.No) %>%summarise(mean.Rsq = mean(Rsq), 
                                                                           mean.RMSE = mean(RMSE),
                                                                           std.Rsq = sd(Rsq),
                                                                           std.RMSE = sd(RMSE))

samplesize.r2.095 <- max(sample.size.stats$mean.Rsq)*0.95
samplesize.r21.05 <- max(sample.size.stats$mean.Rsq)*1.05








