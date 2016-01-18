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
# |  2:  Run script with different model combinations

# |*------------------------------------------------------------------
# | CONTENTS:               
# |
# |  PART 1:  Make subset and list objects
# |  PART 2:  Run Sample Size Test Loop
# |  PART 3:  Tidy outputs
# *-----------------------------------------------------------------
# | UPDATES:               
# |
# |
# |*------------------------------------------------------------------
# | Packages Used:        

#library(randomForest)
library(doParallel)
library(dplyr)
#library(devtools)
#install_github(repo = "environmentalinformatics-marburg/Rsenal")
library(Rsenal)

# |*------------------------------------------------------------------
# | Data Used:               
# | PCtrain from 1.SampleCreation.R
# | PCtest  from 1.SampleCreation.R

# Make subset and list objects --------------------------------------------
sample.test.vector  <- c(rep(seq(from = 1000,to = 29000,by = 1000),40))

#make new data frame that will be subsetting
#need to test this with different models
#predictors
samples.training  <- PCtrain[,4:9]
#Y
samples.training$pc  <- PCtrain[, 3]

#make new data fram for validation subsetting 
#need to test this with different models
#Predictors
samples.validation  <- PCtest[,4:9]
#Y
samples.validation.actual  <- PCtest[,3]

#make cluster object for parallel-isation
cl <- makeCluster(mc <- getOption("cl.cores", detect_core()-1))
registerDoParallel(cl, cores=12)
clusterExport(cl=cl, varlist=c("samples.training", "samples.validation","samples.validation.actual"),envir=environment())
clusterEvalQ(cl, library(raster))
clusterEvalQ(cl, library(Rsenal))
clusterEvalQ(cl, library(randomForest))


# Run Sample Size Test Loop -----------------------------------------------

sample.runs  <-as.data.frame(do.call(rbind, parLapply(cl = cl,X = sample.test.vector, 
                                                      fun = function(i){
                                                        # select samples from the training data set, i 
                                                        train.sample<- training[sample(nrow(samples.training), i),]
                                                        # run random forest on the training sample
                                                        randfor <- randomForest(pc ~ . , data=train.sample)
                                                        # use the model to predict onto the validation data  
                                                        val.predicted <- stats::predict(randfor,samples.validation)  
                                                        #subset the "real" validation values  
                                                        #run regression stats (Marburg Rsenel) and populate data frame   
                                                        stats <- regressionStats(val.predicted, samples.validation.actual, adj.rsq = TRUE, plot = FALSE)
                                                        return(stats)})))
#stop cluster of breakage
stopCluster(cl)

# Tidy outputs ------------------------------------------------------------

#Add sample size collumn to data frame
sample.runs$Sample.No  <- sample.test.vector

#melt to make plot-abl
sample.runs.melt  <- melt(sample.runs,id.vars = "Sample.No")


sample.runs.melt.forplot  <- rbind(
  filter(sample.runsMelt,variable == "Rsq"),
  filter(sample.runsMelt,variable == "RMSE"))










