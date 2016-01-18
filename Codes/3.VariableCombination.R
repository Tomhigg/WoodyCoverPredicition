# *------------------------------------------------------------------
# | SCRIPT NAME: 3.VariableCombination.R
# | DATE:        15th January 2016 (new format, orginally summer 2015)
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
# |  1:  Run models with different variable combinations
# |  2:  Test models for significant differences
# |  3:  Generate summary statistics for model outputs

# *------------------------------------------------------------------
# | COMMENTS AND TODO:               
# |
# |  1:  Check and dox section 5+6
# |  2: 
# |  3: 

# |*------------------------------------------------------------------
# | CONTENTS:               
# |
# |  PART 1:  Make subset and list objects
# |  PART 2:  RF models runs
# |  PART 3:  Tidy outputs
# |  PART 4:  Model runs using the "best 7" model from RFE
# |  PART 5:  Tidy output and join dataframes
# |  PART 6:  Test if models are significantly different
# |  PART 7:  Make Plot-able objects
# *-----------------------------------------------------------------
# | UPDATES:               
# |
# |
# |*------------------------------------------------------------------
# | Packages Used:               
library(randomForest)
library(doParallel)
library(dplyr)
#library(devtools)
#install_github(repo = "environmentalinformatics-marburg/Rsenal")
library(Rsenal)

# |*------------------------------------------------------------------
# | Data Used:              
# | PCtrain from 1.SampleCreation.R
# | PCtest  from 1.SampleCreation.R
# | new_profile, best model RFE from 2.MachineLearningRFE.R


# Make subset and list objects --------------------------------------------


# Make new vector of cover values, makes cbind quicker later
PCtrain.cover  <- PCtrain$pc
PCtest.cover   <- PCtest$pc


#make a list of vectors for model variable combinations
model.combination.vecList  <- list(c(4:9),c(4:11),c(10:11),c(12:17),c(12:19),c(18:19),
                                   c(4:9,12:17), c(4:19),c(18,19,10,11),
                                   c(20,21),c(20,21,12:17),c(20,21,4:9),c(20,21,4:9,12:17),c(4:21))

#repeat vector list 20 times
model.combination.vecList  <- rep(x = model.combination.vecList,times = 20)

#test that the combinations are right 
for (i in model.combination.vecList){
  print(head(PCtrain[,i]))}

# RF models runs ----------------------------------------------------------
#make cluster object
cl <- makeCluster(mc <- getOption("cl.cores", detectCores()-1))
registerDoParallel(cl, cores=12)
clusterExport(cl=cl, varlist=c("PCtest", "PCtrain","PCtest.cover", "PCtrain.cover","model.combination.vecList"),envir=environment())
clusterEvalQ(cl, library(Rsenal))
clusterEvalQ(cl, library(randomForest))

model.test  <-as.data.frame(do.call(rbind, parLapply(cl = cl,X = model.combination.vecList, 
                                                     fun = function(i){
                                                       # select samples from the training data set, i 
                                                       train.sample<- PCtrain[,i]
                                                       train.sample  <-  cbind(PCtrain.cover ,train.sample)
                                                       # run random forest on the training sample
                                                       randfor <- randomForest(PCtrain.cover ~ . , data=train.sample)
                                                       # use the model to predict onto the validation data  
                                                       val.predicted <- stats::predict(randfor,PCtest[,i])  
                                                       #subset the "real" validation values  
                                                       #run regression stats (Marburg Rsenel) and populate data frame   
                                                       stats <- regressionStats(val.predicted, PCtest.cover, adj.rsq = TRUE, plot = FALSE)
                                                       return(stats)})))
stopCluster(cl)

# Tidy outputs ------------------------------------------------------------

modelNames  <- c("Wet SR","Wet SR+VI","Wet VI",
                 "Dry SR", "Dry SR+VI","Dry VI",
                 "Dry & Wet SR","Dry & Wet SR+VI","Dry & Wet VI",
                 "PALSAR","PALSAR & Dry SR","PALSAR & Wet SR","PALSAR & Wet+Dry SR","All" )

modelNames  <- rep(x = modelNames,times = 20)

model.test$Model  <- modelNames

# Model runs using the "best 7" model from RFE ----------------------------

#get list of the best variable from new_profile rfe object
best.7.rfe.model  <- new_profile$bestVar

rfe_mod_reps  <- rep(list(best.7.rfe.mode),times = 20)

cl <- makeCluster(mc <- getOption("cl.cores", 10))
registerDoParallel(cl, cores=12)
clusterExport(cl=cl, varlist=c("PCtest", "PCtrain","PCtest.cover", "PCtrain.cover","model.combination.vecList", "rfe_mod_reps"),envir=environment())
clusterEvalQ(cl, library(Rsenal))
clusterEvalQ(cl, library(randomForest))

rfe.model.test  <-as.data.frame(do.call(rbind, parLapply(cl = cl,X = rfe_mod_reps, 
                                                         fun = function(i){
                                                           # select samples from the training data set, i 
                                                           train.sample<- PCtrain[,i]
                                                           train.sample  <-  cbind(PCtrain.cover ,train.sample)
                                                           # run random forest on the training sample
                                                           randfor <- randomForest(PCtrain.cover ~ . , data=train.sample)
                                                           # use the model to predict onto the validation data  
                                                           val.predicted <- stats::predict(randfor,PCtest[,i])  
                                                           #subset the "real" validation values  
                                                           #run regression stats (Marburg Rsenel) and populate data frame   
                                                           stats <- regressionStats(val.predicted, PCtest.cover, adj.rsq = TRUE, plot = FALSE)
                                                           return(stats)})))
stopCluster(cl)

# Tidy output and join dataframes -----------------------------------------

rfe.model.test$Model  <- "RFE"

model.test <- rbind(model.test,rfe.model.test)

model.summary.stats <- model.test %>% select(Model, Rsq, RMSE)%>% gather(Metric, Value, Rsq,RMSE) %>% group_by(Model, Metric)%>%
  summarise(mn=mean(Value),std = sd(Value))

# Test if models are significantly different  --------------------------
model.test.r2.aov = aov(Rsq ~ Model, data = model.test[,7:8])
modl.tes.r2.aov.hsd <- TukeyHSD(model.test.r2.aov)



# Make Plot-able objects --------------------------------------------------

model.test.r2.forplot <-    model.test %>% select(Model, Rsq) %>% group_by(Model) %>% summarise(mn=mean(Rsq)) %>% arrange(mn)
model.test.r2.forplot$Model  <- factor(model.test.r2.forplot$Model,levels=unique(model.test.r2.forplot$Model))
model.test.r2.forplot$grp  <- "grp"

model.test.rmse.forplot <-    model.test %>% select(Model, RMSE) %>% group_by(Model) %>% summarise(mn=mean(RMSE)) %>% arrange(mn)
model.test.rmse.forplot$Model  <- factor(model.test.rmse.forplot$Model,levels=unique(model.test.rmse.forplot$Model))
model.test.rmse.forplot$grp  <- "grp"

