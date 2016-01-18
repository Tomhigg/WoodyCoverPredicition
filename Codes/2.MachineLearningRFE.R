# *------------------------------------------------------------------
# | SCRIPT NAME: 2.MachineLearningRFE.R
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
# |  1: Run recursive feature eliminations 
# |  2: Create "plotable" object from rfe results
# |  3: Generate summary statistics for rfe output
# |
# *------------------------------------------------------------------
# | COMMENTS AND TODO:               
# |
# |  1:  
# |  2: 
# |  3: 

# |*------------------------------------------------------------------
# | CONTENTS:               
# |
# |  PART 1:  Recursive Feature Elimination
# |  PART 2:  Make plot-able objects from RFE output 1.RMSE & R2
# |  PART 3:  Make plot-able objects from RFE output 2.Variable Importance
# |  PART 4:  Make plot-ables object from RFE output 3. Predicted vs Actual for heatscatter
# |  PART 5:  Get summary stats from rfe outputs
# *-----------------------------------------------------------------
# | UPDATES:               
# |
# |
# |*------------------------------------------------------------------
# | Packages Used:            

library(caret)
library(doParallel)
library(reshape2)
library(dplyr)

# |*------------------------------------------------------------------
# | Data Used: 

# | PCtrain from 1.SampleCreation.R
# | PCtest  from 1.SampleCreation.R

# Recursive Feature Elimination -------------------------------------------

#number of variable combination sizes to test
subsets <- c(1:19)

# Controll object detailing the model building procedures
ctrl <- rfeControl(functions = rfFuncs,
                   method = "cv",
                   repeats = 20,
                   verbose = FALSE,
                   allowParallel = TRUE,
                   returnResamp = "all")

#make cluster object to run rfe in parallel
cl <- makeCluster(mc <- getOption("cl.cores", detectCores()-1))
registerDoParallel(cl, cores=12)
#this takes a long time 
rfProfile <- rfe(x = PCtrain[,4:21],y = PCtrain[,3], sizes = subsets, rfeControl = ctrl)
#remember to stop cluster or everything starts to break :(
stopCluster(cl)


# profile with the best 7 variables
new_profile <- update(rfProfile,PCtrain[,4:21],y = PCtrain[,3], size = 7)

# Make plot-able objects from RFE output 1.RMSE & R2 ----------------------------------

# RMSE & Rsquared for each repeat and variable number
rfeMetrics  <- rfProfile$resample
rfeMetrics$Resample  <- NULL
rfeMetrics  <- melt(data = rfeMetrics,id.vars = "Variables")

# Make plot-able objects from RFE output 2.Variable Importance ------------

####  Variable Importance 
Variable.importance  <- varImp(rfProfile)
Variable.importance  <- data.frame(Variable.importance)
Variable.importance$Variable  <- row.names(Variable.importance)
# for loop to create "class" variable based on 1st charecter in variable 
for(i in 1:nrow(Variable.importance)) {
  row <- Variable.importance[i,]
  if(substr(x =Variable.importance$Variable[i],start = 1,stop = 1)=="W"){
    Variable.importance$Class[i]<- "Wet Landsat"
  } else {
    if(substr(x =Variable.importance$Variable[i],start = 1,stop = 1)=="H"){
      Variable.importance$Class[i] <- "Radar"    #
    } else {
      Variable.importance$Class[i] <- "Dry Landsat"   
    }
  }}
rm(i,row)
# add class for VI
for(i in 1:nrow(Variable.importance)) {
    row <- Variable.importance[i,]
  if(substr(x =Variable.importance$Variable[i],start = 5,stop = 5)=="M"){
    Variable.importance$VI[i]<- "VI"
  } else {
    if(substr(x =Variable.importance$Variable[i],start = 5,stop = 5)=="N"){
      Variable.importance$VI[i] <- "VI"    #
    } 
    else {
      if(substr(x =Variable.importance$Variable[i],start = 5,stop = 5)=="B"){
        Variable.importance$VI[i] <- "SR"    #
      } 
      else {
        Variable.importance$VI[i] <- " "   
      }
    }}}
  rm(i,row)
  
# merge cols to make class.var, needed for ggplot colouring
Variable.importance$classvi <- do.call(paste, c(Variable.importance[c("Class", "VI")], sep = " ")) 
    
#reorder data frame in importance 
Variable.importance <- transform(Variable.importance,Variable = reorder(Variable, Overall))
Variable.importance$VariableLabel  <- str_replace(string = Variable.importance$Variable,pattern = "_",replacement = " ")
Variable.importance <- transform(Variable.importance,VariableLabel = reorder(VariableLabel, Overall))

 
  

# Make plot-ables object from RFE output 3. Predicted vs Actual for heatscatter --------

predicted  <- stats::predict(rfProfile, PCtest[,4:21])
coverValues  <- data.frame(predicted, PCtest$pc)
coverValues  <- rename(coverValues, Actual = PCtest.pc)
coverValues  <- rename(coverValues, Predicted = predicted)

# Get summary stats from rfe outputs ------------------------------------------
 
#set variable as factor
rfeMetrics$Variables  <- factor(rfeMetrics$Variables)

#mean and standard deviation for each variable subset, for R2 and RMSE
rfe_grouped_metrics <- rfeMetrics %>% 
    group_by(Variables,variable) %>%
    summarise(std=sd(value), mn= mean(value))
  
  








