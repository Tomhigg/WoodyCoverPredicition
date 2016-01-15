# Packages and preliminaries ----------------------------------------------------------------
library(raster)
library(spatial.tools)
library(teamlucc)
library(devtools)
install_github(repo = "environmentalinformatics-marburg/Rsenal")
library(Rsenal)
library(randomForest)
library(foreach)
library(tidyr)
library(dplyr)


study_area <- readOGR(dsn = "F:\\Projects\\shrub_cover_paper",layer = "Study_area")

# Proj4 co-ordinate string for South Africa Albers Equal Area
SA_AEAC_CRS  <- "+proj=aea +lat_1=-24 +lat_2=-33 +lat_0=0 +lon_0=24 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs "

# Load Landsat and PALSAR data --------------------------------------------
Landsat.list  <- list.files(path = "G:\\Projects\\shrub_cover_paper\\Input_data\\2008\\",pattern = "sr_band.*.tif$",full.names = TRUE,recursive = TRUE)
Landsat.wet  <- stack(sapply(X = Landsat.list[1:6],FUN = raster))
Landsat.wet  <- crop(x = Landsat.stack1,y = study_area)
msavi.wet  <- MSAVI2(red = (subset(Landsat.stack1,subset = 3)*0.0001), nir =(subset(Landsat.stack1,subset=4)*0.0001))*10000
ndvi.wet  <- NDVI(red = (subset(Landsat.stack1,subset = 3)*0.0001), nir =(subset(Landsat.stack1,subset=4)*0.0001))*10000

Landsat.dry  <- stack(sapply(X = Landsat.list[7:12],FUN = raster))
Landsat.dry  <- crop(x = Landsat.stack2,y = study_area)
msavi.dry  <- MSAVI2(red = (subset(Landsat.stack2,subset = 3)*0.0001), nir =(subset(Landsat.stack2,subset=4)*0.0001))*10000
ndvi.dry  <- NDVI(red = (subset(Landsat.stack2,subset = 3)*0.0001), nir =(subset(Landsat.stack2,subset=4)*0.0001))*10000

Landsat  <- stack(Landsat.wet, msavi.wet, ndvi.wet, Landsat.dry, msavi.dry,ndvi.dry)

band_names <- c("Wet_B1", "Wet_B2","Wet_B3","Wet_B4","Wet_B5","Wet_B7","Wet_MSAVI","Wet_NDVI","Dry_B1", "Dry_B2","Dry_B3","Dry_B4","Dry_B5","Dry_B7","Dry_MSAVI","Dry_NDVI")
names(Landsat) <- band_names
rm(Landsat.list,Landsat.stack1,Landsat.stack2,msavi.dry,msavi.wet,ndvi.dry,ndvi.wet)

palsar_stack  <- stack(sapply(X = list.files(path = "G:\\Projects\\shrub_cover_paper\\Input_data\\PALSAR\\",pattern = "*.tif$",full.names = TRUE),FUN = raster))

palsar_stack  <- spatial_sync_raster(unsynced = palsar_stack,reference = Landsat,method = "ngb")
palsar_stack  <- 10*log10((palsar_stack+0.001)^2)+-83
names(palsar_stack) <- c("HH", "HV")
Landsat_palsar  <- stack(Landsat,palsar_stack)
rm(palsar_stack,Landsat,band_names)

# Make training & validation csv ------------------------------------------
shrub.mask.list  <- list.files(path = "F:\\Projects\\shrub_cover_paper\\Training_data\\NGI_aerial_imagery\\shrub_masks\\",
                               pattern = "*.tif$",
                               full.names = TRUE)

Landsat_SA_CRS <-"F:\\Projects\\Shrub_cover_paper\\Training_data\\Landsat_SA_AEAC.tif"
from <- c(0,1, 2, 3)
to <-   c(2,1, 2, 2)

All.data <- csv_batch_parallel(no_cores = 10,list = shrub.mask.list,
                                 predImage = Landsat_SA_CRS,fromVals = from,toVals = to,
                                 numSamps = 10000)

xy.dats  <- SpatialPoints(coords = All.data[,1:2])
crs(xy.dats)  <- SA_AEAC_CRS
 
xy.dats <-spTransform(x = xy.dats,CRSobj =crs(Landsat_palsar)

All.data.extracts  <-cbind(All.data, extract(Landsat_palsar, y = xy.dats)) 

set.seed(3456)
trainIndex <- createDataPartition(All.data.extracts$pc, p = .5,
                                  list = FALSE,
                                  times = 1)

PCtrain <- All.data.extracts[ trainIndex,]
PCtest  <- All.data.extracts[-trainIndex,]

# Recursive Feature Elimination -------------------------------------------
subsets <- c(1:19)

ctrl <- rfeControl(functions = rfFuncs,
                   method = "cv",
                   repeats = 20,
                   verbose = FALSE,
                   allowParallel = TRUE,
                   returnResamp = "all")

cl <- makeCluster(mc <- getOption("cl.cores", 12))
registerDoParallel(cl, cores=12)
rfProfile <- rfe(x = PCtrain[,4:21],y = PCtrain[,3], sizes = subsets, rfeControl = ctrl)
#remember to stop cluster or everything starts to break :(
stopCluster(cl)


# profile with the best 7 variables

new_profile <- update(rfProfile,PCtrain[,4:21],y = PCtrain[,3], size = 7)
  

# Make plot-able objects from RFE output ----------------------------------

#### RMSE & Rsquared
rfeMetrics  <- rfProfile$resample
rfeMetrics$Resample  <- NULL
rfeMetrics  <- melt(data = rfeMetrics,id.vars = "Variables")

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
  }}
rm(i,row)

# merge cols to make class.var, needed for ggplot colouring

Variable.importance$classvi <- do.call(paste, c(Variable.importance[c("Class", "VI")], sep = " ")) 


#reorder data frame in importance 
Variable.importance <- transform(Variable.importance,Variable = reorder(Variable, Overall))
Variable.importance$VariableLabel  <- str_replace(string = Variable.importance$Variable,pattern = "_",replacement = " ")
Variable.importance <- transform(Variable.importance,VariableLabel = reorder(VariableLabel, Overall))

#get standard error of the best model for +1 standard error rule
metrics_rfe_18var_r2_rmse  <- filter(rfeMetrics,Variables == 18, variable == "RMSE"| variable== "Rsquared")
  
mean_stdError_bestMod <- tapply(metrics_rfe_18var_r2_rmse$value,metrics_rfe_18var_r2_rmse$variable,mean)
 <- tapply(metrics_rfe_18var_r2_rmse$value,metrics_rfe_18var_r2_rmse$variable,std.error)


predicted  <- stats::predict(rfProfile, PCtest[,4:21])
coverValues  <- data.frame(predicted, PCtest$pc)
coverValues  <- rename(coverValues, Actual = PCtest.pc)
coverValues  <- rename(coverValues, Predicted = predicted)

# Get figures from model outputs ------------------------------------------


rfeMetrics$Variables  <- factor(rfeMetrics$Variables)
  
rfe_grouped_metrics <- rfeMetrics %>% 
                        group_by(Variables,variable) %>%
                        summarise(std=sd(value), mn= mean(value))


sample_grouped_metrics <- sample.runsMelt %>% 
  group_by(Sample.No,variable) %>%
  summarise(std=sd(value), mn= mean(value))

modelcombin.metrcs <- model.test.melt%>% 
  group_by(Model,variable) %>%
  summarise(std=sd(value), mn= mean(value))

# Training size test ------------------------------------------------------

# sample.test.vector  <- c(rep(seq(from = 2000,to = 20000,by = 500),30))
# 
# sample.test.fun <- function(i){
#     # select samples from the training data set, i 
# train.sample<- training[sample(nrow(training), i),]
#     # drop the unused variables so only PC and Landsat wet are present
# train.sample  <- cbind(train.sample[3] ,train.sample[4:9])
#     # run random forest on the training sample
# randfor <- randomForest(pc ~ . , data=train.sample)
#     # use the model to predict onto the validation data  
# predicted <- stats::predict(randfor,validation[,4:9])  
#     #subset the "real" validation values  
# actual  <- validation[,3]
#     #run regression stats (Marburg Rsenel) and populate data frame   
# stats <- regressionStats(predicted, actual, adj.rsq = TRUE, plot = FALSE)
# return(stats)
#   }
# 
# # Bizare code line to make the outputs a proper data frame opposed to multiple lists :s 
# sample.size.tests  <- as.data.frame(do.call(rbind,lapply(X = sample.test.vector,FUN = sample.test.fun)))
# 
# #add sample number to data fram 
# sample.size.tests$Sample.No  <- sample.test.vector
# 
# 
# 
# sample.test.vector2  <- c(rep(seq(from = 20000,to = 30000,by = 500),30))
# 
# # Bizare code line to make the outputs a proper data frame opposed to multiple lists :s 
# sample.size.tests2  <- as.data.frame(do.call(rbind,lapply(X = sample.test.vector2,FUN = sample.test.fun)))
# 
# #add sample number to data fram 
# sample.size.tests2$Sample.No  <- sample.test.vector2

# Parallel Sample Size Test -----------------------------------------------

training  <- PCtrain[,4:9]
training$pc  <- PCtrain[, 3]

validation  <- PCtest[,4:9]
validation.actual  <- PCtest[,3]

cl <- makeCluster(mc <- getOption("cl.cores", 10))
registerDoParallel(cl, cores=12)
clusterExport(cl=cl, varlist=c("training", "validation","validation.actual"),envir=environment())
clusterEvalQ(cl, library(raster))
clusterEvalQ(cl, library(Rsenal))
clusterEvalQ(cl, library(randomForest))

sample.test.vector  <- c(rep(seq(from = 1000,to = 29000,by = 1000),40))

sample.runs  <-as.data.frame(do.call(rbind, parLapply(cl = cl,X = sample.test.vector, 
                                  fun = function(i){
   # select samples from the training data set, i 
   train.sample<- training[sample(nrow(training), i),]
  # run random forest on the training sample
   randfor <- randomForest(pc ~ . , data=train.sample)
   # use the model to predict onto the validation data  
   val.predicted <- stats::predict(randfor,validation)  
   #subset the "real" validation values  
   #run regression stats (Marburg Rsenel) and populate data frame   
   stats <- regressionStats(val.predicted, validation.actual, adj.rsq = TRUE, plot = FALSE)
   return(stats)})))

sample.runs$Sample.No  <- sample.test.vector
sample.runsMelt  <- melt(sample.runs,id.vars = "Sample.No")


sample.runsMelt.FORPLOT  <- rbind(
  filter(sample.runsMelt,variable == "Rsq"),
  filter(sample.runsMelt,variable == "RMSE"))


#standard error of best model for plot

sample.runsMelt.FORPLOT %>% 
  filter(Sample.No   == "29000") %>%
  group_by(variable)%>%
  summarise(mn = mean(value),stdE = std(value))%>%
  mutate( std_high = mn + std )

# Sample Size with best rfe model -----------------------------------------

best.7.rfe.mode  <- new_profile$bestVar

training.rfemod  <- PCtrain[,best.7.rfe.mode]
training.rfemod$pc  <- PCtrain[, 3]

validation.rfe  <- PCtest[,best.7.rfe.mode]
validation.actual.rfe  <- PCtest[,3]


cl <- makeCluster(mc <- getOption("cl.cores", 10))
registerDoParallel(cl, cores=12)
clusterExport(cl=cl, varlist=c("training.rfemod", "validation.rfe","validation.actual.rfe"),envir=environment())
clusterEvalQ(cl, library(raster))
clusterEvalQ(cl, library(Rsenal))
clusterEvalQ(cl, library(randomForest))

sample.test.vector  <- c(rep(seq(from = 1000,to = 29000,by = 1000),40))

sample.runs.rfemod  <-as.data.frame(do.call(rbind, parLapply(cl = cl,X = sample.test.vector, 
                                                      fun = function(i){
                                                        # select samples from the training data set, i 
                                                        train.sample<- training.rfemod[sample(nrow(training.rfemod), i),]
                                                        # run random forest on the training sample
                                                        randfor <- randomForest(pc ~ . , data=training.rfemod)
                                                        # use the model to predict onto the validation data  
                                                        val.predicted <- stats::predict(randfor,validation.rfe)  
                                                        #subset the "real" validation values  
                                                        #run regression stats (Marburg Rsenel) and populate data frame   
                                                        stats <- regressionStats(val.predicted, validation.actual.rfe, adj.rsq = TRUE, plot = FALSE)
                                                        return(stats)})))

sample.runs.rfemod$Sample.No  <- sample.test.vector



sample.runsMelt.rfemod  <- melt(sample.runs.rfemod,id.vars = "Sample.No")


sample.runsMelt.rfemod.FORPLOT  <- rbind(
  filter(sample.runsMelt.rfemod,variable == "Rsq"),
  filter(sample.runsMelt.rfemod,variable == "RMSE"))


#standard error of best model for plot

sample.runsMelt.FORPLOT %>% 
  filter(Sample.No   == "29000") %>%
  group_by(variable)%>%
  summarise(mn = mean(value),stdE = std(value))%>%
  mutate( std_high = mn + std )

# Different Model combinations --------------------------------------------

# Make new vector of cover values, makes cbind quicker later
PCtrain.cover  <- PCtrain$pc

#make a list of vectors for model variable combinations
model.combination.vecList  <- list(c(4:9),c(4:11),c(10:11),c(12:17),c(12:19),c(18:19),
                                   c(4:9,12:17), c(4:19),c(18,19,10,11),
                                  c(20,21),c(20,21,12:17),c(20,21,4:9),c(20,21,4:9,12:17),c(4:21))

model.combination.vecList  <- rep(x = model.combination.vecList,times = 20)

#test that the combinations are right 
for (i in model.combination.vecList){
    print(head(PCtrain[,i]))}


cl <- makeCluster(mc <- getOption("cl.cores", 12))
registerDoParallel(cl, cores=12)
clusterExport(cl=cl, varlist=c("PCtest", "PCtrain","validation.actual", "PCtrain.cover","model.combination.vecList"),envir=environment())
clusterEvalQ(cl, library(raster))
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
                                                        stats <- regressionStats(val.predicted, validation.actual, adj.rsq = TRUE, plot = FALSE)
                                                        return(stats)})))
stopCluster(cl)



modelNames  <- c("Wet SR","Wet SR+VI","Wet VI",
                 "Dry SR", "Dry SR+VI","Dry VI",
                 "Dry & Wet SR","Dry & Wet SR+VI","Dry & Wet VI",
                 "PALSAR","PALSAR & Dry SR","PALSAR & Wet SR","PALSAR & Wet+Dry SR","All" )

modelNames  <- rep(x = modelNames,times = 20)

model.test$Model  <- modelNames


rfe_mod_reps  <- rep(list(best.7.rfe.mode),times = 20)

cl <- makeCluster(mc <- getOption("cl.cores", 10))
registerDoParallel(cl, cores=12)
clusterExport(cl=cl, varlist=c("PCtest", "PCtrain","validation.actual", "PCtrain.cover","model.combination.vecList", "rfe_mod_reps"),envir=environment())
clusterEvalQ(cl, library(raster))
clusterEvalQ(cl, library(Rsenal))
clusterEvalQ(cl, library(randomForest))

ref.model.test  <-as.data.frame(do.call(rbind, parLapply(cl = cl,X = rfe_mod_reps, 
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
                                                       stats <- regressionStats(val.predicted, validation.actual, adj.rsq = TRUE, plot = FALSE)
                                                       return(stats)})))
stopCluster(cl)

ref.model.test$Model  <- "RFE"


model.test <- rbind(model.test,ref.model.test)


mod.mets.forlatex <- model.test %>% select(Model, Rsq, RMSE)%>% gather(Metric, Value, Rsq,RMSE) %>% group_by(Model, Metric)%>%
  summarise(mn=mean(Value),std = sd(Value))

# testing if models are significantly different  --------------------------
modelCombo.R2  <- model.test[,7:8]
modelCombo.R2.aov = aov(Rsq ~ Model, data = modelCombo.R2)
r2.hsd <- TukeyHSD(modelCombo.R2.aov)
plot(TukeyHSD(modelCombo.R2.aov))


PALSAR & Wet+Dry SR-  Dry & Wet SR   0.00104766   1.047660e-03  0.0008003056  0.0012950143     0
PALSAR & Dry SR-     Dry SR          0.00104766    1.039082e-02  0.0101434656  0.0106381744     0
Wet SR-PALSAR & Wet SR               0.01268594    -1.268594e-02 -0.0129332993 -0.0124385905     0
 

# testing residuals -------------------------------------------------------

coverValues  <- mutate(coverValues, Residual= Actual-Predicted)

# changing pixel size -----------------------------------------------------

scalevec  <- 2:10

##BatchAggregate(inRaster = Landsat_palsar,scaleVector = scalevec,NoCores = 12,outNames = "F:\\Projects\\shrub_cover_paper\\pixelSizeTests\\allData")

cl <- makeCluster(mc <- getOption("cl.cores", 12))
registerDoParallel(cl, cores=12)
clusterExport(cl=cl, varlist=c("Landsat_palsar"),envir=environment())
clusterEvalQ(cl, library(raster))

 foreach(i=2:10)%dopar%{
     filenam <- paste0("F:\\Projects\\shrub_cover_paper\\pixelSizeTests\\allData",i,".tiff")
    raster::aggregate(x = Landsat_palsar,fact = i,FUN = mean, filename=filenam ,bylayer=FALSE)}

for(i in 2:10) {
  filenam <- paste0("F:\\Projects\\shrub_cover_paper\\pixelSizeTests\\allData",i,".tiff")
  aggregate(x = Landsat_palsar,fact = i,FUN = mean, filename=filenam ,bylayer=FALSE)}

# predict on to raster brick ----------------------------------------------

all.data.stacl = Landsat_palsar

all.vars.cover  <- predict(object = Landsat_palsar,rfProfile)

dir.create(path = "coverrasters")
writeRaster(x = all.vars.cover,filename = "coverrasters//Allvars.ti")


# Meta Data  --------------------------------------------------------------

LS_8_metadata  <- read.table(file = "Landsat_metadata/LSR_LANDSAT_8_95521.txt",header = TRUE,sep = ",")
LS_7_metadata  <- read.table(file = "Landsat_metadata/LSR_LANDSAT_ETM_COMBINED_95519.txt",header = TRUE,sep = ",",fill= TRUE)
LS_5_metadata  <- read.table(file = "Landsat_metadata/LSR_LANDSAT_TM_95518.txt",header = TRUE,sep = ",",fill=TRUE)

LS_8_metadata  <- LS_8_metadata[,1:]
LS_7_metadata  <- 
LS_5_metadata  <-




meta_data_list  <- list("LS_8_metadata","LS_7_metadata","LS_5_metadata")
for i in (meta_data_list):
  
  LS7meta <- LS_7_metadata %>% select(WRS.Path, WRS.Row, Date.Acquired) %>% 
  mutate(PathRow=paste(WRS.Path, WRS.Row,sep="_"),
         month=as.numeric(paste(substr(x = Date.Acquired,start = 6,stop = 7))),
         year= as.numeric(paste(substr(x=Date.Acquired,start=1,stop=4)))) %>% 
    mutate(season = ifelse(test =  month %in% c(5:9), yes = "D" ,no =  "W"))%>%
  group_by(PathRow, year,season)%>% summarise(cnt = n())







