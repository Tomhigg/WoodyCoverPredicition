# *------------------------------------------------------------------
# | SCRIPT NAME: 1.SampleCreation.R
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
# |  1: Load, clip, and stack predictor data (Landsat, PALSAR)
# |  2: Create samples of percentage cover for corresponding Landsat pixels
# |  3: Split samples into training and testing subsets 

# *------------------------------------------------------------------
# | COMMENTS AND TODO:               
# |
# |  1:   Should copy the Vegetation index functions into zebu package as 
# |       teamlucc is not maintained so liable to decay - DONE Switched to RSto
# |  2:   Add palsar backscatter function to zebu - DONE
# |  3: 

# |*------------------------------------------------------------------
# | CONTENTS:               
# |
# |  PART 1:  Format Landsat Data
# |  PART 2:  Format PALSAR Data
# |  PART 3:  Extract Predictor Values
# |  PART 4:  Split into Training and Testing Sets
# *-----------------------------------------------------------------
# | UPDATES:               
# |
# |
# |*------------------------------------------------------------------
# | Packages Used:               
library(raster)
library(rgdal)
library(RStoolbox)
library(spatial.tools)
library(zebu)
library(fracCover)
library(doParallel)
library(caret)

# |*------------------------------------------------------------------
# | Data Used: 
setwd("D:/Projects_new/woodycovermodels/CodeData/")

#load shapefile of study area
study.area <- readOGR(dsn = "Data/shapefiles",layer = "StudyArea")

#list of landsat SR bands, recursive as there are two folders (dry and wet)
Landsat.list  <- list.files(path = "Data/Imagery/Landsat/",
                            pattern = "sr_band.*.tif$",
                            full.names = TRUE,
                            recursive = TRUE)

#list of PALSAR files
PALSAR.list  <- list.files(path = "Data/Imagery/PALSAR/",
                           pattern = "*.tif$",
                           full.names = TRUE,
                           recursive = FALSE)

#list of high-res shrub layers
shrub.mask.list  <- list.files(path = "/Training_data/NGI_aerial_imagery/shrub_masks/",
                               pattern = "*.tif$",
                               full.names = TRUE)

#Landsat layer in the SA_AEA projction, loaded here as takes forever to reproject
Landsat_SA_CRS <-"/Training_data/Landsat_SA_AEAC.tif"



# Proj4 co-ordinate string for South Africa Albers Equal Area
SA_AEAC_CRS  <- "+proj=aea +lat_1=-24 +lat_2=-33 +lat_0=0 +lon_0=24 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs "

# Format Landsat Data  --------------------------------------------
#wet season Landsat
Landsat.wet  <- stack(sapply(X = Landsat.list[grep("2008089",Landsat.list)],FUN = raster))
Landsat.wet  <- crop(x = Landsat.wet,y = study.area)
msavi.wet <- spectralIndices(img =Landsat.wet,red = 3,nir = 4,indices = "MSAVI" ,scaleFactor = 0.0001)
ndvi.wet <- spectralIndices(img =Landsat.wet,red = 3,nir = 4,indices = "NDVI" ,scaleFactor = 0.0001)

#dry season Landsat
Landsat.dry  <- stack(sapply(X = Landsat.list[grep("2008249",Landsat.list)],FUN = raster))
Landsat.dry  <- crop(x = Landsat.dry,y = study.area)
msavi.dry <- spectralIndices(img =Landsat.dry,red = 3,nir = 4,indices = "MSAVI" ,scaleFactor = 0.0001)
ndvi.dry <- spectralIndices(img =Landsat.dry,red = 3,nir = 4,indices = "NDVI" ,scaleFactor = 0.0001)


#stack all Landsat variable and rename to "Season_band/VI"
Landsat  <- stack(Landsat.wet, msavi.wet, ndvi.wet, Landsat.dry, msavi.dry,ndvi.dry)
band_names <- c("Wet_B1", "Wet_B2","Wet_B3","Wet_B4","Wet_B5","Wet_B7","Wet_MSAVI","Wet_NDVI","Dry_B1", "Dry_B2","Dry_B3","Dry_B4","Dry_B5","Dry_B7","Dry_MSAVI","Dry_NDVI")
names(Landsat) <- band_names

#remove excess items
rm(Landsat.list,Landsat.stack1,Landsat.stack2,msavi.dry,msavi.wet,ndvi.dry,ndvi.wet)

# Format PALSAR Data ------------------------------------------------------

#load raw DN layers
palsar.stack  <- stack(sapply(X = PALSAR.list,FUN = raster))
#sync to the CRS/Extent/Resolution of the Landsat layer, using NGB to avoid changing values
palsar.stack  <- spatial_sync_raster(unsynced = palsar.stack,reference = Landsat,method = "ngb")
#convert the DN's to backscatter using the folmual 10*log10((DN+0.001)^2)+-83, function in zebu package
palsar.stack  <- PalsarBackscatter(palsar.stack)
#Name layer according to polarization 
names(palsar.stack) <- c("HH", "HV")

#join stacks, remove unnessesary files
Landsat_palsar  <- stack(Landsat,palsar.stack)
rm(palsar_stack,Landsat,band_names)

# Extract Predictor Values ------------------------------------------
#from and to values for the reclassification function, class of intrest = 1
from <- c(0,1, 2, 3)
to <-   c(2,1, 2, 2)

#fracCover function to make training data by extracting percentage cover for 10000 Landsat pixels 
samples.cover <- csv_batch_parallel(no_cores = detectCores()-1,
                               list = shrub.mask.list,
                               predImage = Landsat_SA_CRS,
                               fromVals = from,toVals = to,
                               numSamps = 10000)


# make spatial point layer from the extracted samples
samples.cover.points  <- SpatialPoints(coords = samples.cover[,1:2])
#assign CRS
crs(samples.cover.points)  <- SA_AEAC_CRS
# reproject to match the Landsat/Palsar raster objects
samples.cover.points <-spTransform(x = samples.cover.points,CRSobj =crs(Landsat_palsar)
#extract values from Landsat_palsar stack and join to the percentage cover values in "samples.cover"                      
samples.cover.predictors  <-cbind(samples.cover, extract(Landsat_palsar, y = samples.cover.points)) 
          

# Split into Training and Testing Sets ------------------------------------

#split the "samples.cover.predictors" into training and testing data with equal probability distributions of cover
set.seed(3456)
trainIndex <- createDataPartition(samples.cover.predictors$pc, p = .5,
                                                        list = FALSE,
                                                        times = 1)
#split the list into seperate dataframe objects                      
PCtrain <- All.data.extracts[ trainIndex,]
PCtest  <- All.data.extracts[-trainIndex,]

