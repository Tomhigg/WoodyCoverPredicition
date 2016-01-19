# *------------------------------------------------------------------
# | SCRIPT NAME: 6.LandsatMetadata.R
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
# *------------------------------------------------------------------
# | COMMENTS AND TODO:               
# |
# |  1:  
# |  2: 
# |  3: 

# |*------------------------------------------------------------------
# | CONTENTS:               
# |
# |  PART 1:  
# |  PART 2: 
# |  PART 3: 
# *-----------------------------------------------------------------
# | UPDATES:               
# |
# |
# |*------------------------------------------------------------------
# | Packages Used:               

library(dplyr)
library(tidyr)
# |
# |*------------------------------------------------------------------
# | Data Used:               

LS_8_metadata  <- read.table(file = "LandsatMetadata/L_8_meta.csv",header = TRUE,sep = ",")
LS_7_metadata  <- read.table(file = "LandsatMetadata/L_7_meta.csv",header = TRUE,sep = ",",fill= TRUE)
LS_5_metadata  <- read.table(file = "LandsatMetadata/L_5_meta.csv",header = TRUE,sep = ",",fill=TRUE)


LS_7_metadata <- LS_7_metadata[1:23400,]

  
  meta_data_list  <- list("LS_8_metadata","LS_7_metadata","LS_5_metadata")
for i in (meta_data_list):
  
LS.7.counts   <- LS_7_metadata %>% select(WRS.Path, WRS.Row, Date.Acquired) %>% 
                              mutate(PathRow=paste(WRS.Path, WRS.Row,sep="_"),
                                     month=as.numeric(paste(substr(x = Date.Acquired,start = 6,stop = 7))),
                                     year= as.numeric(paste(substr(x=Date.Acquired,start=1,stop=4)))) %>% 
                              mutate(season = ifelse(test =  month %in% c(5:9), yes = "D" ,no =  "W"))%>%
                              group_by(PathRow, year,season) %>% summarise(count = n())%>%
                              mutate(YearSeason=paste(year,season,sep = "."))

LS.8.counts   <- LS_8_metadata %>% select(WRS.Path, WRS.Row, Date.Acquired) %>% 
  mutate(PathRow=paste(WRS.Path, WRS.Row,sep="_"),
         month=as.numeric(paste(substr(x = Date.Acquired,start = 6,stop = 7))),
         year= as.numeric(paste(substr(x=Date.Acquired,start=1,stop=4)))) %>% 
  mutate(season = ifelse(test =  month %in% c(5:9), yes = "D" ,no =  "W"))%>%
  group_by(PathRow, year,season) %>% summarise(count = n())%>%
  mutate(YearSeason=paste(year,season,sep = "."))

LS.8.counts   <- LS_8_metadata %>% select(WRS.Path, WRS.Row, Date.Acquired) %>% 
  mutate(PathRow=paste(WRS.Path, WRS.Row,sep="_"),
         month=as.numeric(paste(substr(x = Date.Acquired,start = 6,stop = 7))),
         year= as.numeric(paste(substr(x=Date.Acquired,start=1,stop=4)))) %>% 
  mutate(season = ifelse(test =  month %in% c(5:9), yes = "D" ,no =  "W"))%>%
  group_by(PathRow, year,season) %>% summarise(count = n())%>%
  mutate(YearSeason=paste(year,season,sep = "."))











