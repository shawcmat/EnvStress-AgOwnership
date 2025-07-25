library(sf)
library(tidyverse)
library(raster)
library(ggplot2)
library(ggpubr)
library(gridExtra)

setwd("<Set your working data directory here>")

# Load Initial Dataset

load("./MasterFile/MainDataV0/Land_Ownership_New_Countries.RData")

LandData <- st_as_sf(Fullv2)

#-----------------------------------------------------------------
# Part 1 - Add new DHS surveys and generate global ID
#-----------------------------------------------------------------

albania <- read_sf("./MasterFile/NewDHSSurveys/Albania17/shps/sdr_subnational_data_dhs_2017.shp")
bangladesh <- read_sf("./MasterFile/NewDHSSurveys/Bangladesh17/shps/sdr_subnational_data_dhs_2017.shp")
pakistan <- read_sf("./MasterFile/NewDHSSurveys/Pakistan17/shps/sdr_subnational_data_dhs_2017.shp")
peru <- read_sf("./MasterFile/NewDHSSurveys/Peru12/shps/sdr_subnational_data_dhs_2012.shp")
tajikistan <- read_sf("./MasterFile/NewDHSSurveys/Tajikistan17/shps/sdr_subnational_data_dhs_2017.shp")

newdata <- rbind(albania, bangladesh, pakistan, peru, tajikistan)

newLandData <- bind_rows(newdata, LandData)

names(newLandData)

newLandData$Matt_ID <- paste(newLandData$CNTRYNAMEE, newLandData$SVYYEAR, (1:nrow(newLandData)), sep="_")

LandData <- newLandData

rm(albania, bangladesh, pakistan, peru, tajikistan, newdata, newLandData, Fullv2)

save(LandData, file = "./MasterFile/MainDataV1/MainDataV1.Rdata")


#-------------------------------------------------------------------------------
# Part 2 - Add Cropland Percent measure and growing season start and end fields
#-------------------------------------------------------------------------------


Cropland <- raster("./CroplandPercent/asap_mask_crop_v03.tiff")

Cropland_ex <- raster::extract(Cropland, LandData, fun=mean, na.rm=TRUE, df=TRUE)

Cropland_ex$asap_mask_crop_v03
Cropland_ex$asap_mask_crop_v03ADJ <- .5*(Cropland_ex$asap_mask_crop_v03)

LandData$percentcrop <-Cropland_ex$asap_mask_crop_v03ADJ

PhenoStart<- raster("./Phenology/phenos1_v03.tif")
PhenoSeasonEnd<-raster("./Phenology/phenoe1_v03.tif") 

PhenoStart <- clamp(PhenoStart, upper = 108, useValues = FALSE)
PhenoSeasonEnd <- clamp(PhenoSeasonEnd, upper = 108, useValues = FALSE)

ex <- raster::extract(PhenoStart, LandData, fun=mean, na.rm=TRUE, df=TRUE)
colnames(ex)<-c("ID","phenos1_v03")

exEND <- raster::extract(PhenoSeasonEnd, LandData, fun=mean, na.rm=TRUE, df=TRUE)
colnames(exEND)<-c("ID","phenoe1_v03")

LandData$phenos1_v03<-ex$phenos1_v03
LandData$phenoe1_v03<-exEND$phenoe1_v03

LandData$CalendarDayStart<-0
LandData$CalendarDayStart <- ifelse(LandData$phenos1_v03 <= 36, LandData$phenos1_v03,LandData$CalendarDayStart)
LandData$CalendarDayStart <- ifelse(LandData$phenos1_v03 > 36 & LandData$phenos1_v03 <=72, LandData$phenos1_v03-36,LandData$CalendarDayStart)
LandData$CalendarDayStart <- ifelse(LandData$phenos1_v03 > 72, LandData$phenos1_v03-72,LandData$CalendarDayStart)

LandData$CalendarDayEND<-0
LandData$CalendarDayEND <- ifelse(LandData$phenoe1_v03 <= 36, LandData$phenoe1_v03,LandData$CalendarDayEND)
LandData$CalendarDayEND <- ifelse(LandData$phenoe1_v03 > 36 & LandData$phenoe1_v03 <=72, LandData$phenoe1_v03-36,LandData$CalendarDayEND)
LandData$CalendarDayEND <- ifelse(LandData$phenoe1_v03 > 72, LandData$phenoe1_v03-72,LandData$CalendarDayEND)

LandData$CalendarDayStart<-round((LandData$CalendarDayStart * 10), digits = 0)
LandData$CalendarDayEND<-round((LandData$CalendarDayEND* 10), digits = 0)

LandData$CalendarStartDate <- as.Date(LandData$CalendarDayStart, origin = "2015-12-31", tz = "UTC") 
LandData$CalendarEndDate <- as.Date(LandData$CalendarDayEND, origin = "2015-12-31", tz = "UTC")

LandData$SOSMonth<-strftime(LandData$CalendarStartDate, "%m")
LandData$EOSMonth<- strftime(LandData$CalendarEndDate, "%m")

LandData$GrowSeasonLength<-(LandData$phenoe1_v03 - LandData$phenos1_v03)
LandData$GrowSeasonLengthDays<-round((LandData$GrowSeasonLength * 10), digits = 0)

rm(Cropland, Cropland_ex, ex, exEND, PhenoSeasonEnd, PhenoStart)

save(LandData, file = "./MasterFile/MainDataV2/MainDataV2.Rdata" )


#-------------------------------------------------------------------------------
# Part 3 - Add SPEI12 Precipitation Measures
#-------------------------------------------------------------------------------

SPEI12_Function <- function(LandData){
  
  print("Starting SPEI12 Function")
  print("Step 1, load and clean spei12.nc file")
  
  nc_data<-nc_open("./Precipitation/spei12.nc")
  
  spei.array <- ncvar_get(nc_data, "spei")
  
  fillvalue <- ncatt_get(nc_data, "spei", "_FillValue")
  
  spei.array[spei.array == fillvalue$value] <- NA
  
  spei.array<-na.omit(spei.array)
  
  rm(fillvalue)
  
  nc_close(nc_data)
  
  rm(nc_data)
  
  
  print("Step 2, Extracting SPEI values (Long) ")
  
  print(paste0("Starting Dimensions:  ", dim(LandData)))
  
  CountryList <-unique(LandData$CNTRYNAMEE)
  
  print("List of countries:")
  
  print(CountryList)
  
  AllCountriesComplete <- data.frame(matrix(ncol = (ncol(LandData)+5))) 
  
  for(c in CountryList){
    
    workingcountry <- subset(LandData, CNTRYNAMEE == c)
    
    yearlist <- unique(workingcountry$SVYYEAR)
    
    GetSliceValues <- function(StartYear, EndYear){
      
      StartMonth <- (((StartYear-5) - 1900) * 12) #Number of months from the start of the dataset to December 16th of the first needed year t-5.
      EndMonth <- (((EndYear-1) - 1900) * 12) #Number of months from the start of the dataset to the last desired year, t-1.
      Result <- c(StartMonth)
      x <- ((EndMonth - StartMonth)/12) # Finds the number of years between first and last.
      for( i in 1:x){
        Result <- cbind(Result,((12*i) + StartMonth)) # Appends a new record to the result for each year in the dataset.
      }
      return(Result) # Results in values that represent December 16th for each year between t-5 of the first input year and t-1 of the last input year.
    }
    
    WorkingSlices <- GetSliceValues(min(yearlist), max(yearlist))
    
    slicestack <- raster::stack()
    
    num <- 1
    
    for(slice in WorkingSlices){
      
      name <- paste( "SPEI12", ((slice/12) + 1900), sep = '_')
      sliceraster <- spei.array[ , , slice]
      sliceraster <- raster(t(sliceraster), xmn = -179.75, xmx = 179.75, ymn = -89.75 , ymx = 89.75, crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
      sliceraster <- flip(sliceraster, direction = 'y')
      slicestack <- addLayer(slicestack, sliceraster)
      names(slicestack)[num] <- name
      assign(name, sliceraster)
      num <- num + 1
      
    }
    
    
    CountryAllYears <- data.frame(matrix(ncol = (ncol(LandData)+5)))
    
    for(year in yearlist){
      
      name <- paste(c, year, sep="_")
      
      print(paste("Starting", name, sep = "  "))
      
      workingyear <- subset(workingcountry, SVYYEAR == year)
      
      yearT1 <- raster::subset(slicestack, paste("SPEI12", as.character(as.numeric(year) - 1), sep = "_"))
      
      yearT2 <- raster::subset(slicestack, paste("SPEI12", as.character(as.numeric(year) - 2), sep = "_"))
      
      yearT3 <- raster::subset(slicestack, paste("SPEI12", as.character(as.numeric(year) - 3), sep = "_"))
      
      yearT4 <- raster::subset(slicestack, paste("SPEI12", as.character(as.numeric(year) - 4), sep = "_"))
      
      yearT5 <- raster::subset(slicestack, paste("SPEI12", as.character(as.numeric(year) - 5), sep = "_"))
      
      TStack <- raster::stack(yearT1, yearT2, yearT3, yearT4, yearT5)
      names(TStack) <- c("SPEI12_T1", "SPEI12_T2", "SPEI12_T3", "SPEI12_T4", "SPEI12_T5")
      
      extractoutput <- raster::extract(TStack, workingyear, fun = mean, na.rm = TRUE, df = FALSE)
      
      yearoutput <- cbind(workingyear, extractoutput)
      
      yearoutput <- data.frame(yearoutput)
      
      names(CountryAllYears) <- names(yearoutput)
      
      #print("Number of Columns Post extract")
      #print(ncol(yearoutput))
      #print(names(yearoutput))
      #print("Number of columns in empty data frame")
      #print(ncol(CountryAllYears))
      # print(names(CountryAllYears))
      
      CountryAllYears <- rbind(CountryAllYears, yearoutput)
      
      print(paste(name, "Complete", sep = " "))
      
      
    }
    
    names(AllCountriesComplete) <- names(CountryAllYears)
    AllCountriesComplete <- rbind(AllCountriesComplete, CountryAllYears)
    
    print(paste0("Updated Dimensions:  "))
    print(dim(AllCountriesComplete))
    
  }
  
  
  print("all countries complete")
  
  AllCountriesComplete <- drop_na(AllCountriesComplete, Matt_ID)
  
  AllCountriesComplete <- st_sf(AllCountriesComplete)
  
  print("Updated Field Names:")
  
  print(names(AllCountriesComplete))
  
  print("Final Dimensions:  ")
  print(dim(AllCountriesComplete))
  
  return(AllCountriesComplete)
  
}

LandData <- SPEI12_Function(LandData)

save(LandData, file = "./MasterFile/MainDataV3/MainDataV3.Rdata" )

#-------------------------------------------------------------------------------------------
# Part 4 - Add SPEI03 Growing Season Precipitation Measures
#--------------------------------------------------------------------------------------------

SPEI03_Function <- function(LandData){
  
  LandData$SOSMonth <- as.numeric(LandData$SOSMonth)
  LandData$EOSMonth <- as.numeric(LandData$EOSMonth)
  
  print("Starting SPEI03 Function")
  print("Step 1, load and clean spei03.nc file")

  nc_data<-nc_open("./Precipitation/spei03_v2.nc")
  
  spei.array <- ncvar_get(nc_data, "spei")
  
  fillvalue <- ncatt_get(nc_data, "spei", "_FillValue")
  
  spei.array[spei.array == fillvalue$value] <- NA
  
  spei.array<-na.omit(spei.array)
  
  rm(fillvalue)
  
  nc_close(nc_data)
  
  rm(nc_data)
  
  print("Step 2, Extracting SPEI values (Long) ")
  print("Starting Dimensions:  ")
  print(dim(LandData))
  
  countrylist <- unique(LandData$CNTRYNAMEE)
  
  AllCountriesComplete <- data.frame(matrix(ncol = (ncol(LandData) + 5)))
  
  for(cntry in countrylist){
    
    print(paste0("Starting  ", cntry, sep="  "))
    
    workingcountry <- subset(LandData, CNTRYNAMEE == cntry)
    
    yearlist <- unique(workingcountry$SVYYEAR)
    
    CountryAllYears <- data.frame(matrix(ncol = (ncol(LandData)+5)))
    
    for(year in yearlist){
      
      workingyear <- subset(workingcountry, SVYYEAR == year)
      
      reqyears <- c(year-1, year-2, year-3, year-4, year-5, year-6)
      
      GetSliceValues <- function(InputYears){
        
        Jan1 <- (((InputYears[1]) - 1901) * 12) + 1 #Number of months from the start of the dataset to December 16th of the first needed year t-5.
        Dec1 <- (((InputYears[1]) - 1900) * 12) #Number of months from the start of the dataset to December 16of the last desired year, t-1.
        Result1 <- seq.int(Jan1, Dec1, 1)
        
        Jan2 <- (((InputYears[2]) - 1901) * 12) + 1 #Number of months from the start of the dataset to December 16th of the first needed year t-5.
        Dec2 <- (((InputYears[2]) - 1900) * 12) #Number of months from the start of the dataset to December 16of the last desired year, t-1.
        Result2 <- seq.int(Jan2, Dec2, 1)
        
        Jan3 <- (((InputYears[3]) - 1901) * 12) + 1 #Number of months from the start of the dataset to December 16th of the first needed year t-5.
        Dec3 <- (((InputYears[3]) - 1900) * 12) #Number of months from the start of the dataset to December 16of the last desired year, t-1.
        Result3 <- seq.int(Jan3, Dec3, 1)
        
        Jan4 <- (((InputYears[4]) - 1901) * 12) + 1 #Number of months from the start of the dataset to December 16th of the first needed year t-5.
        Dec4 <- (((InputYears[4]) - 1900) * 12) #Number of months from the start of the dataset to December 16of the last desired year, t-1.
        Result4 <- seq.int(Jan4, Dec4, 1)
        
        Jan5 <- (((InputYears[5]) - 1901) * 12) + 1 #Number of months from the start of the dataset to December 16th of the first needed year t-5.
        Dec5 <- (((InputYears[5]) - 1900) * 12) #Number of months from the start of the dataset to December 16of the last desired year, t-1.
        Result5 <- seq.int(Jan5, Dec5, 1)
        
        Jan6 <- (((InputYears[6]) - 1901) * 12) + 1 #Number of months from the start of the dataset to December 16th of the first needed year t-5.
        Dec6 <- (((InputYears[6]) - 1900) * 12) #Number of months from the start of the dataset to December 16of the last desired year, t-1.
        Result6 <- seq.int(Jan6, Dec6, 1)
        
        Result <- data.frame(Result1, Result2, Result3, Result4, Result5, Result6)
        
        return(Result) # Results in values that represent December 16th for each year between t-6 of the first input year and t-1 of the last input year.
      }
      
      SliceTable <- GetSliceValues(reqyears)
      
      names(SliceTable) <- c(paste('Year', year-1, sep = '_'), 
                             paste('Year', year-2, sep = '_'),
                             paste('Year', year-3, sep = '_'),
                             paste('Year', year-4, sep = '_'),
                             paste('Year', year-5, sep = '_'),
                             paste('Year', year-6, sep = '_'))
      
      slicestack1 <- raster::stack()
      slicestack2 <- raster::stack()
      slicestack3 <- raster::stack()
      slicestack4 <- raster::stack()
      slicestack5 <- raster::stack()
      slicestack6 <- raster::stack()
      
      num <- 1
      
      for( col in colnames(SliceTable)){
        
        for(slice in SliceTable[[col]]){
          
          sliceraster <- spei.array[ , , slice]
          sliceraster <- raster(t(sliceraster), xmn = -179.75, xmx = 179.75, ymn = -89.75 , ymx = 89.75, crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
          sliceraster <- flip(sliceraster, direction = 'y')
          if(num == 1){
            slicestack1 <- addLayer(slicestack1, sliceraster)
          } else if(num == 2){
            slicestack2 <- addLayer(slicestack2, sliceraster)
          } else if(num == 3){
            slicestack3 <- addLayer(slicestack3, sliceraster)
          } else if(num == 4){
            slicestack4 <- addLayer(slicestack4, sliceraster)
          } else if(num == 5){
            slicestack5 <- addLayer(slicestack5, sliceraster)
          } else if(num == 6){
            slicestack6 <- addLayer(slicestack6, sliceraster)
          }
          
        }
        
        num <- num + 1
        
      }
      
      names(slicestack1) <- c("Month_1", "Month_2", "Month_3", "Month_4", "Month_5", "Month_6",
                              "Month_7","Month_8","Month_9","Month_10", "Month_11", "Month_12")
      names(slicestack2) <- c("Month_1", "Month_2", "Month_3", "Month_4", "Month_5", "Month_6",
                              "Month_7","Month_8","Month_9","Month_10", "Month_11", "Month_12")
      names(slicestack3) <- c("Month_1", "Month_2", "Month_3", "Month_4", "Month_5", "Month_6",
                              "Month_7","Month_8","Month_9","Month_10", "Month_11", "Month_12")
      names(slicestack4) <- c("Month_1", "Month_2", "Month_3", "Month_4", "Month_5", "Month_6",
                              "Month_7","Month_8","Month_9","Month_10", "Month_11", "Month_12")
      names(slicestack5) <- c("Month_1", "Month_2", "Month_3", "Month_4", "Month_5", "Month_6",
                              "Month_7","Month_8","Month_9","Month_10", "Month_11", "Month_12")
      names(slicestack6) <- c("Month_1", "Month_2", "Month_3", "Month_4", "Month_5", "Month_6",
                              "Month_7","Month_8","Month_9","Month_10", "Month_11", "Month_12")
      
      IDList <- unique(workingyear$Matt_ID)
      
      YearAllIDs <- data.frame(matrix(ncol = (ncol(LandData)+5)))
      
      for(ID in IDList){
        
        workingID <- subset(workingyear, Matt_ID == ID)
        
        SOS <- workingID$SOSMonth
        EOS <- workingID$EOSMonth
        
        if(SOS < EOS){
          
          seasonmonths <- seq.int(SOS,EOS,1)
          
          ID_T1 <- raster::calc(raster::subset(slicestack1, seasonmonths), fun = mean)
          
          ID_T2 <- raster::calc(raster::subset(slicestack2, seasonmonths), fun = mean)
          
          ID_T3 <- raster::calc(raster::subset(slicestack3, seasonmonths), fun = mean)
          
          ID_T4 <- raster::calc(raster::subset(slicestack4, seasonmonths), fun = mean)
          
          ID_T5 <- raster::calc(raster::subset(slicestack5, seasonmonths), fun = mean)
          
          
          IDStack <- raster::stack(ID_T1, ID_T2, ID_T3, ID_T4, ID_T5)
          names(IDStack) <- c("SPEI03_T1", "SPEI03_T2", "SPEI03_T3", "SPEI03_T4", "SPEI03_T5")
          
          extractoutput <- raster::extract(IDStack, workingID, fun = mean, na.rm = TRUE, df = FALSE)
          
          IDoutput <- cbind(workingID, extractoutput)
          IDoutput <- data.frame(IDoutput)
          
          names(YearAllIDs) <- names(IDoutput)
          
          YearAllIDs <- rbind(YearAllIDs, IDoutput)
          
          
          
        } else if(EOS < SOS){
          
          startseasonmonths <- seq.int(SOS, 12)
          endseasonmonths <- seq.int(1, EOS)
          
          ID_T1 <- raster::calc(raster::stack(raster::subset(slicestack2, startseasonmonths), raster::subset(slicestack1, endseasonmonths)), fun = mean)
          
          ID_T2 <- raster::calc(raster::stack(raster::subset(slicestack3, startseasonmonths), raster::subset(slicestack2, endseasonmonths)), fun = mean)
          
          ID_T3 <- raster::calc(raster::stack(raster::subset(slicestack4, startseasonmonths), raster::subset(slicestack3, endseasonmonths)), fun = mean)
          
          ID_T4 <- raster::calc(raster::stack(raster::subset(slicestack5, startseasonmonths), raster::subset(slicestack4, endseasonmonths)), fun = mean)
          
          ID_T5 <- raster::calc(raster::stack(raster::subset(slicestack6, startseasonmonths), raster::subset(slicestack5, endseasonmonths)), fun = mean)
          
          IDStack <- raster::stack(ID_T1, ID_T2, ID_T3, ID_T4, ID_T5)
          names(IDStack) <- c("SPEI03_T1", "SPEI03_T2", "SPEI03_T3", "SPEI03_T4", "SPEI03_T5")
          
          extractoutput <- raster::extract(IDStack, workingID, fun = mean, na.rm = TRUE, df = FALSE)
          
          IDoutput <- cbind(workingID, extractoutput)
          IDoutput <- data.frame(IDoutput)
          
          names(YearAllIDs) <- names(IDoutput)
          
          YearAllIDs <- rbind(YearAllIDs, IDoutput)
          
        }
        
        #Remove raster::calc function from this if you want the individual month rasters.
        
        
      } #End ID Level
      
      names(CountryAllYears) <- names(YearAllIDs)
      CountryAllYears <- rbind(CountryAllYears, YearAllIDs)
      
    } #End Year Level
    
    names(AllCountriesComplete) <- names(CountryAllYears)
    AllCountriesComplete <- rbind(AllCountriesComplete, CountryAllYears)
    
    print(paste(cntry, "complete", sep = "  "))
    
    print("Dimensions Update:")
    print(dim(AllCountriesComplete))
    
  } # End Country Level
  
  print("all countries complete")
  
  AllCountriesComplete <- drop_na(AllCountriesComplete, Matt_ID)
  
  AllCountriesComplete <- st_sf(AllCountriesComplete)
  
  print(names(AllCountriesComplete))
  
  print(dim(AllCountriesComplete))
  
  return(AllCountriesComplete)
  
} 

LandData <- SPEI03_Function(LandData)

save(LandData, file = "./MasterFile/MainDataV4/MainDataV4.Rdata" )

#---------------------------------------------------------------------------------------------
# Part 5 - Operationalize SPEI measures, adding threshold counts, mean, and change over time.
#---------------------------------------------------------------------------------------------

LandDataNew <- data.frame(LandData)

LandDataNew$SPEI12_Minus2 <-  0
LandDataNew$SPEI12_Minus1 <-  0
LandDataNew$SPEI12_Minus05 <- 0

LandDataNew$SPEI03_Minus2 <- 0
LandDataNew$SPEI03_Minus1 <- 0
LandDataNew$SPEI03_Minus05 <- 0

LandDataNew$SPEI12_Mean <- 0
LandDataNew$SPEI03_Mean <- 0

LandDataNew$LandOwnChange <- NA


for(row in 1:nrow(LandDataNew)){
  
  
  spei12_vector <- c(LandDataNew[row, "SPEI12_T1"], LandDataNew[row, "SPEI12_T2"], 
                     LandDataNew[row, "SPEI12_T3"], LandDataNew[row, "SPEI12_T4"], 
                     LandDataNew[row, "SPEI12_T5"])
  
  spei03_vector <- c(LandDataNew[row, "SPEI03_T1"], LandDataNew[row, "SPEI03_T2"], 
                     LandDataNew[row, "SPEI03_T3"], LandDataNew[row, "SPEI03_T4"], 
                     LandDataNew[row, "SPEI03_T5"])
  
  print(spei12_vector)
  
  #Add Count variables at various thresholds.
  
  LandDataNew[row, "SPEI12_Minus2"] <- length(which(spei12_vector <= -2))
  print(LandDataNew[row, "SPEI12_Minus2"])
  
  LandDataNew[row, "SPEI12_Minus1"] <- length(which(spei12_vector <= -1))
  print(LandDataNew[row, "SPEI12_Minus1"])
  
  LandDataNew[row, "SPEI12_Minus05"] <- length(which(spei12_vector <= -.5))
  print(LandDataNew[row, "SPEI12_Minus05"])
  
  LandDataNew[row, "SPEI03_Minus2"] <- length(which(spei03_vector <= -2))
  print(LandDataNew[row, "SPEI03_Minus2"])
  
  LandDataNew[row, "SPEI03_Minus1"] <- length(which(spei03_vector <= -1))
  print(LandDataNew[row, "SPEI03_Minus1"])
  
  LandDataNew[row, "SPEI03_Minus05"] <- length(which(spei03_vector <= -.5))
  print(LandDataNew[row, "SPEI03_Minus05"])
  
  
  #Add Mean SPEI value variables
  
  LandDataNew[row, "SPEI12_Mean"] <- mean(spei12_vector)
  LandDataNew[row, "SPEI03_Mean"] <- mean(spei03_vector)
  
  
}


#  Add change in land ownership Variable to units with multiple years of data. Calculated as difference between
#  survey year and previously available survey year. If first available year or only one year is available,
#  assigned a value of NA.

countrylist <- unique(LandDataNew$CNTRYNAMEE)

AllUnitsComplete <- data.frame(matrix(ncol = (ncol(LandDataNew))))

for(cntry in countrylist){
  
  workingcountry <- subset(LandDataNew, CNTRYNAMEE == cntry)
  
  unitlist <- unique(workingcountry$DHSREGEN)
  
  for(unt in unitlist){
    
    workingunit <- subset(workingcountry, DHSREGEN == unt)
    
    yearlist <- workingunit$SVYYEAR
    
    ownershiplist <- workingunit$HCAGONHLND
    
    reftable <- data.frame(yearlist, ownershiplist)
    
    reftable <- arrange(reftable, yearlist)
    
    if(length(yearlist) > 1){
      
      print(paste0(paste0("Multiple Years Available in ", cntry, sep = "_"), unt))
      
      for(row in 1:nrow(workingunit)){
        
        rowyear <- workingunit[row, "SVYYEAR"]
        
        rowownership <- workingunit[row, "HCAGONHLND"]
        
        if(rowyear != min(yearlist)){
          
          for(refrow in 1:nrow(reftable)){
            
            if(rowyear == reftable[refrow,"yearlist"]){
              
              workingunit[row, "LandOwnChange"] <- reftable[refrow, "ownershiplist"] - reftable[ refrow-1, "ownershiplist"]
              
            }
            
          }
          
        }
        
      }
      
    }
    
    names(AllUnitsComplete) <- names(workingunit)
    
    AllUnitsComplete <- rbind(AllUnitsComplete, workingunit)
    
    print(dim(AllUnitsComplete))
    
  }
  
}

AllUnitsComplete <- drop_na(AllUnitsComplete, Matt_ID)

AllUnitsComplete <- st_sf(AllUnitsComplete)

print(names(AllUnitsComplete))

print(dim(AllUnitsComplete))

LandDataNew <- AllUnitsComplete




LandData <- LandDataNew

rm(AllUnitsComplete, LandDataNew, reftable, workingcountry, workingunit, cntry, country, countrylist,
   ownershiplist, refrow, row, rowownership, rowyear, spei03_vector, spei12_vector, unit, unitlist, unt, yearlist)

save(LandData, file = "./MasterFile/MainDataV5/MainDataV5.Rdata")


#-----------------------------------------------------------------------------------
# Part 6 - Drop unwanted fields to create final dataset for use in modeling script.
#-----------------------------------------------------------------------------------

names(LandData)

fields2keep <- c("Matt_ID", "SVYYEAR", "CNTRYNAMEE", "DHSREGEN","HCAGONHLND",
                 "HCAGONHANM", "percentcrop", "phenos1_v03",
                 "phenoe1_v03", "CalendarDayStart", "CalendarDayEND",
                 "CalendarStartDate", "CalendarEndDate", "SOSMonth",
                 "EOSMonth", "GrowSeasonLength", "GrowSeasonLengthDays",
                 "SPEI12_T1","SPEI12_T2", "SPEI12_T3", "SPEI12_T4", "SPEI12_T5",
                 "SPEI03_T1", "SPEI03_T2", "SPEI03_T3", "SPEI03_T4", "SPEI03_T5",
                 "SPEI12_Minus2", "SPEI12_Minus1", "SPEI12_Minus05", "SPEI03_Minus2",
                 "SPEI03_Minus1", "SPEI03_Minus05", "SPEI12_Mean", "SPEI03_Mean", 
                 "LandOwnChange", "geometry")

LandData_new <- LandData %>% dplyr::select(all_of(fields2keep))

names(LandData_new)

LandData <- LandData_new

save(LandData, file = "./MasterFile/MainDataV6/MainDataV6.Rdata")


