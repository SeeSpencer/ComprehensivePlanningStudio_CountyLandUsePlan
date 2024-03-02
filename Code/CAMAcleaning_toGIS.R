#####
#for planning studio

###################
# READING IN THE DATA and INSTALLING PACKAGES
###################

LandCard<-read.table('InsertFilePath',sep='|', header=T)
unique(LandCard$LandType)
library(dplyr)
library(ggplot2)
library(dplyr)


View(LandCard)
install.packages("sf")
library(sf)


##########################
# CLEANING DATA FOR JOIN TO SPATIAL LAYER
##########################


LandCardClean <- LandCard %>% 
  mutate(PropertyNumber= gsub("-", "", PropertyNumber),)
# Replacing the connotation between groupings in parcel ID to standard letter
# Instead of "PO-123-123-1234" it will be replaced with "PO1231231234"
# This is required because the format of these ids for shape file are in this
# format.


write.csv(LandCardClean, "InsertFilePath", row.names=FALSE)
# saving the new file to test join in GIS
# TOO BIG OF FILE TO CLASSIFY

parcelLayers <- read_sf("InsertFilePath")
# Reading in the shapefile to merge together in R

View(LandCardClean)
View(parcelLayers)


#######################
# DATA EXPLORATION
#######################

unique(parcelLayers$mlocStrS_1)
unique(LandCardClean$LandUsageType)
# random check variables

colnames(LandCardClean)
head(LandCardClean)
# checking for the names and content of variables

colnames(parcelLayers)
unique(parcelLayers$TaxpCity)
unique(LandCardClean$AgUse)

# returns "yes" or "no" (Y or N) for current ag use

unique(LandCardClean$SoilType)

# literal soil classification data

unique(LandCardClean)
head(parcelLayers)

unique(LandCardClean$LandType)
unique(LandCardClean$land_type)

# These are duplicated variable conditions that both return:

#   "C"  "R"  "W"  "A5" "A8" "A0" "AH" "A3" "L"  "A9" "A7" "L1" "A2" "A1" "P"  
#   "H"  "G1" "S1" "G2" "S3" "L2" "S2" "A4" "G3" "O"  "S4" "G4" "AD" ""  

# Further analysis can be done on any differences otherwise treat as synonymous



unique(LandCardClean$LandUsageType)
#   Land usage type variable returns:

#   "C" "R" "W" ""  "L" "P" "H" "O"

#   This is the overly aggregated labels seen in data provided to us


######################
# MERGING THE DATA
#####################


mergedParcels <-left_join(parcelLayers, LandCardClean, by=c('SMDA_NUM'='PropertyNumber'))
st_crs(mergedParcels)
# Left join of data to one another based on parcel ID and checking it still
# remains spatial (ohio south plane Coordinate Reference System)

#st_write(mergedParcels, "/Users/coyoteobjective/Desktop/crp fall studio/darkecountyinfoJOIN.shp")
# Saving the data to use in gis DONT see below




#################
# The Further Analysis
#################



unique(LandCardClean$LandUsageType)
unique(LandCardClean$land_type)
unique(LandCardClean$LandType)

checkland_type<-LandCardClean %>% group_by(land_type) %>% count
View(checkland_type)

checkLandType<-LandCardClean %>% group_by(LandType) %>% count
View(checkLandType)

# They are synonymous

unique(mergedParcels$mClassific)

####
# https://codes.ohio.gov/ohio-administrative-code/rule-5703-25-10
###

unique(parcelLayers$mClassific)
 checkmerged<- subset(mergedParcels, select = c(mClassific, LandType))
View(checkmerged)

unique(LandCard$UseCode)


#######
# Locating matching code to provided outdated data set to appropriate updated source
######

DwellingCard<-read.table('InsertFilePathCama',sep='|', header=T)

# future research in rebuilding my own national structures inventory dataset
# for hazus (or whatever replaces it)

unique(DwellingCard$UseCode)
# 101 to 685

unique(parcelLayers$mClassific)
# 0 to 840

range(DwellingCard$UseCode)
range(as.numeric(DwellingCard$UseCode))
# mode(DwellingCard$UseCode) <- numeric !!!error!!!
# most likely NA values

range(DwellingCard$UseCode,na.rm=TRUE)
range(parcelLayers$mClassific, na.rm=TRUE)

# missing in DwellingCard

# 100	Agricultural vacant land

# 690	Graveyards, monuments, and cemeteries
# 700	Community urban redevelopment corporation tax abatements (R.C. 1728.10)
# 710	Community reinvestment area tax abatements
# 720	Municipal improvement tax abatements (R.C. 5709.41)
# 730	Municipal urban redevelopment tax abatements (R.C. 725.02)
# 740	Other tax abatements (R.C. 165.01 and 303.52)
# 800	Agricultural land and improvements owned by a public utility other than a railroad
# 810	Mineral land and improvements owned by a public utility other than a railroad
# 820	Industrial land and improvements owned by a public utility other than a railroad
# 830	Commercial land and improvements (including all residential property) owned by a public utility other than a railroad
# 840	Railroad real property used in operations
# 850	Railroad real property not used in operations
# 860	Railroad personal property used in operations
# 870	Railroad personal property not used in operations
# 880	Public Utility personal property other than rail-roads


#I think I found it

ValueHistoryCard<-read.table('InsertFilePathtoCAMA',sep='|', header=T)

ValueHistoryCard$mclassificationid
range(ValueHistoryCard$mclassificationid)
# 100 to 881 is reported range, Ohio range is 100 to 880 of hard categories but notes public utility
# can go up to 899




############
#
###########
View(ValueHistoryCard)

ValueHistoryClean <- ValueHistoryCard %>% 
  mutate(PropertyNumber= gsub("-", "", mpropertynumber),)
# Replacing the connotation between groupings in parcel ID to standard letter
# Instead of "PO-123-123-1234" it will be replaced with "PO1231231234"
# This is required because the format of these ids for shape file are in this
# format.

UpdatedCurrentUse <-left_join(parcelLayers, ValueHistoryClean, by=c('SMDA_NUM'='PropertyNumber'))
st_crs(mergedParcels)
# Left join of data to one another based on parcel ID and checking it still
# remains spatial (ohio south plane Coordinate Reference System)

View(UpdatedCurrentUse)

library(mapview)
# mapview(UpdatedCurrentUse) #takes way too long and crashes half load :(

# nope barplot(UpdatedCurrentUse$mclassificationid)
head(UpdatedCurrentUse)
colnames(UpdatedCurrentUse)

UpdatedLandUse<- subset(UpdatedCurrentUse, select = c(OBJECTID,
                                                      mclassificationid,
                                                      ExemptLand,
                                                      AbateLand,
                                                      HomesteadLand,
                                                      CAUV,
                                                      CensusCode,
                                                      Neighborho,
                                                      mstateScho,
                                                      geometry,
                                                      DeedNumber,
                                                      OwnName,
                                                      NumberOfPr,
                                                      TaxpCity
                                                      ))
head(UpdatedLandUse)
range(UpdatedLandUse$ExemptLand, na.rm = TRUE)
# 0 to 3224010
range(UpdatedLandUse$mclassificationid, na.rm = TRUE)
# 100 to 850
range(UpdatedLandUse$AbateLand, na.rm = TRUE)
# 0 to 25200
unique(UpdatedLandUse$TaxpCity)

# interesting enough my home town shows up here, which means this is where the owner
# resides with out of county owners shown (I am assuming Boston is the East coast one)

st_write(UpdatedLandUse, "InsertFilePath")
# Saving the data to use in gis
