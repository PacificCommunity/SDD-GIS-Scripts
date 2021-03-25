###AUTOMATIC GPS AND EA CODES CLEANING - STAT DATA COLLECTION IN SURVEY SOLUTIONS####
###EXAMPLE - VANUATU 2020 POPULATION AND HOUSING CENSUS###
##Luis de la Rua - SPC - SDD - March 2021

library(maptools)
library(raster)
library(sf)
library(sp)
library(spData)
library(rgeos)
library(ggplot2)
library(dplyr)
library(rgdal)
library(leaflet)
library(magrittr)
library(writexl)

#set work directory
setwd("path")

#Load EA framework
EA <- st_read("path")
#tidy up the layer we only need the EA code. We keep admin level codes and names and centroid coordinates of each of the EAs
EA<- EA[,c('eaid','acid','acname','pname','x_cent','y_cent')]
names(EA)[names(EA) == 'eaid'] <- "ea_code_framework"

#load buffer EA limits
buffer <- st_read("path")
buffer$buffer <- 1 # we need this to identify points in buffer later
buffer <- buffer[,'buffer'] #tidy up

#set CRS we are working from now using EA's crs
WGS84CRS <- crs(EA)

#load HH locations from SS in csv format from census dataset processed with do file

HHloc_csv <- read.csv("path")

#rename hh coordinates
names(HHloc_csv)[names(HHloc_csv) == "g4_dwelling_gps__Latitude" ] <- "y"
names(HHloc_csv)[names(HHloc_csv) == "g4_dwelling_gps__Longitude"] <- "x"
colnames(HHloc_csv)

#csv table to sf object
HHloc_sf <- st_as_sf(HHloc_csv, coords = c("x","y"), crs = WGS84CRS, remove=FALSE) 
str(HHloc_sf)
crs(HHloc_sf)

#Spatial join HHloc - EA to get EA code geographically
HHloc_sjoin <- st_join(HHloc_sf,EA,join=st_intersects)

#replace null values on ea_code_framework field by 0
HHloc_sjoin$ea_code_framework[is.na(HHloc_sjoin$ea_code_framework)]<- 0
#HHloc_sjoin$EA[is.na(HHloc_sjoin$EA)]<- 0 #Not necessary already cleaned in STATA do file

#st_write (HHloc_sjoin, "path",delete_layer = T)


##CLEANNING ERRORS##

#1-Clean EA codes

#CREATE FIELD "ea_corrected" we use ea code from interview as input and we modify according to the errors found
HHloc_sjoin$ea_corrected <- HHloc_sjoin$ea_number
#order EA codes together in the table

HHloc_sjoin <- (relocate(HHloc_sjoin,"ea_code_framework", .after = "ea_number"))
HHloc_sjoin <- (relocate(HHloc_sjoin,"ea_corrected", .after = "ea_code_framework"))



##Spatial join with buufer layer to CLASSIFY POINTS BY BUFFER IN/OUT FIELD IN = 1, OUT = 0
HHloc_sjoin <- st_join(HHloc_sjoin,buffer,join=st_intersects)
HHloc_sjoin$buffer[is.na(HHloc_sjoin$buffer)]<- 0
#order columns
HHloc_sjoin <- (relocate(HHloc_sjoin,"buffer", .after = "ea_corrected"))

#CASE WHERE INTERVIEW_EAcode != FWORK_EAcode AND FWORK_EAcode!=0 (point within FWORK) AND POINT OUT OF BUFFER
HHloc_sjoin$ea_corrected <- ifelse((HHloc_sjoin$ea_number != HHloc_sjoin$ea_code_framework) & (HHloc_sjoin$ea_code_framework != 0) & (HHloc_sjoin$buffer == 0),
                                   HHloc_sjoin$ea_code_framework,#for this case GPS coord aligned with the correction as we are geting ea code from FWork
                                   HHloc_sjoin$ea_corrected)

#INTERVIEWS WITH NO EA CODE -> ASSIGN EA CODE FROM EA FWORK
#CASE WHEN INTERVIEW EACODE = 0 AND WE ASSIGN CODE FROM FWORK
HHloc_sjoin$ea_corrected<-ifelse(HHloc_sjoin$ea_number== 0 ,HHloc_sjoin$ea_code_framework ,HHloc_sjoin$ea_corrected)

#2-Clean GPS null coordinates

#CASE WHERE NO GPS POINT RECORDED BUT EA CODE ENTERED IN INTERVIEW -> ASSIGN POLYGON CENTROID (FORCED WITHIN) COORDINATES
#CREATE FIELDS X Y CORRECTED, TAKING BY DEFAULT XY COORDINATES FROM TABLET GPS (x,y)

#bring centroid coordinates for interviews that did not intersect with framework using table join (not spatial) with EA layer
HHloc_sjoin <- merge.data.frame(HHloc_sjoin,EA,by.x="ea_number",by.y="ea_code_framework",all.x=TRUE)
#get centroid coordinates in rows where spatjoin didnt work
HHloc_sjoin$x_cent.x <- ifelse((is.na(HHloc_sjoin$x_cent.x ) & (HHloc_sjoin$ea_code_framework == 0)),HHloc_sjoin$x_cent.y,HHloc_sjoin$x_cent.x)
HHloc_sjoin$y_cent.x <- ifelse((is.na(HHloc_sjoin$y_cent.x ) & (HHloc_sjoin$ea_code_framework == 0)),HHloc_sjoin$y_cent.y,HHloc_sjoin$y_cent.x)

#order columns
HHloc_sjoin <- (relocate(HHloc_sjoin,"ea_number",.after="interview__key"))
#rename fields
names(HHloc_sjoin)[names(HHloc_sjoin) == "x_cent.x"] <- "x_cent"
names(HHloc_sjoin)[names(HHloc_sjoin) == "y_cent.x"] <- "y_cent"

#tidy up unncecesary fields 
HHloc_sjoin<- HHloc_sjoin[, -c(21:23)] 
HHloc_sjoin<- HHloc_sjoin[, -c(23:29)] 
#create corrected coordinates field and implement the cleaning conditions for interviews with no coordinates or out of the framework.
HHloc_sjoin$xcor <- ifelse((HHloc_sjoin$x==0 | HHloc_sjoin$ea_code_framework==0),HHloc_sjoin$x_cent,HHloc_sjoin$x)
HHloc_sjoin$ycor <- ifelse((HHloc_sjoin$y==0 | HHloc_sjoin$ea_code_framework==0),HHloc_sjoin$y_cent,HHloc_sjoin$y)

#order columns
HHloc_sjoin <- (relocate(HHloc_sjoin,"xcor",.after="x"))
HHloc_sjoin <- (relocate(HHloc_sjoin,"ycor",.after="x"))

#track points that have been relocated to centroid
HHloc_sjoin$centroid <- ifelse((HHloc_sjoin$x==0 | HHloc_sjoin$ea_code_framework==0),HHloc_sjoin$centroid<-1,HHloc_sjoin$centroid<-0)

#POINTS TO CORRECT MANUALLY...

#lets target them
HHloc_sjoin$manual <- 0
HHloc_sjoin <- (relocate(HHloc_sjoin,"manual",.after="xcor"))

#Points with EA code error within buffer OR Points with coordinates, no EA code and out of fwork
HHloc_sjoin$manual <- ifelse(((HHloc_sjoin$ea_number != HHloc_sjoin$ea_code_framework) & (HHloc_sjoin$buffer == 1))|
                               ((HHloc_sjoin$ea_number ==0)&(HHloc_sjoin$x!=0)),
                             HHloc_sjoin$manual<-1,
                             HHloc_sjoin$manual<-0)

num_man_errors <- nrow(HHloc_sjoin[HHloc_sjoin$manual == 1,])

#EXPORT CSV to export to QGIS and correct manually 
#set time string for file names
time_date<-format(Sys.time(),'_%Y%m%d_%H%M')

HHloc_sjoin$drop <- ifelse((HHloc_sjoin$ea_corrected == 0) & (HHloc_sjoin$x==0),
                           HHloc_sjoin$drop <- 1,
                           HHloc_sjoin$drop<- 0)

write.csv(HHloc_sjoin,file = paste0("path/toclean_man",time_date,'.csv'))

#the dropped interviews...take them from HHloc_sjoin


dropped <- subset(HHloc_sjoin,HHloc_sjoin$drop == 1)

write_xlsx(dropped,"path/dropped.xlsx")

