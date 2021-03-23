###AUTOMATIC GPS CHECKINGS AND MONITORING - STAT DATA COLLECTION IN SURVEY SOLUTIONS####
##Luis de la Rua - SPC - SDD - October 2020
###VANUATU 2020 POPULATION AND HOUSING CENSUS###
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


#set work directory
setwd("path")

#Load EA framework
EA <- st_read("path")
#tidy up the layer we only need the EA code
EA<- EA[,c('eaid','acid','acname','pname')]

#load buffer EA limits
buffer <- st_read("path")
buffer$buffer <- 1 # we need this to identify points in buffer later
buffer <- buffer[,'buffer'] #tidy up

#set CRS we are working from now using EA's crs
WGS84CRS <- crs(EA)

#HH locations from SS in csv format
HHloc_csv <- read.csv("path")

#rename hh coordinates
names(HHloc_csv)[names(HHloc_csv) == "g4_dwelling_gps__Latitude"] <- "y"
names(HHloc_csv)[names(HHloc_csv) == "g4_dwelling_gps__Longitude"] <- "x"
colnames(HHloc_csv)

#csv table to sf object `/ we deal with this later for the moment we import points in shp directly
HHloc_sf <- st_as_sf(HHloc_csv, coords = c("x","y"), crs = WGS84CRS)
str(HHloc_sf)
crs(HHloc_sf)

#Spatial join HHloc - EA to get EA code geographically
join <- st_join(HHloc_sf,EA,join=st_intersects)
join <- st_join(join,buffer, join=st_intersects)

#replace null values on ea_2015 field and buffer
join$ea_number[is.na(join$ea_number)]<- 0
join$eaid[is.na(join$eaid)]<- 0
join$buffer[is.na(join$buffer)]<- 0


##CORRECT interv where intEAcode <> geoEAcode
#out the buffer we replace automatically by geoEAcode
join$eaidcorr <- ifelse(join$ea_number != join$eaid & join$buffer==0 , join$eaid , join$ea_number) 
#points with errors within the 30m buffer (potential accuracy error)to be checked and cleaned manually so we identify them with man=1
join$man<- ifelse(join$ea_number != join$eaid & join$buffer==1,1,0) 

##CORRECT interv with no EA code assigning EA code geographically (here the buffer condition does not affect)
join$eaidcorr <- ifelse(join$ea_number==0,join$eaid,join$ea_number) 

##CORRECT interv with no GPS 
#assign centroid not yet, wait to the end of the field work

#st_write (join, "path",delete_layer = T)


#subset code / location errors where EA != ea_2015

rallerrors <- subset(join,EA != ea_2015)
NUMrallerrors<- nrow(rallerrors)
NUMrallerrors
# st_write (rallerrors, "path",delete_layer = T)



#Subset points within and out of buffers
allerrors_buffer <- st_join(rallerrors,buffer,join=st_intersects)
allerrors_buffer$buffer[is.na(allerrors_buffer$buffer)] <-0


#subsetting all points with errors within the 30m buffer (potential accuracy error)
rbufferrors <- subset(allerrors_buffer,buffer== 1)
NUMrbufferrors <- nrow(rbufferrors)
NUMrbufferrors

#subsetting points with errors out of the 30 m buffer (means EA code problem)
rcodeerrors <- subset(allerrors_buffer,buffer==0)
NUMrcodeerrors <- nrow(rcodeerrors)
NUMrcodeerrors

#subsetting points with no GPS coordinates (needs to be reported to enumerator to repeat GPS question)
rnoGPSerrors <- subset(HHloc_sjoin, buildingGPS__Accuracy == '.a')
NUMrnoGPSerrors <- nrow(rnoGPSerrors)
NUMrnoGPSerrors

#subseting Questionnaires with missing EA code missing from questionnaire
rnoEAcodeerrors <- subset(HHloc_sjoin,EA==0)
NUMrnoEAcodeerrors <- nrow(rnoEAcodeerrors)
NUMrnoEAcodeerrors

#subseting errors System GPS duplicates, merge x and y fields and find not unique
HHloc_csv$GPS<-paste(HHloc_csv$x,HHloc_csv$y,sep="_")
HHloc_csv <- transform(HHloc_csv, count=ave(GPS,GPS,FUN = length))

rdupGPSodeerrors <- subset(HHloc_csv,count!=1)
NUMrdupGPSodeerrors  <- nrow(rdupGPSodeerrors )
NUMrdupGPSodeerrors

#Export all the tables into csv to be read by NSO
time_date<-format(Sys.time(),'_%Y%m%d_%H%M')


write.csv(rallerrors,file = paste0("rcsv/allerrors",time_date,'.csv'))
write.csv(rbufferrors,file = paste0("rcsv/rbufferrors",time_date,'.csv'))
write.csv(rcodeerrors,file = paste0("rcsv/rcodeerrors",time_date,'.csv'))
write.csv(rnoGPSerrors,file = paste0("rcsv/rnoGPSerrors",time_date,'.csv'))
write.csv(rnoEAcodeerrors,file = paste0("rcsv/rnoEAcodeerrors",time_date,'.csv'))
write.csv(rdupGPSodeerrors,file = paste0("rcsv/rdupGPSodeerrors",time_date,'.csv'))

#Export all errors to

#Map visualization of the geographic errors outputs

#blocks viewer screens and forces the maps to be displayed on browser
options(viewer = NULL)

#1st test mapping all errors
map_allerrors<- leaflet() %>%
  addPolygons(data= EA, color = "#ff7f00", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0,
              label = EA$ea_2015,
              labelOptions = labelOptions(noHide=T,textOnly = T,style = list(
                "color" = "white",
                "text-align" = "center",
                "font-family" = "sans-serif",
                "font-weight" = "bold",
                "font-size" = "15px"))) %>%
  addCircles(data = rallerrors,radius = 3, color = 'red',
             popup =paste(rallerrors$EA, rallerrors$intrvw__k,sep = ' / '))%>%
  addProviderTiles(providers$CartoDB.Positron)%>%#putting some background when ESRI stops working
  addProviderTiles(providers$Esri.WorldImagery)%>%
  setView(172.976187, 1.326785,zoom=17)
map_allerrors

#2nd test map errors classified by type of error
map_errors_type <- leaflet() %>%
  addPolygons(data= EA, color = "#ff7f00", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0,
              group = "EA",
              label = EA$ea_2015,
              labelOptions = labelOptions(noHide=T,textOnly = T,style = list(
                "color" = "#333333",
                "text-align" = "center",
                "font-family" = "sans-serif",
                "font-weight" = "bold",
                "font-size" = "18px"))) %>%
  addCircles(data = rcodeerrors,radius = 3, 
             color = 'red',
             group = "EA code Errors",
             popup = paste(rcodeerrors$EA, rcodeerrors$interview__key,sep = ' / ')) %>%
  addCircles(data = rbufferrors,radius = 3,
             color = 'blue',
             group = "Code errors close to boundaries",
             popup = paste(rbufferrors$EA, rbufferrors$interview__key,sep = ' / ')) %>%
  addCircles(data = rnoEAcodeerrors,radius = 3,
             color = 'green',
             group = "EA code missing",
             popup = paste(rnoEAcodeerrors$EA, rnoEAcodeerrors$interview__key,sep = ' / ')) %>%
  addCircles(lng = rdupGPSodeerrors$x,lat=rdupGPSodeerrors$y,radius = 3,
             color = 'black',
             group = "GPS duplicate",
             popup = paste(rdupGPSodeerrors$EA, rdupGPSodeerrors$interview__key,sep = ' / '))%>%
  addProviderTiles(providers$OpenStreetMap.Mapnik)%>%
  addProviderTiles(providers$Esri.WorldImagery, group = "ESRI_Sat" ) %>%
  addLayersControl(
    overlayGroups = c("EA", "EA code Errors","Code errors close to boundaries","EA code missing","GPS duplicate"),
    options = layersControlOptions(collapsed = FALSE))%>%
  setView(172.976187, 1.326785,zoom=17)
map_errors_type



