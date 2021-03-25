###AUTOMATIC GPS AND EA CODES CLEANING / LAST MODIFICATIONS AFTER MANUAL CLEANING - STAT DATA COLLECTION IN SURVEY SOLUTIONS####
##Luis de la Rua - SPC - SDD - MARCH 2021
#VUT PHC


library(sf)
library(sp)
library(spData)
library(rgeos)
library(dplyr)
library(rgdal)
library(tidyr)
library(tidyverse)
library(writexl)

setwd("path")

#load point layer cleaned
points <- st_read("path")
#tidy up points
points <- points[,c("interview__key","ea_corrected","geom","centroid")]

#load EA limits (we need it to correct iid and vid codes and names)
EA <- st_read("path")

#tidy up EA
EA<-as.data.frame( EA[,c("eaid","acid","iid","pid","acname","iname","pname")])

#Extract lat long from geom field

points <- points %>%
  mutate(lat = unlist(map(points$geom,1)),
         long = unlist(map(points$geom,2)))
points

#get island and village names iid and inames from EA framework
points <- merge(points,EA,by.x="ea_corrected", by.y="eaid",all.x=T)

#clean and export to xls

points <- points[,c("interview__key","ea_corrected","lat","long","acid", "acname","iid", "iname","pid","pname","centroid")]

write_xlsx(points,"path/GIS_processed.xlsx")




