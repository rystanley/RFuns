## Assign NAFO divisions

#load libraries ----------------
library(data.table)
library(dplyr)
library(ggplot2)
library(sf)
library(RCurl)

#source function to crop the data
script <- getURL("https://raw.githubusercontent.com/rystanley/RFuns/master/R/CropFun.R", 
                 ssl.verifypeer = FALSE)
eval(parse(text = script),envir=.GlobalEnv)

#common projection
latlong <- "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"

#read in data
dat <- fread("Andrew Czich/BNAM_BottomTemp_2012_Monthly.csv")%>%
  data.frame()

NAFO <- st_read("c:/Users/stanleyr/Documents/Github/Fundian/data/NAFO_Divisions_Shapefiles/Divisions.shp")%>%
        st_transform(latlong)%>%
        select(ZONE,geometry)%>%
        filter(!is.na(ZONE))
   
#Crop the data to the area of interest (based on CropFun parameters)
#convert to sf object with assigned projection
#assign each point a "ZONE" based on the NAFO zones
#convert ZONE from factor to character

dat <- filter(dat,CropFun(dat%>%select(Longitude,Latitude)))%>%
           st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%
           st_join(.,NAFO,join=st_intersects)%>%
           mutate(ZONE=as.character(ZONE))

#Now you have a new column "ZONE" that has the NAFO zone assigned to each point. 
