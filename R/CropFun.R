CropFun <- function(xy,xmin=-68,xmax=-45,ymin=42,ymax=50){
  
  require(dplyr)
  require(sf)
  require(sp)
  
  latlong <- "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"
  planar <- "+proj=utm +zone=20 +datum=NAD83 +units=km +no_defs +ellps=GRS80 +towgs84=0,0,0"
  
  bb <- cbind(c(xmin,xmax,xmax,xmin,xmin),
              c(ymin,ymin,ymax,ymax,ymin)) %>%
    Polygon() %>% list() %>%
    Polygons(ID = 'ID1') %>% list() %>%
    SpatialPolygons(proj4string = CRS(latlong))%>%
    st_as_sf()%>%
    st_transform(planar)
 
  index <- xy%>%
    st_as_sf(coords = c("Longitude","Latitude"),crs=latlong)%>%
    st_transform(planar)%>%
    st_intersects(bb,sparse = F)

  return(index)
  
}
