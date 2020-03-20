to46N<-function(spatial_object){
  spatial_object_utm<-spTransform(spatial_object, CRS("+proj=utm +zone=46 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
  return(spatial_object_utm)
}
towgs84<-function(spatial_object){
  spatial_object_wgs<-spTransform(spatial_object, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
  return(spatial_object_wgs)
}


spatial_join<-function(point,poly){
  if (class(point)=="SpatialPoints"){
    df_over<-over(point,poly)
    df_joined<-data.frame(coordinates(point), df_over)
    coordinates(df_joined)<-~x+y
    proj4string(df_joined)<-proj4string(point)
    return (df_joined)}
  if (class(point)=="SpatialPointsDataFrame"){
    df_over<-over(point,poly)
    df_joined<-data.frame(coordinates(point),point@data, df_over)
    coordinate_names<-colnames(coordinates(point))
    coordinates(df_joined)<-coordinate_names
    proj4string(df_joined)<-proj4string(point)
    return (df_joined)}
  
  
}
