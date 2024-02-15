#SAMPLING
#
#This file contains the code to produce versions of datazones/wards, sample the
#DEFRA air pollution data in their area, and attach the sampled value as a new
#column

library(raster)
library(tools)
library(stringr)

#Given a set of polygons, sources the requested pollution data for the specified
#year, and then attaches the average value across each polygon to it in a new
#column per pollution type. If no year is specified, 2022 is used. If no
#pollution types are specified, all types with data for the specified year are
#attached.

attachPollutionData <- function(polys,year=2022,pTs=NULL){
  if (is.null(pTs)){
    pTs <- getDEFRAPollutants()
  }
  print("attaching pollution data:")
  for (pol in pTs){
    if (is.null(getDownloadURL(pol))){
      next
    }
    print(paste(" ",pol,"...",sep=""))
    rPath <- paste("data/derived/",pol,"_",year,".grd",sep="")
    raster <- loadOrGenerate(rPath,makeAirPolRaster,loadFunction=raster::raster,year=year,pol=pol)
    polys <- joinOnRasterData(raster,polys,pol)
  }
  return(polys)
}

#Sources the Glasgow datazones, all available pollution rasters, and adds a
#column for each, specifying the average value across each polygon. The modified
#shapefile is then saved to the path specified.

makeAirPolDZs <- function(path="data/output/datazones_with_pollution.shp",area="greater"){
  if (area == "greater"){
    zp <- "data/derived/datazones/glasgowDatazones.shp"
    gdz <- loadOrGenerate(zp,getGlasgowDatazones,read_sf)
  }
  else{
    zp <- "data/derived/datazones/cityRegionDatazones.shp"
    gdz <- loadOrGenerate(zp,getCityRegionDatazones,read_sf)

  }
  gdP <- attachPollutionData(gdz)
  st_write(gdP,path)
}

#Same as makeAirPolDZs, but for electoral wards instead of datazones.

makeAirPolWards <- function(path="data/output/wards_with_pollution.shp"){
  wp <- "data/derived/wards/glasgowWards.shp"
  wds <- loadOrGenerate(wp,extractGlasgowWards,read_sf)
  gdP <- attachPollutionData(wds)
  st_write(gdP,path)
}
