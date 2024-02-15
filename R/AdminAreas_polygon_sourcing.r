#POLYGON SOURCING
#
#This file contains functions for automatically producing the polygons
#representing datazones and electoral wards across greater glasgow

library(sf)

#Automatically downloads the 2020 settlement boundaries as defined by NRS. This
#is used as a source for the boundary of Greater Glasgow.

getSettlementBoundaries <- function(path="data/source/settlements/settlementBoundaries.shp"){
  source <- "https://www.nrscotland.gov.uk/files/geography/dataset/Settlements2020boundaries.zip"
  hold <- ".temprepo/bdt.zip"
  prepareDir(hold)

  downloadFile(source,hold,ident="2020 settlement boundary data",mode="wb")
  unzipShapefile(hold,path)
  file.remove(hold)
}

#Downloads all the 2011 scottish datazone boundaries and saves them to the
#specified path.

getScottishDatazones <- function(path="data/source/datazones/2011Datazones.shp"){
  source <- "https://maps.gov.scot/ATOM/shapefiles/SG_DataZoneBdry_2011.zip"
  hold <- ".temprepo/dz.zip"
  prepareDir(hold)

  downloadFile(source,hold,ident="2011 datazones",mode="wb")
  unzipShapefile(hold,path)
  file.remove(hold)
}

getBoundaryLines <- function(path="data/source/OS/boundaryLines.zip"){
  source <- "https://api.os.uk/downloads/v1/products/BoundaryLine/downloads?area=GB&format=ESRI%C2%AE+Shapefile&redirect"
  downloadFile(source,path,ident="Open OS Boundary Data",mode="wb",timeout=1800)
}

#Extracts the electoral wards shapefile from the OS BoundaryLine Zip

getElectoralWards <- function(path="data/source/wards/electoralWards.shp"){
  blp <- "data/source/OS/boundaryLines.zip"
  boundaryLines <- loadOrGenerate(blp,getBoundaryLines,parrot)
  tDir <- dirname(path)
  uzP <- ".temprepo/dz"
  prepareDir(paste(uzP,"test.txt",sep="/"))
  subPath <- "Data/GB"
  tName <- "scotland_and_wales_const_region"
  unzipShapefile(boundaryLines,path,uzP,tName,subPath)
  unlink(uzP,recursive=T)
}

#Extracts the parish regions shapefile from the OS BoundaryLine Zip

getParishRegions <- function(path="data/source/parishes/parishRegions.shp"){
  blp <- "data/source/OS/boundaryLines.zip"
  boundaryLines <- loadOrGenerate(blp,getBoundaryLines,parrot)
  tDir <- dirname(path)
  uzP <- ".temprepo/dz"
  prepareDir(paste(uzP,"test.txt",sep="/"))
  subPath <- "Data/GB"
  tName <- "parish_region"
  unzipShapefile(boundaryLines,path,uzP,tName,subPath)
  unlink(uzP,recursive=T)
}

#Extracts any ward from the wards data whose name starts with "Glasgow" and
#saves it to a new file.

extractGlasgowWards <- function(path="data/derived/wards/glasgowWards.shp"){
  wp <- "data/source/wards/electoralWards.shp"
  wards <- loadOrGenerate(wp,getElectoralWards,read_sf)

  glasgowWards <- wards[startsWith(wards$NAME,"Glasgow"),]

  st_write(glasgowWards,path)
}

#Takes the settlement boundaries file, extracts anything named "Greater
#Glasgow", and saves it to its own shapefile.

extractGreaterGlasgowBoundary <- function(path="data/derived/greaterGlasgow/greaterGlasgow.shp"){
  bp <- "data/source/settlements/settlementBoundaries.shp"
  bounds <- loadOrGenerate(bp,getSettlementBoundaries,read_sf)

  glasgow <- bounds[bounds$name=="Greater Glasgow",]

  if (length(glasgow$name) != 1){
    print("!!! - wrong amount of Glasgows! The boundary subsetting's knackered!")
    quit()
  }

  st_write(glasgow,path)
}

getGlasgowCityRegionBoundary <- function(path="data/derived/glasgowCityRegion/cityRegion.shp"){
  parP <- "data/source/parishes/parishRegions.shp"
  parishes <- loadOrGenerate(parP,getParishRegions,read_sf)
  parishWhiteList <- c("EAST_DUNBARTONSHIRE","EAST_RENFREWSHIRE","GLASGOW_CITY","INVERCLYDE","NORTH_LANARKSHIRE","RENFREWSHIRE","SOUTH_LANARKSHIRE","WEST_DUNBARTONSHIRE")

  relPar <- parishes[parishes$FILE_NAME %in% parishWhiteList,]
  bound <- st_union(relPar)
  st_write(bound,path)
}

#Takes the scottish datazones and the polygon representing Greater Glasgow, and
#filters the datazones to only include ones that have some percentage of their
#area within it. This is necessary since the datazones and city boundary don't
#quite line up, even though they're meant to. My best guess is that the problem
#is that these are the 2011 datazones, and the boundary has been updated more
#recently than that.

getGlasgowDatazones <- function(path="data/derived/datazones/glasgowDatazones.shp"){
  print("getting glasgow specific datazones...")
  gp <- "data/derived/greaterGlasgow/greaterGlasgow.shp"
  gla <- loadOrGenerate(gp,extractGreaterGlasgowBoundary,read_sf)
  glp <- gla$geometry[1]

  zp <- "data/source/datazones/2011Datazones.shp"
  zones <- loadOrGenerate(zp,getScottishDatazones,read_sf)

  gZones <- filterByRegion(zones,glp,"DataZone",0.25)
  st_write(gZones,path)

}

getCityRegionDatazones <- function(path="data/derived/datazones/cityRegionDatazones.shp"){
  print("getting glasgow city region datazones...")
  rp <- "data/derived/glasgowCityRegion/cityRegion.shp"
  reg <- loadOrGenerate(rp,getGlasgowCityRegionBoundary,read_sf)
  glp <- reg$geometry[1]

  zp <- "data/source/datazones/2011Datazones.shp"
  zones <- loadOrGenerate(zp,getScottishDatazones,read_sf)

  gZones <- filterByRegion(zones,glp,"DataZone",0.25)
  st_write(gZones,path)

}
