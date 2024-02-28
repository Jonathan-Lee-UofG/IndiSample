#UTIL FUNCTIONS
#
#This file contains a load of functions which are useful, but ultimately not
#really relevant to understanding the processing pipeline. All functions within
#were written by Jonathan Lee in late January, 2024.

library(raster)
library(tools)
library(stringr)
library(exactextractr)
library(raster)

#a lazy-evaluation data sourcing function. The idea is that you give the path
#where some data should be, a function which would create it, and a function
#which can parse the file. The function will then check if the file already
#exists, generate it if it doesn't, then unconditionally parse it with the load
#function. I assure you this is more helpful than it sounds!

loadOrGenerate <- function(path,genFun,loadFunction=NULL,...){
  if (file.exists(path) == F){
    prepareDir(path)
    genFun(path,...)
  }
  if (is.null(loadFunction)){
    loadFunction <- function(ppath){
      obj <- get(load(ppath)[1])
      return(obj)
    }
  }
  return(loadFunction(path))
}

parrot <- function(x){
  return(x)
}

#essentially a wrapper around 'download.file' with some QoL stuff, most notably
#an automatic but customisable message to the user and the ability to
#temporarily override the download timeout.

downloadFile <- function(source,dest,ident=NULL,timeout=300,...){
  if (is.null(ident)){
    ident <- basename(dest)
  }
  print(paste("Downloading ", ident, " from '",source,"'. This could take a while...",sep=""))
  ot <- getOption("timeout")
  options(timeout = max(timeout, ot))
  download.file(source, dest,...)
  options(timeout = ot)
}

#Takes a zip with a shapefile in it, and extracts it to the desired path, such
#that the .shp file will have the path defined by shpPath. The help here is
#that it also grabs and renames all the accompanying files that are part of the
#shapefile.

unzipShapefile <- function(source,shpPath,exPath=NULL,tName=NULL,subPath=NULL){
  tDir <- dirname(shpPath)
  if (is.null(exPath)){
    exPath <- tDir
  }
  unzip(source,exdir=exPath)

  if (!is.null(subPath)){
    exPath <- paste(exPath,subPath,sep="/")
  }
  nName <- file_path_sans_ext(basename(shpPath))
  for (file in list.files(exPath)){
    if (is.null(tName)){
      tName <- file_path_sans_ext(basename(file))
    }
    if (file_path_sans_ext(file) == tName){
      fulp <- paste(exPath,"/",file,sep="")
      nLoc <- paste(tDir,str_replace(file,tName,nName),sep="/")
      file.rename(fulp,nLoc)
    }
  }
}

#Given a path, ensures that all the directories in it exist.

prepareDir <- function(path){
  dp <- dirname(path)
  if (dir.exists(dp) == F){
    dir.create(dp,recursive=T)
  }
}

#given a CSV in the form used by DEFRA and the name of the field containing its
#data, converts it into a 1km grid and saves it to the specified output path.

createDEFRARasterFromDataframe <- function(data,outPath,dataField){
  bng <- '+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +datum=OSGB36 +units=m +no_defs'
  xyf <- data[,c("x","y",dataField)]
  rast <- rasterFromXYZ(xyf,crs=bng)
  writeRaster(rast,outPath)
}

#uses exact_extract from the exactextractr package to sample the raster provided
#for each polygon to derive its mean value, which is written to the fieldname
#specified. Returns the modified polygons.

joinOnRasterData <- function(raster,polys,field,mode="mean"){
  polys[field] <- exact_extract(raster,polys,mode)
  return(polys)
}

filterByRegion <- function(polys,region,uField,threshold=0.25){
  gui <- vector()
  for (i in 1:nrow(polys)){
    pol <- polys[i,]
    geo <- pol$geometry

    ins <- st_intersection(geo,region)
    if (length(ins) >0 && as.double(st_area(ins[1])/st_area(geo)) > threshold){
      gui <- append(gui,pol[[uField]])
    }
  }

  return(polys[polys[[uField]] %in% gui,])
}
