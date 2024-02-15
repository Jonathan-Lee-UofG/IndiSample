#PLOT GENERATION
#
#This file contains the functions to create maps showing air pollution across
#datazones and electoral wards across greater glasgow.

library(tools)
library(stringr)
library(ggplot2)
library(sf)
library(raster)

makeAirPolPlots <- function(polygons,outDir,polName="Polygon",pols=NULL,width=1920,height=1080){
  if (is.null(pols)){
    pols <- getDEFRAPollutants()
  }
  prepareDir(paste("plots",outDir,"test.txt",sep="/"))
  outDir <- paste("plots/",outDir,sep="")

  cJust <- element_text(hjust = 0.5)
  cMar <- unit(c(0,0,0,0),"lines")

  for (col in colnames(polygons)){
    if (!(col %in% pols)){
      next
    }
    pTitle <- paste(getMetric(col)," by ",polName,sep="")
    unit <- getUnit(col)
    pubPlot <- ggplot(polygons)+geom_sf(aes(fill = .data[[col]]))+theme(plot.title = cJust)+labs(title=pTitle,fill=paste("Concentration (",unit,")",sep=""))+scale_fill_gradient(limits=c(0,max(polygons[[col]])),low="white",high="red")
    ggsave(filename=paste(col,".png",sep=""),path=outDir,device="png",plot=pubPlot,units="px",width=width,height=height,bg="white")
  }
}

#plots for datazones in greater glasgow area
makeAirPolDatazonePlots <- function(pols=NULL){
  zp <- "data/output/datazones_with_pollution.shp"
  pdz <- loadOrGenerate(zp,makeAirPolDZs,read_sf,"greater")

  makeAirPolPlots(pdz,"datazones","Datazone")
}

makeAirPolCRDatazonePlots <- function(pols=NULL){
  zp <- "data/output/city_region_datazones_with_pollution.shp"
  pdz <- loadOrGenerate(zp,makeAirPolDZs,read_sf,"city_region")
  makeAirPolPlots(pdz,"city region datazones","City Region Datazone",pols,width=1920*3,height=1080*4)
}

makeAirPolWardPlots <- function(pols=NULL){
  wp <- "data/output/wards_with_pollution.shp"
  wdz <- loadOrGenerate(wp,makeAirPolWards,read_sf)
  makeAirPolPlots(wdz,"wards","Ward",pols)
}

makeAllAirPolPlots <- function(){
  makeAirPolDatazonePlots()
  makeAirPolCRDatazonePlots()
  makeAirPolWardPlots()
}
