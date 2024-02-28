makeHeatDemandPlots <- function(polygons,outDir,polName="Polygon",totUnit="kwh",width=1920,height=1080){
  prepareDir(paste("plots/heatDemand",outDir,"test.txt",sep="/"))
  outDir <- paste("plots/heatDemand/",outDir,sep="")
  
  cJust <- element_text(hjust = 0.5)
  cMar <- unit(c(0,0,0,0),"lines")
  
  totField <- paste(totUnit,"HeatAD",sep="_")
  
  pTitle <- paste("Annual Heat Demand by ",polName,sep="")
  pubPlot <- ggplot(polygons)+geom_sf(aes(fill = .data[[totField]]))+theme(plot.title = cJust)+labs(title=pTitle,fill=paste("Annual Demand (",totUnit,")",sep=""))+scale_fill_gradient(limits=c(0,max(polygons[[totField]])),low="white",high="red")
  ggsave(filename=paste("Annual Demand.png",sep=""),path=outDir,device="png",plot=pubPlot,units="px",width=width,height=height,bg="white")
  
  pTitle <- paste("Annual Heat Demand per m2 by ",polName,sep="")
  pubPlot <- ggplot(polygons)+geom_sf(aes(fill = kwh_HtDnsA))+theme(plot.title = cJust)+labs(title=pTitle,fill="Annual (kwh/m2)")+scale_fill_gradient(limits=c(0,max(polygons$kwh_HtDnsA)),low="white",high="red")
  ggsave(filename=paste("Annual Demand by Area.png",sep=""),path=outDir,device="png",plot=pubPlot,units="px",width=width,height=height,bg="white")
  
}

#plots for datazones in greater glasgow area
makeHeatDemandDatazonePlots <- function(){
  zp <- "data/derived/heatDemand/glasgowDatazones.shp"
  pdz <- loadOrGenerate(zp,attachHeatDemandGreaterDZData,read_sf)
  
  makeHeatDemandPlots(pdz,"datazones","Datazone")
}

#plots for datazones in glasgow city region
makeHeatDemandRegionDatazonePlots <- function(){
  zp <- "data/derived/heatDemand/cityRegionDatazones.shp"
  pdz <- loadOrGenerate(zp,attachHeatDemandRegionDZData,read_sf)
  
  makeHeatDemandPlots(pdz,"regionDatazones","Datazone",width=1920*3,height=1080*4)
}

makeHeatDemandWardPlots <- function(){
  zp <- "data/derived/heatDemand/wards.shp"
  pdz <- loadOrGenerate(zp,sampleHeatDemandbyWard,read_sf)
  
  makeHeatDemandPlots(pdz,"wards","Electoral Ward",totUnit="Mwh")
}

makeAllHeatDemandPlots <- function(){
  makeHeatDemandDatazonePlots()
  makeHeatDemandRegionDatazonePlots()
  makeHeatDemandWardPlots()
}