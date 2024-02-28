attachDatazoneHeat <- function(dzs){
  dzp <- "data/source/heatDemand/heatDatazones.csv"
  downURL <- "https://heatmap.data.gov.scot/downloads/Datazone_Heat_Demand.csv"
  dzHeat <- loadOrGenerate(dzp,downloadFile,source=downURL,ident="Datazone Heat Demands",loadFun=read.csv)
  heatFil <- select(dzHeat,-c(Area_m2,Name))
  joined <- left_join(dzs,heatFil,by=join_by(DataZone==DataZone2011))
  joined <- rename(joined,kwh_HeatAD=TotalHeatDemand_kWh_y,kwh_HtDnsA=DemandDensity_kWh_y_m2)
  return(joined)
}

attachHeatDemandGreaterDZData <- function(path="data/derived/heatDemand/glasgowDatazones.shp"){
  gp <- "data/derived/datazones/glasgowDatazones.shp"
  dzs <- loadOrGenerate(gp,getGlasgowDatazones,read_sf)
  
  joined <- attachDatazoneHeat(dzs)
  st_write(joined,path)
}

attachHeatDemandRegionDZData <- function(path="data/derived/heatDemand/cityRegionDatazones.shp"){
  rp <- "data/derived/datazones/cityRegionDatazones.shp"
  dzs <- loadOrGenerate(rp,getCityRegionDatazones,read_sf)
  
  joined <- attachDatazoneHeat(dzs)
  st_write(joined,path)
}

sampleHeatDemandbyWard <- function(path="data/derived/heatDemand/wards.shp"){
  wp <- "data/derived/wards/glasgowWards.shp"
  wards <- loadOrGenerate(wp, extractGlasgowWards, read_sf)  
  
  hp <- "data/source/heatDemand/demand250m.tiff"
  hs <- "https://sg-heatmap.s3.eu-west-1.amazonaws.com/hmp_250m.tiff"
  heatRas <- loadOrGenerate(hp,downloadFile,source=hs,ident="Raster Heat Usage Data",mode="wb",raster)
  
  heatWards <- joinOnRasterData(heatRas,wards,"Mwh_HeatAD",mode="sum")
  heatWards$kwh_HtDnsA <- heatWards$Mwh_HeatAD/st_area(heatWards$geometry)
  heatWards$Mwh_HeatAD <- heatWards$Mwh_HeatAD/1000
  
  st_write(heatWards,path)
}