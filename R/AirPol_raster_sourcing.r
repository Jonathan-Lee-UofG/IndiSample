#RASTER SOURCING
#
#Functions to create rasters of the DEFRA air pollution data.

library(raster)
library(tools)
library(stringr)

#Given a pollutant and a year (default 2022), this function will source its
#DEFRA data csv and turn it into a raster, which it will then save to the path
#specified.

makeAirPolRaster <- function(path,pol,year=2022){
  print(paste("creating raster for ",year," ",pol," data...",sep=""))
  sPath <- paste("data/source/",pol,"_",year,".csv",sep="")
  dp <- getDownloadURL(pol,year)
  csFile <- loadOrGenerate(sPath,downloadFile,source=dp,dest=sPath,ident=paste(year,pol,"csv"),loadFunction = function(x){return(read.csv(x,skip=5))})

  dfi <- NULL
  for (cn in colnames(csFile)){
    if (cn != "x" && cn != "y" && cn != "gridcode"){
      dfi <- cn
      break
    }
  }

  createDEFRARasterFromDataframe(csFile,path,dfi)
}
