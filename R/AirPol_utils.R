#AIRPOLUTILS
#
#This file contains a load of utility functions that pertain only to the DEFRA
#air pollution data. They're mostly exercises in edge-cases and frustration.
#Only a fool would trust them.

#Hands back all the pollutants that DEFRA has air quality data for. This is no
#Guarantee that data exists for any given year!

getDEFRAPollutants <- function(){
  return(c("PM10","PM25","NOX","NO2","CO8hr","COMean","SO2","Ozone","Benzene"))
}

#A massive and sprawling function which takes a year and pollution type and
#turns it into the URL for the relevant defra data. Returns NULL if it can't.
#Their urls are a total mess with a bunch of edge cases, so don't trust this
#function too much...

getDownloadURL <- function(pol,year=2022){
  polL <- str_to_lower(pol)
  base <- "https://uk-air.defra.gov.uk/datastore/pcm/map"
  ey <- substr(toString(year),3,4)
  if (polL == "benzene"){
    if (year < 2003 || year > 2022){
      return(NULL)
    }
    return(paste(base,"bz",year,".csv",sep=""))
  }
  if (polL == "ozone"){
    if (year < 2003 || year > 2022){
      return(NULL)
    }
    if (year == 2003){
      return("https://uk-air.defra.gov.uk/datastore/pcm/mapdaysgt12003_2.csv")
    }
    if (year == 2004){
      return("https://uk-air.defra.gov.uk/datastore/pcm/mapdgt12004.csv")
    }
    if (year == 2005){
      return("https://uk-air.defra.gov.uk/datastore/pcm/mapdgt120_2005r.csv")
    }

    return(paste(base,"dgt120",ey,".csv",sep=""))
  }
  if (polL == "so2"){
    if (year < 2002 || year > 2022){
      return(NULL)
    }
    if (year == 2002){
      return("https://uk-air.defra.gov.uk/datastore/pcm/mapso202annd.csv")
    }
    if (year < 2015){
      return(paste(base,"so2",ey,"ann.csv",sep=""))
    }
    return(paste(base,"so2",year,".csv",sep=""))
  }
  if (polL == "co8hr"){
    if (year < 2002 || year > 2010){
      return(NULL)
    }
    ex <- ".csv"
    if (year < 2005){
      ex <- "_01.csv"
    }
    return(paste(base,"com8hr",year,ex,sep=""))
  }
  if (polL == "comean"){
    if (year < 2002 || year > 2010){
      return(NULL)
    }
    ex <- ".csv"
    if (year < 2005){
      ex <- "_01.csv"
    }
    return(paste(base,"coamean",year,ex,sep=""))
  }
  if (polL == "nox" || polL == "no2"){
    if (year < 2001 || year > 2022){
      return(NULL)
    }
    ex <- ".csv"
    if (year == 2001){
      ex <- "_3.csv"
    }
    else if (year == 2002){
      ex <- "_1.csv"
    }
    return(paste(base,polL,year,ex,sep=""))
  }
  if (polL == "pm25" || polL == "pm10"){
    if (year < 2001 || year > 2022){
      return(NULL)
    }
    if (year == 2001 && polL == "pm25"){
      return(NULL)
    }
    ext <- "g.csv"
    if (year < 2003){
      ext <- ".csv"
    }
    if (year == 2003){
      if (polL == "pm10"){
        return("https://uk-air.defra.gov.uk/datastore/pcm/mappm102003s15a.csv")
      }
      return("https://uk-air.defra.gov.uk/datastore/pcm/mappm252003grav.csv")
    }
    if (year == 2005){
      if (polL == "pm10"){
        return("https://uk-air.defra.gov.uk/datastore/pcm/mappm1005ac.csv")
      }
      return("https://uk-air.defra.gov.uk/datastore/pcm/mappm2505ac.csv")
    }
    if (year == 2006){
      ext <- "gh.csv"
    }
    return(paste(base,polL,year,ext,sep=""))
  }
}

getUnit <- function(pol, year=2022){
  polL <- str_to_lower(pol)
  if (polL == "co"){
    return("mg m-3")
  }
  if (polL == "ozone"){
    return("Days")
  }
  return("µg m-3")
}

getMetric <- function(pol,year=2022){
  polL <- str_to_lower(pol)
  if (polL == "ozone"){
    return("Days Where Max 8-hr Concentration >120 µg m-3")
  }
  return(paste("Annual Mean of",pol,"in Air"))
}
