library(ggplot2)
library(sf)
library(maps)
library(dplyr)
library(readxl)
library(rlang)
library(tidyverse)
library(tidygeocoder)
library(ggmap)
library(tmaptools)
library(RCurl)
library(jsonlite)
library(leaflet)
library(mapview)
library(geosphere)


register_google(key = "AIzaSyB_Xyp3sB8pxmKZc3ovq-oibWMh2wH0Ba8")

nursedatacan = read_excel("/Users/diegoperez/Desktop/Data.xlsx", sheet = 1)
nursedatapos = read_excel("/Users/diegoperez/Desktop/Data.xlsx", sheet = 2)


TNFunction <- function(nurse_num) {
  NurseFilter <- function(nurse_num) {
  # Extract information for the given nurse number
  nurse <- nursedatacan[nurse_num,]
  ProfText <- nurse$ProfText
  SpecText <- nurse$SpecText
  ST_Pref <- nurse$ST_Pref
  City_Pref <- nurse$City_Pref
  # Filter nurse data based on professional text, specialization text, and state preference
  NurseFilter <- filter(nursedatapos, Prof == ProfText, GroupSpec == SpecText, ST == ST_Pref)
  # Check if no records found for the given specialization and state preference
  if (nrow(NurseFilter) == 0) {
    filST_Pref <- filter(nursedatapos, Prof == ProfText, ST == ST_Pref)
    if (nrow(filST_Pref) == 0) {
      filProf <- filter(nursedatapos, Prof == ProfText)
      print(filProf)
    } else { 
      NurseSubset <- filST_Pref  # Assign the filtered data to a new subset variable
      print(filST_Pref)
    }
  } else {
    NurseSubset <- NurseFilter  # Assign the filtered data to a new subset variable
    print(NurseFilter)
  }
}

#NurseFilter(11322)
nurse <- nursedatacan[nurse_num,]
#nurse$City_Pref
city1 <- geocode(nurse$City_Pref)
#print(city1)
#NurseFilter(11322)$City
geonurse <- as.data.frame(geocode(NurseFilter(nurse_num)$City))
#print(geonurse)
#distHaversine(geonurse,city1,r=3956)
geonursefunction <- function(geonurse) {
  distance <- distHaversine(geonurse,city1,r=3956)
  print(distance)
}
geonursefunction(geonurse)

}
TNFunction(11322)

#needs futher fixing, error when distance is zero
# Ex. prefrence is Atlanta, any hospital in the city of Atlanta would be a zero resulting in a error 
# Sol. if statement that can say if zero give 100% but if greater than zero compute percent through function 
