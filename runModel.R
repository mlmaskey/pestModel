rm(list=ls())
cat("\014")
library(ggplot2)
library(ggpubr)
library(reshape2)
library(lubridate)
source("function4GDD.R")
source("modelNOW.R")
listFrame = read.csv('ListLocations.csv')
countyList = levels(listFrame$County.1)
Stat = 'MEDIAN'
#---Pistachio -----
dfPistachio = list()
dfAlmond = list()
k=1
for (idCounty in 1:length(countyList)){
# for (idCounty in 1:length(countyList)){
    countyName =countyList[idCounty]
  cropName = 'Pistachio'
  for(i in 1:5){
    idLoc = i-1
    dfGDDPistachio <- modelNOW(countyName, idLoc, cropName = 'Pistachio', Stat)
    dfGDDAlmond <- modelNOW(countyName, idLoc, cropName = 'Almond', Stat)
    dfPistachio[[k]] = dfGDDPistachio
    dfAlmond[[k]] = dfGDDAlmond
    k=k+1
  }
} 

 