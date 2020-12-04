modelNOW <- function(countyName, idLoc, cropName, Stat){
  ##---MainProgram___----
  listFrame = read.csv('ListLocations.csv')
  if(cropName == 'Almond'){
    # threshAc = c(300, 1050, 700, 700, 700, 700)
    threshAc = c(300, 1050, 831.56, 831.56, 831.56, 831.56)
  } else if (cropName == 'Pistachio'){
    # threshAc = c(300, 1050, 550, 550, 550, 550)
    threshAc = c(300, 1050, 757.04, 757.04, 757.04, 757.04)
  }
  ## GDD Parameters-------
  Tbase = 54.86
  Tcutoff = 95
  tbase = convertF2C(Tbase)
  tcutoff = convertF2C(Tcutoff)
  threshAc = convertF2C(threshAc)
  ## Locations-------
  locName = paste(countyName, idLoc, sep = '')
  lat = listFrame$Latitude[listFrame$County==locName]
  long = listFrame$Longitude[listFrame$County==locName]
  modelList = c('ACCESS1',	'CANESM21',	'CAESM1BGC',
                'CCSM4',	'CMCC_CMS',	'CNRCCM5',
                'GFDL_CM3',	'HADGECC',	'HADGEES',	'MICRO5')
  ## GDD Model and individual plots-------
  for(idModel in 1:length(modelList)){
    df.site<- analyzePlot(locName, modelList, idModel,
                          Stat, cropName, tbase, tcutoff,
                          threshAc, countyName, lat, long)
  }
  ## GDD Combined plots-------
  dataSetR45 = interpretSet(locName, modelList, cropName, 1)
  dataSetR85 = interpretSet(locName, modelList, cropName, 2)
  
  ## GDD CI plots-------
  exportResultRCP45 <- exportResult(locName, cropName, 'RCP45')
  exportResultRCP85 <- exportResult(locName, cropName, 'RCP85')
  
  ## Statistics and slope calculations for RCP45 -----
  dataFirstRCP45 = dataSetR45$dataFirst
  dataSecondRCP45 = dataSetR45$dataSecond
  dataThirdRCP45 = dataSetR45$dataThird
  dataFourthRCP45 = dataSetR45$dataFourth
  dataFifthRCP45 = dataSetR45$dataFifth
  dataSixthRCP45 = dataSetR45$dataSixth
  StatSummary1R45 = getStatSummary(dataFirstRCP45)
  StatSummary2R45 = getStatSummary(dataSecondRCP45)
  StatSummary3R45 = getStatSummary(dataThirdRCP45)
  StatSummary4R45 = getStatSummary(dataFourthRCP45)
  StatSummary5R45 = getStatSummary(dataFifthRCP45)
  StatSummary6R45 = getStatSummary(dataSixthRCP45)
  overallStat = rbind(StatSummary1R45, StatSummary2R45,StatSummary3R45, 
                      StatSummary4R45,StatSummary5R45,StatSummary6R45)
  
  
  fileName2Save1 = 'RCP45Year'
  dir2Save = paste('Output/', cropName, '/Statistics', sep = '')
  if (dir.exists(dir2Save)==FALSE){dir.create(dir2Save)}
  fileName2Save = paste(dir2Save, '/1', locName, fileName2Save1, 'Data.csv', sep = '')
  write.csv(dataFirstRCP45, fileName2Save)
  fileName2Save = paste(dir2Save, '/2', locName, fileName2Save1, 'Data.csv', sep = '')
  write.csv(dataSecondRCP45, fileName2Save)
  fileName2Save = paste(dir2Save, '/3', locName, fileName2Save1, 'Data.csv', sep = '')
  write.csv(dataThirdRCP45, fileName2Save)
  fileName2Save = paste(dir2Save, '/4', locName, fileName2Save1, 'Data.csv', sep = '')
  write.csv(dataFourthRCP45, fileName2Save)
  fileName2Save = paste(dir2Save, '/5', locName, fileName2Save1, 'Data.csv', sep = '')
  write.csv(dataFifthRCP45, fileName2Save)
  fileName2Save = paste(dir2Save, '/6', locName, fileName2Save1, 'Data.csv', sep = '')
  write.csv(dataSixthRCP45, fileName2Save)
  fileName2Save = paste(dir2Save, '/7', locName, fileName2Save1, 'Data.csv', sep = '')
  write.csv(overallStat, fileName2Save)
  
  ## Statistics and slope calculations for RCP85 ---
  dataFirstRCP85 = dataSetR85$dataFirst
  dataSecondRCP85 = dataSetR85$dataSecond
  dataThirdRCP85 = dataSetR85$dataThird
  dataFourthRCP85 = dataSetR85$dataFourth
  dataFifthRCP85 = dataSetR85$dataFifth
  dataSixthRCP85 = dataSetR85$dataSixth
  StatSummary1R85 = getStatSummary(dataFirstRCP85)
  StatSummary2R85 = getStatSummary(dataSecondRCP85)
  StatSummary3R85 = getStatSummary(dataThirdRCP85)
  StatSummary4R85 = getStatSummary(dataFourthRCP85)
  StatSummary5R85 = getStatSummary(dataFifthRCP85)
  StatSummary6R85 = getStatSummary(dataSixthRCP85)
  overallStat = rbind(StatSummary1R85, StatSummary2R85,StatSummary3R85, StatSummary4R85,StatSummary5R85)
  
  fileName2Save1 = 'RCP85Year'
  if (dir.exists(dir2Save)==FALSE){dir.create(dir2Save)}
  fileName2Save = paste(dir2Save, '/1', locName, fileName2Save1, 'Data.csv', sep = '')
  write.csv(dataFirstRCP85, fileName2Save)
  fileName2Save = paste(dir2Save, '/2', locName, fileName2Save1, 'Data.csv', sep = '')
  write.csv(dataSecondRCP85, fileName2Save)
  fileName2Save = paste(dir2Save, '/3', locName, fileName2Save1, 'Data.csv', sep = '')
  write.csv(dataThirdRCP85, fileName2Save)
  fileName2Save = paste(dir2Save, '/4', locName, fileName2Save1, 'Data.csv', sep = '')
  write.csv(dataFourthRCP85, fileName2Save)
  fileName2Save = paste(dir2Save, '/5', locName, fileName2Save1, 'Data.csv', sep = '')
  write.csv(dataFifthRCP85, fileName2Save)
  fileName2Save = paste(dir2Save, '/6', locName, fileName2Save1, 'Data.csv', sep = '')
  write.csv(dataSixthRCP85, fileName2Save)
  fileName2Save = paste(dir2Save, '/7', locName, fileName2Save1, 'Data.csv', sep = '')
  write.csv(overallStat, fileName2Save)
}