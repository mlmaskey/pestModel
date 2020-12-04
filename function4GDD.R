getGDD <- function(avgT, Tbase, Tcutoff){
  if(is.na(avgT) == FALSE){
    if(avgT>Tbase && avgT<Tcutoff){
      GDDval = avgT - Tbase
    } else if (avgT >= Tcutoff){
      GDDval = Tcutoff - Tbase
    } else if (avgT<= Tbase){
      GDDval = 0
    } else {GDDval = NaN}
  } else {GDDval = NaN}
  
  return(GDDval)
}

convertF2C <- function(F){return((F-32)*(5/9))
}

getGDDvec <- function(data, idStart, tbase, tcutoff){
  vecGDD = matrix(0, nrow = nrow(data), ncol = 1)
  for(i in 1:nrow(data)){
    if(i<idStart){
      vecGDD[i] = 0
    } else{
      if (is.nan(data$avg[i])==FALSE || is.na(data$avg[i])==FALSE){
        vecGDD [i] = getGDD(data$avg[i], tbase, tcutoff)
      } else{
        vecGDD [i] = NaN
      }
      
    }
  }
  return(vecGDD)
}

accumulateGDD<- function(vecGDD, idStart){
  vecAc = matrix(0, nrow = length(vecGDD), ncol = 1)
  for(i in 1:length(vecGDD)){
    if(i<idStart){
      vecAc[i] = 0
    } else if (i== idStart){
      vecAc[i] = vecGDD[i]
    }
     else{
      vecAc[i]  = vecAc[i-1] + vecGDD[i]
    }
  }
  return(vecAc)
}

analyzeData1 <- function(dataSet, Nyears, idyearStart, tbase, tcutoff, threshAc){
  yearVec = year(dataSet$date)
  data = list()
  ncycle = length(threshAc);
  dayGDD = matrix(NaN, nrow=Nyears, ncol = (ncycle+1))
  for(i in 1:(Nyears)){
    print(paste("Analyzing the year ", idyearStart, sep = ''))
    idyear = idyearStart 
    dayGDD[i, 1] = idyear 
    data1 = dataSet[which(yearVec== as.character(idyear)),]
    dateStart = as.Date(paste(idyear, 1,1,sep = "-" ))
    idStart = which(data1$date==dateStart)
    vecGDD <- getGDDvec(data1, idStart, tbase, tcutoff)
    data1$GDD = vecGDD
    GDDdateStart = as.Date(paste(idyear, 1,1,sep = "-" ))
    GDDdateEnd = as.Date(paste(idyear, 10,31,sep = "-" ))
    idStart = which(data1$date==GDDdateStart)  
    idEnd = which(data1$date==GDDdateEnd)  
    vecGDD[(idEnd+1):length(vecGDD)] = 0
    for(j in 1:ncycle){
      vecAcGDD = accumulateGDD(vecGDD, idStart)
      idThresholds = which(vecAcGDD>=threshAc[j])
      if (length(idThresholds)!=0){
        dayofYear = idThresholds[1]
        idStart = dayofYear + 1
      } else {dayofYear = NaN}
      
      dayGDD[i, j+1] = dayofYear
    }
    
    # data1$AcGDD = vecAcGDD
    idyearStart = idyear + 1
  }
  dataList = list(data= dataSet, dayofYear=dayGDD)
  return(dataList)
}

setDat <- function(minTemp, maxTemp, idSet){
  nRecords = nrow(minTemp)
  idyearStart = minTemp[1,1]
  idyearEnd = minTemp[nRecords,1]
  Nyears = idyearEnd - idyearStart + 1
  dateVec = as.Date(paste(minTemp[,1],minTemp[,2],minTemp[,3],sep = "-" ))
  avgTemp = (minTemp[,idSet] + maxTemp[,idSet])/2
  tempRcP45 = data.frame(date=dateVec, min=minTemp[,idSet], max=maxTemp[,idSet], avg=avgTemp)
  setLists = list(set = tempRcP45, idyearStart = idyearStart, Nyears = Nyears)
  return(setLists)
}

list2mat <- function(inlist){
  ndim = ncol(inlist)
  ndata = nrow(inlist)
  matlist = matrix(0, nrow = ndata, ncol = ndim)
  for(i in 1:ndim){
    unlistI = unlist(inlist[,i])
    for(j in 1:ndata){
      matlist[j, i] = unlistI[j]
    }
  }
  return(matlist)
} 

avgData <- function(dataX, tau){
  ndata = nrow(dataX)
  X = dataX$X
  Y = dataX$Y
  avgData = matrix(NaN, nrow = (ndata-tau+1), ncol = 2)
  for(i in 1:(ndata-tau+1)){
      Y1 = Y[i:(i+tau-1)]
      avgData[i,1] = X[i]
      avgData[i,2] = round(mean(Y1, na.rm = TRUE))
  }
  return(avgData)
}

dirName <- function(cropName, siteName){
  dir0 = 'Output'
  if (dir.exists(dir0)==FALSE){dir.create(dir0)}
  dir2Save = paste(dir0, '/', cropName, sep = '')
  if (dir.exists(dir2Save)==FALSE){dir.create(dir2Save)}
  dir2Save = paste(dir2Save, '/', siteName, sep = '')
  if (dir.exists(dir2Save)==FALSE){dir.create(dir2Save)}
  return(dir2Save)
}

day2month<-function(data){
  ndata = length(data)
  if(ndata%%4 == 0){
    ndaysVec = c(31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
  } else{
    ndaysVec = c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
  }
  cumDays = cumsum(ndaysVec)
  cumData = cumsum(data)
  cumDataMonth = cumData[cumDays]
  monthData = matrix(NaN, nrow = 1, ncol = 12)
  monthData[1] = cumDataMonth[1]
  for(i in 2:12){
    monthData[i] = cumDataMonth[i]-cumDataMonth[i-1]
  }
  return(monthData/ndaysVec)
  
}
readyData <-function(listData, modelList, idModel){
  minDate = listData$min
  avgData = listData$avg
  maxData = listData$max
  tempSet = data.frame(date = minDate$Date, min = minDate[modelList[idModel]],
                       max = maxData[modelList[idModel]], avg = avgData[modelList[idModel]])  
  colnames(tempSet) = c('date', 'min', 'max', 'avg')
  return(tempSet)
}

analyzePlot<- function(locName, modelList, idModel, caseStat, cropName, tbase, tcutoff, 
                       threshAc, countyName, lat, long){
  siteName = paste(locName, modelList[idModel], sep = '')
  print(paste('-----', siteName, '-----', sep = ''))
  print('-----RCP4.5-----')
  listDataRCP45 <- processData(siteName, locName, modelList, 1, caseStat)
  tempSetRCP45 <-readyData(listDataRCP45, modelList, idModel)
  idyearStart <- year(tempSetRCP45$date[1])
  idyearEnd <- year(tempSetRCP45$date[nrow(tempSetRCP45)])
  Nyears = idyearEnd - idyearStart +1
  result.RCP45 = analyzeData1 (tempSetRCP45, Nyears, idyearStart, tbase, tcutoff, threshAc)
  print('-----RCP8.5-----')
  listDataRCP85 <- processData(siteName, locName, modelList, 2, caseStat)
  tempSetRCP85 <-readyData(listDataRCP85, modelList, idModel)
  result.RCP85 = analyzeData1 (tempSetRCP85, Nyears, idyearStart, tbase, tcutoff, threshAc)
  
  dayofYearRCP45 <- result.RCP45$dayofYear
  dayofYearRCP85 <- result.RCP85$dayofYear
  colnames(dayofYearRCP45) <- c('Year', 'First', 'Second', 'Third', 'Fourth', 'Fifth', 'Sixth')
  colnames(dayofYearRCP85) <- c('Year', 'First', 'Second', 'Third', 'Fourth', 'Fifth', 'Sixth')
  dir2Save = dirName(cropName, siteName)
  fileOut.csv = paste(dir2Save, '/RCP45Year.csv', sep = '')
  write.csv(dayofYearRCP45, fileOut.csv) 
  fileOut.csv = paste(dir2Save, '/RCP85Year.csv', sep = '')
  write.csv(dayofYearRCP85, fileOut.csv) 
  yearVec = idyearStart:idyearEnd
  cycleList = c('First', 'Second', 'Third', 'Fourth', 'Fifth', 'Sixth')
  X = yearVec
  idhistYearEnd = which(X==2005)
  for(i in 1:6){
    fileName = paste(dir2Save, "/Figure", i, ".png", sep = '')
    Xhis = X[1:idhistYearEnd]
    Y = dayofYearRCP45[,i+1]
    Y1 = dayofYearRCP85[,i+1]
    meanObs = mean(Y[1:idhistYearEnd], na.rm = TRUE)
    Yhis = Y
    Yhis[(idhistYearEnd+1):Nyears]= "NaN"
    Xtrend = X[(idhistYearEnd+1):Nyears]
    YR45 =Y
    YR45[1:idhistYearEnd] = "NaN"
    meanR45 = mean(Y[(idhistYearEnd+1):Nyears], na.rm = TRUE)
    meanR85 = mean(Y1[(idhistYearEnd+1):Nyears], na.rm = TRUE)
    textH = paste('Mean (Historical) = ', sprintf("%04.3f", meanObs), sep = "")
    text45 = paste('Mean (R4.5) = ', sprintf("%04.3f", meanR45), sep = "")
    text85 = paste('Mean (R8.5) = ', sprintf("%04.3f", meanR85), sep = "")
    lengNan <- length(which(Y=="NaN"))
    lengNan1 <- length(which(Y1=="NaN"))
    if(meanObs != "NaN" || meanR45 !="NaN" || meanR85 != "NaN"){
      YR85 = Y1
      YR85[1:idhistYearEnd] = "NaN"
      graphTitle = paste(cycleList[i], " Cycle for ", countyName, ' (', lat, ', ', long, ')')
      png(filename = fileName, width = 1000, height = 800)
      leg.text = c("Historical", "RCP4.5", "RCP8.5")
      plot(X, Yhis, type = 'l', col = "black", lwd = 2,
           main = graphTitle, xlab = "Year", ylab =  'DOY', xaxs ='i', xaxt='n',
           xlim = c(yearVec[1], yearVec[length(yearVec)]), ylim = c(min(Y, Y1, na.rm = TRUE)*0.9, max(Y, Y1, na.rm = TRUE)*1.1), 
           cex.lab=1.5, cex.axis=1.5, cex.main=2)
      axis(1, at = c(seq(yearVec[1], yearVec[length(yearVec)], by=10),yearVec[length(yearVec)]), cex.axis=1.5)
      lines(X, YR45, col="blue", lty=1, cex=1, lwd = 2)
      lines(X, YR85, col="red", lty=1, cex=1, lwd = 2)
      op <- par(cex = 2)
      legend('bottomleft',  legend = leg.text, horiz = F, 
             col = c("black", "blue", "red"), lty = 1, 
             lwd =  2, cex = 1.1, box.lty = 0, pch=c(NA,NA) , bty = "n")
      dev.off()
    }
  }
  return(list(RCP45 = dayofYearRCP45, RCP85=dayofYearRCP85))
}

gatherData <-function(siteName, locName, modelList, idScene){
  if(idScene==1){
    idCol = 4
  } else{idCol = 5}
  nmodel = length(modelList)
  fileMin = paste('Data', siteName, 'tasmin.csv', sep = '/')
  minTemp = read.csv(fileMin, header = F, sep = ",")
  dateVec = as.Date(paste(minTemp[,1],minTemp[,2],minTemp[,3],sep = "-" ))
  dfMinTemp = matrix(NaN, nrow = nrow(minTemp), ncol = nmodel)
  dfMaxTemp = matrix(NaN, nrow = nrow(minTemp), ncol = nmodel)
  ## reading data
  for (idModel in 1:nmodel) {
    print(paste('Gathering for ', locName, ' from model', modelList[idModel], sep = ''))
    siteName1 = paste(locName, modelList[idModel], sep = '')
    fileMin = paste('Data', siteName1, 'tasmin.csv', sep = '/')
    fileMax = paste('Data', siteName1, 'tasmax.csv', sep = '/')
    minTemp = read.csv(fileMin, header = F, sep = ",")
    maxTemp = read.csv(fileMax, header = F, sep = ",")
    tempRCP <- setDat(minTemp, maxTemp, idCol)
    dfMinTemp [, idModel] = tempRCP$set$min
    dfMaxTemp [, idModel] = tempRCP$set$max
    
  }
  
  ## calculating mean and median
  nRecords = nrow(dfMinTemp)
  vecMeanMin = vector(length = nRecords)
  vecMeanMax = vector(length = nRecords)
  vecMedianMin = vector(length = nRecords)
  vecMedianMax = vector(length = nRecords)
  for (i in 1:nRecords) {
    vecMeanMin[i] = mean(dfMinTemp[i, 2:ncol(dfMinTemp)], na.rm = TRUE)
    vecMeanMax[i] = mean(dfMaxTemp[i, 2:ncol(dfMaxTemp)], na.rm = TRUE)
    vecMedianMin[i] = median(dfMinTemp[i, 2:ncol(dfMinTemp)], na.rm = TRUE)
    vecMedianMax[i] = median(dfMaxTemp[i, 2:ncol(dfMaxTemp)], na.rm = TRUE)
    
  }
  dfMinTemp = as.data.frame(dfMinTemp)
  dfMaxTemp = as.data.frame(dfMaxTemp)
  colnames(dfMinTemp) = modelList
  colnames(dfMaxTemp) = modelList
  dfMinTemp$MEAN = vecMeanMin
  dfMinTemp$MEDIAN = vecMedianMin
  dfMaxTemp$MEAN = vecMeanMax
  dfMaxTemp$MEDIAN = vecMedianMax
  dateVec = data.frame(Date = dateVec)
  dfMinTemp = cbind(dateVec, dfMinTemp)
  dfMaxTemp = cbind(dateVec, dfMaxTemp)
  
  return(list(min = dfMinTemp, max = dfMaxTemp))
}

processData <- function(siteName, locName, modelList, idScene, caseStat){
  set2Analyze.RCP45 <-gatherData(siteName, locName, modelList, idScene)
  minData = set2Analyze.RCP45$min
  maxData = set2Analyze.RCP45$max
  idyearStart = year(minData$Date[1]) 
  idyearEnd = year(minData$Date[nrow(minData)])
  Nyears = idyearEnd- idyearStart +1 
  idHist = which(minData$Date<=2005)
  
  if(caseStat=='MEDIAN'){
    minData = minData[, c(1:11, 13)]
    maxData = maxData[, c(1:11, 13)]
  } else if(caseStat=='MEAN') {
    minData = minData[, c(1:12)]
    maxData = maxData[, c(1:12)]
  }
  minData[idHist,2:11] = minData[idHist, 12]
  maxData[idHist,2:11] = maxData[idHist, 12]
  
  avgData = minData
  avgData[,2:12] = (minData[,2:12]+maxData[,2:12])/2
  return(list(min = minData, max = maxData, avg = avgData))
}


plotConfide.mean<-function(locName, cropName, ModelType, genName){
  listFrame = read.csv('ListLocations.csv')
  nameLoc = as.character(listFrame$County)
  modelList = c('ACCESS1',	'CANESM21',	'CAESM1BGC', 'CCSM4',	'CMCC_CMS',	'CNRCCM5',
                'GFDL_CM3',	'HADGECC',	'HADGEES',	'MICRO5')
  numGCM = length(modelList)
  yearHist = '2005'
  vecHist = matrix(NaN, nrow = 150, ncol = 1)
  matrixPorjection = matrix(NaN, nrow = 150, ncol = numGCM)
  for(idGCM in 1:numGCM){
    dir2read = paste('Output/', cropName, '/', locName, modelList[idGCM], sep = '')
    file2read = paste(dir2read, '/', ModelType, 'Year.csv', sep = '')
    dataRead = read.table(file2read, header = TRUE, sep = ',')
    idHistYear = which( dataRead$Year<=yearHist)
    vecHist[1:length(idHistYear)] = dataRead[, genName][1:length(idHistYear)]
    matrixPorjection[(length(idHistYear)+1):150, idGCM] = dataRead[, genName][(length(idHistYear)+1):150]
  }
  colnames(matrixPorjection) <- modelList
  meanVec = matrix(NaN, nrow = 150, ncol = 1)
  sdVec = matrix(NaN, nrow = 150, ncol = 1)
  for(i in 1:150){
    meanVec[i] <- mean(matrixPorjection[i,], na.rm = TRUE)
    sdVec[i] <- sd(matrixPorjection[i,], na.rm = TRUE)
  }
  
  plotData <- data.frame(year = dataRead$Year, Hist = vecHist, mean=meanVec, sd=sdVec)
  eb <- aes(ymax = mean + sd, ymin = mean - sd) #aesthetic
  
  ggplot(data = plotData, aes(x = year)) + 
    geom_line(aes(y = Hist, color='black'), size = 1.5) + 
    geom_line(aes(y = mean, color='red'), linetype = 'solid', size = 1.5) + 
    geom_line(aes(y = mean - sd, color='slateblue'))+ # adding boundarline
    geom_line(aes(y = mean + sd, color='slateblue'))+
    geom_ribbon(aes(ymax = mean + sd, ymin = mean - sd), alpha = 0.2)+
    xlab('Year') + ylab('DOY') +
    ggtitle(genName) + 
    scale_color_manual(name='Legend',
                       values = c('black' = 'black', 'red' = 'red', 'slateblue' = 'slateblue'),
                       labels=c('Observed', 'Average Projected', '95% CI')) + 
    scale_x_continuous(limits = c(min(plotData$year), max(plotData$year)), expand = c(0,0))+
    theme_bw() + 
    theme(panel.grid = element_blank(),
          axis.ticks =  element_blank()) #not to show grid 
  
}

plotConfide.median<-function(locName, cropName, ModelType, genName){
  listFrame = read.csv('ListLocations.csv')
  nameLoc = as.character(listFrame$County)
  modelList = c('ACCESS1',	'CANESM21',	'CAESM1BGC', 'CCSM4',	'CMCC_CMS',	'CNRCCM5',
                'GFDL_CM3',	'HADGECC',	'HADGEES',	'MICRO5')
  numGCM = length(modelList)
  yearHist = '2005'
  vecHist = matrix(NaN, nrow = 150, ncol = 1)
  matrixPorjection = matrix(NaN, nrow = 150, ncol = numGCM)
  for(idGCM in 1:numGCM){
    dir2read = paste('Output/', cropName, '/', locName, modelList[idGCM], sep = '')
    file2read = paste(dir2read, '/', ModelType, 'Year.csv', sep = '')
    dataRead = read.table(file2read, header = TRUE, sep = ',')
    idHistYear = which( dataRead$Year<=yearHist)
    vecHist[1:length(idHistYear)] = dataRead[, genName][1:length(idHistYear)]
    matrixPorjection[(length(idHistYear)+1):150, idGCM] = dataRead[, genName][(length(idHistYear)+1):150]
  }
  colnames(matrixPorjection) <- modelList
  medianVec = matrix(NaN, nrow = 150, ncol = 1)
  meanVec = matrix(NaN, nrow = 150, ncol = 1)
  sdVec = matrix(NaN, nrow = 150, ncol = 1)
  for(i in 1:150){
    medianVec[i] <- median(matrixPorjection[i,], na.rm = TRUE)
    meanVec[i] <- mean(matrixPorjection[i,], na.rm = TRUE)
    sdVec[i] <- sd(matrixPorjection[i,], na.rm = TRUE)
  }
  
  plotData <- data.frame(year = dataRead$Year, Hist = vecHist, mean=meanVec, median=medianVec, sd=sdVec)
  eb <- aes(ymax = mean + sd, ymin = mean - sd) #aesthetic
  
  ggplot(data = plotData, aes(x = year)) + 
    geom_line(aes(y = Hist, color='black'), size = 1.5) + 
    geom_line(aes(y = median, color='red'), linetype = 'solid', size = 1.5) + 
    geom_line(aes(y = mean - sd, color='slateblue'))+ # adding boundarline
    geom_line(aes(y = mean + sd, color='slateblue'))+
    geom_ribbon(aes(ymax = mean + sd, ymin = mean - sd), alpha = 0.2)+
    xlab('Year') + ylab('DOY') +
    ggtitle(genName) + 
    scale_color_manual(name='Legend',
                       values = c('black' = 'black', 'red' = 'red', 'slateblue' = 'slateblue'),
                       labels=c('Observed', 'Median Projected', '95% CI')) + 
    scale_x_continuous(limits = c(min(plotData$year), max(plotData$year)), expand = c(0,0))+
    theme_bw() + 
    theme(panel.grid = element_blank(),
          axis.ticks =  element_blank()) #not to show grid 
  
}

exportResult <- function(locName, cropName, scenType){
  dir2Save = paste('Output/', cropName, '/Graphs', sep = '')
  if (dir.exists(dir2Save)==FALSE){dir.create(dir2Save)}
  dir2Save = paste(dir2Save, '/', locName, sep = '')
  if (dir.exists(dir2Save)==FALSE){dir.create(dir2Save)}
  dir2Save = paste(dir2Save, '/', scenType, sep = '')
  if (dir.exists(dir2Save)==FALSE){dir.create(dir2Save)}
  
  
  plt.first.mean  <- plotConfide.mean(locName = locName, cropName =  cropName, ModelType = scenType, genName  = 'First')
  plt.Second.mean <- plotConfide.mean(locName = locName, cropName =  cropName, ModelType = scenType, genName  = 'Second')
  plt.Third.mean  <- plotConfide.mean(locName = locName, cropName =  cropName, ModelType = scenType, genName  = 'Third')
  plt.fourth.mean <- plotConfide.mean(locName = locName, cropName =  cropName, ModelType = scenType, genName  = 'Fourth')
  plt.fifth.mean  <- plotConfide.mean(locName = locName, cropName =  cropName,  ModelType = scenType, genName  = 'Fifth')
  plt.sixth.mean  <- plotConfide.mean(locName = locName, cropName =  cropName,  ModelType = scenType, genName  = 'Sixth')
  
  file1 = paste(dir2Save, '/1', cropName, 'S', scenType, 'mean.png', sep = '' )
  file2 = paste(dir2Save, '/2', cropName, 'S', scenType, 'mean.png', sep = '' )
  file3 = paste(dir2Save, '/3', cropName, 'S', scenType, 'mean.png', sep = '' )
  file4 = paste(dir2Save, '/4', cropName, 'S', scenType, 'mean.png', sep = '' )
  file5 = paste(dir2Save, '/5', cropName, 'S', scenType, 'mean.png', sep = '' )
  file6 = paste(dir2Save, '/6', cropName, 'S', scenType, 'mean.png', sep = '' )
  fileAll = paste(dir2Save, '/0', cropName, 'S', scenType, 'mean.png', sep = '' )
  ggsave(file1,plot = plt.first.mean, width = 8, height = 4)
  ggsave(file2,plot = plt.Second.mean, width = 8, height = 4)
  ggsave(file3,plot = plt.Third.mean, width = 8, height = 4)
  ggsave(file4,plot = plt.fourth.mean, width = 8, height = 4)
  ggsave(file5,plot = plt.fifth.mean, width = 8, height = 4)
  ggsave(file6,plot = plt.sixth.mean, width = 8, height = 4)
  pltall <- ggarrange(plt.first.mean, plt.Second.mean, plt.Third.mean, 
                      plt.fourth.mean, plt.fifth.mean, plt.sixth.mean, 
                      nrow = 3, ncol = 2, common.legend = TRUE)
  ggsave(fileAll,plot = pltall, width = 15, height = 12)
  
  plt.first.median  <- plotConfide.median(locName = locName, cropName =  cropName, ModelType = scenType, genName  = 'First')
  plt.Second.median <- plotConfide.median(locName = locName, cropName =  cropName, ModelType = scenType, genName  = 'Second')
  plt.Third.median  <- plotConfide.median(locName = locName, cropName =  cropName, ModelType = scenType, genName  = 'Third')
  plt.fourth.median <- plotConfide.median(locName = locName, cropName =  cropName, ModelType = scenType, genName  = 'Fourth')
  plt.fifth.median  <- plotConfide.median(locName = locName, cropName =  cropName,  ModelType = scenType, genName  = 'Fifth')
  plt.sixth.median  <- plotConfide.median(locName = locName, cropName =  cropName,  ModelType = scenType, genName  = 'Sixth')
  
  file1 = paste(dir2Save, '/1', cropName, 'S', scenType, 'median.png', sep = '' )
  file2 = paste(dir2Save, '/2', cropName, 'S', scenType, 'median.png', sep = '' )
  file3 = paste(dir2Save, '/3', cropName, 'S', scenType, 'median.png', sep = '' )
  file4 = paste(dir2Save, '/4', cropName, 'S', scenType, 'median.png', sep = '' )
  file5 = paste(dir2Save, '/5', cropName, 'S', scenType, 'median.png', sep = '' )
  file6 = paste(dir2Save, '/6', cropName, 'S', scenType, 'median.png', sep = '' )
  fileAll = paste(dir2Save, '/0', cropName, 'S', scenType, 'median.png', sep = '' )
  ggsave(file1,plot = plt.first.median, width = 8, height = 4)
  ggsave(file2,plot = plt.Second.median, width = 8, height = 4)
  ggsave(file3,plot = plt.Third.median, width = 8, height = 4)
  ggsave(file4,plot = plt.fourth.median, width = 8, height = 4)
  ggsave(file5,plot = plt.fifth.median, width = 8, height = 4)
  ggsave(file6,plot = plt.sixth.median, width = 8, height = 4)
  pltall <- ggarrange(plt.first.median, plt.Second.median, plt.Third.median, 
                      plt.fourth.median, plt.fifth.median, plt.sixth.median, 
                      nrow = 3, ncol = 2, common.legend = TRUE)
  ggsave(fileAll,plot = pltall, width = 15, height = 12)
  
}
rowMedians<-function(dataSet){
  nRecords = nrow(dataSet)
  dataSet = as.matrix(dataSet)
  medianVec = vector(length = nRecords)
  for (i in 1:nRecords) {
    medianVec[i] = median(dataSet[i,], na.rm = TRUE)
  }
  return(medianVec)
}
plotSeries<-function(dataSet, cycleName){
  dd = melt(dataSet, id=c("Year"))
  fig<-ggplot(dd) + geom_line(aes(x=Year, y=value, colour=variable)) +
    labs(title = cycleName,x='Year', y = 'DOY', color = 'Legend\n')+
    scale_colour_manual(name='Legend', values=c('black', 'cyan', 'blue', 'orange', 'yellow', 'green', 
                                                'red', 'brown', 'gray', 
                                                'tomato', 'salmon', 'black', 'magenta')) +
    theme_bw() 
  return(fig)
}
interpretSet<-function(locName, modelList, cropName, idscen){
  dataSet1 = data.frame(matrix(NaN, nrow=150, ncol=(1+length(modelList))))
  dataSet2 = data.frame(matrix(NaN, nrow=150, ncol=(1+length(modelList))))
  dataSet3 = data.frame(matrix(NaN, nrow=150, ncol=(1+length(modelList))))
  dataSet4 = data.frame(matrix(NaN, nrow=150, ncol=(1+length(modelList))))
  dataSet5 = data.frame(matrix(NaN, nrow=150, ncol=(1+length(modelList))))
  dataSet6 = data.frame(matrix(NaN, nrow=150, ncol=(1+length(modelList))))
  colnames(dataSet1) = c('Year', modelList)
  colnames(dataSet2) = c('Year', modelList)
  colnames(dataSet3) = c('Year', modelList)
  colnames(dataSet4) = c('Year', modelList)
  colnames(dataSet5) = c('Year', modelList)
  colnames(dataSet6) = c('Year', modelList)
  if(idscen==1){
    fileName2Read1 = 'RCP45Year'
  }else{
    fileName2Read1 = 'RCP85Year'
  }
  for(idModel in 1:length(modelList)){
    siteName = paste(locName, modelList[idModel], sep = '')
    dir2Read = paste('Output/', cropName, '/', siteName, '/', sep = '')
    fileName2Read = paste(dir2Read, '/', fileName2Read1, '.csv', sep = '')
    dataRCP = read.csv(fileName2Read)
    dataSet1[,1] = dataRCP$Year
    dataSet2[,1] = dataRCP$Year
    dataSet3[,1] = dataRCP$Year
    dataSet4[,1] = dataRCP$Year
    dataSet5[,1] = dataRCP$Year
    dataSet6[,1] = dataRCP$Year
    dataSet1[,idModel+1] = dataRCP$First
    dataSet2[,idModel+1] = dataRCP$Second
    dataSet3[,idModel+1] = dataRCP$Third
    dataSet4[,idModel+1] = dataRCP$Fourth
    dataSet5[,idModel+1] = dataRCP$Fifth
    dataSet6[,idModel+1] = dataRCP$Sixth
  }
  idHist = which(dataSet1$Year>2005)
  dataSet1$avgHist=rowMeans(dataSet1[, modelList])
  dataSet1$medHist=rowMedians(dataSet1[, modelList])
  dataSet1$avgHist[idHist] = NaN
  dataSet1$medHist[idHist] = NaN
  dataSet2$avgHist=rowMeans(dataSet2[, modelList])
  dataSet2$medHist=rowMedians(dataSet2[, modelList])
  dataSet2$avgHist[idHist] = NaN
  dataSet2$medHist[idHist] = NaN
  dataSet3$avgHist=rowMeans(dataSet3[, modelList])
  dataSet3$avgHist=rowMedians(dataSet3[, modelList])
  dataSet3$avgHist[idHist] = NaN
  dataSet3$medHist[idHist] = NaN
  dataSet4$avgHist=rowMeans(dataSet4[, modelList])
  dataSet4$medHist=rowMedians(dataSet4[, modelList])
  dataSet4$avgHist[idHist] = NaN
  dataSet4$medHist[idHist] = NaN
  dataSet5$avgHist=rowMeans(dataSet5[, modelList])
  dataSet5$medHist=rowMedians(dataSet5[, modelList])
  dataSet5$avgHist[idHist] = NaN  
  dataSet5$medHist[idHist] = NaN
  dataSet6$avgHist=rowMeans(dataSet5[, modelList])
  dataSet6$medHist=rowMedians(dataSet6[, modelList])
  dataSet6$avgHist[idHist] = NaN  
  dataSet6$medHist[idHist] = NaN
  idProj = which(dataSet1$Year<=2005)
  dataSet1[idProj, modelList]=NaN
  dataSet2[idProj, modelList]=NaN
  dataSet3[idProj, modelList]=NaN
  dataSet4[idProj, modelList]=NaN
  dataSet5[idProj, modelList]=NaN
  dataSet6[idProj, modelList]=NaN
  dataSet1$avgProj =rowMeans(dataSet1[, modelList], na.rm = TRUE)
  dataSet1$medProj =rowMedians(dataSet1[, modelList])
  dataSet2$avgProj =rowMeans(dataSet2[, modelList], na.rm = TRUE)
  dataSet2$medProj =rowMedians(dataSet2[, modelList])
  dataSet3$avgProj =rowMeans(dataSet3[,modelList], na.rm = TRUE)
  dataSet3$medProj =rowMedians(dataSet3[, modelList])
  dataSet4$avgProj =rowMeans(dataSet4[, modelList], na.rm = TRUE)
  dataSet4$medProj =rowMedians(dataSet4[, modelList])
  dataSet5$avgProj =rowMeans(dataSet5[, modelList], na.rm = TRUE)
  dataSet5$medProj =rowMedians(dataSet5[, modelList])
  dataSet6$avgProj =rowMeans(dataSet6[, modelList], na.rm = TRUE)
  dataSet6$medProj =rowMedians(dataSet6[, modelList])
  
  
  dir2Save = paste('Output/', cropName, '/', locName, fileName2Read1, sep = '')
  dir.create(dir2Save)
  fileName2Image = paste(dir2Save, '/', fileName2Read1, 'Figure1.png', sep = '')
  newDataSet1 = ggSet(dataSet1)
  Figure1<-plotSeries(newDataSet1, cycleName  = 'First Cycle')
  ggsave(fileName2Image, plot = Figure1, width = 8, height = 4, units = 'in', dpi = 300)
  fileName2Image = paste(dir2Save, '/', fileName2Read1, 'Figure2.png', sep = '')
  newDataSet2 = ggSet(dataSet2)
  Figure2<-plotSeries(newDataSet2, cycleName  = 'Second Cycle')
  ggsave(fileName2Image, plot = Figure2, width = 8, height = 4, units = 'in', dpi = 300)
  fileName2Image = paste(dir2Save, '/', fileName2Read1, 'Figure3.png', sep = '')
  newDataSet3 = ggSet(dataSet3)
  Figure3<-plotSeries(newDataSet3, cycleName  = 'Thirdd Cycle')
  ggsave(fileName2Image, plot = Figure3, width = 8, height = 4, units = 'in', dpi = 300)
  fileName2Image = paste(dir2Save, '/', fileName2Read1, 'Figure4.png', sep = '')
  newDataSet4 = ggSet(dataSet4)
  Figure4<-plotSeries(newDataSet4, cycleName  = 'Fourth Cycle')
  ggsave(fileName2Image, plot = Figure4, width = 8, height = 4, units = 'in', dpi = 300)
  fileName2Image = paste(dir2Save, '/', fileName2Read1, 'Figure5.png', sep = '')
  newDataSet5 = ggSet(dataSet5)
  Figure5<-plotSeries(newDataSet5, cycleName  = 'Fifth Cycle')
  ggsave(fileName2Image, plot = Figure5, width = 8, height = 4, units = 'in', dpi = 300)
  newDataSet6 = ggSet(dataSet6)
  fileName2Image = paste(dir2Save, '/', fileName2Read1, 'Figure6.png', sep = '')
  Figure6<-plotSeries(newDataSet6, cycleName  = 'Sixth Cycle')
  ggsave(fileName2Image, plot = Figure6, width = 8, height = 4, units = 'in', dpi = 300) 
  return(list(dataFirst = newDataSet1, dataSecond = newDataSet2, 
              dataThird = newDataSet3, dataFourth = newDataSet4, dataFifth = newDataSet5, dataSixth = newDataSet6, 
              Fig1 = Figure1, Fig2 = Figure2, Fig3 = Figure3, Fig4 = Figure4, Fig5 = Figure5, Fig6 = Figure6))
  
}

ggSet <- function(dataSet){
  newDataSet = data.frame(Year = dataSet$Year, HISTORICAL = dataSet$avgHist, 
                          ACCESS1 = dataSet$ACCESS1, CANESM21 = dataSet$CANESM21,
                          CAESM1BGC = dataSet$CAESM1BGC, CCSM4 = dataSet$CCSM4,   
                          CMCC_CMS = dataSet$CMCC_CMS,  CNRCCM5 = dataSet$CNRCCM5,  
                          GFDL_CM3 = dataSet$GFDL_CM3, HADGECC = dataSet$HADGECC,  
                          HADGEES = dataSet$HADGEES,  MICRO5 = dataSet$MICRO5, 
                          MEDIAN = dataSet$medProj, AVERAGE = dataSet$avgProj)
return(newDataSet)
}

getStats <- function(X, Y){
  if(length(which(is.nan(Y))) ==length(Y)){
    statVal = c(NaN, NaN, NaN, NaN, NaN, NaN, NaN)
  } else{
    miN = min(Y, na.rm = TRUE)
    maX = max(Y, na.rm = TRUE)
    mu = mean(Y, na.rm = TRUE)
    std= sd(Y, na.rm = TRUE)
    med = round(median(Y, na.rm = TRUE))
    set.na = detectNA(X, Y)
    X.na = set.na$X
    Y.na = set.na$Y
    if (length(Y.na)!=0){
      modfit = lm(Y.na~X.na)
      slope = modfit$coefficients[2]
      intercept = modfit$coefficients[1]
    } else{
      modfit = NaN
      slope = NaN
      intercept = NaN
    }
    
    statVal = c(miN, maX, mu, std, med, slope, intercept)
    
  }
  
  return(statVal)
}
detectNA <- function(X, Y){
  idNNA <- which(is.na(Y)==FALSE & is.nan(Y)==FALSE )
  X = X[idNNA]
  Y = Y[idNNA]
  return(list(X=X, Y=Y))
}
getStatSummary <- function(df.Input){
  statsArray = data.frame(matrix(NaN, nrow = 7, ncol = (ncol(df.Input)-1) ))
  colnames(statsArray) = colnames(df.Input)[2:ncol(df.Input)]
  rownames(statsArray) = c('Minimum', 'Maximum', 'Mean', 'Std', 'Median', 'Slope', 'Intercept')
  statsArray[, 1] = getStats(df.Input$Year, df.Input$HISTORICAL)
  statsArray[, 2] = getStats(df.Input$Year, df.Input$ACCESS1)
  statsArray[, 3] = getStats(df.Input$Year, df.Input$CANESM21)
  statsArray[, 4] = getStats(df.Input$Year, df.Input$CAESM1BGC)
  statsArray[, 5] = getStats(df.Input$Year, df.Input$CCSM4)
  statsArray[, 6] = getStats(df.Input$Year, df.Input$CMCC_CMS)
  statsArray[, 7] = getStats(df.Input$Year, df.Input$CNRCCM5)
  statsArray[, 8] = getStats(df.Input$Year, df.Input$GFDL_CM3)
  statsArray[, 9] = getStats(df.Input$Year, df.Input$HADGECC)
  statsArray[, 10] = getStats(df.Input$Year, df.Input$HADGEES)
  statsArray[, 11] = getStats(df.Input$Year, df.Input$MICRO5)
  statsArray[, 12] = getStats(df.Input$Year, df.Input$MEDIAN)
  statsArray[, 13] = getStats(df.Input$Year, df.Input$AVERAGE)
  return(statsArray)
}

