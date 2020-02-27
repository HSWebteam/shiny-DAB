library(eeptools)
library(gamlss)

percetielPlot <- function(storedData, percentieScore) {
  hist(as.numeric(
    storedData[,2]), 
    main='De verdeling populatie 6-12 jarige',
    xlab='Score',
    ylab='kansdichtheid')
  abline(v=percentieScore/100,col="red")
}

densityScoreAgePlot <- function(data){
  plot(data$a, data$b, type='l', 
       ylim=c(0.01, max(data$b)),
       main='De verdeling gegeven leeftijd in dagen', 
       xlab='de log goed-fout verhouding', 
       ylab='kansdichtheid', 
       las=1)
  abline(v=data$abline,col='red')
}

growthPlot<-function(model, data, ageDays, meanprop){
  if(is.null(meanprop)) {
    return(FALSE)
  }

  centiles(model, data$lftd/365, legend=FALSE, cent=c(5,10,25,50,75,90,95),
    xlim=c(6,13), xaxp=c(6,13,7), col.centiles=c(1,3,4,5,6,7,'orange'), 
    lwd.centiles=1.5, xlab='Leeftijd in jaren', ylab='Log goed-fout verhouding',
    main='Percentielcurves', points=F)
  abline(h = log(meanprop/(1-meanprop)), col = 'lightgray')
  abline(v = ageDays/365, col = 'lightgray')
  legend(6, 6.5, legend = c('95','90','75','50','25','10','5'),
    col = c('orange',7,6,5,4,3,1), lwd=1.5, bty='o', bg='white')
  points(ageDays/365, log(meanprop/(1-meanprop)), pch=19, col="red")
}
