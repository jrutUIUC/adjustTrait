adjustTrait<- function(data=sub, traitName= "Grain.yield...kg.ha.CO_321.0001218"){
  m0 <- SpATS(response = traitName, spatial = ~ SAP(colNumber, rowNumber, nseg = c(10,20)), 
              genotype = "germplasmName", fixed = ~ blockNumber, data = data, control = list(tolerance = 1e-03))
  spat.trend.1 <- obtain.spatialtrend(m0, grid=c(length(c(min(sub$colNumber):max(sub$colNumber))), length(c(min(sub$rowNumber):max(sub$rowNumber))))) 
  rowMat<- matrix(rep(min(sub$rowNumber):max(sub$rowNumber), length(c(min(sub$colNumber):max(sub$colNumber)))), ncol=length(c(min(sub$colNumber):max(sub$colNumber))))
  colMat<- matrix(rep(min(sub$colNumber):max(sub$colNumber), 
                      length(c(min(sub$rowNumber):max(sub$rowNumber)))), 
                  ncol=length(c(min(sub$colNumber):max(sub$colNumber))), byrow=TRUE)
  df<- data.frame(colNumber= as.vector(colMat), rowNumber= as.vector(rowMat), 
                  spatial=as.vector(spat.trend.1$fit))
  sub2<- merge(data, df, by=c('colNumber', 'rowNumber'), all.x=TRUE)
  sub2$newtrait<- sub2[,traitName]- sub2$spatial
  colnames(sub2)[which(colnames(sub2)=='newtrait')]<- paste('Adj.', traitName, sep='')
  sub2<- sub2[,-which(colnames(sub2)=='spatial')]
  return(sub2)
}

