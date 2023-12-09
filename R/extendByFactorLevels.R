#df <- data.frame(a=c(1,2), b=c(3,4), c=factor(c('a','b'), levels = c('a','b')), d=factor(c('c','d'), levels = c('c','d')), e=c('ahoj', 'nazdar'), data=as.Date(c('2020-01-01', '2020-01-02')))

extendByFactorLevels <- function(df) {
  factors <- unlist(lapply(colnames(df), function(x) { is.factor(df[[x]]) }))
  row <- df[1,]
  result <- df
  for(i in which(factors)) {
    for(level in levels(df[,i])) {
      newrow <- row
      newrow[i] <- level
      result <- rbind(result, newrow)
    }
  }
  #print(dim(df))
  #print(dim(result))
  return(result)
}