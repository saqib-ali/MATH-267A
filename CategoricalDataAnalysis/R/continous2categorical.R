continous2categorical <-
function(x){
  numberoffactors <- ncol(x)-1
  out <- data.frame(0,matrix(nrow=nrow(x),ncol=1))
  for (i in 1:numberoffactors){
    
    
    
    labs <- c("low", "low-medium", "medium", "medium-high", "high")
    vartemp <- cut(x[,i], breaks = 5, labels = labs)
    
    
    out[i]<-vartemp
    
  }
  i<- i+1
  out[i] <- x[i]
  colnames(out)<-colnames(x)
  return(data.frame(out))
}
