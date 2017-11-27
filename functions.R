
continous2categorical <- function(x){
  numberoffactors <- ncol(x)-1
  out <- data.frame(0,matrix(nrow=nrow(x),ncol=1))
  for (i in 1:numberoffactors){
    
    
    varmin <- min(x[i])
    varmax <- max(x[i])
    varstep <- (varmax-varmin)/5
    
    vartemp <- as.character(cut(as.numeric(x[,i]), seq(varmin, varmax, varstep)))
    
    out[i]<-vartemp
    
  }
  i<- i+1
  out[i] <- x[i]
  colnames(out)<-colnames(x)
  return(data.frame(out))
}
debug(continous2categorical)