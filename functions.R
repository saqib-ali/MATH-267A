# created continous2categorical function. This function takes a data frame of continous variables and converts to a data frame of categorical variables. The variable is the response variable.

continous2categorical <- function(x){
  numberoffactors <- ncol(x)-1
  out <- data.frame(0,matrix(nrow=nrow(x),ncol=1))
  for (i in 1:numberoffactors){
    
    
    varmin <- min(x[i])
    varmax <- max(x[i])
    varstep <- (varmax-varmin)/5
    
    labs <- c("low", "low-medium", "medium", "medium-high", "high")
    #vartemp <- as.character(cut(x[,i], seq(varmin, varmax, varstep)))
    vartemp <- cut(x[,i], breaks = 5, labels = labs)
    
    
    out[i]<-vartemp
    
  }
  i<- i+1
  out[i] <- x[i]
  colnames(out)<-colnames(x)
  return(data.frame(out))
}
