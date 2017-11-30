catbarchart <-
function(x){
  
  xcolumnnames <- colnames(x)
  responsecol <- ncol(x)
  
  
  plot_hist <- function (column, data, response) ggplot(data, aes(x=get(column), ..count..)) +geom_bar(aes(fill=get(response)), position="dodge") + xlab(column) + scale_fill_discrete(name=response)
  
  myplots <- lapply(colnames(x), plot_hist, data = x, response=xcolumnnames[responsecol])
  myplots <- myplots[-length(myplots)]
  
  grid.arrange(grobs = myplots, ncol=1)   
  
}
