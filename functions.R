# continous2categorical function. This function takes a data frame of continous variables and 
# converts to a data frame of categorical variables. The last variable is the response variable.

continous2categorical <- function(x){
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



#creating contingency matrix for categorical data analysis
count_mat<- function(df){
  
  #Takes in a nx2 data frame
  df_dim<- dim(df) #dimensions of the data frame
  if (df_dim[2] == 2 && length(df_dim)==2){
    factor_df1 <- as.factor(df[,1])
    factor_df2 <- as.factor(df[,2])
    # converting the df columns into factor and storing in the variable factor_df
    lev_col1 = levels(factor_df1)
    lev_col2 = levels(factor_df2)
    len_col1 = length(lev_col1) #length of the first column
    len_col2 = length(lev_col2) #length of the second column
    
    
    val = 1
    for (i in lev_col1){
      for(j in lev_col2){
        val = c(val,length(which(df[,1]==i & df[,2]==j)))
      }
    }
    out = matrix(val[-1], byrow = TRUE, nrow = length(lev_col1), dimnames = list(lev_col1,lev_col2) )
  }
  else (out = "check dimension")
  return (out) 
}

#testing for independence between two categorical variable 
chisq.indep <- function (m, level = 0.05, digits = 4, print = TRUE) 
{
  r.sum <- rowSums(m)
  c.sum <- colSums(m)
  n <- sum(m)
  exp.ct <- outer(r.sum, c.sum, "*")/n
  res <- m - exp.ct
  p.res <- res/sqrt(exp.ct)
  X.sq <- sum(p.res^2)
  G.sq <- 2 * sum(m * (log(m) - log(exp.ct)))
  df <- (nrow(m) - 1) * (ncol(m) - 1)
  c.val <- qchisq(level, df = df, lower.tail = FALSE)
  est.se <- sqrt(exp.ct * outer((1 - r.sum/n), (1 - c.sum/n), 
                                "*"))
  s.res <- res/est.se
  if (print) {
    cat("Chi-squared test of independence\n")
    cat("  Level = ", level, ", df = ", df, ", critical value = ", 
        round(c.val, digits), "\n", sep = "")
    cat("  X-squared = ", round(X.sq, digits), "\n", sep = "")
    cat("  G-squared = ", round(G.sq, digits), sep = "")
    if(X.sq > c.val | G.sq > c.val){
      cat("\n", sep = "","The test statistic value is greater than critical value. We reject the null hypothesis and conclude that the two variable are not independent")
    } else {
      cat("\n", sep = "","The test statistic value is less than critical value. We fail to reject the null hypothesis and conclude that the two variable are independent")
    }
  }
  
}
#creating a table with local or global odds ratios 
odds.ratios <- function(m, type = "local") {
  nr <- nrow(m)
  if (nr < 2) stop("number of rows is less than two")
  nc <- ncol(m)
  if (nc < 2) stop("number of columns is less than two")
  if (length(type) > 1) stop("only one type is allowed")
  
  opts <- c("local", "global")
  type <- pmatch(type, opts)
  if (is.na(type)) stop("only \"local\" or \"global\" allowed for type")
  
  result <- matrix(NA, nrow = nr - 1, ncol = nc - 1)
  
  if (type == 1)
    for (i in 1:(nr - 1))
      for (j in 1:(nc - 1))
        result[i, j] <- m[i, j] * m[i + 1, j + 1] / (m[i, j + 1] * m[i + 1, j])
  
  if (type == 2)
    for (i in 1:(nr - 1))
      for (j in 1:(nc - 1)) {
        num <- as.numeric(sum(m[1:i, 1:j])) * as.numeric(sum(m[(i+1):nr, (j+1):nc]))
        den <- as.numeric(sum(m[1:i, (j+1):nc])) * as.numeric(sum(m[(i+1):nr, 1:j]))
        result[i, j] <- num / den
      }
  
  result
}

#plotting fourfold plots for odds ratios 
plotlocalor <- function(m, col = c("azure4", "aquamarine4")) {
  nr <- nrow(m)
  if (nr < 2) stop("number of rows is less than two")
  nc <- ncol(m)
  if (nc < 2) stop("number of columns is less than two")
  
  par(mfrow = c(nr - 1, nc - 1), mar=c(0,0,0,0))
  for (i in 1:(nr - 1))
    for (j in 1:(nc - 1)) { 
      fourfoldplot(m[i:(i+1), j:(j+1)], color = col)
    }
}

# catbarchart plots each variable against the response variable. 
# The last variable is the response variable.

catbarchart <- function(x){
  
  xcolumnnames <- colnames(x)
  responsecol <- ncol(x)
  
  
  plot_hist <- function (column, data, response) ggplot(data, aes(x=get(column), ..count..)) +geom_bar(aes(fill=get(response)), position="dodge") + xlab(column) + scale_fill_discrete(name=response)
  
  myplots <- lapply(colnames(x), plot_hist, data = x, response=xcolumnnames[responsecol])
  myplots <- myplots[-length(myplots)]
  
  grid.arrange(grobs = myplots, ncol=1)   
  
}

