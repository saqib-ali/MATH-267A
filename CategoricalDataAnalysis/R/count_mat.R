count_mat <-
function(df){
  
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
