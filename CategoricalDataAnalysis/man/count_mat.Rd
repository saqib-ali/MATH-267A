\name{count_mat}
\alias{count_mat}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
creating contingency matrix for categorical data analysis
}
\description{
Takes a data frame of at least two observations of two categorical variables 
  and returns a contingency table of the data 
}
\usage{
count_mat(df)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df}{
df is a data frame with dimentions nx2 of two categorical variablles. 
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
  \item{m}{a contingency matrix of numerical values with dimension kxn where k is the number of categories in the first variable and n is the number of categories in the second variable  }  
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Maham Niaz
}
\note{
works similar to the table() function
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{


  #create vector 1 with three levels
  a = c("A","A","B","A", "B","B","C","A","C","B")
  #create vector 2 with 4 levels
  b = c(1,2,1,4,1,2,2,3,4,3)
  # create dataframe with a and b vectors as columns
  df = cbind(a,b)
  #return count matrix 
  m = count_mat(df)
  m
  
## The function is currently defined as
function (df) 
{
    df_dim <- dim(df)
    if (df_dim[2] == 2 && length(df_dim) == 2) {
        factor_df1 <- as.factor(df[, 1])
        factor_df2 <- as.factor(df[, 2])
        lev_col1 = levels(factor_df1)
        lev_col2 = levels(factor_df2)
        len_col1 = length(lev_col1)
        len_col2 = length(lev_col2)
        val = 1
        for (i in lev_col1) {
            for (j in lev_col2) {
                val = c(val, length(which(df[, 1] == i & df[, 
                  2] == j)))
            }
        }
        out = matrix(val[-1], byrow = TRUE, nrow = length(lev_col1), 
            dimnames = list(lev_col1, lev_col2))
    }
    else (out = "check dimension")
    return(out)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
