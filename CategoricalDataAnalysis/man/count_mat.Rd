\name{count_mat}
\alias{count_mat}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
creating contingency matrix for categorical data analysis
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
count_mat(df)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df}{
%%     ~~Describe \code{df} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
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
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

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
