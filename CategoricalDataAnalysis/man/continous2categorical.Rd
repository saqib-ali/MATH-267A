\name{continous2categorical}
\alias{continous2categorical}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  continous2categorical function.
}
\description{

continous2categorical function. This function takes a data frame of continous variables and converts to a data frame of categorical variables. The last variable is the response variable.

}
\usage{
continous2categorical(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{A dataframe with Continous Variables for Factors. Last column is the Response Variable}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A Dataframe with the Categorical variables. Last colums is the Response variable
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

data("crabs2")
continous2categorical(crabs2)

## The function is currently defined as
function (x) 
{
    numberoffactors <- ncol(x) - 1
    out <- data.frame(0, matrix(nrow = nrow(x), ncol = 1))
    for (i in 1:numberoffactors) {
        labs <- c("low", "low-medium", "medium", "medium-high", 
            "high")
        vartemp <- cut(x[, i], breaks = 5, labels = labs)
        out[i] <- vartemp
    }
    i <- i + 1
    out[i] <- x[i]
    colnames(out) <- colnames(x)
    return(data.frame(out))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
