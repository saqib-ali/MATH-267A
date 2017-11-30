\name{odds.ratios}
\alias{odds.ratios}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
 creating a table with local or global odds ratios 
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
odds.ratios(m, type = "local")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{m}{
%%     ~~Describe \code{m} here~~
}
  \item{type}{
%%     ~~Describe \code{type} here~~
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
function (m, type = "local") 
{
    nr <- nrow(m)
    if (nr < 2) 
        stop("number of rows is less than two")
    nc <- ncol(m)
    if (nc < 2) 
        stop("number of columns is less than two")
    if (length(type) > 1) 
        stop("only one type is allowed")
    opts <- c("local", "global")
    type <- pmatch(type, opts)
    if (is.na(type)) 
        stop("only \"local\" or \"global\" allowed for type")
    result <- matrix(NA, nrow = nr - 1, ncol = nc - 1)
    if (type == 1) 
        for (i in 1:(nr - 1)) for (j in 1:(nc - 1)) result[i, 
            j] <- m[i, j] * m[i + 1, j + 1]/(m[i, j + 1] * m[i + 
            1, j])
    if (type == 2) 
        for (i in 1:(nr - 1)) for (j in 1:(nc - 1)) {
            num <- as.numeric(sum(m[1:i, 1:j])) * as.numeric(sum(m[(i + 
                1):nr, (j + 1):nc]))
            den <- as.numeric(sum(m[1:i, (j + 1):nc])) * as.numeric(sum(m[(i + 
                1):nr, 1:j]))
            result[i, j] <- num/den
        }
    result
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
