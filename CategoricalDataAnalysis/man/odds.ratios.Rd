\name{odds.ratios}
\alias{odds.ratios}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
 creating a table with local or global odds ratios 
}
\description{
This function takes in a contingency table and returns local or global odds ratios for all the subtables formed from the table. 
 The function gives out the odds ratios in the form of n-1 by m-1 matrix.
}
\usage{
odds.ratios(m, type = "local")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{m}{
The two dimentional contingency table for which all the local and global odds ratios are required
}
  \item{type}{
the type of odds ratios required. This argument can take values local or global only. The default is set to local.
   Note that global odds ratios make sense for ordinal data for both variables. 
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
 \item{result}{a matrix of odds ratios is returned. The dimentions  of the matrix are n-1 by k-1 where n and k are the number of rows and columns of contingency table m}
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
 #attaching dataset crabs2
 data("crabs2")
 # create contingency matrix for variable spine and satelite 
 m = table(crabs2$spine, crabs2$satellite)
 or1 = odds.ratios(m, "global")
 or1 #gives matrix for global odds ratios
 or2 = odds.ratios(m)
 or2 #gives matrix for local odds ratios

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
