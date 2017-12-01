\name{plot.local.or}
\alias{plot.local.or}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
plotting fourfold plots for odds ratios 
}
\description{
This function takes in a contingency table and returns (k-1)(l-1) fourfold plots for odds ratio of all 2x2 subtables in the matrix 
  m with dimentions kxl 
}
\usage{
plot.local.or(m, col = c("azure4", "aquamarine4"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{m}{
a two dimentional contingency matrix
}
  \item{col}{
The color of the four-fold plots. The default is azure4 and aquamarine4. The second color corresponds to the 
    variable with higher odds of success.
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

  #create 2x2 matrix
  m = matrix(c(1,5,13,6))
  plot.local.or(m) # returns a single plot shpwing descriptive summary of odds ratio
  
  #create 4x4 matrix
  m = matrix(c(1,5,13,6,3,5,14,16,36,45,4,6,5,8,9,56), nrow = 4)
  plot.local.or(m) # returns 3x3 plots for the odds ratios of 2x2 subtables in the m matrix
  
## The function is currently defined as
function (m, col = c("azure4", "aquamarine4")) 
{
    nr <- nrow(m)
    if (nr < 2) 
        stop("number of rows is less than two")
    nc <- ncol(m)
    if (nc < 2) 
        stop("number of columns is less than two")
    par(mfrow = c(nr - 1, nc - 1))
    for (i in 1:(nr - 1)) for (j in 1:(nc - 1)) {
        fourfoldplot(m[i:(i + 1), j:(j + 1)], color = col)
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
