\name{chisq.indep}
\alias{chisq.indep}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  testing for independence between two categorical variable  
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
chisq.indep(m, level = 0.05, digits = 4, print = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{m}{
%%     ~~Describe \code{m} here~~
}
  \item{level}{
%%     ~~Describe \code{level} here~~
}
  \item{digits}{
%%     ~~Describe \code{digits} here~~
}
  \item{print}{
%%     ~~Describe \code{print} here~~
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
function (m, level = 0.05, digits = 4, print = TRUE) 
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
    }
    invisible(list(X.sq = X.sq, df = df, expected = exp.ct, pearson.res = p.res, 
        std.res = s.res))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
