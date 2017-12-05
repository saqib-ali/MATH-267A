\name{chisq.indep}
\alias{chisq.indep}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Testing for independence between two categorical variable  
}
\description{
This function takes in contingency matrix and tests for Chi Squared Independence. The function returns the two test statistics. 
  X^2 and G^2, which is Pearson test statistic and Likelihood Ratio test statistic respectively.    
}
\usage{
chisq.indep(m, level = 0.05, digits = 4, print = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{m}{
m is an at least two by two matrix or the contingency matrix. Preferably with rows corresponding to explanatory variable
 and coulmns corresponding to response variable.  
}
  \item{level}{
level is the significance level of the test. 
    The null hypothesis is rejected if the p-value is less than a predetermined level, alpha. 
    alpha is called the significance level, and is the probability of rejecting the null hypothesis given that it is true (a type I error). 
    The default value is set to 0.05. 
}
  \item{digits}{
integer indicating the number of decimal places or significant digits to be used. The default is set to 4.  
}
  \item{print}{
Default is set to TRUE. If print is set to TRUE the output of the test gets printed. 
    If you do not want to see the output, set it equal to FALSE.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
\item{}{If print is set to TRUE returns the value of level, degree of freedom, critical value rounded to the neares digit, value of pearson statistic and value of likelihood ratio test statistic}
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

#attach dataset crabs
  data("crabs2")
  #create a contingency matrix for crabs color and satelite 
  m = table(crabs2$color, crabs2$satellite)
  # returns chi squared test of independence for the two variable, color of the crab and satelite status, which is either TRUE or FALSE 
  chisq.indep(m)
  
  
  
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
    if(X.sq > c.val | G.sq > c.val){
      cat("\n", sep = "","The test statistic value is greater than critical value. We reject the null hypothesis and conclude that the two variable are not independent")
    } else {
      cat("\n", sep = "","The test statistic value is less than critical value. We fail to reject the null hypothesis and conclude that the two variable are independent")
    }
  }
  
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
