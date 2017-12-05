\name{catbarchart}
\alias{catbarchart}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Plot Barchart for Categorical Data
}
\description{
  This function plot a Barchart for Categorical Data. The Dataframe must have Categorical Data. The Last Column of the Data Frame must be the Response Variable


}
\usage{
catbarchart(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{A Dataframe with Categorical Data. Last Column is the Response Variable}
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
Saqib Ali
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{


# catbarchart create Barchart of Categorical Data. The last colmn of the Dataset should be the Response Variable. All variables should be Categorical Data. Use the continous2categorical function to conver Continous Data to Categorical Data

data("crabs2")
head(crabs2)
catbarchart(continous2categorical(crabs2))



## The function is currently defined as
function (x) 
{
    xcolumnnames <- colnames(x)
    responsecol <- ncol(x)
    plot_hist <- function(column, data, response) ggplot(data, 
        aes(x = get(column), ..count..)) + geom_bar(aes(fill = get(response)), 
        position = "dodge") + xlab(column) + scale_fill_discrete(name = response)
    myplots <- lapply(colnames(x), plot_hist, data = x, response = xcolumnnames[responsecol])
    myplots <- myplots[-length(myplots)]
    grid.arrange(grobs = myplots, ncol = 1)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
