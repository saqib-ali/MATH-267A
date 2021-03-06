pkgname <- "CategoricalDataAnalysis"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
base::assign(".ExTimings", "CategoricalDataAnalysis-Ex.timings", pos = 'CheckExEnv')
base::cat("name\tuser\tsystem\telapsed\n", file=base::get(".ExTimings", pos = 'CheckExEnv'))
base::assign(".format_ptime",
function(x) {
  if(!is.na(x[4L])) x[1L] <- x[1L] + x[4L]
  if(!is.na(x[5L])) x[2L] <- x[2L] + x[5L]
  options(OutDec = '.')
  format(x[1L:3L], digits = 7L)
},
pos = 'CheckExEnv')

### * </HEADER>
library('CategoricalDataAnalysis')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
cleanEx()
nameEx("CategoricalDataAnalysis-package")
### * CategoricalDataAnalysis-package

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: CategoricalDataAnalysis-package
### Title: Categorical Data Analysis
### Aliases: CategoricalDataAnalysis-package CategoricalDataAnalysis
### Keywords: package

### ** Examples


data("crabs2")
catbarchart(continous2categorical(crabs2))




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("CategoricalDataAnalysis-package", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("catbarchart")
### * catbarchart

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: catbarchart
### Title: Plot Barchart for Categorical Data
### Aliases: catbarchart
### Keywords: ~kwd1 ~kwd2

### ** Examples



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



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("catbarchart", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("chisq.indep")
### * chisq.indep

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: chisq.indep
### Title: Testing for independence between two categorical variable
### Aliases: chisq.indep
### Keywords: ~kwd1 ~kwd2

### ** Examples


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



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("chisq.indep", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("continous2categorical")
### * continous2categorical

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: continous2categorical
### Title: continous2categorical function.
### Aliases: continous2categorical
### Keywords: ~kwd1 ~kwd2

### ** Examples


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



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("continous2categorical", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("count_mat")
### * count_mat

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: count_mat
### Title: creating contingency matrix for categorical data analysis
### Aliases: count_mat
### Keywords: ~kwd1 ~kwd2

### ** Examples



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



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("count_mat", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("crabs")
### * crabs

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: crabs
### Title: Horseshoe crabs data on characteristics of female crabs. The
###   data includes color spine width weight and the number of satelites
###   attracted by the male and female pair
### Aliases: crabs
### Keywords: datasets

### ** Examples

data(crabs)
str(crabs) #gives the summary of the dataset ; 
plot(crabs) 
  



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("crabs", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("crabs2")
### * crabs2

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: crabs2
### Title: contains the data analyzed by Brockmann (1996) and is discussed
###   extensively in Agresti (2002). This is a dataframe
### Aliases: crabs2
### Keywords: datasets

### ** Examples

data(crabs2)
head(crabs2)
str(crabs2)
## maybe str(crabs2) ; plot(crabs2) ...



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("crabs2", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("odds.ratios")
### * odds.ratios

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: odds.ratios
### Title: creating a table with local or global odds ratios
### Aliases: odds.ratios
### Keywords: ~kwd1 ~kwd2

### ** Examples


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



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("odds.ratios", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("plotlocalor")
### * plotlocalor

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: plotlocalor
### Title: plotting fourfold plots for odds ratios
### Aliases: plotlocalor
### Keywords: ~kwd1 ~kwd2

### ** Examples


  #create 2x2 matrix
  m = matrix(c(1,5,13,6), nrow=2)
  plotlocalor(m) # returns a single plot shpwing descriptive summary of odds ratio
  
  #create 4x4 matrix
  m = matrix(c(1,5,13,6,3,5,14,16,36,45,4,6,5,8,9,56), nrow = 4)
  plotlocalor(m) # returns 3x3 plots for the odds ratios of 2x2 subtables in the m matrix
  
  #local odds ratios for crabs data
  #load data
  data("crabs2")
  #subsetting two columns
  data <- crabs2[,c(2,5)] #using color and satelite information of horseshoe crab
  #create matrix
  m = table(data)
  #plot local odds ratios 
  plotlocalor(m)
  
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



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("plotlocalor", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
graphics::par(get("par.postscript", pos = 'CheckExEnv'))
### * <FOOTER>
###
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
