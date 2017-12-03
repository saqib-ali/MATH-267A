chisq.indep <-
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
