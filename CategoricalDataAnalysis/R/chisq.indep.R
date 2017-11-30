chisq.indep <-
function(m, level = 0.05, digits = 4, print = TRUE) {
  r.sum <- rowSums(m) #works faster than apply
  c.sum <- colSums(m)
  n <- sum(m)
  
  exp.ct <- outer(r.sum, c.sum, "*")/n        # to get expected counts
  res <- m - exp.ct
  p.res <- res / sqrt(exp.ct)                 # for Pearson residual
  X.sq <- sum(p.res^2)                        # for Pearson statistic
  G.sq <- 2* sum(m * (log(m) - log(exp.ct)))  # for LR statistic
  df <- (nrow(m) - 1) * (ncol(m) - 1)
  c.val <- qchisq(level, df = df, lower.tail = FALSE)
  
  # standardized Pearson residual
  est.se <- sqrt(exp.ct * outer((1-r.sum/n), (1-c.sum/n), "*"))
  s.res <- res / est.se                     
  
  if (print) { 
    cat("Chi-squared test of independence\n")
    cat("  Level = ", level, ", df = ", df, ", critical value = ", round(c.val, digits), 
        "\n", sep = "")
    cat("  X-squared = ", round(X.sq, digits), "\n", sep = "")
    cat("  G-squared = ", round(G.sq, digits), sep = "")
    
  }
  invisible(list(X.sq = X.sq, df = df, expected = exp.ct, 
                 pearson.res = p.res, std.res = s.res))
}
