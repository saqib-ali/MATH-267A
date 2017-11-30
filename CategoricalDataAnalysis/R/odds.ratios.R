odds.ratios <-
function(m, type = "local") {
  nr <- nrow(m)
  if (nr < 2) stop("number of rows is less than two")
  nc <- ncol(m)
  if (nc < 2) stop("number of columns is less than two")
  if (length(type) > 1) stop("only one type is allowed")
  
  opts <- c("local", "global")
  type <- pmatch(type, opts)
  if (is.na(type)) stop("only \"local\" or \"global\" allowed for type")
  
  result <- matrix(NA, nrow = nr - 1, ncol = nc - 1)
  
  if (type == 1)
    for (i in 1:(nr - 1))
      for (j in 1:(nc - 1))
        result[i, j] <- m[i, j] * m[i + 1, j + 1] / (m[i, j + 1] * m[i + 1, j])
  
  if (type == 2)
    for (i in 1:(nr - 1))
      for (j in 1:(nc - 1)) {
        num <- as.numeric(sum(m[1:i, 1:j])) * as.numeric(sum(m[(i+1):nr, (j+1):nc]))
        den <- as.numeric(sum(m[1:i, (j+1):nc])) * as.numeric(sum(m[(i+1):nr, 1:j]))
        result[i, j] <- num / den
      }
  
  result
}
