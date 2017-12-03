plotlocalor <-
function(m, col = c("azure4", "aquamarine4")) {
  nr <- nrow(m)
  if (nr < 2) stop("number of rows is less than two")
  nc <- ncol(m)
  if (nc < 2) stop("number of columns is less than two")
  
  par(mfrow = c(nr - 1, nc - 1))
  for (i in 1:(nr - 1))
    for (j in 1:(nc - 1)) { 
      fourfoldplot(m[i:(i+1), j:(j+1)], color = col)
    }
}
