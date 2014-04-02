benford <- function(x, d=1, remove.small=TRUE, fix.size=-1){
  
  # 1. Prepare vector x for computations
  x <- na.omit(x)
  if (remove.small=TRUE)
    x <- x[x%/%(10^(d-1))!=0]
  if (fix.size !=-1)
    x <- sample(x, fix.size)
  N <- length(x)
  
  # 2. Obtain vector of significant digits
  vec <- sapply(x, digito_sig, d=d)
  
}
