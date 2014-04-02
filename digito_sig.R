digito_sig <- function(x, d) {
  # Returns the d-th significant digit of a numerical value
  x <- abs(x)
  if (x == 0) {
    return(0)
  } else {
    if (x >= 10) {
      while(x >= 10) {
        x <- x/10
      }   
    }
    if (x < 1) {
      while(x < 1) {
        x <- x*10
      }
    }      
    return(trunc(((10^(d-1))*x)%%10))
  }
}