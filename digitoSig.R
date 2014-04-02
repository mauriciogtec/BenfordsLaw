digitoSig <- function(number, d=1){
  # Returns the d-th significant digit of a numerical value
  return(as.numeric(substr(number,d,d)))
}