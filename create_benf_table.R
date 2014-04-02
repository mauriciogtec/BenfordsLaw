benf_table <- function(vec, d, N) {
  aux.table <- table(benford)
  tabla <- vector('numeric',10)
  names(tabla) <- 0:9
  for (d in 0:9){
    if (d %in% names(aux.table)){
      tabla[d+1] <- aux.table[which(names(aux.table)==d)]
    }
  }
  if (d==1) tabla <- tabla[-1]
  prob <- as.vector(prop.table(tabla)) 
  
  benford.prob <- vector("numeric",10)
  for (i in 0:9){
    benford.prob[i+1] <- sum(log(1+1/(10*(10^(d-2)):(10^(d-1)-1)+i),10))  
  }
  if (d==1) 
    benford.prob <- benford.prob[-10] 
  
  expected <- N*benford.prob
  
  create.benf.table <- data.frame('Frecuencia'=as.vector(tabla),'Probabilidad'=round(prob,3),
                      "Frecuencia.esp"=round(expected,0),
                      "Benford"=round(benford.prob,3))
  if (d != 1) row.names(Tabla) <- 0:9
  return(create.benf.table)
}