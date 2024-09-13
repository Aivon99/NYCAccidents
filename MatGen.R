library(sf)
# 
# Matr_adiacenza <- function(CountObj) {
#   N <- length(CountObj)  
#   
#   A <- matrix(nrow = N, ncol = N)
#   
#   for(i in 1:N) {
#     for(j in 1:N) {
#       if(i == j) {
#         A[i, j] <- 0
#       } else {
#         
#         dist <- st_distance(CountObj[i,], CountObj[j, ])
#         
#        
#         if (dist == 0) {
#           A[i, j] <- 1
#         } else {
#           A[i, j] <- 0
#         }
#       }
#     }
#   }
#   
#   return(A)
# }


##Generare matrice A dato colonna poligoni




Matr_adj <- function(CountObj){
  N <- length(CountObj)  
  
  A <- matrix(nrow = N, ncol = N)
  
  for(i in 1:N) {
    for(j in i:N) {
      if(i == j) {
        A[i, j] <- 0
      } else {
        dist <- as.numeric(st_distance(CountObj[i], CountObj[j ]))
        
        if (dist == 0) {
          A[i, j] <- 1
          A[j, i] <- 1
        } else {
          A[i, j] <- 0
          A[j, i] <- 0
        }
      }
    }
  }
  return(A)
}
#da verificare correttezza, decisamente non ottimizzato, tanto valeva farlo in java



Matr_D <- function(matA){
  N <- nrow(matA)
  matD <- matrix(0, nrow= N, ncol = N)
  Filler <- colSums(matA)
  for(i in 1:N){
  
    matD[i,i] <- Filler[i]
    
  }
  
  return(matD)
}


D_diag <- function(matD){
  vect <- numeric(length(matD[1,]))
  
  for(i in 1:length(matD[1,]))
    vect[i] <- matD[i,i]

return(vect)  
  
}



