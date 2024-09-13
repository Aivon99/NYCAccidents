GraphReformat <- function(A,D){
N <-  length(A[1,]) 

vectD <- vector(length = N)
for(i in 1:N){
  vectD[i] <- D[[i]][i] 
}

node1 <- c()
node2 <- c()
n_edges <- 0  
    
for(i in 1:(N-1)){
  for(j in (i+1):(N)){
    if(A[i,j] == 1){
      node1 <- c(node1, i)
      node2 <- c(node2, j)
      
      n_edges <- n_edges +1
    }
  }
}  

return(list(N = N, node1 = node1, node2 = node2, n_edges = n_edges))
}