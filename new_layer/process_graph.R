processGraph <- function(g_directed, k, hidden_nodes = numeric()) {
  library(igraph)
  
  E <- get.edgelist(g_directed)
  original_edge <- get.edgelist(g_directed)
  e <- dim(E)[1]  # Number of edges
  E <- matrix(as.numeric(E), ncol = 2)  # Edge list
  
  n <- 1000000  # Example, replace with actual logic or value
  
  epsilon <- matrix(numeric(n * k), nrow = n, ncol = k)
  
  for(l in c(1:k)){
    a<-runif(1,-10,-1)
    s<-runif(1,1,10)
    # Generate 'n' numbers from a normal distribution with mean 0 and standard deviation 'sd'.
    epsilon[,l] <-runif(n,a,s)-0.5*(a+s)
  }
  
  lambda <- numeric()
  
  for(l in c(1:e)){
    m<-rbinom(1,1,0.5)
    if(m==0){
      lambda[l]<-runif(1,-1,-0.3)
    }
    else{
      lambda[l]<-runif(1,0.3,1)
    }
  }
  
  
  DAGgraphtolambda<-function(O,k,g,lambda){
    e<-dim(E)[1]
    Lambda<-matrix(0,k,k)
    I<-diag(k)
    for(i in c(1:e)){
      Lambda[O[i,2],O[i,1]]<-lambda[i]
    }
    L<-solve(I-Lambda)
    return(L)
  }
  
  W<-DAGgraphtolambda(E,k,g_directed,lambda)    # Coefficient matrix
  x<-t(W%*%t(epsilon))
  
  if(length(hidden_nodes) > 0){
    x <- x[, -hidden_nodes]
  }
  
  result_list <- list(x_data = x, original_edge_data = original_edge)
  return(result_list)
}
