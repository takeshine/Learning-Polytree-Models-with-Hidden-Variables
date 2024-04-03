# Libraries
library(igraph)  # Graph plotting and analysis library
library(Matrix)  # Matrix operations library
library(kernlab) # Kernel-based Machine Learning Library
library(bnlearn) # Bayesian Network Learning library
library(pcalg)   # Causal structure learning and causal inference library


E <- matrix(c(
  "1", "20",
  "2", "20",
  "3", "6",
  "19", "6",
  "19", "7",
  "4", "7",
  "5", "22",
  "20", "22",
  "20", "10",
  "20", "23",
  "6", "23",
  "8", "11",
  "21", "11",
  "21", "24",
  "22", "24",
  "22", "25",
  "9", "25",
  "9", "18",
  "23", "12",
  "23", "13",
  "24", "14",
  "24", "15",
  "25", "16",
  "25", "17"
), ncol=2, byrow=TRUE)

# Create graph from edge list
g <- graph_from_edgelist(E, directed=TRUE)  # Set directed=TRUE if it's a directed graph

# Define colors for each vertex
V(g)$color <- ifelse(V(g)$name %in% c("19", "20", "21", "22", "23", "24", "25"), "pink", "lightblue")

# Plot the graph with specified vertex colors and labels
plot(g, vertex.label.cex=0.8, vertex.size=8, edge.arrow.size=0.35, vertex.color=V(g)$color)



e<-dim(E)[1]  
#Number of edges
E<-matrix(as(E,"numeric"),ncol=2)                                   #Edge list


n <- 100000
k <- 25


#Initialize the error vector with a matrix of 0s.
epsilon <- matrix(numeric(n*k), nrow = n, ncol = k) 
# Set standard deviation to a random number from uniform distribution between 0.5 and 5.
for(l in c(1:k)){
  sd <- runif(1, 4, 6)
  # Generate 'n' numbers from a normal distribution with mean 0 and standard deviation 'sd'.
  epsilon[,l] <- rnorm(n, mean = 0, sd = sd) 
}

# Here we generate lambda
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

#Takes as input a graph g, and vector of edge-coefficients lambda, the size of the graph k and the list of oriented edges E (for DAGS)
#Returns the weight matrix L
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


W<-DAGgraphtolambda(E,k,g,lambda)                          #Coefficient matrix
x<-t(W%*%t(epsilon))
# Assuming x is your data matrix

# Drop columns corresponding to nodes 19 through 25
x <- x[, -c(19:25)]

file_path <- paste0("/Users/takeshine/Desktop/sample_size/data/data_20.csv")
write.csv(x, file_path, row.names = FALSE)



