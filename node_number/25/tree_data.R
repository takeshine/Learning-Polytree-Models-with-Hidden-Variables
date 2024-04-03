# Libraries
library(igraph)  # Graph plotting and analysis library
library(Matrix)  # Matrix operations library
library(kernlab) # Kernel-based Machine Learning Library
library(bnlearn) # Bayesian Network Learning library
library(pcalg)   # Causal structure learning and causal inference library


# Function pruferwithskeleton
pruferwithskeleton <- function(k, h){
  # Check if the number of hidden nodes is greater than the total number of nodes.
  if(h > k){
    stop("Number of hidden nodes (h) cannot exceed total number of nodes (k)")
  }
  
  # If the total number of nodes is greater than 2.
  if(k > 2){
    # Generate a sequence of k-2 random numbers between 1 and k.
    P_orig <- ceiling(runif(k-2, min = 1, max = k))
    P <- P_orig  # Make a copy of the generated sequence
    V_orig <- 1:k # Generate a sequence of k numbers from 1 to k
    V <- V_orig   # Make a copy of the generated sequence
    
    # Initialize the adjacency matrix and the skeleton matrix.
    adj_matrix <- Matrix(matrix(numeric(k * k), ncol = k),sparse=TRUE)
    adj_matrixs <-Matrix(matrix(numeric(k * k), ncol = k),sparse=TRUE) 
    
    # Loop through k-2 iterations to populate the adjacency matrix and the skeleton matrix.
    for(i in 1:(k-2)){
      complement <- setdiff(V, P)
      v_0 <- min(complement)
      V <- setdiff(V, v_0)
      adj_matrixs[which(V_orig == v_0), which(V_orig == P_orig[i])] <- adj_matrixs[which(V_orig == P_orig[i]), which(V_orig == v_0)]<-1
      
      m<-rbinom(1,1,0.5)
      if(m==0){
        adj_matrix[which(V_orig == v_0), which(V_orig == P_orig[i])] <- 1
      }
      else{
        adj_matrix[which(V_orig == P_orig[i]), which(V_orig == v_0)] <- 1
      }
      P <- P[2:length(P)]
    }
    adj_matrixs[which(V_orig == V[1]), which(V_orig == V[2])]<-adj_matrixs[which(V_orig == V[2]), which(V_orig == V[1])]<-1
    m<-rbinom(1,1,0.5)
    if(m==0){
      adj_matrix[which(V_orig == V[1]), which(V_orig == V[2])] <- 1
    }
    else{
      adj_matrix[which(V_orig == V[2]), which(V_orig == V[1])] <- 1
    }
  }
  else{
    adj_matrixs<-matrix(c(0,1,1,0),nrow=2,byrow=TRUE)
    m<-rbinom(1,1,0.5)
    if(m==0){
      adj_matrix<-matrix(c(0,1,0,0),nrow=2,byrow=TRUE)
    }
    else{
      adj_matrix<-matrix(c(0,0,1,0),nrow=2,byrow=TRUE)
    }
  }
  
  # Add hidden nodes
  hidden_nodes <- integer(0) # Initialize empty vector for storing hidden nodes
  potential_hidden_nodes <- V_orig # Initialize potential hidden nodes as all nodes
  
  
  condition_one <- integer(0)
  condition_two <- integer(0)
  # While the number of hidden nodes is less than h, attempt to add more hidden nodes
  while(length(hidden_nodes) < h){
    if(length(potential_hidden_nodes) == 0) break # Add this line to check if potential_hidden_nodes is empty
    
    # Randomly select a node from the potential hidden nodes
    node <- sample(potential_hidden_nodes, 1)
    
    # Remove the selected node from the pool of potential hidden nodes
    potential_hidden_nodes <- setdiff(potential_hidden_nodes, node)
    
    # Get the children and parents of the node in the adjacency matrix
    children <- which(adj_matrix[node,] == 1)
    parents <- which(adj_matrix[,node] == 1)
    
    # Check if the selected node meets the conditions to be a hidden node
    if(length(children) >= 2 && length(parents) == 1 && !(parents %in% hidden_nodes)){
      # The node has exactly one parent, and the parent is not hidden
      hidden_nodes <- c(hidden_nodes, node)
      condition_one <- c(condition_one, node)
    } else if(length(children) == 2 && length(parents) == 0){
      
      hidden_children_count <- sum(children %in% hidden_nodes)
      if(hidden_children_count < 1){
        hidden_nodes <- c(hidden_nodes, node)
        condition_two <- c(condition_two, node)
      }
    }
  } 
  ret <- list(Directed = adj_matrix, Skeleton = adj_matrixs, Hidden = hidden_nodes,  ConditionOne = condition_one, ConditionTwo = condition_two)
  return(ret)
}

#set number of nodes and hidden nodes
k <- 25
# k <- sample(30:35, 1) # Randomly select k between 26 and 35, including 26 and 35
h <- floor(k / 3) # Set h as the floor of k divided by 3
n <- 1000
# Please input number nodes first and the number of hidden nodes
# Generate the polytree with hidden nodes

result <- pruferwithskeleton(k, h)
while(length(result$ConditionOne) < 2 || length(result$ConditionTwo) < 2) {
  result <- pruferwithskeleton(k, h)
}

# Get original labels of non-hidden and hidden nodes
non_hidden_nodes <- setdiff(1:k, result$Hidden)

# Create new labels for non-hidden and hidden nodes
new_labels_non_hidden <- seq_along(non_hidden_nodes)
new_labels_hidden <-  seq_len(length(result$Hidden)) + length(non_hidden_nodes)
hidden_nodes <- new_labels_hidden

# Generate graph
g_directed <- graph_from_adjacency_matrix(result$Directed, mode = "directed")

# Replace old labels with new ones
node_label_map <- c(setNames(new_labels_non_hidden, non_hidden_nodes), setNames(new_labels_hidden, result$Hidden))

# Apply new labels
V(g_directed)$name <- node_label_map[as.character(1:nrow(result$Directed))]

# Assign color based on if nodes are hidden or not
V(g_directed)$color <- ifelse(V(g_directed)$name %in% as.character(node_label_map[as.character(result$Hidden)]), "pink", "lightblue")

layout <- layout_nicely(g_directed)
layout <- layout * 6

# Plot with layout
plot(g_directed, layout = layout, vertex.label=V(g_directed)$name
     , vertex.size=8
     , edge.arrow.size=0.4
     , vertex.color=V(g_directed)$color
     , vertex.label.color = "black" # set the color of labels as black
     , vertex.label.cex =1)


E<-get.edgelist(g_directed)
original_edge <- get.edgelist(g_directed)
e<-dim(E)[1]  
#Number of edges
E<-matrix(as(E,"numeric"),ncol=2)                                   #Edge list


#Initialize the error vector with a matrix of 0s.
epsilon <- matrix(numeric(n*k), nrow = n, ncol = k) 
# Set standard deviation to a random number from uniform distribution between 0.5 and 5.
for(l in c(1:k)){
  a<-runif(1,-10,-1)
  s<-runif(1,1,10)
  # Generate 'n' numbers from a normal distribution with mean 0 and standard deviation 'sd'.
  epsilon[,l] <-runif(n,a,s)-0.5*(a+s)
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


W<-DAGgraphtolambda(E,k,g_directed,lambda)                          #Coefficient matrix
x<-t(W%*%t(epsilon))
# Assuming x is your data matrix


# Check if there are hidden nodes
if(length(hidden_nodes) > 0){
  # Remove hidden node columns from the data matrix
  x <- x[, -hidden_nodes]
}

# Assuming `x` and `original_edge` are the objects you want to return
result_list <- list(x_data = x, original_edge_data = original_edge)

# Return the list
result_list