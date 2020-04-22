# Invert reduced Laplacian matrix
# Input: an adjacency matrix A of an undirected graph
# Output: the inverted reduced Laplacian matrix
irl = function(A) {
  # number of nodes
  n = dim(A)[1]
  
  # ignore self-edges
  diag(A) = 0
  
  # get Laplacian matrix	
  L = -A
  diag(L) = A %*% rep(1,n)
  
  # get reduced Laplacian matrix by removing first row and column
  L = L[-1,-1]
  
  # invert reduced Laplacian matrix
  L = solve(L)
  
  # add first row and column of all 0s
  L = rbind(0,L)
  L = cbind(0,L)
  
  return(L)
}

# Computes resistance distance matrix
# Input: an adjacency matrix A of an undirected graph
# Output: resistance distance matrix 
resistance = function(A) {
  
  # number of nodes
  n = dim(A)[1]
  
  # invert reduced Laplacian
  L = irl(A)
  
  # compute resistance matrix
  e = rep(1, n)
  R = e %*% t(diag(L)) + diag(L) %*% t(e) - 2*L
  
  return(R)
}



# Computes current-flow closeness centrality 
# Input: an adjacency matrix A of an undirected graph
# Output: a vector with the centrality score for each node 
cfc = function(A) {
  
  # number of nodes
  n = dim(A)[1]
  
  # invert reduced Laplacian
  R = resistance(A)
  
  # compute centralities
  c = 1 / rowMeans(R)
  
  return(c)
}

# Computes current-flow betweenness centrality 
# Input: an adjacency matrix A of an undirected graph
# Output: a vector with the centrality score for each node 
cfb = function(A) {
  
  # number of nodes
  n = dim(A)[1]
  
  # invert reduced Laplacian
  R = resistance(A)
  
  # compute centralities
  f = 0
  b = rep(0,n)
  for (i in 1:n) {
    #compute neighborhood of i
    nei = which(A[i,] != 0)
    for (s in 1:(n-1)) {
      for (t in (s+1):n) {
        if ((s == i) | (t == i)) {
          f = 1
        } 
        else {  
          x = A[i,nei]
          y = abs((R[s,i] - R[t,i]) - (R[s,nei] - R[t,nei]))
          f = (x %*% y) / 4 
        }  
        b[i] = b[i] + f
      }
    }
    b[i] = 2 * b[i] / (n * (n-1)) 
  }
  
  return(b)
}


# percolation removes nodes from a graph and computes 
# the size of the giant connected component
# INPUT
# g: graph to percolate
# size: number of nodes to remove 
# d: removal vector
# OUTPUT
# giant: a vector with sizes of giant components when nodes are removed
percolate = function(g, size, d) {
  
  giant = vector()
  
  # initial size of giant component
  c = components(g)
  giant[1] = max(c$csize)
  
  names(d) = 1:length(d)
  d = sort(d, decreasing=TRUE)
  vital = as.integer(names(d[1:size]))
  
  for (i in 1:size) {
    c = components(delete_vertices(g, vital[1:i]))
    giant[i+1] = max(c$csize)
  }
  
  return(giant)
  
}
