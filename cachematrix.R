## Coursera - R Programming - Asignment 2
## This file has a pair of functions that cache the inverse of a matrix.


## makeCacheMatrix() function creates a special "matrix" object that 
## can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

  k <- NULL
  set_m <- function(y){
    x <<- y
    k <<- NULL
    
  }
  
  get_m <- function() x
  ## calculate the inverse of the matrix
  set_m_inv <- function(solve) k <<- solve
  get_m_inv <- function() k
  list(set_m = set_m, get_m = get_m, set_m_inv = set_m_inv, get_m_inv = get_m_inv)
  
}


## cacheSolve() function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been 
## calculated (and the matrix has not changed), then the cachesolve 
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  
  m <- x$get_m_inv()
  
  ## if inverse of the passed matrix is in cache, retrieve it and return
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  
  ## if inverse is not in the cache, compute the inverse of the matrix returned by makeCacheMatrix() and return
  matrix <- x$get_m()
  m <- solve(matrix, ...)
  x$set_m_inv(m)
  m
  
  
}
