## Put comments here that give an overall description of what your
## functions do

#This file is made up of two functions that are designed to 
#calculate and cache the inverse of a matrix

#makeCacheMatrix: This function creates a special "matrix" object 
#that can cache its inverse.

#cacheSolve: This function computes the inverse of the special "matrix" 
#returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed), 
#then the cachesolve should retrieve the inverse from the cache.
#Computing the inverse of a square matrix can be done with the solve function in R. For example, if X is a square invertible matrix, then solve(X) returns its inverse.

## Write a short comment describing this function

#This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  # The makeCacheMatrix function creates a list made up of four functions
  #  - set, get, setmatrix and getmatrix
  # These functions will either read or write a matrix, or read or write the inverse
  # This function should be used in conjuction with the cachesolve functioninverse <- NULL
  #1- set the matrix, and null the inverse
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  #2 - return the matrix
  get <- function() x
  #3 - calculate the inverse of the matrix 
  setmatrix <- function(solve) inverse <<- solve
  #4 - return the inverse
  getmatrix <- function() inverse
  #output the four functions in a list
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}


## Write a short comment describing this function
# This function computes the inverse of the special "matrix" 

cacheSolve <- function(x, ...) {
  # This function computes the inverse of the special "matrix" 
  # returned by makeCacheMatrix above. 
  # If the inverse has already been calculated (and the matrix has not changed), 
  # then the cachesolve should retrieve the inverse from the cache.
  
  #call the getmatrix function to see if the inverse is cached
  #return the inverse if cached
  inverse <- x$getmatrix()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  #if the matrix has not been cached then get the matrix data  
  data <- x$get()
  #calculate the inverse of the matrix
  inverse <- solve(data, ...)
  #store the inverse 
  x$setmatrix(inverse)
  #return the inverse
  inverse
}
