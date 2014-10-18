## These functions implement calculation of the inverse of a matrix and caching the results.

## This function creates a special "matrix" object that can cache its inverse.
## Returned value is a special vector, which contains functions to set and get the matrix
## and set and get the result of inversion
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setInverse <- function(newInverse) inverse <<- newInverse
  getInverse <- function() inverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## This function takes as a parameter result of makeCacheMatrix. This function returns the
## inverse matrix of the given inversible matrix. If the matrix has been cached already
## this method does not calculate the inversion again.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getInverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setInverse(inverse)
  inverse
}