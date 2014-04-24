## Solution to Programming Assignment 2 (Caching the Inverse of a Matrix)

## This function creates a special "matrix" object that can cache its inverse.
##
## The structure of this object is a vector of four functions:
## (set, get, setInverse, getInverse)

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setInverse <- function(i) inverse <<- i
  getInverse <- function() inverse
  c(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}


## This function calculates the inverse of a matrix
## using the "solve" function, and caching the result
##
## Note that we do not allow extra parameters
## because those are not reflected in the cache
cacheSolve <- function(x) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getInverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data)
  x$setInverse(data)
  data
  
}
