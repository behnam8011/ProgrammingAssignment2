## This script contains two function that help cache
## and restore the inverse of a matrix

## This function will return a speciual list of four 
## functions that will enable us to get/set both the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x<<-y
    inv<<-NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv<<- inverse
  getInverse <- function() inv
  list(get = get, set = set, getInverse = getInverse, setInverse = setInverse)
}


## This function returns the inverse of the matrix. 
## If inverse is cached, the function returns the cached value rather than 
## recomputing the inverse

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  a <- solve(data,...)
  x$setInverse(a)
  a
  
}
