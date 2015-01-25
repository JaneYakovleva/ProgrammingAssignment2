## Functions that cache the inverse of a matrix

## Creates a special "vector", which is really a list containing a function to
##  set the value of the matrix
##  get the value of the matrix
##  set the inverse of the matrix
##  get the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(m) {
    x <<- m
    inverse <<- NULL
  }
  get <- function() x
  setInverse <- function(m) inverse <<- m
  getInverse <- function() inverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Computes the inverse of the special "matrix" returned by makeCacheMatrix above.
cacheSolve <- function(x, ...) {
  inverse <- x$getInverse()
  if(!is.null(inverse)) {
    message("getting inverse matrix")
    return(inverse)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m     
}
