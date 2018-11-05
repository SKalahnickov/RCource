## Put comments here that give an overall description of what your
## functions do

## A wrapping over the matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) i <<- inverse
  getInverse <- function() i
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## Cache solver

cacheSolve <- function(x, ...) {
    m <- x$getInverse()
    if(!is.null(m)) {
      return(m)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setInverse(i)
    i
}