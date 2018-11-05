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


#check

m <- matrix(c(3, 3.2, 3.5, 3.6), 2, 2)
obj <- makeCacheMatrix(m)
print(obj$get()) #initial matrix
print(obj$getInverse()) #initial cache value
print(cacheSolve(obj)) #calling cach solve
print(obj$getInverse()) #new cache value
