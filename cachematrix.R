## Put comments here that give an overall description of what your
## functions do

## A wrapping over the matrix
## It's like an objects with its getters and setters and all it does is 
## allowing to get and set values for the initial matrix and an inverse one

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
## The function is intended for more optimal calculating of an inverse matrix.
## It checks whether the inverse value was calculated (and thus saved to cache) and returns it.
## If not, it calculates it, saves to the cache and returns it.

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


#Here is some checking with a solvable matrix

m <- matrix(c(3, 3.2, 3.5, 3.6), 2, 2)
obj <- makeCacheMatrix(m)
print(obj$get()) #initial matrix
print(obj$getInverse()) #initial cache value
print(cacheSolve(obj)) #calling cach solve
print(obj$getInverse()) #new cache value
