## Following functions calculate matrix inverse and cache it for the 
## future uses.

## Creates cache-matrix object for storing matrix and its cached inverse.
## x - invertible matrix (square)
## Returns a list of set, get, setInverse and getInverse functions

makeCacheMatrix <- function(x = matrix()) {
  inverse = NULL
  
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setInverse <- function(i) inverse <<- i
  getInverse <- function() inverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Returns inverse of a matrix. Result is cached the first time it's calculated,
## and used for future calls.
## x - a cache-matrix, result of makeCacheMatrix
## ... - additional arguments to be passed to solve()


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
