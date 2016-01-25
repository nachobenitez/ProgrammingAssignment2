## This function takes a matrix and returns a list
## of 4 objects, including that matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInv <- function(hello) inv <<- hello
  getInv <- function() inv
  list(set = set, get = get,
       setInv = setInv, getInv = getInv)
}

## This function computes the inverse of a matrix
## only if it cannot be cached from memory.

cacheSolve <- function(x, ...) {
  inv <- x$getInv()
  if(!is.null(inv)){
    message("getting cached data...")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data,...)
  x$setInv(inv)
  inv
}
