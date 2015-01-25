## These functions provide functionality to cache the inverse of a matrix,
## so that it doesn't have to be re-computed.  A matrix should first be
## stored using makeCacheMatrix.  The inverse should be computed using cacheSolve,
## which will store the inverse in the object return by makeCacheMatrix.
## Here is an example of how to use these functions:
##	m <- matrix( c(1,2,3, 3,2,1, 0,1,1), 3, 3)
##	z <- makeCacheMatrix(m)
##	z$getinverse()
##	cacheSolve(z)
##	z$getinverse()



## This creates a list of functions for setting/getting
## a matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(x1) {
    x <<- x1
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inv1) inv <<- inv1
  getinverse <- function() inv

  ## The four functions are stored/returned in this list
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



## This function accepts objects created by makeCacheMatrix.
## It returns the inverse, if it already exists.  Otherwise it computes it,
## stores it, and then returns it.

cacheSolve <- function(x, ...) {
  ## If inverse is already computed, return it
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting inverse of matrix")
    return(inv)
  }

  ## Otherwise, compute/store/return the inverse
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  ## Return a matrix that is the inverse of 'x'
  inv
}
