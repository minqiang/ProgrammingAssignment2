## The overall purpose of this file is to provide a cached version
##    of taking the inverse of a matrix. If the inverse of a matrix
##    is already computed, then we simply take the cached result. 
##    Only when the inverse has not been computed, will we compute
##    the inverse.
##
## This file contains two functions:
## 1. makeCacheMatrix(). 
## 2. cacheSolve(). 
##
## Example run:
##   x <- makeCacheMatrix(diag(7:10))
##   cacheSolve(x)  
##   ## first run, it will compute the inverse using solve()
##   cacheSolve(x)  
##   ## second run, it will retrieve the inverse using x$getinverse() 
##
## I modified the program a little bit so that the program 
## can also compute inverse for atomic items
##  Example: 
##   cacheSolve(diag(7:10))
##

## This function takes a matrix object and set 
##          up a wrapper for this object which contains 4 member 
##          functions: set, get, setinverse, and getinverse
# 
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function retruns the inverse of a wrapped 
##          matrix object which can be constructed from the 
##          function makeCacheMatrix().
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  if(is.atomic(x)) { 
    ## I added this so program will not fail if called with atomic
    ## matrices
    inv <- solve(x)
  }  
  else  {
    inv <- x$getinverse()
    if(!is.null(inv)) {
      message("getting cached data")
      return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
  }
  
  inv
}


