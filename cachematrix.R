
### Caching the Inverse of a Matrix ###

## Matrix inversion is usually a costly computation and there may be some benefit 
## to caching the inverse of a matrix rather than computing it repeatedly. 
## The following pair of functions cache the inverse of a matrix. Computing 
## the inverse of a square matrix can be done with the solve function in R.


## The function makeCacheMatrix creates a special "matrix" object that can
## cache its inverse, which is really a list containing a function to: 
## (1) set the value of the matrix, (2) get the value of the matrix, 
## (3) set the value of the inverse, (4) get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  a <- NULL
  set <- function(b) {
    x <<- b
    a <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) a <<- inverse
  getinverse <- function() a
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated (and the matrix 
## has not changed), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  a <- x$getinverse ()
  if(!is.null(a)) {
    message("getting cached data")
    return(a)
  }
  data <- x$get()
  a <- solve(data, ...)
  x$setinverse(a)
  a
}
